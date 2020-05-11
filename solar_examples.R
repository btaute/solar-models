library(tidyverse)
library(lubridate)

source('solar_functions.R')

# Example 1:  Run an Energy Estimate ----

# Load in a weather and site-specific data
weather <- read_csv('sample-data/weather.csv') %>%
    mutate(datetime = mdy_hm(datetime)) %>%
    column_to_rownames(var = 'datetime')

utc_offset <- as.integer(-8)
lat <- 37.45
lon <- -122.45
elevation <- 91

# Run default energy estimate and visualize results
energy <- estimate_energy(dc_capacity = 130e6, ac_capacity = 100e6,
                          racking = 'tracker', lat = lat, lon = lon,
                          weather = weather, utc_offset = utc_offset,
                          elevation = elevation)

print(paste0('NCF = ', round(energy$mc$ncf * 100, 2), '%'))

energy$output %>%
    group_by(month, hour) %>%
    summarise(output = mean(output) / 1e6) %>%
    ggplot(aes(x = hour, y = as.factor(month), fill = output)) +
    geom_tile() +
    scale_fill_gradient(low='navyblue', high="yellow") +
    labs(x = 'Hour', y = 'Month', fill = 'Output (MW)')


# Example 2. Model PV Degradation by batch running energy estimates ----

# Let's do a ground-mounted, fixed-tilt system this time instead of a tracker
multiyear <- estimate_multiyear_energy(last_year = 30, dc_capacity = 12e6,
                                       ac_capacity = 10e6,
                                       racking = 'ground-mount',
                                       lat = lat, lon = lon,
                                       weather = weather,
                                       utc_offset = utc_offset,
                                       elevation = elevation)

multiyear$output %>%
    group_by(year) %>%
    summarise(aep = sum(output)) %>%
    mutate(delta = aep / lag(aep)) %>%
    ggplot(aes(x = year - 1900, y = delta)) +
    geom_path() +
    labs(x = 'Operation Year', y = 'Percent of Prior Year Production')


# Example 3. Customize Default Inputs ----
# See how gcr and albedo affect plant production

result_vector <- rep(0, 20)

i <- 1

for (albedo_rate in c(.2, .4)) {
    
    for (coverage in seq(.31, .5, .02)) {
        
        result <- estimate_energy(dc_capacity = 130e6, ac_capacity = 100e6,
                                  racking = 'tracker', lat = lat, lon = lon,
                                  weather = weather, utc_offset = utc_offset,
                                  elevation = elevation,
                                  custom_inputs = list(albedo = albedo_rate,
                                                       gcr = coverage))
        
        result_vector[i] <- result$mc$aep
        
        i <- i + 1
    }
}

data.frame(albedo_0.2 = result_vector[1:10], albedo_0.4 = result_vector[11:20],
           gcr = seq(.31, .5, .02)) %>%
    gather('key', 'value', -gcr) %>%
    ggplot(aes(x = gcr, y = value / 1e9, color = key)) +
    geom_path() +
    labs(x = 'Ground Coverage Ratio', y = 'Annual Output (GWh)')


# Example 4. Using an API Key ----
# To get an energy estimate using the NREL PSM3 model, fill in the email and
# api_key strings below.

email <- 'INSERT_EMAIL_ADDRESS'
api_key <- 'INSERT_API_KEY'

energy <- estimate_energy(130e6, 100e6, 'tracker', lat = lat, lon = lon, 
                          email = email, api_key = api_key)