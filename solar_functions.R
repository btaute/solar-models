# Functions that integrate solar energy estimating functions written in Python
# with inputs to those functions written in R (often coming from Shiny Apps)

# Set up the environment ----

# Use tidyverse functions to organize data
require(dplyr)
require(tibble)
require(lubridate)
require(readr)

# Set up a virtual environment with all the required python modules
require(reticulate)
use_virtualenv('solar_env')

for (module in c('pandas', 'matplotlib', 'numpy', 'pytz', 'pvlib',
                  'pvfactors', 'types')) {
  
  if (!py_module_available(module)) {
    
   virtualenv_install('solar_env', module)
  
  }

}

# Assuming solar_functions.py will be in same directory
source_python('solar_functions.py')

# Functions loaded from solar_functions:
# get_effective_irradiance:  Converts ground irradiance into plane-of-array
# get_modelchain: Sets up a "model chain" object
# get_psm3: Gets NSRDB PSM3 resource data and metadata
# get_results: Sets typical result metrics on modelchain (NCF, AEP)
# get_system: Sets up a PVSystem object
# run_plant_model: Models custom plant losses
# run_pvwatts_ac: Models custom AC losses and PVWatts AC conversion model
# run_pvwatts_dc: Models custom dc losses and PVWatts DC conversion model


get_project_inputs <- function(dc_capacity, ac_capacity, racking, lat,
                               poi_capacity, interconnection_voltage,
                               degradation_year, custom_inputs) {
  # Prepares a list of inputs to provide the solar models
  
  # Parameters:
  # dc_capacity: Nominal DC Capacity at STC Conditions (front of module only)(W)
  # ac_capacity: Inverter-rated Capacity at site design temperature (Wac)
  # racking: String from list(tracker, ground-mount, rooftop, canopy)
  # lat: degreee decimal of project latitude; used here as default fixed-tilt
  #   angle
  # poi_capacity: The max amount of power that can be injected into grid (W)
  # interconnection_voltage: String from list(high, medium, low)
  # degradation_year: Current Year of module operation
  # (1 = 1st year, 2 means year1 degradation + .5*year2)
  # custom_inputs: a list of solar model inputs that will override the defaults
  
  # ----------
  
  # Default Inputs Regardless of Tracker Choice  ----
  
  default_inputs <- list(
    
    # DC losses (.015 = 1.5%)
    dc_cabling = .015,
    module_quality = -.003,
    mismatch = .01,
    lid = .015,
    # warrantied at end of year
    degradation_firstyear = .02,
    # linear warranty used in years 2 onward
    degradation_annual = .0045,
    
    # Bifacial losses (.05 = 5%)
    rear_shading = .05,
    rear_mismatch = .1,
    
    # AC losses (.99 = 99%)
    inverter_efficiency_peak = .99,
    ac_collection = .005,
    transmission_loss = .003,
    power_factor = 1,
    
    # Plant losses (.01 = 1%)
    # Plant losses get applied to AEP but not 8760,
    # since we can't model them in timeseries accurately
    # (don't know when an inverter will fail)
    availability_loss = .01,
    
    # Module Parameters
    temp_coeff = -.0037,
    bifaciality = 0.75,
    module_efficiency = 0.19,
    ashrae_coeff = .03
    
  )
  
  
  # Default Inputs Dependent on Tracker Choice ----
  
  if (racking == 'tracker') {
    
    default_inputs <- c(default_inputs, list(
      # degrees
      axis_tilt = 0,
      max_angle = 60,
      backtrack = TRUE,
      
      # degrees (180 = South, 90 = East, 0 = North)
      axis_azimuth = 0,
      # fixed-tilt only
      surface_azimuth = NULL,
      surface_tilt = NULL,
      
      gcr = .33,
      # Assuming one module in portrait (meters)
      collector_width = 2,
      
      soiling_loss = .01,
      # Assume panels high enough to shed snow
      snow_loss = 0,
      albedo = .2,
      
      # Default PVSyst temperature model parameters
      temperature_model_parameters = list(
        # 29 for freestanding; 15 for insulated
        'u_c' = 29,
        'u_v' = 0
      )
      
    ))
    
  } else if (racking == 'ground-mount') {
    
    default_inputs <- c(default_inputs, list(
      # tracker only
      axis_tilt = NULL,
      max_angle = NULL,
      backtrack = NULL,
      
      # Tilt is in South direction for PVFactors
      axis_azimuth = 90,
      surface_azimuth = 180,
      surface_tilt = lat,
      
      gcr = .4,
      # Assuming 4 modules in landscape (meters)
      collector_width = 4,
      
      soiling_loss = .015,
      # Assuming benefits of snow cleaning even out snow days at tilt
      snow_loss = 0,
      albedo = .2,
      
      # Default PVSyst temperature model parameters
      temperature_model_parameters = list(
        'u_c' = 29,
        'u_v' = 0
      )
      
    ))
    
    
  } else if (racking == 'canopy') {
    
    default_inputs <- c(default_inputs, list(
      # tracker only
      axis_tilt = NULL,
      max_angle = NULL,
      backtrack = NULL,
      
      # degrees
      axis_azimuth = 90,
      surface_azimuth = 180,
      surface_tilt = 7,
      
      gcr = .8,
      # Assuming 7 modules in portrait (meters)
      collector_width = 14,
      
      soiling_loss = .02,
      # Reduced module tilt angle increases snow loss percentage
      snow_loss = .02,
      albedo = .15,
      
      # Default PVSyst temperature model parameters
      temperature_model_parameters = list(
        'u_c' = 29,
        'u_v' = 0
      )
      
    ))
    
    # Rooftop
  } else {
    
    default_inputs <- c(default_inputs, list(
      # tracker only
      axis_tilt = NULL,
      max_angle = NULL,
      backtrack = NULL,
      
      # degrees
      axis_azimuth = 90,
      surface_azimuth = 180,
      surface_tilt = 10,
      
      gcr = .75,
      # Assume 1 module in landscape (meters)
      collector_width = 1,
      
      soiling_loss = .02,
      snow_loss = .02,
      # Reduced module tilt angle increases snow loss percentage
      albedo = .25,
      
      temperature_model_parameters = list(
        # Less air movement on back of rooftop panels than other racking
        'u_c' = 15,
        'u_v' = 0
      ) 
      
    ))
    
  }
  
  
  # Calculated Inputs ----
  
  # Override Default Inputs Before Calcs
  inputs <- replace(default_inputs, names(custom_inputs), custom_inputs)
  
  # Axis height depends on racking type; all values in meters
  if (racking == 'tracker') {
    
    axis_height <- inputs$collector_width / 2 *
      cos(pi / 180 * inputs$max_angle) + .5
    
  } else if (racking == 'ground-mount') {
    
    axis_height <- inputs$collector_width / 2 *
      cos(pi / 180 * inputs$surface_tilt) + .5
    
  } else if (racking == 'canopy') {
    
    axis_height <- 4
    
    # rooftop
  } else {
    
    axis_height <- inputs$collector_width / 2 *
      cos(pi / 180 * inputs$surface_tilt) + .05
    
  }
  
  module_parameters <- list(
    'pdc0' = dc_capacity,
    'gamma_pdc' = inputs$temp_coeff,
    'bifaciality' = inputs$bifaciality,
    'efficiency' = inputs$module_efficiency,
    'b' = inputs$ashrae_coeff
  )
  
  # Irradiance Losses #
  
  soiling_loss <- 1 - (1 - inputs$soiling_loss) * (1 - inputs$snow_loss)
  
  # DC Losses #
  
  # Degradation:  Linear degrdation and somewhat symmetrical monthly yields,
  # so midyear degradation is applied as constant throughout entire year.
  if (degradation_year == 1) {
    
    degradation_loss <- (inputs$degradation_firstyear - inputs$lid) / 2 +
      inputs$lid
    
  } else { 
    
    # Year1 Degradation + Every Subsequent Year Degradation +
    # 1/2 Current Year Degradation
    degradation_loss <- inputs$degradation_firstyear + (degradation_year - 2) * 
      inputs$degradation_annual + inputs$degradation_annual / 2
    
  }
  
  # Total DC Losses
  dc_losses <- 1 - (1-inputs$module_quality) * (1-degradation_loss) * 
    (1-inputs$mismatch) * (1-inputs$dc_cabling) 
  
  # Bifacial Losses #
  
  bifacial_losses <- 1 - (1-inputs$rear_shading) * (1-inputs$rear_mismatch)
  
  # AC Losses (in W) #
  
  # Padmount Transformer exponential loss function (Steps Low to Med Voltage)
  pmt_rating <- ac_capacity
  pmt_peak_loss <- .007 * pmt_rating
  pmt_constant_loss <- .0013 * pmt_rating
  
  # Main Power Transformer exponential loss function (Steps Med to High Voltage)
  mpt_top_rating <- ac_capacity
  mpt_bottom_rating <- mpt_top_rating * .6
  mpt_peak_loss <- .0017 * mpt_top_rating
  mpt_constant_loss <- .0004 * mpt_top_rating
  
  # Interconnection Voltage Assumption Adjustments to AC Losses
  if (interconnection_voltage != 'high') {
    
    transmission_loss <- 0
    mpt_peak_loss <- 0
    mpt_constant_loss <- 0
  
  }
  
  if (interconnection_voltage == 'low') {
    
    pmt_peak_loss <- 0
    pmt_constant_loss <- 0
  
  }
  
  # Plant Losses #
  
  plant_losses <- 1 - (1-inputs$availability_loss)
  
  # Final Input Override ----
  
  # If a Calculated Input was provided in custom_inputs, 
  # it will override the results of the calcs
  
  inputs <- c(inputs, list(axis_height = axis_height,
                           module_parameters = module_parameters,
                           soiling_loss = soiling_loss,
                           degradation_loss = degradation_loss,
                           dc_losses = dc_losses,
                           bifacial_losses = bifacial_losses,
                           pmt_rating = pmt_rating,
                           pmt_peak_loss = pmt_peak_loss,
                           pmt_constant_loss = pmt_constant_loss,
                           mpt_top_rating = mpt_top_rating,
                           mpt_bottom_rating = mpt_bottom_rating,
                           mpt_peak_loss = mpt_peak_loss,
                           mpt_constant_loss = mpt_constant_loss,
                           transmission_loss = transmission_loss,
                           plant_losses = plant_losses))
  
  
  return(inputs)
  
}


estimate_energy <- function(dc_capacity, ac_capacity, racking, lat, lon,
                            poi_capacity = ac_capacity,
                            interconnection_voltage = 'medium',
                            time_step = 60,
                            degradation_year = 1, name = '',
                            weather = NULL,  utc_offset = NULL,
                            elevation = NULL, nrel_fudge = 1, year = 'tmy',
                            email = NULL, api_key = NULL,
                            custom_inputs = list()) {
  # Estimate the time series production of a solar project.  If a weather
  # profile isn't provided, then an hourly profile from NSRDB PSM3 model will
  # be downloaded.
  
  # Required Parameters:
  # dc_capacity: Nominal DC Capacity at STC Conditions (front of module only)(W)
  # ac_capacity: Inverter-rated Capacity at site design temperature (Wac)
  # racking: String from list(tracker, ground-mount, rooftop, canopy)
  # lat: The latitude of the project (decimal degrees)
  # lon: The longitude of the project (decimal degrees)
  
  # Required Parameters with Defaults:
  # poi_capacity: The max amount of power that can be injected into grid (W)
  # interconnection_voltage: String from list(high, medium, low)
  # time_step: Timestep of weather dataframe, used for results calcs (minutes)
  # degradation_year: Current Year of module operation
  #   (1 = 1st year, 2 means year1 degradation + .5*year2)
  # name: The name of the project, saved in the modelchain instance
  
  # Parameters Requiring a call to get_psm3 if not provided:
  # weather: dataframe with rownames=datetime, columns = ['ghi', 'dni', 'dhi',
  #   'temp_air', 'wind_speed', 'surface_albedo', soiling]
  # utc_offset: int; hours offset from UTC
  # elevation: int; meters above sea-level
  
  # Parameters Required only if calling get_psm3:
  # nrel_fudge: a correction factor to apply to PSM3 irradiance
  # year: A string of the year to get weather data
  # email: The email address associated with an NREL Developer Network API Key
  # api_key: An NREL Developer Network API Key (string)
  
  # Optional Paremeters:                          
  # custom_inputs: a list of solar model inputs that will override the defaults

  # Returns:
  # mc: the model chain used in the energy model
  # output: a dataframe with time series output of the model
  
  # ----------
  
  # Pepar Solar Model Inputs ----
  
  inputs <- get_project_inputs(dc_capacity, ac_capacity, racking, lat,
                               poi_capacity, interconnection_voltage,
                               degradation_year, custom_inputs)
  
  # If site weather or metadata wasn't provided, get them from NSRDB PSM3
  if (is.null(weather) | is.null(utc_offset) | is.null(elevation)) {
    
    # Call NSRDB for TMY
    psm3 <- get_psm3(lat = lat, lon = lon, year = year,
                     time_step = time_step, soiling_loss = inputs$soiling_loss,
                     nrel_fudge = nrel_fudge, email = email, api_key = api_key)
    
    if (is.null(weather)) {
      
      weather <- psm3$weather
      
    }
    if (is.null(utc_offset)) {
      
      utc_offset <- psm3$utc_offset
      
    }
    if (is.null(elevation)) {
      
      elevation <- psm3$elevation
      
    }
  }
  
  # If weather data doesn't have albedo or soiling,
  # turn default inputs into a timeseries.
  if (is.null(weather$surface_albedo)) {
    
    weather$surface_albedo <- inputs$albedo
    
  }
  
  if (is.null(weather$soiling)) {
    
    weather$soiling <- inputs$soiling_loss
    
  }
   
   
  # Call Solar Models ----
  
  system <- get_system(racking = racking, axis_height = inputs$axis_height,
                       collector_width = inputs$collector_width,
                       module_parameters = inputs$module_parameters,
                       temperature_model_parameters =
                         inputs$temperature_model_parameters,
                       axis_azimuth = inputs$axis_azimuth, gcr = inputs$gcr,
                       axis_tilt = inputs$axis_tilt,
                       max_angle = inputs$max_angle,
                       backtrack = inputs$backtrack,
                       surface_tilt = inputs$surface_tilt,
                       surface_azimuth = inputs$surface_azimuth,
                       albedo = inputs$albedo)
  
  mc <- get_modelchain(lat, lon, utc_offset, elevation, name, system,
                       inputs$degradation_loss, inputs$bifacial_losses,
                       inputs$dc_losses, ac_capacity,
                       inputs$inverter_efficiency_peak, 
                       inputs$pmt_peak_loss, inputs$pmt_rating,
                       inputs$pmt_constant_loss, inputs$ac_collection,
                       inputs$mpt_peak_loss, inputs$mpt_bottom_rating,
                       inputs$mpt_constant_loss, inputs$transmission_loss,
                       poi_capacity, inputs$plant_losses, time_step)
  
  mc <- get_effective_irradiance(mc, weather, utc_offset) %>%
    run_pvwatts_dc %>%
    run_pvwatts_ac %>%
    run_plant_model %>%
    get_results
  
  output <- rownames_to_column(mc$plant, 'datetime') %>%
    mutate(datetime = ymd_hms(datetime),
           year = year(datetime),
           month = month(datetime),
           day = day(datetime),
           hour = hour(datetime),
           ghi = weather$ghi,
           fpoa = mc$front_irradiance,
           bpoa = mc$back_irradiance,
           tpoa = mc$effective_irradiance_soiled,
           dc = mc$dc$output,
           ac = mc$ac$output)
  
  return(list('mc' = mc, 'output' = output))
  
}


estimate_multiyear_energy <- function(last_year, first_year = 1,
                                      degradation_annual = .0045,
                                      degradation_firstyear = .02,
                                      dc_capacity, ac_capacity, racking, lat,
                                      lon, poi_capacity = ac_capacity,
                                      interconnection_voltage = 'medium',
                                      time_step = 60, name = '',
                                      weather = NULL,  utc_offset = NULL,
                                      elevation = NULL, nrel_fudge = 1,
                                      year = 'tmy', email = NULL,
                                      api_key = NULL, custom_inputs = list()) {
  # Estimate energy with degradation accumulating over a several year period.
  # Each degradation assessment assumes the same TMY input.
  
  # Multiyear-Specific Parameters:
  # last_year: The last year of the multiyear analysis
  # first_year: The first year of the multiyear analysis
  # degradation_annual: Linear degradation of modules each year after year 1
  # degradation_firstyear: Degradation of module at the end of year 1 (.02 = 2%)
  
  # Passthrough Parameters to estimate_energy():  
  # dc_capacity: Nominal DC Capacity at STC Conditions (front of module only)(W)
  # ac_capacity: Inverter-rated Capacity at site design temperature (Wac)
  # racking: String from list(tracker, ground-mount, rooftop, canopy)
  # lat: The latitude of the project (decimal degrees)
  # lon: The longitude of the project (decimal degrees)
  # poi_capacity: The max amount of power that can be injected into grid (W)
  # interconnection_voltage: String from list(high, medium, low)
  # time_step: Timestep of weather dataframe, used for results calcs (minutes)
  # name: The name of the project, saved in the modelchain instance
  # weather: dataframe with rownames=datetime, columns = ['ghi', 'dni', 'dhi',
  #   'temp_air', 'wind_speed', 'surface_albedo', soiling]
  # utc_offset: int; hours offset from UTC
  # elevation: int; meters above sea-level
  # nrel_fudge: a correction factor to apply to PSM3 irradiance
  # year: A string of the year to get weather data
  # email: The email address associated with an NREL Developer Network API Key
  # api_key: An NREL Developer Network API Key
  # custom_inputs: a list of inputs to override defaults
  
  # Returns:
  # mc: the model chain from the first energy model; the base for each new run
  # output: a dataframe with time series output of the model
  
  # ----------
  
  # Update custom inputs with the degradation inputs
  # Because of the order, any degradation values put in custom_inputs list will
  # take priority.
  custom_inputs = c(custom_inputs, list(degradation_firstyear = .02,
                                        degradation_annual = .0045))
  
  # Run the first year Energy Estimate
  base <- estimate_energy(
    degradation_year = first_year, dc_capacity = dc_capacity,
    ac_capacity = ac_capacity, racking = racking, lat = lat, lon = lon,
    poi_capacity = poi_capacity,
    interconnection_voltage = interconnection_voltage, time_step = time_step,
    weather = weather, utc_offset = utc_offset,
    elevation = elevation, nrel_fudge = nrel_fudge, year = year, email = email,
    api_key = api_key, custom_inputs = custom_inputs)
  
  # Start the return data frame with each year corresponding to each trial year
  # 1901 is year 1, 1920 is year 20...
  full_output <- base$output %>%
    mutate(year = 1900 + first_year,
           datetime = ymd_h(paste(year, month, day, hour)))
  
  # Save the modelchain to iterate upon
  run_mc <- base$mc
  
  # Get the base degradation and dc_losses without degradation to use as inputs
  # to subsequent years
  base_degradation <- base$mc$degradation_loss
  base_dc_losses <- base$mc$dc_losses
  base_dc_lossfactor <- (1 - base_dc_losses) / (1 - base_degradation)
  
  # Run each subsequent year by calling solar models explicitly
  for (i in 1:(last_year-first_year)) {
    
    run_mc$degradation_loss <- base_degradation + i *
      custom_inputs$degradation_annual
    
    run_mc$dc_losses <- 1 - (base_dc_lossfactor * (1 - run_mc$degradation_loss))
    
    run_mc <- run_pvwatts_dc(run_mc) %>%
      run_pvwatts_ac %>%
      run_plant_model
    
    output <- rownames_to_column(run_mc$plant, 'datetime') %>%
      mutate(datetime = ymd_hms(datetime),
             year = 1901 + i,
             month = month(datetime),
             day = day(datetime),
             hour = hour(datetime),
             datetime = ymd_h(paste(year, month, day, hour)),
             ghi = weather$ghi,
             fpoa = run_mc$front_irradiance,
             bpoa = run_mc$back_irradiance,
             tpoa = run_mc$effective_irradiance_soiled,
             dc = run_mc$dc$output,
             ac = run_mc$ac$output)
    
    full_output <- bind_rows(full_output, output)
    
  }
  
  return(list(mc = base$mc, output = full_output))
  
}