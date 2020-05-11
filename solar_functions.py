# Functions that integrate custom modeling procedures with PVLib

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import pytz
import pvlib
from pvlib.pvsystem import PVSystem
from pvlib.location import Location
from pvlib.modelchain import ModelChain
from pvlib.tracking import SingleAxisTracker
from pvfactors.run import run_timeseries_engine
import types


def get_psm3(lat, lon, email, api_key, year = 'tmy', time_step = 60,
             soiling_loss = .02, nrel_fudge = 1):
  '''
  Calls the NREL Solar Resource Database API to pull weather data from their
  Physical Solar Model version 3.  This call provides weather data and meta data
  about the location.
  
  Parameters:
  lat: float.
    site Latitude in Decimal Degrees
  lon: float.
    site Longitude in Decimal Degrees
  email: string.
    the email associated with an NREL Developer Network API key
  api_key: string.
    NREL Developer Network API key
  year: string.
    year of weather data model (tmy returns a typical meteorological year)
    can be any year from 1998 - 2018 or tmy
  time_step: int.
    resolution of the time series model in minutes (30 or 60)
  soiling_loss: float.
    percent loss of irradiance due to soiling of module; applied as a constant
    loss over time series, although a time series model would be better
  nrel_fudge: float.
    NREL's resource data tends to be biased high.  This is a factor that can be
    used to correct this bias.
  
  Returns:
  weather: pandas.DataFrame
    time series weather data with columns ['ghi', 'dni', 'dhi', 'temp_air',
    'wind_speed', 'surface_albedo']
  utc_offset: int.
    hour offset from UTC for local time zone
  elevation: int.
    meters above sea level
  
  '''

  psm3 = pvlib.iotools.get_psm3(lat, lon, api_key, email, names = year,
                                interval = time_step)
  psm3_df = psm3[1]
  
  weather = psm3_df.filter(['GHI', 'DNI', 'DHI', 'Temperature', 'Wind Speed',
                            'Surface Albedo'])
                            
  weather.columns = ['ghi', 'dni', 'dhi', 'temp_air', 'wind_speed',
                     'surface_albedo']
  
  weather.index = psm3_df.index
  
  weather = weather.assign(soiling = soiling_loss)
  
  #  A fudge factor to account for model bias
  weather.ghi = weather.ghi * nrel_fudge
  weather.dni = weather.dni * nrel_fudge
  weather.dhi = weather.dhi * nrel_fudge
  
  # Site Meta Data
  utc_offset = psm3[0]['Time Zone']
  elevation = psm3[0]['Elevation']
  
  return {'weather': weather, 'utc_offset': utc_offset, 'elevation': elevation}
  
  
def get_system(racking, axis_height, collector_width, axis_azimuth, gcr,
               module_parameters, temperature_model_parameters,
               axis_tilt = 0, max_angle = 0, backtrack = False,
               surface_tilt = 0, surface_azimuth = 0, albedo = 0):  
  '''
  Creates a PVSystem instance from PVLib (an input to ModelChain which defines a
  standard set of PV system attributes and modeling functions).
  
  Parameters:
  racking: string.
    mounting structure used for PV system
    either (tracker, ground-mount, rooftop, or canopy)
  axis_height: float.
    meters of racking tilt axis above ground
  collector_width: float.
    meter width of PV module perpendicular to axis of tilt
  axis_azimuth: float.
    compass direction along which the axis of rotation lies
    measured in decimal degrees East of North
  gcr: float.
    ratio of ground covered by PV modules within array to spacing betweeen
    modules
    used as common metric to refer to module spacing
  module_parameters: dict.
    module-specific parameters used to model dc output given irradiance
  temperature_model_parameters: dict.
    parameters used to convert ambient temperature to PV cell temperature
  axis_tilt: float.
    tracker requirement. The tilt of the axis of rotation in decimal degrees(DD)
  max_angle: float.
    tracker requirement. The max tilt allowed of a single-axis tracker in DD
  backtrack: boolean.
    tracker requirement. Whether or not the tracking system uses a backtracking
    algorithm
  surface_tilt: float.
    non-tracker requirement. Tilt of a fixed-tilt racking system in DD
  surface_azimuth: float.
    non-tracker requirement. Azimuth angle of the module surface East of North
  albedo: float.
    non-tracker requirement. The percent of light reflected from the ground
    
  Returns:
  pvlib.PVSystem
  
  Note: Would have used a kwargs parameter to but doesnt work with reticulate
  '''
  
  # Single-Axis Tracker
  if racking == 'tracker':
      system = SingleAxisTracker(axis_tilt = axis_tilt,
                             axis_azimuth = axis_azimuth,
                             max_angle = max_angle, backtrack = backtrack,
                             gcr = gcr, module_parameters = module_parameters)
  # Fixed-tilt System
  else:
      system = PVSystem(surface_tilt = surface_tilt,
                    surface_azimuth = surface_azimuth,
                    module_parameters = module_parameters)
      
      # These attributes get declared as part of SingleAxisTracker but not in
      # PVSystem
      system.axis_azimuth = axis_azimuth
      system.gcr = gcr
      system.backtrack = False
      
      # If racking is canopy or rooftop
      if racking != 'ground-mount':
          # Override system albedo for canopy and rooftop systems since time
          # series albedo calculations assume typical ground coverage (grass)
          system.albedo = albedo
      
      if racking == 'rooftop':
          system.module_parameters['bifaciality'] = 0
      
  # Adding Attributes allows ModelChain to self-contain all project inputs
  system.axis_height = axis_height    
  system.collector_width = collector_width
  system.temperature_model_parameters = temperature_model_parameters
  
  return system


def get_modelchain(lat, lon, utc_offset, elevation, name, system,
                  degradation_loss, bifacial_losses, dc_losses, ac_capacity,
                  inverter_efficiency_peak,
                  pmt_peak_loss, pmt_rating, pmt_constant_loss, ac_collection,
                  mpt_peak_loss, mpt_bottom_rating, mpt_constant_loss,
                  transmission_loss, poi_capacity, plant_losses, time_step):
  '''
  Creates a ModelChain instance from PVlib which contains all project
  parameters, models, and model results
  
  Parameters:
  lat: float.
    decimal degrees latitude of PVSystem
  lon: float.
    decimal degrees longitude of PVSystem
  utc_offset: int.
    hours local timezone is offset from UTC
  elevation: int.
    meters above sea level
  name: string.
    name of the PV project
  system: pvlib.PVSystem
    PVSystem instance to be modeled with modelchain
  degradation_loss: float.
    percent loss of module performance due to degradation (2% = .02)
  bifacial_losses: float.
    percent loss of bifacial boost due to shading and mismatch
  dc_losses: float.
    percent loss of dc output
  ac_capacity: int or float.
    inverter capacity at site design temperature (Wac)
  inverter_efficiency_peak: float.
    percent of power successfully converted from Wdc to Wac
  pmt_peak_loss: float.
    padmount transformer peak loss factor used in transformer loss equation
  pmt_rating: int or float.
    padmount transformer capacity rating in W
  pmt_constant_loss: float.
    padmount transformer constant loss factor used in transformer loss equation
  ac_collection: float.
    percent losses due to AC collection resistance
  mpt_peak_loss: float.
    main power transformer peak loss factor used in transformer loss equation
  mpt_bottom_rating: float.
    main power transformer lower capacity rating in W
  mpt_constant_loss: float.
    main power transformer constant loss factor used in transformer loss eqn
  transmission_loss: float.
    percent loss due to transmission line resistance
  poi_capacity: int or float.
    max amount of power that can be injected into the point of interconnection W
  plant_losses: float.
    percent losses due to plant operating factors like grid curtailment or
    availability.  These aren't included in time series output but are included
    in results metrics like AEP and NCF
  time_step: int.
    minute resolution of time series.  used when calculating results metrics.
  
  Returns:
  pvlib.modelchain
  
  
  '''
  
  # ModelChain requires a PVSystem and Location and model parameters
  location = Location(lat, lon, utc_offset, elevation, name)
  
  # Implement custom losses model and not one in PVLib
  mc = ModelChain(system, location, spectral_model = 'no_loss',
                  losses_model = 'no_loss', dc_model = 'pvwatts',
                  ac_model = 'pvwatts') 

  # Save model parameters to allow the modelchain instance to be self-contained
  # lat, lon, altitude, tz, name are saved in mc$location
  mc.degradation_loss = degradation_loss
  mc.bifacial_losses = bifacial_losses
  mc.dc_losses = dc_losses
  mc.ac_capacity = ac_capacity
  mc.inverter_efficiency_peak = inverter_efficiency_peak
  mc.pmt_peak_loss = pmt_peak_loss
  mc.pmt_rating = pmt_rating
  mc.pmt_constant_loss = pmt_constant_loss
  mc.ac_collection = ac_collection
  mc.mpt_peak_loss = mpt_peak_loss
  mc.mpt_bottom_rating = mpt_bottom_rating
  mc.mpt_constant_loss = mpt_constant_loss
  mc.transmission_loss = transmission_loss
  mc.poi_capacity = poi_capacity
  mc.plant_losses = plant_losses
  mc.time_step = time_step
  # dc_capacity is input as a module_parameter per pvwatts requirements
  mc.dc_capacity = system.module_parameters['pdc0']
                  
  return mc


def get_effective_irradiance(self, weather, utc_offset):
  '''
  Transform GHI/DNI/DHI from weather dataframe into POA.
  This is a custom method added to the modelchain class.
  Modelchain instance must already be assigned bifacial_losses attribute.
  
  Parameters:
  self: pvlib.modelchain
    a ModelChain instance
  weather: pandas.DataFrame
    time series weather data with columns ['ghi', 'dni', 'dhi', 'temp_air',
    'wind_speed', 'surface_albedo']
  utc_offset: int.
    hour offset of local timezone (in weather DataFrame) from UTC
  
  Returns:
  pvlib.modelchain
  
  '''
  
  # Prepare weather DataFrame ----
  
  # Must be datetimeindex aware
  tz = 'Etc/GMT+' + str(-utc_offset)
  
  # If pd.datetimeindex is naive, localize, otherwise convert
  if(pd.to_datetime(weather.index).tz is None):
    weather.index = pd.to_datetime(weather.index).tz_localize(tz)
  else:
    weather.index = pd.to_datetime(weather.index).tz_convert(tz)
    
  # Check if I should override weather's albedo values
  # (only canopy or rooftop systems will have self.system.albedo values)
  try:
      weather.albedo = self.system.albedo
  except:
      pass
  
  # Run PVLib models ----
  
  self.prepare_inputs(weather)
  self.aoi_model()
  self.spectral_model()
  self.effective_irradiance_model()
  self.effective_irradiance = self.effective_irradiance.fillna(0)
  self.temperature_model()
  
  # Use PVFactors viewshed model ----
      
  # Surface Inputs calculated in prepare_inputs if tracking,
  if isinstance(self.system, SingleAxisTracker):
      surface_tilt = self.tracking.surface_tilt
      surface_azimuth = self.tracking.surface_azimuth
      
  # but must be manually calculated if fixed-tilt
  else:
      surface_tilt = np.repeat(self.system.surface_tilt,
                               len(self.weather.index))
      surface_azimuth = np.repeat(self.system.surface_azimuth,
                                  len(self.weather.index))
      
  # Need a custom function to build the report of the PVFactors simulation
  def pvfactor_build_report(pvarray): return {
    'total_inc_back': pvarray.ts_pvrows[1].back.get_param_weighted(
                                                  'qinc').tolist(),
    'total_inc_front': pvarray.ts_pvrows[1].front.get_param_weighted(
                                                  'qinc').tolist()
  }
  
  pvarray_parameters = {
      'n_pvrows': 3,
      'axis_azimuth': self.system.axis_azimuth,
      'pvrow_height': self.system.axis_height,
      'pvrow_width': self.system.collector_width,
      'gcr': self.system.gcr,
      # front and back reflectivity
      'rho_front_pvrow': 0.01,
      'rho_back_pvrow': .03,
      # sky dome's diffuse horizon band angle
      'horizon_band_angle': 15
    }
  
  pvfactor_report = run_timeseries_engine(pvfactor_build_report,
                                          pvarray_parameters, weather.index,
                                          weather.dni, weather.dhi,
                                          self.solar_position.zenith,
                                          self.solar_position.azimuth,
                                          surface_tilt, surface_azimuth,
                                          weather.surface_albedo)
  
  # Save the PVFactor results
  pvfactor_df = pd.DataFrame(pvfactor_report, index=weather.index)
    
  setattr(self, 'back_irradiance', pvfactor_df.total_inc_back.fillna(0))
  setattr(self, 'front_irradiance', pvfactor_df.total_inc_front.fillna(0))
  
  # Calculate plane of array irradiance losses ----
  
  # PVFactors doesn't account for backtracking, use frontside irradiance from
  # PVLib instead.  (self.effective_irradiance for backtracking systems instead
  # of self.front_irradiance)
  # Effective Irradiance = Frontside POA + Backside POA *
  #                        (Bifaciality Reduced For Backside Losses)
  
  if self.system.backtrack:
      setattr(self, 'effective_irradiance_bifacial',
                  self.effective_irradiance + self.back_irradiance *
                    (self.system.module_parameters['bifaciality'] *
                    (1-self.bifacial_losses)))
  else:
      setattr(self, 'effective_irradiance_bifacial',
                  self.front_irradiance + self.back_irradiance *
                    (self.system.module_parameters['bifaciality'] *
                    (1-self.bifacial_losses)))
  
  # Apply same soiling loss to front-and-backside POA (conservative approach)
  setattr(self, 'soiling', weather.soiling)
  setattr(self, 'effective_irradiance_soiled',
          self.effective_irradiance_bifacial * (1 - self.soiling))
  
  return self


def run_pvwatts_dc(self):
  '''
  Run the pvwatts dc model within pvlib and apply custom dc losses.
  This is a custom method added to modelchain class.
  Modelchain instance must already have the following attributes assigned:
    dc_losses, ac_capacity, inverter_efficiency_peak
  
  '''

  pdc = self.system.pvwatts_dc(g_poa_effective =
                                self.effective_irradiance_soiled,
                              temp_cell = self.cell_temperature)
  
  self.dc = pd.DataFrame({'pdc': pdc})
  self.dc['losses'] = self.dc.pdc * self.dc_losses
  # Note: calling dc.losses time series, not the dc_losses attribute
  self.dc['output'] = self.dc.pdc - self.dc.losses
  self.dc['clipping'] = self.dc.output - (self.ac_capacity / 
                                          self.inverter_efficiency_peak)
  self.dc.clipping[self.dc.clipping < 0] = 0
    
  return self
 
def run_pvwatts_ac(self):
  '''
  Run the pvwatts ac model within pvlib and apply custom ac losses.
  This is a custom method added to modelchain class.
  Modelchain instance must already have the following attributes assigned:
    dc.output, ac_capacity, inverter_efficiency_peak, pmt_peak_loss, pmt_rating,
    pmt_constant_loss, ac_collection, mpt_peak_loss, mpt_bottom_rating,
    mpt_constant_loss, transmission_loss, poi_capacity  
    
  '''
  
  pac = pvlib.pvsystem.pvwatts_ac(pdc = self.dc.output,
                                  pdc0 = self.ac_capacity /
                                    self.inverter_efficiency_peak,
                                  eta_inv_nom = self.inverter_efficiency_peak)
  
  self.ac = pd.DataFrame({'pac': pac})
  
  # PMT losses
  pmt_out = self.ac.pac - (self.pmt_peak_loss *
                          (self.ac.pac / .98 / self.pmt_rating)**2 +
                          self.pmt_constant_loss)
                          
  pmt_out[pmt_out < 0] = 0
  
  # AC Collection losses
  mpt_in = pmt_out * (1 - self.ac_collection)
  
  # Main Power Transformer losses
  mpt_out = mpt_in - (self.mpt_peak_loss *
                     (mpt_in / .98 / self.mpt_bottom_rating)**2 +
                     self.mpt_constant_loss)
  
  mpt_out[mpt_out < 0] = 0
  
  # Transmission line losses and plant clipping
  self.ac['output'] = mpt_out * (1 - self.transmission_loss)
  self.ac['losses'] = self.ac.pac - self.ac.output
  self.ac['clipping'] = self.ac.output - self.poi_capacity
  self.ac.clipping[self.ac.clipping < 0] = 0
    
  return self


def run_plant_model(self):
  '''
  Model plant output, accounting for clipping at poi and plant losses
  This is a custom method added to modelchain class.
  Modelchain instance must already have the following attributes assigned:
    ac.output, ac.clipping, plant_losses
    
  '''
  
  self.plant = pd.DataFrame({'output': self.ac.output - self.ac.clipping})
  self.plant['losses'] = self.plant.output * self.plant_losses
    
  return self

def get_results(self):
  '''
  Calculate common plant performance metrics and assign them to modelchain
  attributes.
  This is a custom method addded to modelchain class.
  Modelchain instance must already have the following attributes assigned:
    plant.output, plant.losses, poi_capacity, dc_capacity, time_step
  
  '''

  self.aep = ((self.plant.output.sum() - self.plant.losses.sum()) *
              self.time_step / 60)
  
  self.ncf = self.aep / self.poi_capacity / 8760
  
  self.energy_yield = self.aep / self.dc_capacity
  
  # Only using front-side irradiance in Performance Ratio calculation
  self.pr = 1 - self.aep / (self.effective_irradiance.sum() *
                self.time_step / 60 * (self.dc_capacity /
                self.system.module_parameters['efficiency'] / 1000))
    
  return self
