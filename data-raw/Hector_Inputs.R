
##  Run Hector and calculate the RF timeseries that will be used in the HIRM validation experiments.
##  Also save the comparison temperature. The working directory for this script should be
## set to the SLCFimpulse/data-raw/

# 0. Set Up ------------------------------------------------------------------------------------------
#devtools::load_all('~/Documents/hector')
library(dplyr)
library(tidyr)
library(assertthat)

# A) The Impulse Response Functions
## A1. Define Functions --------------------------------------------------------------------------------
# Calculate the temperature impulse response to a RF pulse of an emissions sepcies in Hector.
#
# Args
#   ini_file: the path to the Hector ini file to run
#   rf_name: the name of the Hector forcing componet to normalize the global temperature response by
#   emission_name: the name of the Hector emission compoent to doubble
#   impulse_year: the year to doubble the Hector emissions in
#   irf_name: the name of the impulse response function
#   agent_name: the name of the agent being pertrubed, for Hector this does not really apply.
# Returnes: a data frame of the impulse respone function that should work with core impulse response function.

get_Hector_irf <- function(ini_file, rf_name, emission_name, impulse_year, irf_name, agent_name){

  # Prevent factors from being returned by the fetchvars function calls
  options(stringsAsFactors = FALSE)

  assert_that(file.exists(ini_file), msg = 'ini_file does not exist')

  # Run default Hector as the reference run
  core <- newcore(inifile = ini_file)
  run(core)

  # Get temperature and rf data from the reference run.
  reference_run <- fetchvars(core = core, dates = 1750:2100, vars = c(GLOBAL_TEMP(), rf_name)) %>%
    rename(ref_value = value)

  # Get the reference emissions
  ref_emissions <- fetchvars(core = core, dates = impulse_year, vars = emission_name)

  # Doubble the emission in the impulse year.
  impulse_emissions <- ref_emissions
  impulse_emissions$value <- impulse_emissions$value * 2

  # Rerun the Hector core with the new emissions
  setvar(core = core, dates = impulse_emissions$year, var = impulse_emissions$variable,
         values = impulse_emissions$value, unit = impulse_emissions$units)
  reset(core = core)
  run(core)

  # Get the impulse run results
  perterbed_run <- fetchvars(core = core, dates = 1750:2100, vars = c(GLOBAL_TEMP(), rf_name)) %>%
    rename(imp_value = value)

  # What is the difference in temperature and RF between the reference and the pertubation runs?
  reference_run %>%
    left_join(perterbed_run %>%
                rename, by = c("scenario", "year", "variable", "units")) %>%
    mutate(value = imp_value - ref_value) ->
    diff_data

  # Extract the RF pulse (make sure that it is only one year).
  diff_data %>%
    filter(variable != 'Tgav' & abs(value) > 1e-5) %>%
    pull(value) ->
    RF_pulse

  assert_that(length(RF_pulse) == 1, msg = 'Problem with the RF pulse extracted')

  # Now divide the temperature response by the size of the RF pulse.
  diff_data %>%
    filter(variable == 'Tgav') %>%
    # Normalize the temperature response by the RF pulse.
    mutate(value = value / RF_pulse,
           year = year - impulse_year) %>%
    # Convert absolute time to relative time since the pulse year, we are only interested in time since the pulse.
    filter(year >= 0) %>%
    # Add agent and impulse response function names
    mutate(name = irf_name, agent = agent_name, units = 'degC * m2 / W') %>%
    select(name, year, value, units, agent)


}


## A2. Get the Hector Impulse Response Functions ----------------------------------------------------------------

# We determined that it does not matter what scenario you use as the reference so long as the
# carbon cycle is constrained to prevent carbon cycle feedbacks from being incorperated in the
# imnpulse response functions for the recreate SCM experiment.
ini_file <- system.file('input/hector_rcp45_constrained.ini', package = 'hector')

# There is only impulse response function for Hector so the irf name and the agent name does
# not matter.
irf_name   <- 'general'
agent_name <- NA_character_

# Because there is only one impulse respone function for Hector pick on emission species to
# pulse and use the appropriate RF value to normalize the global temperature response by.
rf_name       <- RF_BC()
emission_name <- EMISSIONS_BC()

# We've figured out that it does not matter what year the impulse is in but the earlier
# the impulse year is the longer the impulse response function is and less padding will
# have to happen. (We won't have to add 0s, to the end of the irf to make it long enough
# to convolve with the driving rf time series in the core model)
impulse_year <- 1800

# Get the general Hector impulse response function.
general_irf <- get_Hector_irf(ini_file = ini_file,
                              rf_name = rf_name,
                              emission_name = emission_name,
                              impulse_year = impulse_year,
                              irf_name = irf_name,
                              agent_name = agent_name)

# Extrapolate the IRF by fitting the IRF to an expoential decay curve.
mod <- nls(value ~ a * exp(-b * year), data = general_irf, start = list(a = max(general_irf$value), b = 0.3))
new_years  <- max(general_irf$year) + 1: 3000
new_values <-  predict(mod, list(year = new_years))

# Format the new values in a tibble that will be able to be joined with the general_irf
new_values_tibble <- tibble::tibble(name = unique(general_irf$name),
                                    year = new_years,
                                    value = new_values,
                                    units = unique(general_irf$units),
                                    agent = unique(general_irf$agent))

general_irf %>%
  dplyr::bind_rows(new_values_tibble) %>%
  dplyr::arrange(name, year, units, agent) ->
  general_irf


# B) The Recerate RCPs Driver & Comp Data ---------------------------------------------------------------------------------------

# For the recreate RCP temp validation experiments use all of the RF values from Hector as driver input.
# A vector of the individual RF agents.
rf_componets <- c(RF_BC(),
                  RF_C2F6(),
                  RF_CCL4(),
                  RF_CF4(),
                  RF_CFC11(),
                  RF_CFC113(),
                  RF_CFC114(),
                  RF_CFC115(),
                  RF_CH3BR(),
                  RF_CH3CCL3(),
                  RF_CH3CL(),
                  RF_CH4(),
                  RF_CO2(),
                  RF_H2O(),
                  RF_HALON1211(),
                  RF_CFC12(),
                  RF_HALON1301(),
                  RF_HALON2402(),
                  RF_HCF141B(),
                  RF_HCF142B(),
                  RF_HCF22(),
                  RF_HFC125(),
                  RF_HFC134A(),
                  RF_HFC143A(),
                  RF_HFC227EA(),
                  RF_HFC23(),
                  RF_T_ALBEDO(),
                  RF_HFC245FA(),
                  RF_HFC32(),
                  RF_HFC4310(),
                  RF_N2O(),
                  RF_O3(),
                  RF_OC(),
                  RF_SF6(),
                  RF_SO2D(),
                  RF_SO2I(),
                  RF_VOL())


## B1. RCP 26 -------------------------------------------------------------------------
# Launch and run a hector core.
core <- newcore(system.file('input/hector_rcp26_constrained.ini', package = 'hector'),
                name = 'constrained-rcp26')

reset(core)
run(core)

# THe total climate RF and temperature values to use as comparison data.
comparison_rcp26 <- fetchvars(core, dates = 1750:2100, vars = GLOBAL_TEMP())

# The rf values values for the rcp26 run.
rf_rcp26 <- fetchvars(core, dates = 1750:2100, vars = rf_componets) %>%
  # Add a name column.
  mutate(name = paste0('rcp26_', variable))

# clean up
shutdown(core)

## B2. RCP 45 -------------------------------------------------------------------------
# Launch and run a hector core.
core <- newcore(system.file('input/hector_rcp45_constrained.ini', package = 'hector'), name = 'constrained-rcp45')
run(core)

# THe total climate RF and temperature values to use as comparison data.
comparison_rcp45 <- fetchvars(core, dates = 1750:2100, vars = GLOBAL_TEMP())

# The rf values values for the rcp26 run.
rf_rcp45 <- fetchvars(core, dates = 1750:2100, vars = rf_componets) %>%
  # Add a name column.
  mutate(name = paste0('rcp45_', variable))

# clean up
shutdown(core)

## B3. RCP 60 -------------------------------------------------------------------------
# Launch and run a hector core.
core <- newcore(system.file('input/hector_rcp60_constrained.ini', package = 'hector'), name = 'constrained-rcp60')
run(core)

# THe total climate RF and temperature values to use as comparison data.
comparison_rcp60 <- fetchvars(core, dates = 1750:2100, vars = GLOBAL_TEMP())

# The rf values values for the rcp26 run.
rf_rcp60 <- fetchvars(core, dates = 1750:2100, vars = rf_componets) %>%
  # Add a name column.
  mutate(name = paste0('rcp60_', variable))

# clean up
shutdown(core)

## B4. RCP 85 -------------------------------------------------------------------------
# Launch and run a hector core.
core <- newcore(system.file('input/hector_rcp85_constrained.ini', package = 'hector'), name = 'constrained-rcp85')
run(core)

# The total climate RF and temperature values to use as comparison data.
comparison_rcp85 <- fetchvars(core, dates = 1750:2100, vars = GLOBAL_TEMP())

# The rf values values for the rcp26 run.
rf_rcp85 <- fetchvars(core, dates = 1750:2100, vars = rf_componets) %>%
  # Add a name column.
  mutate(name = paste0('rcp85_', variable))

# clean up
shutdown(core)

# C) Abrupt 4 x CO2 Step -----------------------------------------------------------------------------
# In order to get the abrupt 4 x CO2 run to solve there were a number of changes that had to be made
# to hecotr. We had to change the integration time steps (1) the carbon-cycle-solver.cpp time step
# from dt(0.3) to dt(0.1) and (2) the ocean_component.hpp OCEAN_MIN_TIMESTEP from 0.3 to 0.01.
# For Hector version 2.2.3
#
# Launch and run a hector core.
core <- newcore(system.file('input/hector_rcp26_constrained.ini', package = 'hector'),
                name = 'constrained-rcp26')

# Reset the atmospheric CO2 so that it is held at the constant preindustiral level for.
default_hector_PI <- 276

constant_PI_tibble <- tibble::tibble(year = 1750:2100, value = default_hector_PI)

setvar(core,
       dates = constant_PI_tibble$year,
       var = CA_CONSTRAIN(),
       values = constant_PI_tibble$value,
       unit = getunits(CA_CONSTRAIN()))

reset(core)
run(core)

# The total climate RF and temperature values to use as comparison data.
reference_results <- fetchvars(core, dates = 1750:2100, vars = c(GLOBAL_TEMP(), RF_TOTAL(), RF_CO2())) %>%
  rename(ref_value = value) %>%
  select(-scenario)

# Now do the abrupt 4 x Co2 run.
constant_PI_tibble %>%
  mutate(value = if_else(year >= 1950, value * 4, value)) ->
  step_PI_tibble

setvar(core,
       dates = step_PI_tibble$year,
       var = CA_CONSTRAIN(),
       values = step_PI_tibble$value,
       unit = getunits(CA_CONSTRAIN()))

reset(core)
run(core)

# THe total climate RF and temperature values to use as comparison data.
step_results <- fetchvars(core, dates = 1750:2100, vars = c(GLOBAL_TEMP(), RF_TOTAL(), RF_CO2())) %>%
  rename(step_value = value) %>%
  select(-scenario)

# Cacluate the RF driver and the step temperature comparison.
step_results %>%
  right_join(reference_results, by = c("year", "variable", "units")) %>%
  mutate(value = step_value - ref_value) %>%
  select(year, variable, value, units) ->
  abtrupt_step_results

# Format the abrupt CO2 step RF time series, note that F total and F CO2 are equivalent to one another.
abtrupt_step_results %>%
  filter(variable %in% c(RF_TOTAL(), RF_CO2())) %>%
  mutate(scenario = 'abrupt-4xCO2', name = paste0(scenario, '_', variable), agent = gsub(pattern = 'F', replacement = '', variable)) ->
  step4xCO2_RF


# Format the abrupt CO2 step temperature comparison.
abtrupt_step_results %>%
  filter(variable == GLOBAL_TEMP()) %>%
  mutate(scenario = 'abrupt-4xCO2') ->
  step4xCO2_comparison




# D) Format Outputs -----------------------------------------------------------------------------------

# Format the impulse response function into a list.
Hector_IRF <- list('general_hector' = general_irf)
usethis::use_data(Hector_IRF, overwrite = TRUE, compress = 'xz')

# Format the Hector comparison data into a list.
# Concatenate the comparison data into a single list named by the scenario and save as package data.
copmarison_data <- bind_rows(comparison_rcp26,
                             comparison_rcp45,
                             comparison_rcp60,
                             comparison_rcp85,
                             step4xCO2_comparison)


# Concatenate all of the RF drivers together into a single data frame.
Hector_RF <- bind_rows(rf_rcp26,
                       rf_rcp45,
                       rf_rcp60,
                       rf_rcp85) %>%
  mutate(agent = gsub(pattern = '^F', replacement = '', variable)) %>%
  bind_rows(step4xCO2_RF)

# Split up the RF values by the RF name.
Hector_RF <- split(Hector_RF, Hector_RF$name)


# E) Format the Input Matrix -----------------------------------------------------------------------

# The batch matrix has has the name of the rf dirver as the row names and the name of the impulse
# response function as the column name. Since Hector treats all emission sepcies the same this
# matrix will only have a single column.
Hector_ConfigMatrix <- matrix(data = 0, nrow = length(Hector_RF), ncol = length(Hector_IRF))
row.names(Hector_ConfigMatrix) <- names(Hector_RF)
colnames(Hector_ConfigMatrix)  <- names(Hector_IRF)


# F) Save Script Outputs ---------------------------------------------------------------------------
# Save the comparison data.
Hector_comparison <- split(copmarison_data, copmarison_data$scenario, drop = FALSE)
usethis::use_data(Hector_comparison, overwrite = TRUE, compress = 'xz')

# Save the Hector batch matrix.
usethis::use_data(Hector_ConfigMatrix, overwrite = TRUE, compress = 'xz')

# Save the Hector RF drivers.
usethis::use_data(Hector_RF, overwrite = TRUE, compress = 'xz')

# Save the Hector IRF.
usethis::use_data(Hector_IRF, overwrite = TRUE, compress = 'xz')


