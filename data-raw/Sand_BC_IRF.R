## The purpose of this script is to derive the CESM BC IRF that can be used in HIRM.
## The working driectory should be set to the SLCFimpulse/data-raw

# 0. Set Up --------------------------------------------------------------------------
# Load the required pacakges
library(dplyr)
library(readxl)
library(hector)
library(tidyr)

# The base direcotry should be set to the working direcotry.
BASE_DIR <- here::here('data-raw')

# 1. Import & Format Data -------------------------------------------------------------
# Import the csv file from Sand et al that was sent to Setve Smith. This contains the
# monthly temperature response to a global BC step increase.
path_csv <- file.path(BASE_DIR, 'TREFHT_fig1_toSteveSmith.xlsx')
raw_data <- read_excel(path_csv)

# Determine how many years of data of monthly data we have. This will be used to add
# years to the temperature response data frame.
n_yrs <- nrow(raw_data) / 12

# Create a data frame of year and temperature response. Because the temperature response
# begins at the time of the step the first year of data is actually at year = 0 so make sure
# to index the year column accordingly.
data.frame(year = rep(0:c(n_yrs-1), each = 12),
           value = raw_data$`SAT BC`) %>%
  # Find the annual average.
  group_by(year) %>%
  summarise(value = mean(value)) %>%
  ungroup ->
  data

# 2. Fits -------------------------------------------------------------------------------------------------
# The temperature response is fairly noisy so we will fit an exponential function to it and then use
# results from the fit in the derivative of the expoential as our impulse response function.
# Fit the exponetial results.
fit       <- nls(data$value ~ a *(1 - exp(-data$year / tau)), data = data, start = list(a = 2, tau = 1))
fit_rslts <- predict(fit)

# Save the fits
amplitude <- coefficients(summary(fit))[1,1]
tau       <- coefficients(summary(fit))[2,1]

# Calucalte the BC temperature impulse response function.
year     <- 0:3000
response <- (coef(fit)[['a']]) /coef(fit)[['tau']]  * exp(- year / coef(fit)[['tau']])

# Now that we have the temperature response to the BC emissions step we need to normalize the
# temperature response by the magnitude of the change in the RF.
# We will need to mulitply Hector's BC RF / BC emissions by the size of the emissions step.
#
# Run Hector for some emissions pathwaway, we will use the BC emissions and RF output to determine Hecotr's RF/Tg BC relationship.
core <- newcore(system.file('input/hector_rcp26.ini', package = 'hector'))
run(core)
hector_output <- fetchvars(core, 1850:2100, vars = c(EMISSIONS_BC(), RF_BC()))

hector_output %>%
  select(year, variable, value) %>%
  spread(variable, value) %>%
  mutate(value = FBC/BC_emissions) %>%
  pull(value) %>%
  mean ->
  RF_Emiss

normalize_by <- RF_Emiss * 133

# Format as a data frame
Sand_BC_IRF <- tibble(name = 'BC Sand et al',
                      year = year,
                      value = response / normalize_by,
                      units = 'degC * m2 / W',
                      agent = NA)

# Save Sand IRF
usethis::use_data(Sand_BC_IRF, overwrite = TRUE, compress = 'xz')



# 4. Modify Hector IRF ---------------------------------------------------------------------------------------
## Rescale the Hector IRF so that it has the same magnitude at the Sand BC IRF.

## Pull out the inital response for the Sand BC IRF and Hector's general IRF.
## Use these values to determine the scalar that will scale the Hector
## IRF to match the BC IRF.
Sand_BC_IRF %>%
  filter(year == 0) %>%
  pull(value) ->
  Sand_init_response

SLCFimpulse::Hector_IRF$general_hector %>%
  filter(year == 0) %>%
  pull(value) ->
  Hector_init_response

scalar <-  Sand_init_response / Hector_init_response

SLCFimpulse::Hector_IRF$general_hector %>%
  mutate(name = 'BC modified Hector',
         value = value * scalar) ->
  Hector_BC_IRF

# Save Sand IRF
usethis::use_data(Hector_BC_IRF, overwrite = TRUE, compress = 'xz')

