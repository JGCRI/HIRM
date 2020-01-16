
context("core_IRM")

# Load libs
library(tidyr)
library(dplyr)


# contributing_temp -----------------------------------------------------------------------------
# Import driver and response data
rf_csv  <- read.csv('data/rf_timeseries.csv', stringsAsFactors = F)
irf_csv <- read.csv('data/global_irf.csv', stringsAsFactors = F)

# Parse out the BC rf and irf to used in the testing of the contributing_temp function.
BC_rf  <- filter(rf_csv, agent == 'BC')
BC_irf <- filter(irf_csv, agent == 'BC')

# Expected results
BC_rslt <- read.csv('data/BC_contributing_temp.csv', comment.char = '#', stringsAsFactors = F)

# Test that...
testthat::test_that('contributing_temp', {

  # Make sure that contributing_temp calculates the correct values and returns the
  # correct output.
  temp_BC <- contributing_temp(BC_rf, BC_irf, FALSE)
  testthat::expect_equal(dim(temp_BC), dim(BC_rslt))
  #expect_equal(temp_BC, BC_rslt)

  # Make sure that contributing_temp throws an error when the irf tibble contains
  # values for multiple agents.
  testthat::expect_error(contributing_temp(BC_rf, irf_csv, FALSE), 'irf can only contain information for one agent')
  testthat::expect_error(contributing_temp(rf_csv, irf_csv, FALSE), 'rf can only contain information for one agent')

})


# core_IRM ------------------------------------------------------------------------------------
# Format data into lists of data frames
irf_list <- split(irf_csv, interaction(irf_csv$agent, irf_csv$name, sep = '_'))
rf_list  <- split(rf_csv, rf_csv$name)

# Make the configuration matrix
irf_names <- names(irf_list)
rf_names <- names(rf_list)

config_matrix           <- matrix(rep(0), nrow = length(irf_names), ncol = length(rf_list))
colnames(config_matrix) <- irf_names
rownames(config_matrix) <- rf_names
diag(config_matrix)     <- 1

# Import the expected result
expected_total_temp <- read.csv('data/IRM_temp_results.csv', stringsAsFactors = FALSE)


# Testthat....
testthat::test_that('core_IRM', {

  # Make sure that results match the expected
  rslt <- core_IRM(config_matrix = config_matrix, rf_list = rf_list, irf_list = irf_list, match_agents = TRUE)
  testthat::expect_equal(length(rslt), 2)

  # There is some rounding issue when the values are imported from the csv file, round the values
  # before comparing.
  testthat::expect_equal(round(rslt$total_temp$value, 6), round(expected_total_temp$value, 6))

  # Make sure that the function throws the expected errors.
  testthat::expect_error(core_IRM(config_matrix = c(), rf_list = rf_list, irf_list = irf_list, match_agents = FALSE),
               'config_matrix must be a matrix')


  config_matrix[2,1] <- 1
  testthat::expect_error(core_IRM(config_matrix = config_matrix, rf_list = rf_list, irf_list = irf_list, match_agents = TRUE),
               'irf and rf agent must be the same if match_agents = TRUE')

  config_matrix[2,1] <- 0
  testthat::expect_error(core_IRM(config_matrix = config_matrix, rf_list = rf_list[1:2], irf_list = irf_list, match_agents = TRUE),
               'rf_list is missing FCO2')
  testthat::expect_error(core_IRM(config_matrix = config_matrix, rf_list = rf_list, irf_list = irf_list[1:2], match_agents = TRUE),
               'irf_list is missing CO2_global')

})






