
context("batch_IRM")

# Import and format the core_IRM inputs
rf_csv  <- read.csv('data/rf_timeseries.csv', stringsAsFactors = F)
irf_csv <- read.csv('data/global_irf.csv', stringsAsFactors = F)

irf_list <- split(irf_csv, interaction(irf_csv$agent, irf_csv$name, sep = '_'))
rf_list  <- split(rf_csv, rf_csv$name)

# Make the configuration matrix
irf_names <- names(irf_list)
rf_names <- names(rf_list)

config_matrix           <- matrix(rep(0), nrow = length(irf_names), ncol = length(rf_list))
colnames(config_matrix) <- irf_names
rownames(config_matrix) <- rf_names
diag(config_matrix)     <- 1


# Make the batch list
batch_list <- rep(list(config_matrix), 3)

test_that('batch_IRM', {

  # Check length of the list output
  names(batch_list) <- paste0('run_', 1:length(batch_list))
  rslt <- batch_IRM(batch_list = batch_list, rf_list = rf_list, irf_list = irf_list, match_agents = FALSE)
  testthat::expect_equal(length(rslt), 2)

  # Check that run_name is in each of the returned tibbles
  testthat::expect('name' %in% names(rslt$total_temp), TRUE)
  testthat::expect('name' %in% names(rslt$temp_contributions), TRUE)

  # Check that the run name column contains the correct number of runs
  testthat::expect_equal(length(unique(rslt$total_temp$name)), length(batch_list))

})
