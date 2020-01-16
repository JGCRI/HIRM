
context("uncertainty_IRM")

# Set Up ------------------------------------------------------------------------------------------
# Import and format the core_IRM inputs
rf_csv   <- read.csv('data/rf_timeseries.csv', stringsAsFactors = F)
irf_csv  <- read.csv('data/global_irf.csv', stringsAsFactors = F)

irf_list <- split(irf_csv, interaction(irf_csv$agent, irf_csv$name, sep = '_'))
rf_list  <- split(rf_csv, rf_csv$name)

# Make the configuration matrix
irf_names <- names(irf_list)
rf_names <- names(rf_list)

config_matrix           <- matrix(rep(0), nrow = length(irf_names), ncol = length(rf_list))
colnames(config_matrix) <- irf_names
rownames(config_matrix) <- rf_names
diag(config_matrix)     <- 1


# Test that ---------------------------------------------------------------------------------------
testthat::test_that('uncertainty_IRM', {

  # Ensure that the uncertainty_IRM works as expected
  uncertain   <- list(FBC = 1:2, BC_global = c(1, 3))
  uncert_rslt <- uncertainty_IRM(uncertianty_scalers = uncertain, config_matrix = config_matrix, rf_list = rf_list, irf_list = irf_list,
                               match_agents = FALSE, write_out = NULL)

  # Check the output of the strutucutre and that the different runs contain different values.
  testthat::expect_equal(length(uncert_rslt), prod(lengths(uncertain)))
  testthat::expect_true(any(uncert_rslt$run_1$total_temp$value != uncert_rslt$run_2$total_temp$value))
  testthat::expect_true(any(uncert_rslt$run_1$uncertainty_run != uncert_rslt$run_2$uncertainty_run))
  testthat::expect_true(sum(uncert_rslt$run_1$rf_inputs$value) !=   sum(uncert_rslt$run_3$rf_inputs$value))

  # Parse out the uncertainty scalar combinations and see if it matches our expected pattern.
  scalars_used <- dplyr::bind_rows(lapply(uncert_rslt, function(object){object[['uncertainty_run']]}))

  # Because of the way that the uncertian list is set up and the way that uncertainty_IRM generates the
  # patterns for the scalar combiantions we know that the scalar values for the first element FBC
  # are going to be repeated twice in a row where as elements in the BC_global
  # are going to be repeatted but one values after the other.
  testthat::expect_true(all(dim(scalars_used) == c(prod(lengths(uncertain)), length(uncertain) + 1)))
  testthat::expect_equal(scalars_used$FBC, rep(uncertain$FBC, each = 2))
  testthat::expect_equal(scalars_used$BC_global, rep(uncertain$BC_global, 2))

  # Recreate the core IRM run that the scalars FBC scalar = 1 and BC_global = 1, (this is going to recreate run_1).
  recreate_run1 <- core_IRM(config_matrix = config_matrix, rf_list = rf_list, irf_list = irf_list, match_agents = FALSE)
  testthat::expect_equal(uncert_rslt$run_1$total_temp$value, recreate_run1$total_temp$value)

  # Recreate the run where FBC scalar = 1 and BC_global scalar = 3 this is run number 2.
  new_irf <- irf_list
  new_irf$BC_global$value <- irf_list$BC_global$value * 3
  recreate_run2           <- core_IRM(config_matrix = config_matrix, rf_list = rf_list, irf_list = new_irf, match_agents = FALSE)
  testthat::expect_equal(uncert_rslt$run_2$total_temp$value, recreate_run2$total_temp$value)


  # Ensure that uncertainty_IRM throws the expected error messages.
  testthat::expect_error(uncertainty_IRM(c('k'), config_matrix, rf_list, irf_list, FALSE),  'uncertainty_scalers must be a list')

  missing <- uncertain
  names(missing) <- c('fake', 'BC_global')
  testthat::expect_error(uncertainty_IRM(missing, config_matrix, rf_list, irf_list, FALSE), 'irf and/or rf is missing fake')

  dublicates <- config_matrix
  row.names(dublicates) <- colnames(dublicates)
  names(uncertain) <- c('BC_global', 'CH4_global')
  testthat::expect_error(uncertainty_IRM(uncertain, dublicates, irf_list, irf_list, FALSE), 'BC_global, CH4_global, CO2_global exist in both rf_list and irf_list')

  testthat::expect_error(uncertainty_IRM(uncertianty_scalers = uncertain, config_matrix = config_matrix, rf_list = rf_list, irf_list = irf_list,
                                         match_agents = FALSE, write_out = TRUE, n_core = 1), 'write_out can either be set to NULL or a directory that exists')
  testthat::expect_error(uncertainty_IRM(uncertianty_scalers = uncertain, config_matrix = config_matrix, rf_list = rf_list, irf_list = irf_list,
                                         match_agents = './to/narnia', write_out = TRUE, n_core = 1), 'write_out can either be set to NULL or a directory that exists')

  })
