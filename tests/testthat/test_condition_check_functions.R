context("check_conditions_func")

testthat::test_that('check_columns', {

  # Test to see that the check_columns function throws an error when missing a column and that message
  # details can be incorporated into the error message.
  object <- data.frame(X = rep(1, 5), Y = rep(2, 5), Z = rep(0, 5))

  testthat::expect_error(check_columns(object = object, required_columns = c('X', 'Y', 'fake')),  "missing fake")
  testthat::expect_error(check_columns(object = object, required_columns = c('X', 'Y', 'fake'), message_details = 'test'),  'test missing fake')

})


testthat::test_that('check_time_step', {

  irf <- tibble::tibble(value = sample(1:10, 5), year = c(10, 15, 20, 30, 40))
  rf  <- tibble::tibble(value = sample(1:10, 5), year = c(5, 15, 16, 30, 40))

  testthat::expect_error(check_time(irf = irf, rf), 'irf must contain a constant time step size')

  })


testthat::test_that('check_time_length', {

  irf <- tibble::tibble(value = sample(1:10, 4), year = c(10, 15, 20, 30))
  rf  <- tibble::tibble(value = sample(1:10, 5), year = c(5, 15, 16, 30, 40))

  expect_error(check_time(irf = irf, rf = rf), 'irf and rf differ by -1 time steps.')

})


testthat::test_that('check_agent', {

  irf <- tibble::tibble(value = sample(1:10, 4), year = c(10, 15, 20, 30), agent = 'BC')
  rf  <- tibble::tibble(value = sample(1:10, 5), year = c(5, 15, 16, 30, 40), agent = 'CO2')
  testthat::expect_error(check_agent(irf, rf, match_agents = TRUE), 'irf and rf agent must be the same if match_agents = TRUE')

  irf[['agent']] <- c('BC', 'BC', 'CO2', 'CO2')
  testthat::expect_error(check_agent(irf, rf), 'irf can only contain information for one agent')

})


testthat::test_that('check_config_matrix', {

  m <- c()
  testthat::expect_error(check_config_matrix(m), 'config_matrix must be a matrix')

  m <- matrix(rep(0, 10))
  testthat::expect_error(check_config_matrix(m), 'config_matrix is missing a 1 entry, HIRM will not run')

  m <- matrix(rep(sample(1:3), 10))
  testthat::expect_error(check_config_matrix(m), 'config_matrix can only contain entries of 0 and 1')

  m <- matrix(rep(sample(0:1), 10))
  testthat::expect_error(check_config_matrix(m), 'config_matrix is missing column names, should be the irf names')

  colnames(m) <- 'fake name'
  testthat::expect_error(check_config_matrix(m), 'config_matrix is missing row names, should be the rf time series names')


})
