#' Ensures that an object contains the required columns.
#'
#' \code{check_columns} if the object is missing a required column then this function throws an error. If the
#' object contains all or more of the required columns the object passes this condition check.
#'
#' @param object the data frame or tibble to check
#' @param required_columns names of the columns that must be included in the object
#' @param message_details default set to NULL but if the user sets equal to a string, the contents of the string will be included in the error message
#' @return an error message if a required column is missing
#' @keywords internal
check_columns <- function(object, required_columns, message_details = NULL){

  missing_columns <- which(!required_columns %in% colnames(object))
  if(length(missing_columns) >= 1){

    are_missing <- required_columns[missing_columns]
    stop(message_details, ' missing ', paste(are_missing, collapse = ', '))

  }
}



#' Ensure data for a single RF agent is used on convolution
#'
#' \code{check_agent} make sure that the rf and irf tibbles contain data for a single RF agent, if match_agents
#' is true then make sure that both the RF agent in both rf and irf tibbles are the same.
#'
#' @param irf the impulse response function tibble, must contain a year and value column
#' @param rf the radiative forcing time series tibble, must contain a year and value column
#' @param match_agents a logical value indicating whether to throw an error if the rf agent does not match the irf agent, default value is set to FALSE
#' @return an error message if multiple agent conditions are not met in the rf and irf tibbles
#' @keywords internal
#' @importFrom assertthat assert_that
check_agent <- function(irf, rf, match_agents = FALSE){

  assert_that(length(unique(rf[['agent']])) == 1, msg = 'rf can only contain information for one agent')
  assert_that(length(unique(irf[['agent']])) == 1, msg = 'irf can only contain information for one agent')

  if(match_agents){

    assert_that(unique(rf[['agent']]) == unique(irf[['agent']]), msg = 'irf and rf agent must be the same if match_agents = TRUE')

  }

}



#' Check the irf and rf inputs before performing convolution
#'
#' \code{check_time} checks to see if the IRF and the RF time series can be be convolved in
#' the \code{contributing_temp} function. The irf and rf data frame must contain values that correspond to
#' temporal data that consists of only one time step, that is equal in both data frames and spans an equal number of years.
#' If thes conditions are not met then this function will throw an error and prevent the convolution.
#'
#' @param irf the impulse response function tibble, must contain a year and value column
#' @param rf the radiative forcing time series tibble, must contain a year and value column
#' @return an error message if the two data frames do not meet the criteria.
#' @keywords internal
#' @importFrom assertthat assert_that
check_time <- function(irf, rf){

  diff_by <- nrow(irf) - nrow(rf)
  assert_that(abs(diff_by) == 0, msg = paste0('irf and rf differ by ', diff_by, ' time steps.'))

  assert_that(any(!duplicated(irf[['year']])), msg = 'irf cannot contain multiple entries for the same year')
  assert_that(any(!duplicated(rf[['year']])), msg = 'rf cannot contain multiple entries for the same year')

  irf_step <- diff(irf[['year']])
  rf_step  <- diff(rf[['year']])

  assert_that(length(unique(irf_step)) == 1, msg = 'irf must contain a constant time step size')
  assert_that(length(unique(rf_step)) == 1, msg = 'rf must contain a constant time step size')

  assert_that(all(identical(irf_step, rf_step)), msg = 'irf and rf must contain the same time step size')

}



#' Check the configuration matrix used in core_IRM
#'
#' \code{check_config_matrix} make sure that the configuration matrix meets the following conditions:
#' it is a matrix; that is contains binary entires; that is contains at least one entry equal to one
#' otherwise the contributing_temp wont' run; and that the matrix has column and row names.
#' at least one entry that is equal to one otherwise contributing_temp won't run, and a
#'
#' @param m configuration matrix to check
#' @return an error message if the one of the matrix conditions are not met by the matrix
#' @keywords internal
#' @importFrom assertthat assert_that
check_config_matrix <- function(m){

  assert_that(is.matrix(m), msg = 'config_matrix must be a matrix')
  assert_that(all(m %in% c(0, 1)), msg = 'config_matrix can only contain entries of 0 and 1')
  assert_that(any(1 %in% m), msg = 'config_matrix is missing a 1 entry, HIRM will not run')
  assert_that(!is.null(colnames(m)), msg = 'config_matrix is missing column names, should be the irf names')
  assert_that(!is.null(row.names(m)), msg = 'config_matrix is missing row names, should be the rf time series names')

}


