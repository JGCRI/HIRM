
#' The temperature contribtuion of a single RF agent
#'
#' \code{contributing_temp} convolves a RF timeseries with an IRF for a single radiative forcing agent, this is
#' equivalent to the temperature contirbution of a single radiative forcing agent. This function is used in \code{core_IRM}
#'
#' @param rf a tibble of a RF time series for a single RF agent, must contain a year, value, name, and agent column
#' @param irf a tibble of the IRF to convolve wihth the RF time series, must contain a year, value, name, and agent column
#' @param match_agents a logial value indicating whether the rf agent must match the irf, default set to FALSE
#' @importFrom dplyr %>%
#' @keywords internal
#' @return a tibble containting a year, value, varibale, RF_name, IRF_name, and agent columns

  contributing_temp <- function(rf, irf, match_agents){

    # Silence package checks
    value <- NULL

    # Checks
    # Ensure the rf and irfs have the required column names.
    assertthat::assert_that(all(c('year', 'value', 'name', 'agent') %in% names(rf)), msg = 'rf input missing required columns')
    assertthat::assert_that(all(c('year', 'value', 'name', 'agent') %in% names(irf)), msg = 'irf input missing required columns')


    # Make sure the irf and rf tibbles only contain information for a single RF agent.
    check_agent(irf = irf, rf = rf, match_agents = match_agents)

    # Make sure the irf and rf tibbles meet the time length and time setp requirements.
    #check_time(irf = irf, rf = rf)

    # Convolve the RF timeserie with the IRF. This function will return a vector that is
    # length of irf + length of rf - 1 values long.
    full_rslt <- stats::convolve(rf[["value"]], rev(irf[["value"]]), conj = TRUE, type = "open")

    # Because stats::convolve padds the rf values with 0 to complete the convolution subset the
    # full rslt data so that it only includes entries corresponding to rf values.
    rslt <- full_rslt[1:nrow(rf)]

    # Format the years for the results returned by the convolution. The convolution output year is
    # going to equal the IRF lag + the RF timeseries year, where the IRF lag is equal to the first
    # year of the IRF.
    irf_start <- min(irf[['year']])
    rslt_year <- rf[['year']] + irf_start

    # Format output
    data.frame(year = rslt_year,
               value = rslt,
               variable = 'contributing temp',
               RF_agent = unique(rf[['agent']]),
               RF_name = unique(rf[['name']]),
               IRF_name = unique(irf[['name']]), stringsAsFactors = FALSE)
  }



#' IRM
#'
#' \code{core_IRM} is the IRM, calculates total temperature
#'
#' @param config_matrix a matrix containing boolean indicators that refers to a specific RF time series and IRF to use as inputs in the contributing_temp function
#' @param rf_list a list of the RF time series
#' @param irf_list a list of the IRF
#' @param match_agents default is set to FALSE, a logial value indicating whether the rf agent must match the irf, default set to FALSE
#' @importFrom dplyr %>%
#' @return a list of three tibbles, one of the total temp, one of the total RF, and one containting the contibuting temp from each RF agent
#' @export


  core_IRM <- function(config_matrix, rf_list, irf_list, match_agents = FALSE){

  # Silence package checks
  value <- year <- NULL

  # Check input conditions
  stopifnot(is.list(rf_list))
  stopifnot(is.list(irf_list))
  check_config_matrix(config_matrix)


  # Create a tibble of the RF time series and IRF combinations to use as contributing_temp inputs.
  # These combinations correspond to the configuration matrix entries with the value of 1.
  mapping <- data.frame(which(config_matrix == 1, arr.ind = TRUE))
  mapping$rf_name  <- row.names(config_matrix)[ mapping$row ]
  mapping$irf_name <- colnames(config_matrix)[ mapping$col ]


  # Make sure the rf_list and irf_list contains all of the rf and irf names in the
  # mapping tibble.
  missing_rf <- ! mapping$rf_name %in% names(rf_list)
  if(any(missing_rf)){
    missing_names <-  mapping$rf_name[ which(missing_rf) ]
    stop('rf_list is missing ', paste(missing_names, collapse = ', '))
  }

  missing_irf <- ! mapping$irf_name %in% names(irf_list)
  if(any(missing_irf)){
    missing_names <-  mapping$irf_name[ which(missing_irf) ]
    stop('irf_list is missing ', paste(missing_names, collapse = ', '))
    }


  # Now that all of the core_IRM input checks have passed all the condition checks
  # apply the contirbuting_temp function to each entry in the mapping file to
  # calculate the temperature contributions for each radiative forcing agent.
  mapply(
    function(rf_name, irf_name){
      contributing_temp(rf = rf_list[[rf_name]], irf = irf_list[[irf_name]], match_agents = match_agents)
    },
    rf_name = mapping$rf_name, irf_name = mapping$irf_name, SIMPLIFY = FALSE) %>%
    dplyr::bind_rows() ->
    temp_contributions


  # Finally use the temperature contributions to calcualte the total temperature.
  temp_contributions %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(variable = 'Tgav') ->
    total_temp

  return(list('total_temp' = total_temp, 'temp_contributions' = temp_contributions))

}



