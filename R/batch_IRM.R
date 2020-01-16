
#' Run the core_IRM multiple times
#'
#' \code{batch_IRM} run the core_IRM multiple times and return a list of three tibbles,
#'
#' @param batch_list a list of configuration matrices to use as inputs into the core_IRM
#' @param irf_list the list of irf data frames.
#' @param rf_list the list of pathway data frames.
#' @param match_agents a logical value indicating whether the rf agent must match the irf, default set to FALSE
#' @importFrom dplyr %>%
#' @export

batch_IRM <- function(batch_list, rf_list, irf_list, match_agents){

  # Silence pacakge checks
  variable <- names <- year <- value <- RF_agent <- RF_name <-
    IRF_name <- name <-. <- NULL

  # Check inputs
  assertthat::assert_that(is.list(batch_list), msg = 'batch_list must be a list')
  assertthat::assert_that(!is.null(names(batch_list)), msg = 'elements of batch_list must be named')

  # Run the core IRM using every configuration matrix in the batch list.
  lapply(names(batch_list),
         function(name){

           core_IRM(config_matrix = batch_list[[name]], rf_list = rf_list, irf_list = irf_list, match_agents = match_agents) %>%
             dplyr::bind_rows() %>%
             dplyr::mutate(name = name)
         }) %>%
    dplyr::bind_rows() ->
    rslt

  # Format and return output.
  output <- list()

  rslt  %>%
    dplyr::filter(variable == 'Tgav') %>%
    dplyr::select(name, year, value, variable) ->
    output[['total_temp']]

  rslt %>%
    dplyr::filter(variable ==   "contributing temp") %>%
    dplyr::select(name, year, value, variable, RF_agent, RF_name, IRF_name) ->
    output[['temp_contributions']]

  output
}

