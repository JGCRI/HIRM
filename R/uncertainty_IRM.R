
#' Uncertainty wrapper for the core_IRM
#'
#' \code{uncertainty_IRM} run the core_IRM mulitple times with RF time series and/or IRFs that have been modified
#' by some uncertainty scaler. Depending on the configuraiton of the uncertainty runs the total number of times that
#' HIRM will run increases expoentially, this will cause the run time to increase and the results may be too large
#' to store as a single object, may exceede R's memory limit. The arguement write_out can be used to store the
#' HIRM uncertainty results individually for each uncertainty run. This function as options to do the uncertainty runs in parallel.
#'
#' @param uncertianty_scalers a list of vectors containing uncertainty scalers for a specifc RF time series or IRF. The core_IRM will be run with all possible combinations of the uncertainty scalers.
#' @param config_matrix a matrix containing boolean indicators that refers to a specific RF time series and IRF to use as inputs in the contributing_temp function
#' @param rf_list a list of the RF time series
#' @param irf_list a list of the IRF
#' @param match_agents a logial value indicating whether the rf agent must match the irf, default set to FALSE
#' @param write_out default set to NULL will return a list of the uncertainty IRM results, but if set to a working direcotry will save the individual IRM resutls at the specified lcoation.
#' @param n_core default set to 1, defined the number of cores to parallelize the runs over.
#' @importFrom dplyr %>%
#' @importFrom foreach %do%
#' @return a depends on how the argument write_out is defined. If write_out = NULL then the function will return a nested list of IRM results for each uncertainty run but if write_out = 'A/DIRECTORY' then the function will return a list of file names where the results have been saved for each run.
#' @export
uncertainty_IRM <- function(uncertianty_scalers, config_matrix, rf_list, irf_list, match_agents = FALSE, write_out = NULL, n_core = 1){

  # Silence the package checks
  run <- sclr_name <- NULL

  ## Check the inputs ##
  assertthat::assert_that(is.list(uncertianty_scalers), msg = 'uncertainty_scalers must be a list')

  missing <- setdiff(names(uncertianty_scalers), c(names(irf_list), names(rf_list)))
  assertthat::assert_that(length(missing) == 0, msg = paste0('irf and/or rf is missing ', paste(missing, collapse = ', ')))

  duplicate_names <- intersect(names(irf_list), names(rf_list))
  assertthat::assert_that(length(duplicate_names) == 0, msg = paste0(paste(duplicate_names, collapse = ', '), ' exist in both rf_list and irf_list'))

  # write_out can be null or an existing directory.
  if(is.null(write_out)){
  } else if(is.character(write_out) && dir.exists(write_out)) {
  } else {
    stop('write_out can either be set to NULL or a directory that exists')
  }

  # Set up the indicies to use to loop over / combine the the uncertainty scalars.
  uncert_names <- names(uncertianty_scalers)
  # Save a copy of the number of scalars in each uncertianty_scalers element.
  og_sizes     <- lengths(uncertianty_scalers)
  sizes        <- og_sizes
  # Determine how many times the scalars in each element must be repeated in a row
  # in order to create a the pattern that combines all of the scalars uniquely.
  indices        <- cumprod(c(1L, sizes))
  indices        <- indices[names(indices) %in% uncert_names]
  n_combo        <- max(indices)
  group_sizes    <- n_combo / indices
  og_group_sizes <- group_sizes

  # Initae the scalar index.
  scalar_index        <- rep(1, length.out = length(uncertianty_scalers))
  names(scalar_index) <- uncert_names


  # Combine the uncertianty_scalers based on the indicies.
  output <- foreach::foreach(run = seq_len(n_combo)) %do% {

    # Combine the RF and the IRF HIRM inputs.
    combined_rf_irf <- append(rf_list, irf_list)

    # Depending on the run determine if the scalar index needs to be increased or not.
    if(run == 1){

      change_index <- NULL

    } else {

      status       <- run <= group_sizes & group_sizes != 1
      change_index <- which(!status)

      # If applicable increase the scalar index by 1.
      if(length(change_index) != 0){

        group_sizes[change_index]  <- group_sizes[change_index] + og_group_sizes[change_index]
        scalar_index[change_index] <- scalar_index[change_index] + 1

        # If the scalar index exceedes the original sizes then move the index back to 1
        reset <- c(which(scalar_index > og_sizes), which(scalar_index == og_sizes & group_sizes == 1))
        if(length(reset) != 0){
          scalar_index[reset]  <- 1
        }
      }
    }

    # Replace IRM inputs with inputs perturbed by the uncertainty scalar.
    scalar_values <- foreach::foreach(sclr_name = names(scalar_index), .combine = cbind) %do% {

      scalar_value <- uncertianty_scalers[[sclr_name]][scalar_index[[sclr_name]]]
      new_value    <- combined_rf_irf[[sclr_name]]$value * scalar_value
      combined_rf_irf[[sclr_name]]$value <- new_value

      scalar_value
    }

    # Save the scalar values as data frame.
    colnames(scalar_values) <- names(scalar_index)
    scalar_table            <- data.frame(run = paste0('run_', run), scalar_values, stringsAsFactors = FALSE)

    # Parse out the IRFs and RFs from one another.
    modified_rf  <- combined_rf_irf[ names(combined_rf_irf) %in% names(rf_list) ]
    modified_irf <- combined_rf_irf[ names(combined_rf_irf) %in% names(irf_list) ]

    # Run the IRM using the uncertainty modified rf and irf inputs.
    rlst <- core_IRM(config_matrix = config_matrix, rf_list = modified_rf,
                     irf_list = modified_irf, match_agents = match_agents)

    # Add the run name the output.
    rlst$total_temp$run         <-  scalar_table$run
    rlst$temp_contributions$run <-  scalar_table$run
    rlst$uncertainty_run        <-  scalar_table

    # Format the RF inputs, make sure that it only include values that were used in the HIRM run.
    rf_input_names <- row.names(which(config_matrix == 1, arr.ind = TRUE))
    rlst$rf_inputs     <- dplyr::bind_rows(modified_rf[names(modified_rf) %in% rf_input_names])
    rlst$rf_inputs$run <- scalar_table$run

    # Return the uncertainty results or write them out depending on how
    # the write_output argument.
    if(is.null(write_out)){
      rlst
    } else {
      out_file <- file.path(write_out, paste0('run_', run, '.rds'))
      saveRDS(rlst, file = out_file)
      NULL
    }
  }

  if(is.null(write_out)){

    names(output) <- paste0('run_', 1:n_combo)
    output

  } else {

    NULL
  }


}

