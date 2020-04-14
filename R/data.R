#' Hector_IRF
#'
#' This is a list of the Hector impulse response functions.
#' Each element in the list should match the impulse response function.
#'
#' @format A list of a data frame containing the impulse response functions.
#' \describe{
#' \item{name}{The name of the impulse response function; should match the name of the element in the list}
#' \item{year}{Year since the emissions impulse}
#' \item{value}{The impulse response function value}
#' \item{units}{A string of units for the impulse response function (degC * m2 / W)}
#' \item{agent}{The name of the agent being pulsed}
#' }
#' @family IRF inputs
'Hector_IRF'


#' Sand_BC_IRF
#'
#' This is a dataframe of the BC IRF derived from the temperature response to a BC step test from Sand et al.
#'
#' @format A data frame containing:
#' \describe{
#' \item{name}{The name of the impulse response function; should match the name of the element in the list}
#' \item{year}{Year since the emissions impulse}
#' \item{value}{The impulse response function value}
#' \item{units}{A string of units for the impulse response function (degC * m2 / W)}
#' \item{agent}{The name of the agent being pulsed}
#' }
#' @family IRF inputs
'Sand_BC_IRF'

#' Hector_BC_IRF
#'
#' This is a data frame of a modified version of Hector's general IRF. The magnitude of the IRF was derived
#' from the Sand et al. BC IRF. So that the temporal evolution of this IRF matches Hector's general IRF while
#' the magnitude of the IRF reflects the BC species, specifically.
#'
#' @format A data frame containing:
#' \describe{
#' \item{name}{The name of the impulse response function}
#' \item{year}{Year since the emissions impulse}
#' \item{value}{The impulse response function value}
#' \item{units}{A string of units for the impulse response function (degC * m2 / W)}
#' \item{agent}{The name of the agent being pulsed}
#' }
#' @family IRF inputs
'Hector_BC_IRF'


#' Hector_RF
#'
#' This is a list of Hector RF drivers.
#'
#' @format A list of a data frame containing the RF driver time series.
#' \describe{
#' \item{scenario}{The name of the Hector core the output came from.}
#' \item{year}{Absolute year.}
#' \item{variable}{The name of the value, a type of RF.}
#' \item{value}{The RF value.}
#' \item{units}{The units for the RF value.}
#' \item{name}{The name of the RF driver.}
#' \item{agent}{The name of the RF agent.}
#' }
#' @family RF inputs
'Hector_RF'


#' Hector_comparison
#'
#' This is a list of the Hector temperature anomaly that is used as comparison
#' data in the HIRM validation experiments.
#'
#' @format A list of a data frame containing
#' \describe{
#' #' \item{scenario}{The name of the Hector scenario run}
#' \item{year}{Interger value of the year}
#' \item{variable}{A description of the value Tgav (global mean temperature anomaly)}
#' \item{units}{A string of units for value (degC or W/m2)}
#' }
#' @family Comparison data
'Hector_comparison'

#' Hector_ConfigMatrix
#'
#' This is an empty input configuration matrix for Hector.
#'
#' @section Notes: The core HRIM configuration matrix is set up with RF drivers as rows and IRF as columns.
#' Right now the matix is empty, and only contains the value 0. When values in the matrix are set to 1 then
#' the RF driver and the IRF paring corresponding to the value will be used in HIRM.
#'
#' @format A matrix with named rows and columns.
#' \describe{
#' \item{rows}{The rows are named after the RF drivers.}
#' \item{columns}{The columns are named after the IRFs.}
#' }
#' @family Configuration matrices
'Hector_ConfigMatrix'
