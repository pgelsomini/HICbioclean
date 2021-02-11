#' Continuous measurement sites and their reference sites.
#'
#' Dataset with the site numbers of the continuously measured biological parameters and their nearest periodically measured reference site.
#'
#' @format A data frame with 2 variables:
#' \describe{
#'   \item{MonthlySites}{Site names of the nearest periodically measured site}
#'   \item{ContinuousSites}{Site numbers of the continuously measured sites}
#'   ...
#' }
#' @source Flemish Hydrological Information Center, OMES Monitoring
"ReferenceSiteLinkage"


#' Import codes for the Flemish Hydrological Information Center database .
#'
#' Codes for each site number and parameter for importation into the Flemish Hydrological Information Center database.
#'
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{StationNo}{Continuous measurement station number}
#'   \item{Par}{Parameter}
#'   \item{Code}{Database code}
#'   \item{Unit}{Unit}
#'   ...
#' }
#' @source Flemish Hydrological Information Center
"zrxFileStationCodes"
