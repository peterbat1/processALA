
#' Show galah interpretation of ALA Occurrence fields
#'
#' @return A dataframe listing all field names available for use with the galah package functions.
#' @export
#' @details
#' The returned dataframe shows all the information associated with the set of fields which may be specified when instigating a data download in the function \link{fetchALAdata}.
#'
#' In their infinite wisdom, the galah developers have (again without warning) decided that a few important fields will be summarily renamed from the internal ALA download name. This occurred with the release of galah 1.5.1 in January 2023.
#'
#' The original form of this function returned the ALA table of field names. This version returns the galah hacked version so that it complies with the galah functions used to download occurrence data, and so allows you to select field names to be passed in the parameter \emph{theseFields}
#' for the  function \link{fetchALAdata}. The required galah-hacked field names are found in the 'id' column if the returned data.frame.
#'
#' @examples
#' \dontrun{
#' ## Return the standard occurrence fields used in processALA:
#' stdStuff <- showOccFields()
#'
#' }
#'
showOccFields <- function()
{
  return(as.data.frame(galah::show_all(fields)))
}



