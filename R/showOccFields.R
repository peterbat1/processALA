
#' Show ALA Occurrence fields
#'
#' @param fieldSet Character string. Specifies the set of ALA occurrence fields to be returned.
#'
#' @return A dataframe listing all ALA-supplied information for database fields linked to occurence records.
#' @export
#' @details
#' The returned dataframe shows all the information associated with the set of fields specified by \emph{fieldSet}. This is very large body of information. The values which must be supplied to \link{fetchALAdata} in the parameter \emph{fieldSet} are found in the 'name' column.
#'
#' The default value for \emph{fieldSet} is 'standard' and this returns the set of fields used for a default download of records; 'all' shows all available ALA occurrence fields which may be used for customised calls to the function \link{fetchALAdata}.
#'
#'
#' @examples
#' \dontrun{
#' ## Return the standard occurrence fields used in processALA:
#' stdStuff <- showOccFields()
#'
#' ## The full set of stuff:
#' allStuff <- showOccFields(fieldSet = "all")
#' }
#'
showOccFields <- function(fieldSet = "standard")
{
  if (!(fieldSet %in% c("standard", "all")))
    stop("showOccFields: Parameter 'fieldSet' must be one of 'standard' or 'all'")

  # Get list of occurrence fields
  targetURL <- "https://biocache-ws.ala.org.au/ws/index/fields"
  ans <- httr::content(httr::GET(targetURL))

  fullTable <- as.data.frame(dplyr::bind_rows(ans))

  if (fieldSet == "standard")
    return(fullTable[match(stdFields, fullTable$downloadName),])
  else
    return(fullTable)
}



