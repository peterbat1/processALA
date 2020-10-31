
#' Show ALA Occurrence fields
#'
#' @param fieldSet Character string. Specifies the set of ALA occurrence fields to be returned.
#'
#' @return A dataframe listing all ALA-supplied information for the specified set of fields.
#' @export
#' @details {
#' The returned dataframe shows all the information associated with the set of fields specified by 'fieldSet'. This is very large body of information. The values which must be supplied to \link{fetchALAdata} in the parameter 'fieldSet' are found in the 'name' column.
#'
#' The default value for 'fieldSet' is 'standard' and this returns the set of fields used for a default download of records; 'all' shows all available ALA occurrence fields which may be used for customised calls to the function \link{fetchALAdata}.
#'
#' Note that this function is simple wrapper around the ALA4R function \link{ala_fields}.
#' }
#' @examples
#' \dontrun {
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

  fullTable <- ALA4R::ala_fields(fields_type = "occurrence", as_is = TRUE)

  if (fieldSet == "standard")
    return(fullTable[match(stdFields, fullTable$name),])
  else
    return(fullTable)
}
