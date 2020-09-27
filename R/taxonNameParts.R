###############################################################
#' Parse a taxonomic name into components
#'
#' @param taxonName Character variable storing a name to be parsed.
#' @param verbose Logical. Produce diagnostic information when TRUE; default is FALSE
#'
#' @return A named character vector with elements "genus", "species", "infraRank", "infraName"
#' @export
#'
#' @examples
#' \dontrun{
#' ## A genus only:
#' ans <- taxonNameParts("Eucalyptus")
#' ## 'Eucalyptus sp.'
#' ## A binomial
#' ans <- taxonNameParts("Acacia terminalis")
#' ## A trinomial
#' ans <- taxonNameParts("Acacia terminalis subsp. terminalis")
#' }
taxonNameParts <- function(taxonName, verbose = FALSE)
{
  if (verbose) cat("  taxonNameParts called with taxonName =", taxonName, "\n")

  # Set-up a strucuture to hold results of the parse step. Note that ans[2] <-
  # "sp." so that when a genus name is parsed we will return "Agenus sp."
  ans <- c("No_data", "sp.", "No_data", "No_data")
  names(ans) <- c("genus", "species", "infraRank", "infraName")

  bits <- unlist(strsplit(taxonName, " ", fixed = TRUE))
  if(verbose) print(bits)

  for (i in 1:length(bits))
    ans[i] <- bits[i]

  return(ans)
}
