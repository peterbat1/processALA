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

  # Set-up a structure to hold results of the parse step. Note that ans[2] <-
  # "sp." so that when a genus name is parsed we will return "Agenus sp."
  ans <- c("No_data", "sp.", "No_data", "No_data")
  names(ans) <- c("genus", "species", "infraRank", "infraName")

  bits <- unlist(strsplit(taxonName, " ", fixed = TRUE))
  if(verbose) print(bits)

  if (bits[2] != "sp.")
  {
    # Did we split an 'x' name?
    if (bits[2] == "x")
    {
      bits[2] <- paste(bits[2], bits[3], bits[4])
      bits[3] <- ""
      bits[4] <- ""
    }

    # Did we split a hyphenated cultivar name?
    hyphenInd <- grep("'", bits)
    if (length(hyphenInd) > 0)
    {
      bits[2] <- paste(bits[hyphenInd], collapse = " ")
      bits[3] <- ""
      bits[4] <- ""
    }
  }

  for (i in 1:length(bits))
    ans[i] <- bits[i]

  return(ans)
}
