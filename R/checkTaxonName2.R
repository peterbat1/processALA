###############################################################
#' Check a taxon name against the National Species List maintained by ALA
#'
#' Perform a systematic search of taxonomic resources to determine the status of the taxonomic name of a plant species using the resources of APNI and APC.
#'
#' @param thisTaxon Character object holding the name for which a search is to be made.
#' @param quiet Logical. If TRUE progress messages are written to the console; if FALSE (default) then progress messages are suppressed.
#'
#' @return A one row data.frame the following elements:
#' \item{isValid}{Logical. Is the searched for taxonomic name found as an entry in the APNI?}
#' \item{isAccepted}{Logical. Is the searched for taxonomic name accepted by APC?}
#' \item{searchName}{Taxonomic name searched for.}
#' \item{acceptedName}{The accepted taxon name corresponding to the name searched for.}
#' \item{fullAcceptedName}{Binomial/trinomial + author of the accepted taxon name.}
#' \item{acceptedGUID}{The ALA Globally Unique Identifier or GUID; a numeric string uniquely indexing a taxonomic concept in the Australian Plant Name Index (APNI) component of the National Species List (NSL). No match results in a GUID of "Not_accepted" being returned.}
#' \item{acceptedFullGUID}{The full URL form of the GUID: stored as a convenience to speed calls to the ALA API, and assist maintence of code.}
#' \item{formattedAcceptedName}{Full taxonomic name (i.e. binomial/trinomial + author) with simple HTML mark-up to italicise the binomial/trinomial part.}
#' \item{taxonomicRank}{Taxonomic rank of the accepted name.}
#' \item{parentGUID}{Full GUID of the taxonomic parent of the accepted taxon.}
#' \item{parentName}{Name of the taxonomic parent of the accepted taxon.}
#' \item{parentTaxonomicRank}{Taxonomic rank of the parent.}
#' \item{synonyms}{A semi-colon separated list of taxonomic synonyms of the accepted taxon.}
#' \item{apcFamily}{APC family of the accepted taxon.}
#' \item{alaCommonNames}{Common names listed for the taxon on ALA.}
#'
#' @export
#'
#' @examples
#' \dontrun{}
checkTaxonName <- function(thisTaxon = NULL, quiet = TRUE)
{
  if (is.null(thisTaxon))
    stop("'thisTaxon' cannot be NULL: please supply a taxonomic name")

  if (!quiet) cat("Checking taxon name:",thisTaxon,"\n")

  # From this point on, make sure that references to genera are without a
  # trailing "sp." as this can lead to unexpected returns from name searches,
  # whereas searches on the pure genus name will always return clean results
  thisTaxon <- trimws(sub("sp.$", "", trimws(thisTaxon)))

  if (!quiet) cat("  no match in taxonTable synonyms\n  checking APNI for matching names\n")
  ##nameSearch <- ALA4R::search_names(thisTaxon, output_format = "complete")

  # Fetch basic taxonomic information
  basicInfo <- galah::select_taxa(thisTaxon)

  if (length(basicInfo) > 1)
  {
    # Name returned results: we have more to do...
    tmp <- strsplit(basicInfo$taxon_concept_id, "/")
    guid <- tmp[[1]][length(tmp[[1]])]

    # Fetch synonym information
    moreInfo <- httr::content(httr::GET(paste0("https://bie-ws.ala.org.au/ws/species/", basicInfo$taxon_concept_id, ".json")))
    tmp  <- strsplit(moreInfo$taxonConcept$parentGuid, "/")
    parentGUID <- tmp[[1]][length(tmp[[1]])]
    #synonymInfo <- moreInfo$synonyms
    if (length(moreInfo$synonyms) > 0)
      synonyms <- paste(unlist(lapply(moreInfo$synonyms, function(el){el$nameString})), collapse = "; ")
    else
      synonyms = "No_data"

    parentTaxonInfo <- galah::select_taxa(moreInfo$taxonConcept$parentGuid, is_id = TRUE)

    checkResult <- data.frame(isValid = TRUE,
                              isAccepted = TRUE,
                              searchName = thisTaxon,
                              acceptedName = basicInfo[1, "scientific_name"],
                              fullAcceptedName = paste(basicInfo[1, "scientific_name"], basicInfo[1, "scientific_name_authorship"]),
                              genus = basicInfo[1, "genus"],
                              species = basicInfo[1, "scientific_name"],
                              infraSpecificRank = "No_data",
                              infraSpecificEpithet = "No_data",
                              taxonAuthor = "No_data",
                              acceptedGUID = guid,
                              acceptedFullGUID = basicInfo[1, "taxon_concept_id"],
                              formattedAcceptedName = paste0("<i>", basicInfo[1, "scientific_name"], "</i> ", basicInfo[1, "scientific_name_authorship"]),
                              taxonomicStatus = "No_data",
                              taxonomicRank = basicInfo[1, "rank"],
                              parentGUID = parentGUID,
                              parentName = parentTaxonInfo[1, "scientific_name"],
                              parentTaxonomicRank = parentTaxonInfo[1, "rank"],
                              synonyms = synonyms,
                              apcFamily = basicInfo[1, "family"],
                              alaCommonNames = paste(unlist(lapply(moreInfo$commonNames, function(el){el$nameString})), collapse = "; "),
                              stringsAsFactors = FALSE)
  }
  else
  {
    # Name is completely unknown to APNI so return a "Not_accepted" status
    if (!quiet) cat("  APNI returned a negative result\n")
    #thisStatus <- "Not_accepted"
    checkResult <- data.frame(isValid = FALSE,
                              isAccepted = FALSE,
                              searchName = thisTaxon,
                              acceptedName = "Not_accepted",
                              fullAcceptedName = "Not_accepted",
                              genus = "No_data",
                              species = "No_data",
                              infraSpecificRank = "No_data",
                              infraSpecificEpithet = "No_data",
                              taxonAuthor = "No_data",
                              acceptedGUID = "Not_accepted",
                              acceptedFullGUID = "Not_accepted",
                              formattedAcceptedName = "Not_accepted",
                              taxonomicStatus = "No_data",
                              taxonomicRank = "No_data",
                              parentGUID = "No_data",
                              parentName = "No_data",
                              parentTaxonomicRank = "No_data",
                              synonyms = "No_data",
                              apcFamily = "No_data",
                              stringsAsFactors = FALSE)
  }

  class(checkResult) <- c(class(checkResult), "taxonInfo")
  rownames(checkResult) <- ""
  invisible(checkResult)
}
