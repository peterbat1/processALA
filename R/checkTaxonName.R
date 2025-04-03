###############################################################
#' Check a taxon name against the National Species List maintained by ALA
#'
#' Perform a systematic search of taxonomic resources to determine the status of the taxonomic name of a plant species using the resources of APNI and APC.
#'
#' @param thisTaxon Character object holding the name for which a search is to be made.
#' @param plantsOnly Logical. Return information on plant species? Default is TRUE.
#' @param quiet Logical. If TRUE progress messages are written to the console; if FALSE (default) then progress messages are suppressed.
#'
#' @return A one row data.frame with the following elements:
#' \item{isValid}{Logical. Is the searched for taxonomic name found as an entry in the APNI?}
#' \item{isAccepted}{Logical. Is the searched for taxonomic name accepted by APC?}
#' \item{thisTaxon}{Taxonomic name searched for.}
#' \item{acceptedName}{The accepted taxon name corresponding to the name searched for.}
#' \item{fullAcceptedName}{Monomial/binomial/trinomial + author of the accepted taxon name.}
#' \item{genus}{Genus name for specific and infraspecific ranks. Blank for a Family or higher taxonomic name search.}
#' \item{specificEpithet}{Specific epithet for a spcies or infraspecies search. Blank otherwise.}
#' \item{infraSpecificRank}{Taxonomic rank of an infra-specific taxon; blank for other ranks.}
#' \item{infraSpecificEpithet}{Terminal or infra-specific epithet for a trinomial; blank for other taxonomic ranks.}
#' \item{infraSpecificRankAbbrev}{Abbreviation used for an infra-specific rank; blank otherwise.}
#' \item{acceptedGUID}{The ALA Globally Unique Identifier or GUID; a numeric string uniquely indexing a taxonomic concept in the Australian Plant Name Index (APNI) component of the National Species List (NSL). No match results in a GUID of "Not_accepted" being returned.}
#' \item{acceptedFullGUID}{The full URL form of the GUID: stored as a convenience to speed calls to the ALA API, and assist maintence of code.}
#' \item{formattedAcceptedName}{Full taxonomic name (i.e. binomial/trinomial + author) with simple HTML mark-up to italicise the binomial/trinomial part.}
#' \item{taxonomicStatus}{Taxonomic status of the accepted name.}
#' \item{taxonomicRank}{Taxonomic rank of the accepted name.}
#' \item{totalRecords}{Number of occurrence records reported by ALA for the accepted taxon.}
#' \item{parentGUID}{Full GUID of the taxonomic parent of the accepted taxon.}
#' \item{parentName}{Name of the taxonomic parent of the accepted taxon.}
#' \item{parentTaxonomicRank}{Taxonomic rank of the parent.}
#' \item{synonyms}{A semi-colon separated list of taxonomic synonyms of the accepted taxon.}
#' \item{apcFamily}{APC family of the accepted taxon.}
#' \item{alaCommonNames}{Common names listed for the taxon according to ALA.}
#' \item{inferredAcceptedInfo}{Summary of information on 'inferredAccepted' names; these are names which are retained in APNI because they are listed in statutory instruments in one or more jurisdictions within Australia, and are therfore associated with occurrence and other data.}
#'
#' Class "taxonInfo" is appended to the returned object, but it remains 'functionally' a data.frame.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ### Search a valid name
#' ans <- checkTaxonName("Acacia linifolia")
#'
#' ### Search for a clearly invalid name
#' ans <- checkTaxonName("Greenus plantus")
#'   }
checkTaxonName <- function(thisTaxon = NULL, plantsOnly = TRUE, quiet = TRUE)
{
  if (is.null(thisTaxon))
    stop("'thisTaxon' cannot be NULL: please supply a taxonomic name")

  if (!quiet) cat("Checking taxon name:", thisTaxon, "\n")

  # From this point on, make sure that references to genera are without a
  # trailing "sp." as this can lead to unexpected returns from name searches,
  # whereas searches on the pure genus name will always return clean results
  thisTaxon <- trimws(sub("sp.$|spp.$", "", trimws(thisTaxon)))

  # Set a default or empty checkResult data.frame. If any useful taxonomic
  # information is recovered, then columns will be replaced with meaningful data
  checkResult <- data.frame(isValid = FALSE,
                            isAccepted = FALSE,
                            searchName = thisTaxon,
                            searchName_taxonomicStatus = "Not accepted",
                            acceptedName = "",
                            fullAcceptedName = "",
                            genus = "",
                            specificEpithet = "",
                            infraSpecificRank = "",
                            infraSpecificEpithet = "",
                            infraSpecificRankAbbrev = "",
                            taxonAuthor = "",
                            acceptedGUID = "",
                            acceptedFullGUID = "",
                            formattedAcceptedName = "",
                            taxonomicStatus = "",
                            taxonomicRank = "",
                            totalRecords = 0,
                            parentGUID = "",
                            parentName = "",
                            parentTaxonomicRank = "",
                            synonyms = "",
                            apcFamily = "",
                            alaCommonNames = "",
                            inferredAcceptedInfo = "",
                            stringsAsFactors = FALSE)

  class(checkResult) <- c(class(checkResult), "taxonInfo")
  rownames(checkResult) <- ""

  # Abort instantly if user request information and new and therefore completely
  # unknown to APNI/APC taxon
  if (grepl("sp. nov.", thisTaxon))
  {
    if (!quiet) cat(">>>> Cannot search for species novum\n")
    return(checkResult)
  }

  if (plantsOnly)
  {
    name_search_galah <- galah::search_taxa(tibble::tibble(kingdom = "Plantae", scientificName = thisTaxon))
  } else
  {
    name_search_galah <- galah::search_taxa(thisTaxon)
  }

  if (name_search_galah$rank == "kingdom")
  {
    if (!quiet) cat(">>>> Unrecognised taxon name\n")

    # Return an empty result:
    return(checkResult)
  }

  # Ok to take next steps to gather all necessary data elements...
  search_GUID_galah <- galah::search_identifiers(name_search_galah$taxon_concept_id)

  if (search_GUID_galah$success)
  {
    guid_search_bailout <- httr::content(httr::GET("http://bie.ala.org.au/", path = paste0("ws/species/", name_search_galah$taxon_concept_id, ".json")))
    name_search_bailout <- httr::content(httr::GET("http://bie.ala.org.au/", path = "ws/search.json", query = list(q = name_search_galah$scientific_name)))
    name_parts <- stringr::str_split(name_search_galah$scientific_name, " ")[[1]]
    acceptedName <- name_search_galah$scientific_name
    parent_info <- galah::search_identifiers(name_search_bailout$searchResults$results[[1]]$parentGuid)
    total_records <- ifelse(is.null(name_search_bailout$searchResults$results[[1]]$occurrenceCount), 0, name_search_bailout$searchResults$results[[1]]$occurrenceCount)

    common_names <- unlist(lapply(guid_search_bailout$commonNames, function(el){el$nameString}))
    dupl_ind <- which(duplicated(common_names))
    if (length(dupl_ind) > 0) common_names <- common_names[-dupl_ind]

    if (name_search_galah$rank == "genus")
    {
      genus <- name_search_galah$genus
      specificEpithet <- ""
      infraSpecificRank <- ""
      infraSpecificEpithet <- ""
      infraSpecificRankAbbrev <- ""
      taxonomicRank <- name_search_galah$rank
      taxon_author <- name_search_bailout$searchResults$results[[1]]$scientificNameAuthorship
      formattedAcceptedName <- paste0("<i>", genus, "</i> ", name_search_bailout$results$scientific_name_authorship)
    }

    if (name_search_galah$rank == "species")
    {
      genus <- name_search_galah$genus
      specificEpithet <- name_parts[2]
      infraSpecificRank <- ""
      infraSpecificEpithet <- ""
      infraSpecificRankAbbrev <- ""
      taxonomicRank <- name_search_galah$rank

      if (is.null(name_search_bailout$searchResults$results[[1]]$scientificNameAuthorship))
        taxon_author <- name_search_galah$scientific_name_authorship
      else
        taxon_author <- name_search_bailout$searchResults$results[[1]]$scientificNameAuthorship

      formattedAcceptedName <- paste0("<i>", genus, " ", specificEpithet, "</i> ", name_search_bailout$results$scientific_name_authorship)
    }

    if (name_search_galah$rank %in% c("subspecies", "form", "variety"))
    {
      genus <- name_search_galah$genus
      specificEpithet <- name_parts[2]
      infraSpecificRank <- name_search_galah$rank
      infraSpecificEpithet <- name_parts[4]
      infraSpecificRankAbbrev <- name_parts[3]
      taxonomicRank <- name_search_galah$rank
      taxon_author <- parent_info$scientific_name_authorship
      formattedAcceptedName <- paste0("<i>", genus, " ", specificEpithet, "</i> ", parent_info$scientific_name_authorship, " ", infraSpecificRankAbbrev, " <i>", infraSpecificEpithet, "</i> ")
    }

    # Make a data.frame for return
    checkResult <- data.frame(isValid = TRUE,
                              isAccepted = thisTaxon == acceptedName,
                              searchName = thisTaxon,
                              searchName_taxonomicStatus = name_search_bailout$searchResults$results[[1]]$taxonomicStatus,
                              acceptedName = name_search_galah$scientific_name,
                              fullAcceptedName = name_search_bailout$searchResults$results[[1]]$nameComplete,
                              genus = genus,
                              specificEpithet = specificEpithet,
                              infraSpecificRank = infraSpecificRank,
                              infraSpecificEpithet = infraSpecificEpithet,
                              infraSpecificRankAbbrev = infraSpecificRankAbbrev,
                              taxonAuthor = taxon_author,
                              acceptedGUID = strsplit(name_search_galah$taxon_concept_id, "apni/")[[1]][2],
                              acceptedFullGUID = name_search_galah$taxon_concept_id,
                              formattedAcceptedName = formattedAcceptedName,
                              taxonomicStatus = guid_search_bailout$taxonConcept$taxonomicStatus,
                              totalRecords = total_records,
                              taxonomicRank = name_search_galah$rank,
                              parentGUID = parent_info$taxon_concept_id,
                              parentName = parent_info$scientific_name,
                              parentTaxonomicRank = parent_info$rank,
                              synonyms = paste(unlist(lapply(guid_search_bailout$synonyms, function(el){el$nameString})), collapse = "; "),
                              apcFamily = name_search_galah$family, # also supplied by name_search_galah$family
                              alaCommonNames = paste(common_names, collapse = "; "),
                              inferredAcceptedInfo = "", #inferred_info,
                              stringsAsFactors = FALSE)

    if (!quiet) cat(">>>> Successfully compiled data")
    return(checkResult)
  } else
  {
    if (!quiet) cat(">>>> Failed GUID search\n")

    # Return an empty result:
    return(checkResult)
  }
}

