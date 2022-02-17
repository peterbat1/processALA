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
#' \item{parentGUID}{Full GUID of the taxonomic parent of the accepted taxon.}
#' \item{parentName}{Name of the taxonomic parent of the accepted taxon.}
#' \item{parentTaxonomicRank}{Taxonomic rank of the parent.}
#' \item{synonyms}{A semi-colon separated list of taxonomic synonyms of the accepted taxon.}
#' \item{apcFamily}{APC family of the accepted taxon.}
#' \item{alaCommonNames}{Common names listed for the taxon according to ALA.}
#'
#' Class "taxonInfo" is appended to the returned object, but it remains 'functionally' a data.frame.
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

  ans_httr_species_search <- httr::content(httr::GET(paste0("http://bie.ala.org.au/ws/search.json?q=", thisTaxon)))

  # Some search names appear to return NULL values in some fields, soe patch
  # them so that this don't fail later when the values in those fields are used
  #ans_httr_species_search <- lapply(ans_httr_species_search, function(el){if (is.null(el)) el <- "" else el})

  if (length(ans_httr_species_search$searchResults$results) > 1)
  {
    if (!quiet) cat("  Name search returned a result\n")

    # Some search names appear to return NULL values in some fields, so patch
    # them so that this don't fail later when the values in those fields are used
    ans_httr_species_search$searchResults$results[[1]] <- lapply(ans_httr_species_search$searchResults$results[[1]], function(el){if (is.null(el)) el <- "" else el})


    numResults <- length(ans_httr_species_search$searchResults$results)

    namesList <- NULL
    # # make a vector of found field names
    for (i in 1:numResults)
    {
      namesList <- c(namesList, names(ans_httr_species_search$searchResults$results[[i]]))
      duplInd <- which(duplicated(namesList))
      if (length(duplInd) > 0)
      {
        namesList <- namesList[-duplInd]
      }
    }

    allResults <- data.frame(matrix("", numResults, length(namesList)), stringsAsFactors = FALSE)
    colnames(allResults) <- namesList

    for (i in 1:numResults)
    {
      stuff <- ans_httr_species_search$searchResults$results[[i]][names(ans_httr_species_search$searchResults$results[[i]])]
      stuff <- lapply(stuff, function(el){if (is.null(el)) el <- "" else el})
      allResults[i, names(stuff)] <- unlist(stuff)
    }

    # Now we filter the table to remove extraneous matches
    goodInd <- grep(thisTaxon, allResults$name)
    allResults <- allResults[goodInd, ]

    badInd <- grep("NZOR", allResults$guid)
    if (length(badInd) > 0) allResults <- allResults[-badInd, ]

    synonymInd <- grep("SYNONYM|ACCEPTED", toupper(allResults$taxonomicStatus))
    if (length(synonymInd) > 0) allResults <- allResults[synonymInd, ]

    thisTaxonMatch <- grep(paste0("^", thisTaxon, "$"), allResults$scientificName)
    allResults <- allResults[thisTaxonMatch, ]

    if (allResults[1, "taxonomicStatus"] == "accepted")
    {
      #acceptedName <- allResults[1, "name"]
      acceptedFullGUID <- allResults[1, "guid"]
      fullAcceptedName <- allResults[1, "nameComplete"]
    }
    else
    {
      fullAcceptedName <- allResults[1, "acceptedConceptName"]
      acceptedFullGUID <- allResults[1, "acceptedConceptID"]
    }

    # Extract GUID integer part for later use in preparing returned data.frame
    tmp <- strsplit(acceptedFullGUID, "/")
    guid <- tmp[[1]][length(tmp[[1]])]

    # Now that we have a GUID, perform a second call to gather complete
    # information on the accepted taxon concept
    # Fetch synonym information
    moreInfo <- httr::content(httr::GET(paste0("https://bie-ws.ala.org.au/ws/species/", acceptedFullGUID, ".json")))

    # Some search names appear to return NULL values in some fields, soe patch
    # them so that this don't fail later when the values in those fields are used
    moreInfo$taxonConcept <- lapply(moreInfo$taxonConcept, function(el){if (is.null(el)) el <- "" else el})

    acceptedName <- moreInfo$taxonConcept$nameString

    tmp  <- strsplit(moreInfo$taxonConcept$parentGuid, "/")
    parentGUID <- tmp[[1]][length(tmp[[1]])]

    # Extract and format synonym information (is it is present)
    if (length(moreInfo$synonyms) > 0)
      synonyms <- paste(unlist(lapply(moreInfo$synonyms, function(el){el$nameString})), collapse = "; ")
    else
      synonyms = "No_data"

    # Information on the parent taxon
    parentTaxonInfo <- galah::search_identifiers(moreInfo$taxonConcept$parentGuid)

    # Prepare name fields
    if (moreInfo$taxonConcept$rankString %in% c("subspecies", "variety"))
    {
      infraSpecificRank <- moreInfo$taxonConcept$rankString
      nameParts <- strsplit(moreInfo$taxonConcept$nameString, " ", fixed = TRUE)[[1]]
      infraSpecificEpithet <- nameParts[4]
      infraSpecificRankAbbrev <- nameParts[3]
      specificEpithet <- nameParts[2]
      genus <- nameParts[1]

      # ALA is slack and only presents author details for infraspecific ranks in
      # the fully formatted name string, leaving the author field enpty!
      tmp <- strsplit(moreInfo$taxonConcept$nameFormatted, "author\">")

      # There will be at least one author, the "base author":
      baseAuthor <- trimws(strsplit(tmp[[1]][2], "</span>")[[1]][1])

      # Is there an additional author string?
      if (length(tmp[[1]]) > 2)
      {
        author <- trimws(strsplit(tmp[[1]][3], "</span>")[[1]][1])
      }
      else
        author <- ""

      taxonAuthor <- paste(baseAuthor, author)
      formattedAcceptedName <- paste0("<i>", genus, " ", specificEpithet, "</i> ", taxonAuthor, " ", infraSpecificRankAbbrev, " <i>", infraSpecificEpithet, "</i>")
    }
    else
    {
      infraSpecificRank <- ""
      infraSpecificEpithet <- ""
      infraSpecificRankAbbrev <- ""
      nameParts <- strsplit(moreInfo$taxonConcept$nameString, " ", fixed = TRUE)[[1]]
      formattedAcceptedName <- paste0("<i>", acceptedName, "</i> ", moreInfo$taxonConcept$author)
      # Family versus Genus versus Species
      if (length(nameParts) == 1)
      {
        # Family versus Genus
        if (moreInfo$taxonConcept$rankString == "family")
        {
          # Family
          specificEpithet <- ""
          genus <- ""
          taxonAuthor <- moreInfo$taxonConcept$author
          formattedAcceptedName <- paste(acceptedName, taxonAuthor)
        }
        else
        {
          # Genus
          specificEpithet <- ""
          genus <- moreInfo$classification$genus
          taxonAuthor <- moreInfo$taxonConcept$author
          formattedAcceptedName <- paste0("<i>", acceptedName, "</i> ", paste(acceptedName, taxonAuthor))
        }
      }
      else
      {
        # Species
        specificEpithet = nameParts[2]
        genus <- moreInfo$classification$genus
        taxonAuthor <- moreInfo$taxonConcept$author
        formattedAcceptedName <- paste0("<i>", acceptedName, "</i> ", taxonAuthor)
      }
    }

    # Make a data.frame for return
    checkResult <- data.frame(isValid = TRUE,
                              isAccepted = thisTaxon == acceptedName,
                              thisTaxon = thisTaxon,
                              acceptedName = acceptedName,
                              fullAcceptedName = fullAcceptedName,
                              genus = genus,
                              specificEpithet = specificEpithet,
                              infraSpecificRank = infraSpecificRank,
                              infraSpecificEpithet = infraSpecificEpithet,
                              infraSpecificRankAbbrev = infraSpecificRankAbbrev,
                              taxonAuthor = taxonAuthor,
                              acceptedGUID = guid,
                              acceptedFullGUID = acceptedFullGUID,
                              formattedAcceptedName = formattedAcceptedName, #paste0("<i>", acceptedName, "</i> ", moreInfo$taxonConcept$author),
                              taxonomicStatus = moreInfo$taxonConcept["taxonomicStatus"],
                              taxonomicRank = moreInfo$taxonConcept$rankString,
                              parentGUID = parentGUID,
                              parentName = parentTaxonInfo$scientific_name,
                              parentTaxonomicRank = parentTaxonInfo$rank,
                              synonyms = synonyms,
                              apcFamily = moreInfo$classification$family,
                              alaCommonNames = paste(unlist(lapply(moreInfo$commonNames, function(el){el$nameString})), collapse = "; "),
                              stringsAsFactors = FALSE)
  }
  else
  {
    # Name is completely unknown to APNI so so create and return an "empty" result
    if (!quiet) cat("  Name search returned a negative result\n")

    checkResult <- data.frame(isValid = FALSE,
                              isAccepted = FALSE,
                              thisTaxon = thisTaxon,
                              acceptedName = "Not_accepted",
                              fullAcceptedName = "Not_accepted",
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
                              parentGUID = "",
                              parentName = "",
                              parentTaxonomicRank = "",
                              synonyms = "",
                              apcFamily = "",
                              alaCommonNames = "",
                              stringsAsFactors = FALSE)
  }

  class(checkResult) <- c(class(checkResult), "taxonInfo")
  rownames(checkResult) <- ""
  invisible(checkResult)
}
