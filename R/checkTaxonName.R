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
#' ### Search for a clearly innvalid name
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

  # Perform a basic search on the name passed in thisTaxon
  name_search <- httr::content(httr::GET("http://bie.ala.org.au/", path = "ws/search.json", query = list(q = thisTaxon)))

  if (!quiet)
  {
    cat("----------------------------------------\nName search dump:\n")
    print(name_search)
    cat("----------------------------------------\n")
  }

  # Trap completely NULL result
  if (name_search$searchResults$totalRecords == 0)
    return(checkResult)

  # Searching by "name" can, and will!!!, return totally non-biological results
  # such as a geographical entities which vaguely match (fully or partially) the
  # value passed in thisTaxon. Test for the existence of non-taxonomic fields in
  # the result and remove any such results
  non_biological_ind <- which(unlist(lapply(name_search$searchResults$results, function(el){is.null(el$nomenclaturalCode)})))
  if (length(non_biological_ind) > 0) name_search$searchResults$results <- name_search$searchResults$results[-non_biological_ind]

  if (length(name_search$searchResults$results) > 1)
  {
    if (!quiet) cat("  ** Name search returned a result\n")

    if (plantsOnly)
    {
      # Remove non-plant elements from results
      not_Plants_ind <- which(unlist(lapply(name_search$searchResults$results,
                                            function(el){el$nomenclaturalCode != "ICBN"})))
      if (length(not_Plants_ind) > 0) name_search$searchResults$results <- name_search$searchResults$results[-not_Plants_ind]
    }

    numResults <- length(name_search$searchResults$results)

    if (numResults == 0)
    {
      if (!quiet) cat("  No results obtained from name search: empty result will be returned\n")

      return(invisible(checkResult))
    }
    else
    {
      # Some search names appear to return NULL values in some fields, so patch
      # them so that this don't fail later when the values in those fields are used
      for (i in 1:numResults)
        name_search$searchResults$results[[i]] <- lapply(name_search$searchResults$results[[i]],
                                                         function(el){if (is.null(el)) el <- "" else el})

      searchName_taxonomicStatus <- name_search$searchResults$results[[1]]$taxonomicStatus

      if (grepl("SYNONYM", toupper(searchName_taxonomicStatus)) | grepl("MISAPPLIED", toupper(searchName_taxonomicStatus)))
      {
        # We have synonym of some kind, so we need to jump to the accepted concept information and extract additional fields
        acceptedName <- name_search$searchResults$results[[1]]$acceptedConceptName
        acceptedGUID <- strsplit(name_search$searchResults$results[[1]]$acceptedConceptID, "apni/")[[1]][2]
        acceptedFullGUID <- name_search$searchResults$results[[1]]$acceptedConceptID
        searchName_taxonomicStatus <- name_search$searchResults$results[[1]]$taxonomicStatus

        # Perform a new name search to collect some fields
        name_search2 <- httr::content(httr::GET("http://bie.ala.org.au/", path = "ws/search.json", query = list(q = acceptedName)))

        # Some search names appear to return NULL values in some fields, so patch
        # them so that this don't fail later when the values in those fields are used
        for (i in 1:numResults)
          name_search2$searchResults$results[[i]] <- lapply(name_search2$searchResults$results[[i]],
                                                            function(el){if (is.null(el)) el <- "" else el})

        # There may be (will!!??) be partial matches cluttering the results, so find the list element with matching GUID
        match_ind <- 0
        for (this_ind in 1:length(name_search2$searchResults$results))
          if (name_search2$searchResults$results[[this_ind]]$guid == acceptedFullGUID) match_ind <- this_ind

        if (match_ind > 0)
        {
          taxonAuthor <- name_search2$searchResults$results[[1]]$scientificNameAuthorship
          fullAcceptedName <- name_search2$searchResults$results[[1]]$nameComplete
          totalRecords <- name_search2$searchResults$results[[1]]$occurrenceCount
        }
        else
        {
          # Inexplicably, ALA sometimes cannot match the FULL name of the
          # accepted taxon concept during a name search, so we try a guid search
          # and then maybe a new name search on a shorter form of the accepted
          # name
          guid_search_bailout <- httr::content(httr::GET("http://bie.ala.org.au/", path = paste0("ws/species/", acceptedFullGUID, ".json")))
          acceptedName <- guid_search_bailout$taxonConcept$nameString
          name_search_bailout <- httr::content(httr::GET("http://bie.ala.org.au/", path = "ws/search.json", query = list(q = acceptedName)))
          totalRecords <- name_search_bailout$searchResults$results[[1]]$occurrenceCount
          #stop("Arrrgghh!")
        }
      }
      else
      {
        acceptedName <- name_search$searchResults$results[[1]]$scientificName
        acceptedGUID <- strsplit(name_search$searchResults$results[[1]]$guid, "apni/")[[1]][2]
        acceptedFullGUID <- name_search$searchResults$results[[1]]$guid
        taxonAuthor <- name_search$searchResults$results[[1]]$scientificNameAuthorship
        fullAcceptedName <- name_search$searchResults$results[[1]]$nameComplete
        totalRecords <- name_search$searchResults$results[[1]]$occurrenceCount
      }

      if (!quiet)
      {
        cat("  acceptedName: ", acceptedName, "\n")
        cat("  acceptedGUID: ", acceptedGUID, "\n")
        cat("  acceptedFullGUID: ", acceptedFullGUID, "\n")
        cat("  taxonAuthor: ", taxonAuthor, "\n")
        cat("  fullAcceptedName: ", fullAcceptedName, "\n")
        cat("  totalRecords: ", totalRecords, "\n")
      }

      # Run search using accepted GUID to gather all necessary info including parent info and synonyms
      guid_search <- httr::content(httr::GET("http://bie.ala.org.au/", path = paste0("ws/species/", acceptedFullGUID, ".json")))

      tmp  <- strsplit(guid_search$taxonConcept$parentGuid, "/")
      parentGUID <- tmp[[1]][length(tmp[[1]])]
      if (!quiet) cat("  parentGUID: ", parentGUID, "\n")
      parentTaxonInfo <- galah::search_identifiers(guid_search$taxonConcept$parentGuid)

      acceptedName <- guid_search$taxonConcept$nameString
      fullAcceptedName <- guid_search$taxonConcept$nameComplete
      if (!quiet) cat("  acceptedName from moreInfo: ", acceptedName, "\n")

      if (length(guid_search$synonyms) > 0)
        synonyms <- paste(unlist(lapply(guid_search$synonyms, function(el){el$nameString})), collapse = "; ")
      else
        synonyms = "No_data"

      if (!quiet)
      {
        cat("  Synonyms: ", synonyms, "\n")
        cat("----------------------------------------\n")
      }

      # Prepare name fields
      if (guid_search$taxonConcept$rankString %in% c("subspecies", "variety"))
      {
        infraSpecificRank <- guid_search$taxonConcept$rankString
        nameParts <- strsplit(guid_search$taxonConcept$nameString, " ", fixed = TRUE)[[1]]
        infraSpecificEpithet <- nameParts[4]
        infraSpecificRankAbbrev <- nameParts[3]
        specificEpithet <- nameParts[2]
        genus <- nameParts[1]

        # ALA is slack and only presents author details for infraspecific ranks in
        # the fully formatted name string, leaving the author field enpty!
        tmp <- strsplit(guid_search$taxonConcept$nameFormatted, "author\">")

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
        nameParts <- strsplit(guid_search$taxonConcept$nameString, " ", fixed = TRUE)[[1]]
        formattedAcceptedName <- paste0("<i>", acceptedName, "</i> ", guid_search$taxonConcept$author)
        # Family versus Genus versus Species
        if (length(nameParts) == 1)
        {
          # Family versus Genus
          if (guid_search$taxonConcept$rankString == "family")
          {
            # Family
            specificEpithet <- ""
            genus <- ""
            taxonAuthor <-guid_search$taxonConcept$author
            formattedAcceptedName <- paste(acceptedName, taxonAuthor)
          }
          else
          {
            # Genus
            specificEpithet <- ""
            genus <- guid_search$classification$genus
            taxonAuthor <- guid_search$taxonConcept$author
            formattedAcceptedName <- paste0("<i>", acceptedName, "</i> ", paste(acceptedName, taxonAuthor))
          }
        }
        else
        {
          # Species
          specificEpithet = nameParts[2]
          genus <- guid_search$classification$genus
          taxonAuthor <- guid_search$taxonConcept$author
          if (is.null(taxonAuthor)) taxonAuthor <- ""
          formattedAcceptedName <- paste0("<i>", acceptedName, "</i> ", taxonAuthor)
        }
      }
    }

    # Make a data.frame for return
    checkResult <- data.frame(isValid = TRUE,
                              isAccepted = thisTaxon == acceptedName,
                              searchName = thisTaxon,
                              searchName_taxonomicStatus = searchName_taxonomicStatus,
                              acceptedName = acceptedName,
                              fullAcceptedName = fullAcceptedName,
                              genus = genus,
                              specificEpithet = specificEpithet,
                              infraSpecificRank = infraSpecificRank,
                              infraSpecificEpithet = infraSpecificEpithet,
                              infraSpecificRankAbbrev = infraSpecificRankAbbrev,
                              taxonAuthor = taxonAuthor,
                              acceptedGUID = acceptedGUID,
                              acceptedFullGUID = acceptedFullGUID,
                              formattedAcceptedName = formattedAcceptedName, #paste0("<i>", acceptedName, "</i> ", moreInfo$taxonConcept$author),
                              taxonomicStatus = guid_search$taxonConcept["taxonomicStatus"],
                              totalRecords = totalRecords,
                              taxonomicRank = guid_search$taxonConcept$rankString,
                              parentGUID = parentGUID,
                              parentName = parentTaxonInfo$scientific_name,
                              parentTaxonomicRank = parentTaxonInfo$rank,
                              synonyms = synonyms,
                              apcFamily = guid_search$classification$family,
                              alaCommonNames = paste(unlist(lapply(guid_search$commonNames, function(el){el$nameString})), collapse = "; "),
                              inferredAcceptedInfo = "", #inferred_info,
                              stringsAsFactors = FALSE)

    return(invisible(checkResult))
  }
  else
  {
    # Name is completely unknown to APNI so return an "empty" result
    if (!quiet) cat("  Is there a typographical error in thisTaxon?\n")
    return(invisible(checkResult))
  }
}

