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
checkTaxonName <- function(thisTaxon = NULL, quiet = TRUE)
{
  if (is.null(thisTaxon))
    stop("'thisTaxon' cannot be NULL: please supply a taxonomic name")

  if (!quiet) cat("Checking taxon name:",thisTaxon,"\n")

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

  # Searching by "name" can, and will!!!, return totally non-biological results
  # such as a geographical entities which vaguely match (fully or partially)
  # thisTaxon. Test for the existence of non-taxonomic fields in the result
  # and remove any such results
  non_biological_ind <- which(unlist(lapply(name_search$searchResults$results, function(el){is.null(el$nomenclaturalCode)})))
  if (length(non_biological_ind) > 0) name_search$searchResults$results <- name_search$searchResults$results[-non_biological_ind]

  if (length(name_search$searchResults$results) > 1)
  {
    if (!quiet) cat("  Name search returned a result\n")

    # Remove non-plant elements from results
    not_Plants_ind <- which(unlist(lapply(name_search$searchResults$results,
                                          function(el){el$nomenclaturalCode != "ICBN"})))
    if (length(not_Plants_ind) > 0) name_search$searchResults$results <- name_search$searchResults$results[-not_Plants_ind]

    numResults <- length(name_search$searchResults$results)

    if (numResults == 0)
    {
      if (!quiet) cat("  No results returned from name search: empty result returned\n")

      return(invisible(checkResult))
    }
    else
    {
      # Some search names appear to return NULL values in some fields, so patch
      # them so that this don't fail later when the values in those fields are used
      for (i in 1:numResults)
        name_search$searchResults$results[[i]] <- lapply(name_search$searchResults$results[[i]],
                                                         function(el){if (is.null(el)) el <- "" else el})

      # Is an entry saying that searchName is inferredAccepted?
      inferred_ind <- which(unlist(lapply(name_search$searchResults$results,
                                          function(el){grepl("inferredAccepted", el$taxonomicStatus)})))
      if (length(inferred_ind) > 0)
      {
        inferred_info <- NULL
        for (ii in inferred_ind)
          inferred_info <- c(inferred_info, paste0(name_search$searchResults$results[[ii]]$scientificName, "; Source: ",
                                                   name_search$searchResults$results[[ii]]$infoSourceName, "; Common name: ",
                                                   name_search$searchResults$results[[ii]]$commonName))
        inferred_info <- paste(inferred_info, collapse = ", ")
        name_search$searchResults$results <- name_search$searchResults$results[-inferred_ind]
      }
      else
      {
        inferred_info <- "No_data"
      }

      accepted_taxon_ind <- which(unlist(lapply(name_search$searchResults$results,
                                                function(el){(el$name == thisTaxon) & (el$taxonomicStatus == "accepted")})))

      if (length(accepted_taxon_ind) > 0)
      {
        # Get details of the accepted taxon...
        acceptedFullGUID <- name_search$searchResults$results[[accepted_taxon_ind]]$guid
        tmp <- strsplit(acceptedFullGUID, "/")
        guid <- tmp[[1]][length(tmp[[1]])]
      }
      else
      {
        # There may be several types of entry remaining in the results
        # representing various types of synonyms. The focus for this function is
        # to find the correct accepted taxon concept GUID which may be used to
        # gather relevant information including a full account of synonyms etc.
        # So, we focus now on those results whose "name" matches the search name
        # passed in thisTaxon
        name_match_ind <- which(unlist(lapply(name_search$searchResults$results, function (el){el$name == thisTaxon})))

        if (length(name_match_ind) > 0)
        {
          # There may be more than one matching item, so choose just the first index
          # and use that to extract a value for acceptedFullGUID
          acceptedFullGUID <- name_search$searchResults$results[[name_match_ind[1]]]$acceptedConceptID
          tmp <- strsplit(acceptedFullGUID, "/")
          guid <- tmp[[1]][length(tmp[[1]])]

          if (!quiet)
          {
            cat("  Accepted concept name : ", name_search$searchResults$results[[name_match_ind[1]]]$acceptedConceptName, "\n")
            cat("  Accepted concept GUID : ", acceptedFullGUID, "\n")
          }
        }
        else
        {
          # Nothing could be recovered
          if (!quiet) cat("  No fully accepted taxon found: empty result returned with possibly inferred accepted information\n")
          if (inferred_info != "")
          {
            checkResult[1, "inferredAcceptedInfo"] <- inferred_info
            checkResult[1, "searchName_taxonomicStatus"] = "Inferred accepted"
          }
          return(invisible(checkResult))
        }
      }

      # Run search using accepted GUID to gather all necessary info including parent info and synonyms
      guid_search <- httr::content(httr::GET("http://bie.ala.org.au/", path = paste0("ws/species/", acceptedFullGUID, ".json")))

      if (!quiet)
      {
        print("----------------------------------------")
        print(guid_search)
        print("----------------------------------------")
        print(guid_search$synonyms)
        print("----------------------------------------")
      }

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
        print("----------------------------------------")
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
          formattedAcceptedName <- paste0("<i>", acceptedName, "</i> ", taxonAuthor)
        }
      }

      # Make a data.frame for return
      checkResult <- data.frame(isValid = TRUE,
                                isAccepted = thisTaxon == acceptedName,
                                searchName = thisTaxon,
                                searchName_taxonomicStatus = "Accepted",
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
                                taxonomicStatus = guid_search$taxonConcept["taxonomicStatus"],
                                taxonomicRank = guid_search$taxonConcept$rankString,
                                parentGUID = parentGUID,
                                parentName = parentTaxonInfo$scientific_name,
                                parentTaxonomicRank = parentTaxonInfo$rank,
                                synonyms = synonyms,
                                apcFamily = guid_search$classification$family,
                                alaCommonNames = paste(unlist(lapply(guid_search$commonNames, function(el){el$nameString})), collapse = "; "),
                                inferredAcceptedInfo = inferred_info,
                                stringsAsFactors = FALSE)

      return(invisible(checkResult))
    }
  }
  else
  {
    # Name is completely unknown to APNI so return an "empty" result
    if (!quiet) cat("  Is there a typographical error in thisTaxon?\n")
    return(invisible(checkResult))
  }

}
