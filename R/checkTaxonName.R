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
#'
#' @export
#'
#' @examples
#' \dontrun{}
checkTaxonName <- function(thisTaxon = NULL, quiet = FALSE)
{
  #ALA4R::ala_config(caching = "off")

  if (is.null(thisTaxon))
    stop("'thisTaxon' cannot be NULL: please supply a taxonomic name")

  if (quiet) cat("Checking taxon name:",thisTaxon,"\n")

  # From this point on, make sure that references to genera are without a
  # trailing "sp." as this can lead to unexpected returns from name searches,
  # whereas searches on the pure genus name will always return clean results
  thisTaxon <- trimws(sub("sp.$", "", trimws(thisTaxon)))

  if (quiet) cat("  no match in taxonTable synonyms\n  checking APNI for matching names\n")
  nameSearch <- ALA4R::search_names(thisTaxon, output_format = "complete")


  if (is.na(nameSearch$guid))
  {
    # Name is completely unknown to APNI so return a "Not_accepted" status
    if (quiet) cat("  APNI returned a negative result\n")
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
                              taxonomicStatus = "No_data",
                              parentGUID = "No_data",
                              parentName = "No_data",
                              parentTaxonomicRank = "No_data",
                              synonyms = "No_data",
                              apcFamily = "No_data",
                              stringsAsFactors = FALSE)
    class(checkResult) <- c(class(checkResult), "taxonInfo")
  }
  else
  {
    if (nameSearch$taxonomicStatus[1] == "excluded")
    {
      ### PANIC ATTACK!
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
                                acceptedFullGUID = nameSearch$acceptedConceptGuid,
                                formattedAcceptedName = "No_data",
                                taxonomicRank = nameSearch$rank,
                                taxonomicStatus = nameSearch$taxonomicStatus,
                                parentGUID = nameSearch$parentGuid,
                                parentName = "No_data",
                                parentTaxonomicRank = "No_data",
                                synonyms = "No_data",
                                apcFamily = "No_data",
                                stringsAsFactors = FALSE)
      class(checkResult) <- c(class(checkResult), "taxonInfo")

      #print("XXXXXXXXXXXXXXXXXXXX")

      #rownames(checkResult) <- ""
      #invisible(checkResult)
    }
    else
    {
      # We have something to grapple further with
      if (quiet) cat("  APNI says the name is known\n")
      # Is thisTaxon a synonym of an APC accepted species concept?
      if (nameSearch[1, "name"] != nameSearch[1, "acceptedConceptName"])
      {
        if (quiet) cat("  APNI says search name is a synonym of another taxon concept\n")
        speciesInfo <- ALA4R::species_info(guid = nameSearch[1, "acceptedConceptGuid"])

        if (nrow(speciesInfo$synonyms) > 0)
        {
          retainedEntries <- grep("SYNONYM", toupper(speciesInfo$synonyms$taxonomicStatus))
          tmpStr <- as.character(speciesInfo$synonyms[, "nameString"])[retainedEntries]
          synonyms <- paste(tmpStr, collapse = ";")
        }
        else
          synonyms <- "none"

        if (as.character(speciesInfo$taxonConcept[1, "rank"]) == "subspecies")
          infraRank <- "subsp."

        nameParts <- taxonNameParts(as.character(speciesInfo$taxonConcept$nameString), verbose = quiet)

        checkResult <- data.frame(isValid = TRUE,
                                  isAccepted = FALSE,
                                  searchName = thisTaxon,
                                  acceptedName = as.character(speciesInfo$taxonConcept$nameString),
                                  fullAcceptedName = nameSearch$acceptedConceptName,
                                  acceptedGUID = unlist(lapply(nameSearch$acceptedConceptGuid, function(el){strsplit(el, "/")[[1]][6]})),
                                  acceptedFullGUID = nameSearch$acceptedConceptGuid,
                                  formattedAcceptedName = formatTaxonName(speciesInfo$taxonConcept$nameFormatted),
                                  genus = nameParts["genus"],
                                  species = nameParts["species"],
                                  infraSpecificRank = nameParts["infraRank"],
                                  infraSpecificEpithet = nameParts["infraName"],
                                  taxonAuthor = speciesInfo$taxonConcept$author,
                                  taxonomicRank = nameSearch$rank,
                                  taxonomicStatus = nameSearch$taxonomicStatus,
                                  parentGUID = as.character(speciesInfo$taxonConcept$parentGuid),
                                  parentName = speciesInfo$classification$genus,
                                  parentTaxonomicRank = "genus",
                                  synonyms = synonyms,
                                  apcFamily = speciesInfo$classification$family,
                                  stringsAsFactors = FALSE)
        class(checkResult) <- c(class(checkResult), "taxonInfo")
      }
      else
      {
        # Assume now that we have a valid name
        if (quiet) cat("  APNI says search name is an accepted concept\n")
        speciesInfo <- ALA4R::species_info(nameSearch[1, "acceptedConceptName"])

        if (length(speciesInfo) == 0) # We have one of those times when species_info() fails!
        {                             # So use info in nameSearch to set values
          if (quiet) cat("    species_info() call failed: using nameSearch result\n")
          acceptedName <- nameSearch$name
          fullAcceptedName <- nameSearch$nameComplete
          formattedAcceptedName <- "" ########### NEED TO FIX AND MAKE IT DO SUBSP. ditto using species_info() below
          nameParts <- taxonNameParts(nameSearch$name)
          if (nameSearch$rank != "species")
            taxonAuthor <- ""
          else
            taxonAuthor <- nameSearch$author

          taxonRank <- nameSearch$rank
          synonyms <- "none"
          acceptedGUID <- unlist(lapply(as.character(nameSearch$guid), function(el){strsplit(el, "/")[[1]][6]}))
          acceptedFullGUID <- nameSearch$guid
          parentGUID <- unlist(lapply(nameSearch$parentGuid, function(el){strsplit(el, "/")[[1]][6]}))
          newSpeciesInfo <- ALA4R::species_info(guid = nameSearch$parentGuid)
          parentName <- ifelse(((nameSearch$rank == "subspecies") || (nameSearch$rank == "variety")),
                               as.character(newSpeciesInfo$classification$species),
                               as.character(newSpeciesInfo$classification$genus))
          apcFamily <- nameSearch$family
          parentTaxonomicRank <- ifelse(nameSearch$rank == "subspecies", "species", "genus") # WARNING: variety is not dealt with
        }
        else
        {
          if (quiet) cat("    species_info() call was successful\n")
          acceptedName <- as.character(speciesInfo$taxonConcept$nameString)
          fullAcceptedName <- as.character(speciesInfo$taxonConcept$nameComplete)
          taxonAuthor <- as.character(speciesInfo$taxonConcept$author)
          acceptedGUID <- unlist(lapply(as.character(speciesInfo$taxonConcept$guid), function(el){strsplit(el, "/")[[1]][6]}))
          acceptedFullGUID <- as.character(speciesInfo$taxonConcept$guid)
          formattedAcceptedName <- formatTaxonName(speciesInfo$taxonConcept$nameFormatted)
          taxonRank <- as.character(speciesInfo$taxonConcept$rank)
          parentGUID <- ifelse(nameSearch$rank == "subspecies",
                               as.character(speciesInfo$classification$speciesGuid),
                               as.character(speciesInfo$classification$genusGuid))
          parentName <- ifelse(nameSearch$rank == "subspecies",
                               as.character(speciesInfo$classification$species),
                               as.character(speciesInfo$classification$genus))
          parentTaxonomicRank <- ifelse(nameSearch$rank == "subspecies", "species", "genus")
          #synonyms = synonyms
          apcFamily <- as.character(speciesInfo$classification$family)

          if (nrow(speciesInfo$synonyms) > 0)
          {
            retainedEntries <- speciesInfo$synonyms[grep("SYNONYM", toupper(speciesInfo$synonyms$taxonomicStatus)), ]

            illegalInd <- grep("nom. illeg.|nom. inval.", retainedEntries$nomenclaturalStatus)
            if (length(illegalInd) > 0) retainedEntries <- retainedEntries[-illegalInd, ]

            if (nrow(retainedEntries) > 0)
            {
              for (i in nrow(retainedEntries))
              {
                if (retainedEntries[i, "taxonomicStatus"] == "proParteSynonym")
                  retainedEntries[i, "nameString"] <- paste(retainedEntries[i, "nameString"], "(in part)")
              }

              synonyms <- paste(unique(as.character(retainedEntries[, "nameString"])), collapse = ";")
            }
            else
            {
              synonyms <- "none"
            }
          }
          else
            synonyms <- "none"

          infraRank <- switch(as.character(speciesInfo$taxonConcept[1, "rank"]), species = "sp.", subspecies = "subsp.", variety = "var.", form = "f.")

          nameParts <- taxonNameParts(as.character(speciesInfo$taxonConcept$nameString), verbose = quiet)
        }

        checkResult <- data.frame(isValid = TRUE,
                                  isAccepted = TRUE,
                                  searchName = thisTaxon,
                                  acceptedName = acceptedName,
                                  fullAcceptedName = fullAcceptedName,
                                  genus = nameParts["genus"],
                                  species = nameParts["species"],
                                  infraSpecificRank = nameParts["infraRank"],
                                  infraSpecificEpithet = nameParts["infraName"],
                                  taxonAuthor = taxonAuthor,
                                  acceptedGUID = acceptedGUID,
                                  acceptedFullGUID = acceptedFullGUID,
                                  formattedAcceptedName = formattedAcceptedName,
                                  taxonomicRank = taxonRank,
                                  taxonomicStatus = "", #nameSearch$taxonomicStatus,
                                  parentGUID = parentGUID,
                                  parentName = parentName,
                                  parentTaxonomicRank = parentTaxonomicRank,
                                  synonyms = synonyms,
                                  apcFamily = apcFamily,
                                  stringsAsFactors = FALSE)

        class(checkResult) <- c(class(checkResult), "taxonInfo")
      }
    }
  }

  rownames(checkResult) <- ""
  invisible(checkResult)
}
