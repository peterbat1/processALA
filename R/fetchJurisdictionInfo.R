# Restore and Renew project
#
# Collate distribution data for child taxa previously identified as being
# included in genera marked as having one or more naturalised child taxa.
#
# Peter D. Wilson
# 24 August 2015
# 13-16 October 2015: Hacked to process 233 species in the Restore and Renew list
# 27 October 2015: Patched to account for yet another change in the way data is
# passed for some taxa (but not others)...this is just plain stupid, but it seems
# to be the way ALA is managed these days!
# 7 October 2016: Updated to run with changed list of active taxa.
# 11 November 2016: Amended to work with latest URL scheme and make another
# update run.
# 6 November 2017: Revision
# 10 August 2018: Major revision of code
# 14 August 2018: Transformed into a function for use in R&R db
###############################################################################

#' Fetch up to date Jurisdiction information
#'
#' Makes calls to the Atlas of Living Australia (ALA) API to obtain information on the Australian states and territories a species has been accepted by experts to occur. These data are part of the Australian Plant Census (APC), here they are used to perform a stage of ALA occurrence record filtering in the function \code{\link{filterALAdata}}.
#'
#' @param thisTaxon Character object containing the taxon name to be processed.
#' @param trace Logical. If TRUE then debug messages are printed to the console; if FALSE (default) then only sparse progress messages are output.
#'
#' @return A one row data.frame with the following columns:
#' \item{taxonGUID}{The ALA globally unique identifier for the taxon}
#' \item{taxon}{Taxonomic name whose distribution information has been extracted}
#' \item{isAccepted}{Logical value: Is the taxon an accepted name according to the Australian Plant Census (APC)?}
#' \item{isNaturalised}{Logical value: TRUE if it has a status indicating it is \emph{naturalised} in some way in \emph{any} of the jurisdictions}
#' \item{ACT}{Status of the taxon in the Australian Capital Territory}
#' \item{AR}{Status of the taxon in the Ashmore Reef territory}
#' \item{ChI}{Status of the taxon on Christmas Island}
#' \item{CoI}{Status of the taxon on the Cocos (Keeling) Islands}
#' \item{CSI}{Status of the taxon on the Coral Sea Islands}
#' \item{HI}{Status of the taxon on Heard Island}
#' \item{LHI}{Status of the taxon on Lord Howe Island}
#' \item{MDI}{Status of the taxon on McDonald Island}
#' \item{MI}{Status of the taxon on Macquarie Island}
#' \item{NI}{Status of the taxon on Norfolk Island}
#' \item{NSW}{Status of the taxon in New South Wales}
#' \item{NT}{Status of the taxon in the Northern Territory}
#' \item{Qld}{Status of the taxon in Queensland}
#' \item{SA}{Status of the taxon in South Australia}
#' \item{Tas}{Status of the taxon in Tasmania}
#' \item{Vic}{Status of the taxon in Victoria}
#' \item{WA}{Status of the taxon in Western Australia}
#' The status of a taxon may have one of the following values:
#' \item{native}{Occurs naturally within the jurisdiction}
#' \item{native and naturalised}{Occurs naturally in some part of the jurisdiction but has become naturalised in one or more other parts}
#' \item{naturalised}{Established self-sustaining populations occur within the jurisdiction}
#' \item{sparingly naturlised}{Established self-sustaining low abundance or low density populations occur within the jurisdiction}
#' \item{doubtfully naturalised}{Taxon has been reported as naturalised but this status is considered doubtful after expert review}
#' \item{formerly naturalised}{Taxon was naturalised but is now no longer considered present}
#' \item{presumed extinct}{Taxon did occur naturally but is now conidered extinct within the jurisdiction}
#' \item{NA}{There is no record of the taxon from the jurisdiction}
#' This function may be called independently for fun or profit, but its main role is to support the filtering of records by the function \code{\link{filterALAdata}}.
#' @export
#'
#' @examples
#' \dontrun{
#' ## Simple calls to fetch information:
#' ## Nasty weed
#' ans <- fetchJurisdictionInfo("Digitaria sanguinalis")
#' ## Widespread true blue native
#' ans <- fetchJurisdictionInfo("Dodonaea viscosa")
#' ## Naughty native
#' ans <- fetchJurisdictionInfo("Eucalyptus botryoides")}

fetchJurisdictionInfo <- function(thisTaxon = NULL, trace = FALSE)
{
  if (trace) cat("Fetch Jurisdiction information\n----------------------------------\n")

  if (is.null(thisTaxon))
    stop("'thisTaxon' cannot be NULL - please provide a valid taxon name")

  #h <- RCurl::basicHeaderGatherer()

  targetURL <- "https://biodiversity.org.au/nsl/services/rest/node/apni/"

  thisTaxon <- trimws(thisTaxon)

  ans <- checkTaxonName(thisTaxon)

  taxonGUID <- ans$acceptedGUID

  if (any(taxonGUID == "Not_accepted"))
    stop(paste(thisTaxon, "could not be resolved to a valid taxon name"))

  hdr <- c("ACT", "AR",  "ChI", "CoI", "CSI", "HI",  "LHI", "MDI", "MI",  "NI",  "NSW", "NT",  "Qld", "SA",  "Tas", "Vic", "WA")

  # Create a data.frame to collect information for this current taxon:
  newData <- data.frame(taxonGUID = taxonGUID,
                        taxon = thisTaxon,
                        isAccepted = TRUE,
                        isNaturalised = FALSE,
                        matrix(NA , 1, length(hdr), dimnames = list(NULL, hdr)),
                        stringsAsFactors = FALSE)

  if (!grepl("NZOR", taxonGUID))
  {
    # Fetch taxon record
    qString <- paste0(taxonGUID,".json")

    thisURL <- paste0(targetURL, qString)

    rawResponse <- httr::GET(thisURL)

    if (httr::status_code(rawResponse) == 200)
    {
      cookedResponse <- httr::content(rawResponse)

      APC_distribution_str <- cookedResponse$treeElement$profile$`APC Dist.`$value
      #print(APC_distribution_str)

      if (is.null(APC_distribution_str))
      {
        isNaturalised <- "Unknown"
        newData[1, "isNaturalised"] <- "Unknown"
      }
      else
      {
        isNaturalised <- any(grepl("NATURALISED", toupper(APC_distribution_str)))
        newData[1, "isNaturalised"] <- isNaturalised
        regionInfo <- unlist(strsplit(APC_distribution_str, ", "))

        # Find entries which are without a qualifier. They are presumed to be
        # "native" regions. Add "(native)" qualifier to them so that the next
        # processing step can deal with a uniform data structure
        bareNativeInd <- which(!grepl("native|naturalised", regionInfo))
        if (length(bareNativeInd) > 0)
        {
          for (ii in bareNativeInd) regionInfo[ii] <- paste(regionInfo[ii], "(native)")
        }

        regionParts <- strsplit(regionInfo, " (", fixed = TRUE)
        regionName <- unlist(lapply(regionParts, function(el){trimws(el[1])}))
        regionStatus <- unlist(lapply(regionParts, function(el){trimws(sub(")","",el[2], fixed = TRUE))}))

        if (trace) print(regionStatus)

        # Deal with names shown as e.g. "?Tas" for Genranium homeannum. Move the
        # "doubtfully" marker to regionStatus where it is SUPPOSED to be!!!!!
        mismarkedDoubtfulInd <- grep("?", regionName, fixed = TRUE)
        if (length(mismarkedDoubtfulInd) > 0)
        {
          regionStatus <- sub("(", "(doubtfully ", regionStatus[mismarkedDoubtfulInd], fixed = TRUE)
          regionName <- sub("?", "", regionName[mismarkedDoubtfulInd], fixed = TRUE)
        }

        newData[1, regionName] <- regionStatus
      }

      #cat("Success\n")
      if (trace) cat("NATURALISED =", isNaturalised,": NATIVE =", any(grepl("NATIVE", toupper(regionStatus))), "\n")    }
    else
    {
      # Dead cat bouncing...
      if (trace)
      {
        cat("Bad response code:", httr::status_code(rawResponse),"\n")
        print(rawResponse)
      }

      newData <- NA
    }
  }

  return(newData)

}


