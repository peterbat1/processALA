
# Based on a rough working script exploring the new way of using the ALA API written 6 October 2016.
#
# This version written 20 October 2016
# Tweaked 21 July 2017 to grab a full ALA occurrence data set for the dicot species in the taxon list as at
# Version progressively developed to be a function in the package RandR.data
# 2020-07-14: Changed baseURL & fieldSet string to deal with another change in API configuration!

#' Fetch ALA records for one or more taxa
#'
#' Download records from the ALA database using API calls. For each taxon, four files are written: the raw ALA data, and a file for each of herbarium, human observation and NSW survey records.
#'
#' @param taxonList A character vector holding a set of taxon names to be processed. Ignored if taxon identifiers are supplied.
#' @param taxonID A character vector of FULL taxon GUIDs. See Details.
#' @param baseOutputPath Character string. A path to the base folder into which output will be written. An attempt will be made to create the path to the base folder if does not already exist. A sub-folder named for each taxon in \emph{taxonList} will be created and dowloaded files written into it.
#' @param theseFields Character vector. A set of ALA occurrence field names to be returned in the ALA API 'GET' call. The default is a set suited to post-processing of plant occurrence records. See Details below.
#' @param doNameCheck Logical. Should a check of taxonomic names be performed? Default is TRUE; if FALSE, then it is assumed that names are valid and accepted as reported by \code{\link{checkTaxonName}}.
#' @param ALA_registered_email Character string. Email address registered with ALA for occurrence data downloads.
#' @param verbose Logical. Should additional progress messages be produced?
#' @return None
#' @export
#' @details {
#' The function \emph{fetchALAdata} handles the process of fetching occurrence data from ALA for one or more taxa. For each taxon, the function saves four csv files:
#' \itemize{
#' \item raw data
#' \item raw herbarium (voucher) data
#' \item raw human observation data
#' \item raw NSW Vegetation Survey data (extracted from the human observation records using a cunning bit of jiggery-pokery)}
#'
#' These files can be used for whatever purpose you have mind, or they can be passed through the companion function \code{\link{filterALAdata}} to make a first pass at cleaning the data. Let me be perfectly frank: \emph{ALA data is full of detritus and needs to be thoroughly cleaned before use!}
#'
#' Occurrence fields: The parameter \emph{theseFields} allows you to specify the occurrence fields to be supplied in the resulting data table. The default is set to a list of fields which have proven useful in post-processing plant occurrence records. The list of available fields is long and includes many options which you may wish to use in your own spin on filtering and processing the resulting data table.
#'
#' See help for \link{showOccFields} for information on available fields and their definitions.
#'
#' If taxon identifiers (GUIDs) are supplied in parameter \emph{taxonID}, they must be the FULL identifier specified for ALA taxa. For plant taxa, this counter-intuitively has the form of a URL.
#'
#' For example, the full GUID for \emph{Acacia linifolia} is "https://id.biodiversity.org.au/node/apni/2906316"
#'
#' If you have used \code{\link{checkTaxonName}}, then the full GUID is available in the field "acceptedFullGUID". Alternatively, you can use galah::galah_identify() to obtained this information.
#' }
#' @examples
#' \dontrun{
#' ## Download or refresh all data for a single taxon
#'  fetchALAdata("Acacia linifolia", baseOutpath = "/home/peterw/Restore and Renew/newData/")
#'
#' ## For a number of species
#'  fetchALAdata(c("Acacia linifolia","Wilkiea hugeliana","Banksia ericifolia","Angophora costata"),
#'               baseOutpath = "/home/peterw/Restore and Renew/newData/")}

fetchALAdata <- function(taxonList = NULL,
                         taxonID = NULL,
                         baseOutputPath = defaultOutputFolder,
                         theseFields = "",
                         doNameCheck = TRUE,
                         ALA_registered_email = "",
                         verbose = FALSE)
{
  if ((is.null(taxonList)) & (is.null(taxonID)))
    stop("'taxonList' or 'taxonID' must provide one or more taxon names or GUIDs")

  if ((!is.null(taxonList)) & (!is.null(taxonID)))
  {
    warning("Information in taxonID will be used, and taxonList and doNameCheck will be ignored")
    doNameCheck <- FALSE # Force this state as we will use GUIDs exclusively
  }
  else
    taxonList <- trimws(taxonList)

  if (is.null(baseOutputPath))
    stop("A value for 'baseOutputPath' is required.")

  if (theseFields == "")
    theseFields <- stdFields

  if (!all(theseFields %in% showOccFields()[, "id"]))
  {
    bad_ind <- which(!(theseFields %in% showOccFields()[, "id"]))
    stop(paste0("The following field names passed in 'theseFields' are not valid: ", paste(theseFields[bad_ind], collapse = ", "),". Call showOccFields() to check & correct names."))
  }

  if ((galah::galah_config()$user$email == "") & (ALA_registered_email == ""))
    stop("You must either run galah_config() to set an ALA-registered email address, or supply one via parameter 'ALA_registered_email'")

  cat("Fetch ALA occurrence data for a set of species\n==============================================\n")

  cat("  Fetching data for", length(taxonList), "taxa\n")

  if (doNameCheck)
  {
    cat("  Checking taxonomic names:")
    nameCheck <- lapply(taxonList, function(el) {checkTaxonName(el)})

    accepted <- unlist(lapply(nameCheck, function(el){el[1, "isAccepted"]}))
    acceptedGUIDS <- unlist(lapply(nameCheck, function(el){el[1, "acceptedFullGUID"]}))

    if (!all(accepted))
    {
      cat("\nThe following names were not accepted by ALA and will be skipped:", paste(taxonList[accepted == FALSE], collapse = ", "), "\n")
      taxonList <- taxonList[-which(!accepted)]
      acceptedGUIDS <- acceptedGUIDS[-which(!accepted)]
      if (length(taxonList) == 0) stop("There are no taxa left to process")
    }

    cat("  Name check completed\n")
  }
  else
    acceptedGUIDs <- taxonID

  # OK, we got this far, so now for the downloads...
  cat("  Processing:\n")

  for (thisTaxonID in acceptedGUIDS)
  {
    thisTaxon <- taxonList[which(acceptedGUIDS == thisTaxonID)]
    cat("    ", thisTaxon, "\n")
    this_Taxon <- gsub(" ", "_", thisTaxon, fixed = TRUE)

    # Make destFolder string and tidy double path separators which might happen
    # if baseOutputPath is supplied with a trailing separator
    destFolder <- gsub("//", "/", file.path(baseOutputPath, thisTaxon))
    if (!dir.exists(destFolder))
    {
      if (verbose) cat("      destFolder ", destFolder, " created\n")
      dir.create(destFolder, recursive = TRUE)
    }

    destFile <- paste0(destFolder, "/", this_Taxon, ".csv")
    meta_destFile <- paste0(destFolder, "/", this_Taxon, "_metadata.csv")

    ans <- data.frame(galah::atlas_occurrences(identify = galah::galah_identify(thisTaxon),
                                               select = galah::galah_select(all_of(theseFields))))

    write.csv(ans, destFile, row.names = FALSE)

    if (verbose) cat("      CSV-file fetched and saved\n")

    ##### Clean and extract component data
    ##### AVH-supplied specimen data

    # Selected records marked as PRESERVED_SPECIMEN
    ansHerb <- subset(ans, ans$basisOfRecord == "PRESERVED_SPECIMEN")

    if (nrow(ansHerb) > 0)
    {
      # Save the unfiltered herbarium records
      outFile <- paste0(gsub("//", "/", file.path(baseOutputPath, thisTaxon)), "/", this_Taxon, "_herbariumRecords.csv")
      write.csv(ansHerb, outFile, row.names = FALSE)
      if (verbose) cat("        ", nrow(ansHerb), "herbarium records found and extracted\n")
    }

    # Select HumanObservations
    ansHumanObs <- subset(ans, ans$basisOfRecord == "HUMAN_OBSERVATION")

    if (nrow(ansHumanObs) > 0)
    {
      # Save the unfiltered human observation records
      outFile <- paste0(gsub("//", "/", file.path(baseOutputPath, thisTaxon)), "/", this_Taxon, "_humanObservations.csv")
      write.csv(ansHumanObs, outFile, row.names = FALSE)
      if (verbose) cat("        ", nrow(ansHumanObs), "human observation records found and extracted\n")
    }

    ##### NSW VIS-supplied survey observational records
    ansSurvey <- subset(ans, ans$collectionCode == "BioNet Atlas of NSW Wildlife")

    surveyInd <- grep("Vegetation Survey", ansSurvey$datasetName)

    if (length(surveyInd) > 0)
    {
      ansSurvey <- ansSurvey[surveyInd, ]
      outFile <- paste0(gsub("//", "/", file.path(baseOutputPath, thisTaxon)), "/", this_Taxon, "_surveyRecords.csv")
      write.csv(ansSurvey, outFile, row.names = FALSE)
      if (verbose) cat("        ", nrow(ansSurvey), "NSW Veg Survey records found and extracted\n")
    }
  }

  cat(paste("fetchALAdata finished at", Sys.time()), "\n\n")
}
