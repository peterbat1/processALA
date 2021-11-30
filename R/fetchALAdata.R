
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
#' @param taxonList A character vector holding a set of taxon names to be processed.
#' @param baseOutputPath Character string. A path to the base folder into which output will be written. An attempt will be made to create the path to the base folder if does not already exist. A sub-folder named for each taxon in taxonList will be created and dowloaded files written into it.
#' @param theseFields Character vector. A set of ALA occurrence field names to be returned in the API 'GET' call. The default is a set suited to post-processing of plant occurrence records. See Details below.
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
#' These files can be used for whatever purpose you have mind, or they can be passed through the companion function \code{\link{filterALAdata}} to make a first pass at cleaning the data. Let me perfectly frank, \emph{ALA data is full of detritus and needs to be thoroughly cleaned before use!}
#'
#' Occurrence fields: The parameter \emph{theseFields} allows you to specify the occurrence fields to be supplied in the resulting data table. The default is set to a list of fields which have proven useful in post-processing plant occurrence records. The list of available fields is long and includes many options which you may wish to use in your own spin on filtering and processing the resulting data table.
#'
#' See help for \link{showOccFields} for information on available fields and their definitions.
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
                         baseOutputPath = defaultOutputFolder,
                         theseFields = "",
                         doNameCheck = TRUE,
                         ALA_registered_email = "",
                         verbose = FALSE)
{
  if (is.null(taxonList))
    stop("'taxonList' must provide one or more taxon names")
  else
    taxonList <- trimws(taxonList)

  if (is.null(baseOutputPath))
    stop("A value for 'baseOutputPath' is required.")

  if (theseFields == "")
    theseFields <- stdFields

  galah::galah_config(verbose = verbose)

  badFieldnameInd <- which(!(theseFields %in% showOccFields()[, "downloadName"]))
  if (length(badFieldnameInd) > 0)
    stop(paste0("The following field names passed in 'theseFields' are not valid: ", paste(theseFields[badFieldnameInd], collpase = ", "),". Call showOccFields() to check & correct names."))

  if ((galah::galah_config()$email == "") & (ALA_registered_email == ""))
    stop("You must either run galah_config() to set an ALA-registered email address, or supplied one via parameter 'ALA_registered_email'")

  cat("Fetch ALA occurrence data for a set of species\n==============================================\n")

  cat("  Fetching data for", length(taxonList), "taxa\n")

  if (doNameCheck)
  {
    cat("  Checking taxonomic names:")
    nameCheck <- lapply(taxonList, function(el) {checkTaxonName(el)})

    accepted <- unlist(lapply(nameCheck, function(el){el[1, "isAccepted"]}))

    if (!all(accepted))
    {
      cat("\nThe following names were not accepted by ALA and will be skipped:", paste(taxonList[accepted == FALSE], collapse = ", "), "\n")
      taxonList <- taxonList[-which(accepted == FALSE)]
      if (length(taxonList) == 0) stop("There are no taxa left to process")
    }

    cat("  Name check completed\n")
  }

  # OK, we got this far, so now for the downloads...
  cat("  Processing:\n")

  for (thisTaxon in taxonList)
  {
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
    #theseFields <- unlist(strsplit("id,catalogue_number,taxon_name,institution_code,collection_code,collection_name,latitude,longitude,coordinate_uncertainty,collector,month,year,basis_of_record,verbatim_locality,data_provider,dataset_name", ","))

    ans <- galah::ala_occurrences(taxa = galah::select_taxa(thisTaxon),
                                  #filters = galah::select_filters(),
                                  columns = galah::select_columns(theseFields))

    # ans <- ALA4R::occurrences(taxon = paste0("\"",thisTaxon,"\""),
    #                           fields = stdFields, #theseFields,
    #                           download_reason_id = 4,
    #                           email = "peterdonaldwilson@gmail.com",
    #                           verbose = verbose,
    #                           use_data_table = FALSE)

    write.csv(ans, destFile, row.names = FALSE)
    #write.csv(ans$meta, meta_destFile, row.names = FALSE)

    if (verbose) cat("      CSV-file fetched and saved\n")

    ##### Clean and extract component data
    ##### AVH-supplied specimen data

    # Select only PreservedSpecimens sourced from AVH
    ansHerb <- subset(ans, ans$dataProvider == "Australia's Virtual Herbarium")

    if (nrow(ansHerb) > 0)
    {
      # Save the unfiltered herbarium records
      outFile <- paste0(gsub("//", "/", file.path(baseOutputPath, thisTaxon)), "/", this_Taxon, "_herbariumRecords.csv")
      write.csv(ansHerb, outFile, row.names = FALSE)
      if (verbose) cat("        ", nrow(ansHerb), "herbarium records found and extracted\n")
    }

    # Select HumanObservations
    ansHumanObs <- subset(ans, ans$dataProviderName != "Australia's Virtual Herbarium")

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
