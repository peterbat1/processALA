
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
#' @param taxonList A character vector holding a set of taxon names to be processed or "all" (default) which causes all taxa in the \emph{acceptedName} field of the R&R database \emph{taxonTable} to be downloaded.
#' @param baseOutputPath Character string. A path to the base folder into which output will be written. An attempt will be made to create the path to the base folder if does not already exist. A sub-folder named for each taxon in taxonList will be created and dowloaded files written into it.
#' @param doNameCheck Logical. Should a check of taxonomic names be performed? Default is TRUE; if FALSE, then it is assumed that names are valid and accepted as reported by \code{\link{checkTaxonName}}
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
#' }
#' @examples
#' \dontrun{
#' ## Download or refresh all data for a single taxon
#'  fetchALAdata("Acacia linifolia", baseOutpath = "/home/peterw/Restore and Renew/newData/")
#'
#' ## For a number of species
#'  fetchALAdata(c("Acacia linifolia","Wilkiea hugeliana","Banksia ericifolia","Angophora costata"),
#'               baseOutpath = "/home/peterw/Restore and Renew/newData/")}

fetchALAdata <- function(taxonList = NULL, baseOutputPath = defaultOutputFolder, doNameCheck = TRUE, verbose = FALSE)
{
  #ALA4R::ala_config(caching = "off", cache_directory="~/Downloads/ALA_cache")

  if (is.null(taxonList))
    stop("'taxonList' must provide one or more taxon names")
  else
    taxonList <- trimws(taxonList)

  if (is.null(baseOutputPath))
    stop("A value for 'baseOutputPath' is required.")

  cat("Fetch ALA occurrence data for a set of species\n==============================================\n")

  #baseURL <- "https://biocache-ws.ala.org.au/ws/occurrences/index/download?q="

  #fieldSet <- "uuid,catalogue_number,scientificName.p,institution_code,collection_code,collection_name,latitude,longitude,coordinate_uncertainty,collectors,month.p,year.p,basis_of_record,raw_locality,data_provider,dataset_name"
  theseFields <- "id,catalogue_number,taxon_name,institution_code,collection_code,collection_name,latitude,longitude,coordinate_uncertainty,collector,month,year,basis_of_record,raw_location_remarks,data_provider,dataset_name"


  ############# Run check of taxonList entries against ALA; report bad names, advise user to check/correct/re-run and stop
  ############# theTaxa will stored accepted names ?and GUIDS, synonyms, etc


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
    theseFields <- unlist(strsplit("id,catalogue_number,taxon_name,institution_code,collection_code,collection_name,latitude,longitude,coordinate_uncertainty,collector,month,year,basis_of_record,verbatim_locality,data_provider,dataset_name", ","))
    # theseFields <- "id, catalogue_number"

    ans <- ALA4R::occurrences(taxon = thisTaxon,
                              fields = theseFields,
                              download_reason_id = 4,
                              email = "peterdonaldlwilson@gmail.com",
                              #verbose = verbose,
                              use_data_table = FALSE)

    write.csv(ans$data, destFile, row.names = FALSE)
    write.csv(ans$meta, meta_destFile, row.names = FALSE)

    if (verbose) cat("      CSV-file fetched\n")

    ##### Clean and extract component data
    ##### AVH-supplied specimen data

    # Select only PreservedSpecimens sourced from AVH
    ansHerb <- subset(ans$data, ans$data$dataProvider == "Australia's Virtual Herbarium")

    if (nrow(ansHerb) > 0)
    {
      # Save the unfiltered herbarium records
      outFile <- paste0(gsub("//", "/", file.path(baseOutputPath, thisTaxon)), "/", this_Taxon, "_herbariumRecords.csv")
      write.csv(ansHerb, outFile, row.names = FALSE)
      if (verbose) cat("        ", nrow(ansHerb), "herbarium records found and extracted\n")
    }

    # Select HumanObservations
    ansHumanObs <- subset(ans$data, ans$data$dataProvider != "Australia's Virtual Herbarium")

    if (nrow(ansHumanObs) > 0)
    {
      # Save the unfiltered human observation records
      outFile <- paste0(gsub("//", "/", file.path(baseOutputPath, thisTaxon)), "/", this_Taxon, "_humanObservations.csv")
      write.csv(ansHumanObs, outFile, row.names = FALSE)
      if (verbose) cat("        ", nrow(ansHumanObs), "human observation records found and extracted\n")
    }

    ##### NSW VIS-supplied survey observational records
    ansSurvey <- subset(ans$data, ans$data$collectionCode == "BioNet Atlas of NSW Wildlife")

    surveyInd <- grep("Vegetation Survey", ansSurvey$datasetName)

    if (length(surveyInd) > 0)
    {
      ansSurvey <- ansSurvey[surveyInd, ]
      outFile <- paste0(gsub("//", "/", file.path(baseOutputPath, thisTaxon)), "/", this_Taxon, "_surveyRecords.csv")
      write.csv(ansHumanObs, outFile, row.names = FALSE)
      if (verbose) cat("        ", nrow(ansSurvey), "NSW Veg Survey records found and extracted\n")
    }
  }

  cat(paste("fetchALAdata finished at", Sys.time()), "\n\n")
}
