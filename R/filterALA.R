# Filter a set of taxon occurrence records using an ALA-derive species x jurisdiction table
# and a raster land-ocean mask
#
# Peter D. Wilson
# Biodiversity Analyst
# Evolutionary Ecology Research Section
# Science & Conservation Branch
# Royal Botanic Garden, Sydney
#
# 27 November 2017; 17 March 2018; 20 September 2020: Re-worked into a component in the stand-alone package "processALA"

#' Filter ALA records
#'
#' Applies one or more filters to previously downloaded ALA data
#'
#' @param taxa A vector of one or more \emph{ALA-accepted} species names to be processed.
#' @param baseDataPath The full path to the folder in which species subfolders may be found which contained output from \code{\link{fetchALAdata}}.
#' @param recType Which type of records will be processed? Must be one of "herbariumRecords", "humanObservations", or "surveyRecords". Abbreviations may used.
#' @param removeDuplicates Logical. If TRUE (default), then duplicate records are removed.
#' @param removeMissingCoords Logical. If TRUE (default), remove records with missing coordinates
#' @param filterByJurisdiction Logical. If TRUE (default), use data showing the Australian jurisdictions in which APC says the taxon is found to eliminate spurious records.
#' @param filterCultivated Logical. If TRUE (default), remove occurrence records identified as belonging to cultivated specimens.
#' @param doNameCheck Logical. Should a check of taxonomic names be performed? Default is TRUE; if FALSE, then it is assumed that names are valid and accepted as reported by \code{\link{checkTaxonName}}
#' @param trace Logical. If TRUE, output some hopefully useful diagnostic information to the console. Default is FALSE (i.e. whisper quiet).
#'
#' @return None
#' @export
#' @details {
#' Data sourced from the Atlas of Living Australia (ALA) needs to be cleaned and reviewed before it is fit for use. This function allows data files output by the sister function \code{\link{fetchALAdata}} to be passed through a selection of automated preliminary filters. Users may filter:
#' \itemize{
#' \item missing or corrupted geo-coordinates;
#' \item cultivated specimens (sadly, some herbaria include samples taken from cultivated plants in the normal voucher collection data! Canberra and Melbourne are serial offenders here);
#' \item duplicated records;
#' \item records from jurisdictions for which APC says a taxon does \emph{not} occur (calls \code{\link{fetchJurisdictionInfo}}).}
#'
#' Any combination of filters may be applied.
#'
#' This function allows you to practice safe data use by applying some sensible filtering processes. However good they may be, automated filters may miss some bad records. \emph{You should \bold{always} review the output from this function before using the data.}
#' }
#'
#' @examples
#' \dontrun{
#' ## Just one taxon, do all filters default setting:
#' filterALAdata("Acacia linifolia", "herbariumRecords", "C:/Documents/myALAstuff")
#'
#' ## A larger set of taxa (order is not important), just remove duplicates:
#' filterALAdata(c("Acacia linifolia", "Acacia decora", "Zieria smithii"),
#'                  "C:/Documents/myALAstuff",
#'                  removeMissingCoordinates = FALSE, filterByJusrisdication = FALSE,
#'                  filterCultivated = FALSE)}
filterALAdata <- function(taxa = NULL,
                          baseDataPath = defaultOutputFolder,
                          recType = c("herbariumRecords", "humanObservations", "surveyRecords"),
                          removeDuplicates = TRUE,
                          removeMissingCoords = TRUE,
                          filterByJurisdiction = TRUE,
                          filterCultivated = TRUE,
                          doNameCheck = TRUE,
                          trace = FALSE)
{
  #ALA4R::ala_config(caching = "off")
  recordTypes <- c("herbariumRecords", "humanObservations", "surveyRecords")
  recTypeInd <- which(grepl(toupper(recType), toupper(recordTypes)))

  filterNames <- c("removeDuplicates", "removeMissingCoords", "filterByJurisdiction", "filterCultivated")
  filterSet <- c(removeDuplicates, removeMissingCoords, filterByJurisdiction, filterCultivated)

  cat("Filter ALA data for a set of species", "\n==============================================\n")

  if (is.null(taxa))
    stop("'taxa' must contain one or more taxonomic names")
  else
    cat("  Filtering", length(taxa), "taxa\n")

  if (!any(grepl(toupper(recType), toupper(c("herbariumRecords", "humanObservations", "surveyRecords")))))
  {
    warning("No recognisable record type in parameter 'recType'!")
    return(NULL)
  }

  if (all(!c(removeDuplicates, removeMissingCoords, filterByJurisdiction)))
  {
    warning("All filter options set to FALSE: nothing to do dude!")
    return(NULL)
  }

  if (is.null(baseDataPath))
  {
    warning("Cannot proceed. Please provide a value for 'baseDataPath'")
    return(NULL)
  }

  cat("  Record type to be filtered:", recordTypes[recTypeInd], "\n")

  cat("  Filters to be applied:", paste(filterNames[filterSet], collapse = ", "), "\n")

  if (doNameCheck)
  {
    cat("  Checking taxonomic names:")
    nameCheck <- lapply(taxa, function(el) {checkTaxonName(el)})

    accepted <- unlist(lapply(nameCheck, function(el){el[1, "isAccepted"]}))

    if (!all(accepted))
    {
      cat("\nThe following names were not accepted by ALA and will be skipped:", paste(taxa[accepted == FALSE], collapse = ", "), "\n")
      taxa <- taxa[-which(accepted == FALSE)]
      if (length(taxa) == 0) stop("There are no taxa left to process")
    }

    cat(" Name check completed\n")
  }

  #cat("RandR: Filter ALA occurrence data for a set of species\n========================================================\n")
  cat(" Processing:\n")
  for (thisTaxon in taxa)
  {
    cat("   ", thisTaxon, "\n")

    this_Taxon <- gsub(" ", "_", thisTaxon, fixed = TRUE)

    inFilename <- gsub("//", "/", paste0(baseDataPath, "/",thisTaxon, "/", this_Taxon,"_", recordTypes[recTypeInd], ".csv"))
    outFilename <- gsub("//", "/", paste0(baseDataPath, "/",thisTaxon, "/", this_Taxon,"_", recordTypes[recTypeInd], "_filtered.csv"))

    if (trace)
    {
      cat("      Input file:", inFilename, "\n")
      cat("      Output file:", outFilename, "\n")
    }

    if (file.exists(inFilename))
    {
      theRecords <- read.csv(inFilename, stringsAsFactors = FALSE)

      if (nrow(theRecords) == 0)
      {
        cat("No records for ", recType, "\n")
      }

      # Find coordinate columns
      longitudeCol <- grep("LONGITUDE", toupper(colnames(theRecords)))[1]
      latitudeCol <- grep("LATITUDE", toupper(colnames(theRecords)))[1]

      if (trace)
      {
        cat("       >>> longitudeCol :", longitudeCol,"\n")
        cat("       >>> latitudeCol  :", latitudeCol,"\n\n")
      }

      # Record type filter:
      if (grepl(toupper(recType), "HERBARIUMRECORDS"))
      {
        basisColInd <- grep("^BASIS", toupper((colnames(theRecords))))

        theRecords <- theRecords[grep("PreservedSpecimen", theRecords[, basisColInd]), ]

        if (trace) cat(" Found", nrow(theRecords), "herbarium records.\n")
        if (nrow(theRecords) == 0) stop("No herbarium records in the data passed in 'theRecords'")
      }

      if (grepl(toupper(recType), "HUMANOBSERVATIONS"))
      {
        theRecords <- theRecords[grep("HumanObservation|''", theRecords$Basis.Of.Record), ]

        if (trace) cat(" Found",nrow(theRecords),"human observation records.\n")
        if (nrow(theRecords) == 0) stop("No human observation records in the data passed in 'theRecords'")
      }

      if (grepl(toupper(recType), "SURVEYRECORDS"))
      {
        if (trace) cat(" Found", nrow(theRecords), "NSW veg. survey records.\n")
        if (nrow(theRecords) == 0) stop("No human observation records in the data passed in 'theRecords'")
      }

      if (filterCultivated)
      {
        # Use ALA-generated flag to do a first filter for cultivated occurrence records
        if (trace) cat("   Filter cultivated records: ")
        naughtyRecordsInd <- which(theRecords$Cultivated...escapee == "true")
        if (length(naughtyRecordsInd) > 0) theRecords <- theRecords[-naughtyRecordsInd,]

        # And we should also do a secondary filter because some records tagged "CULTIVATED:..."
        # in the "raw _ locality" field are not represented in the "Cultivated...escapee" field.
        # Such annoying inconsistencies are part and parcel of the modern ALA:
        badRecords <- grep("^CULTIVATED", toupper(theRecords$Locality))
        if (length(badRecords > 0))
        {
          theRecords <- theRecords[-badRecords,]

          if (trace)
          {
            cat(" Found", length(badRecords), "cultivated records;", nrow(theRecords), "remain\n")
          }
        }
        else
        {
          if (trace) cat("None found\n")
        }
      }

      if (removeDuplicates)
      {
        # Remove duplicated records
        if (trace) cat("       Removing duplicates: ")
        badRecords <- which(duplicated(paste(theRecords[, latitudeCol], theRecords[, longitudeCol], sep = "_")))
        if (length(badRecords) > 0)
        {
          theRecords <- theRecords[-badRecords,]

          if (trace)
            cat("      Found", length(badRecords), "duplicates;", nrow(theRecords), "records remain\n")
        }
        else
        {
          if (trace) cat("None found\n")
        }
      }

      if (removeMissingCoords)
      {
        # Remove missing coordinate records
        if (trace) cat("       Filter missing geo-coordinate records: ")

        badRecords <- union(which(theRecords[, latitudeCol] == ""), which(is.na(theRecords[, latitudeCol])))
        if (length(badRecords) > 0)
        {
          theRecords <- theRecords[-badRecords,]
          if (trace) cat("       Found",length(badRecords)," with missing latitude; ",nrow(theRecords)," remain\n")
        }
        else
        {
          if (trace) cat("None found\n")
        }
      }

      if (filterByJurisdiction)
      {
        # spatial procesing will fail if there are missing coordinates in the occurrence records, so perform an ad hoc cleaning
        if (!removeMissingCoords)
        {
          warning("removeMissingCoords = FALSE but they must be removed to complete a jurisdiction filter; missing coordinate records have been removed")
          badRecords <- union(which(theRecords[, latitudeCol] == ""), which(is.na(theRecords[, latitudeCol])))
          if (length(badRecords) > 0)
          {
            theRecords <- theRecords[-badRecords,]
            if (trace) cat("       Found", length(badRecords), " with missing latitude; ", nrow(theRecords), " remain\n")
          }
          else
          {
            if (trace) cat("None found\n")
          }
        }

        jurisdictionTable <- fetchJurisdictionInfo(thisTaxon)

        if (is.na(jurisdictionTable))
        {
          if (trace) cat("       ALA cannot provide jurisdiction information at the moment - skipping jurisdiction filter\n")
        }
        else
        {
          # Now find out which jurisdiction each occurrence record falls
          if (trace) cat("       Filter by jurisdication:\n")
          spdf <- sp::SpatialPointsDataFrame(coords = theRecords[, c(longitudeCol, latitudeCol)], data = theRecords, proj4string = sp::CRS("+proj=longlat +ellps=WGS84"))
          polyMatch <- sp::over(spdf, ozPoly)

          # Have we been passed a trinomial? If so, then we check the base binomial against APC jurisdiction info:
          nameParts <- strsplit(thisTaxon, " ")
          if (length(nameParts[[1]]) >= 2) thisTaxon2 <- paste(nameParts[[1]][1], nameParts[[1]][2])

          if (trace) cat("         Filtering any records not falling in Australian juridictions for ", thisTaxon2, ":", sep = "")

          polyFilter <- which(is.na(polyMatch$STATE_CODE))

          if (trace) print(polyMatch$STATE_CODE)

          if (length(polyFilter) > 0)
          {
            theRecords <- theRecords[-polyFilter,]
            polyMatch <- polyMatch[-polyFilter,]
            if (trace) cat(" found",length(polyFilter),"records to be removed;",nrow(theRecords),"records remain\n")
          }
          else
          {
            if (trace) cat(" None found\n")
          }

          # Translate numeric codes into abbreviations for matching against the appropriate entry in the jurisdictionTable
          hits <- stateLookup[as.character(polyMatch$STATE_CODE),2]
          if (trace) print(hits)

          # In which jurisdictions does this taxon occur according to the APC?
          inThisLot <- colnames(jurisdictionTable[1, 4:ncol(jurisdictionTable)])
          #if (trace) print(inThisLot)
          inThisLot <- inThisLot[-which(is.na(jurisdictionTable[1, 4:ncol(jurisdictionTable)]))]
          if (trace)
          {
            cat("\n=========================\nAPC-reported jurisdictions:\n")
            print(inThisLot)
            cat("=========================\n")
          }

          # Now test to see if any records are associated with jurisdictions in which APC does not record presence, trim and return the data.frame
          if (trace) cat("         Filtering any records falling in Australian juridictions not supported by APC data: ")
          naughtyRecordsInd <- which(is.na(match(hits, inThisLot)))

          if (length(naughtyRecordsInd) > 0)
          {
            if (trace)
            {
              #cat("\n\n",hits[1] == inThisLot[1],"\n\n")
              cat("\n========================\nnaughtyRecordsInd: ", naughtyRecordsInd, "\n\n")
              #cat("theRecords[naughtyRecordsInd]:\n")
              print(theRecords[naughtyRecordsInd,])
              cat("========================\n")
            }

            theRecords <- theRecords[-naughtyRecordsInd,]
            if (trace) cat(" found",length(naughtyRecordsInd),"records to be removed;",nrow(theRecords),"records remain\n")
          }
          else
          {
            if (trace) cat("None found\n")
          }
        }
      }

      if (trace) cat("       Final number of records: ", nrow(theRecords),"\n")

      if (nrow(theRecords) > 0)
        write.csv(theRecords, outFilename, row.names = FALSE)
    }
  }

  cat(paste("filterALAdata finished at", Sys.time()), "\n")

  invisible(NULL)
}

