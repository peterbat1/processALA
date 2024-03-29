# Set global parameters
.onLoad <- function(libname, pkgname)
{
  systemInfo <- Sys.info()
  if (systemInfo["sysname"] == "Linux")
  {
    defaultOutputFolder <<- "~/Downloads/ALA_dump"
  }
  else
  {
    defaultOutputFolder <<- paste0("C:/Users/", systemInfo["user"], "/Downloads/ALA_dump")
  }

  # stdFields <<-  c("id", "catalogue_number", "taxon_name", "institution_code", "collection_code",
  #                  "collection_name", "latitude", "longitude", "coordinate_uncertainty", "collector",
  #                  "month", "year", "basis_of_record", "verbatim_locality", "data_provider",
  #                  "dataset_name")

  # stdFields <<-  c("id", "catalogNumber", "scientificName", "institutionCode", "collectionCode",
  #                  "collectionName", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "recordedBy",
  #                  "month", "year", "basisOfRecord", "verbatimLocality", "dataProviderName",
  #                  "datasetName")
}
