# Set global parameters
.onLoad <- function(libname, pkgname)
{
  ALA4R::ala_config(caching = "off")

  systemInfo <- Sys.info()
  if (systemInfo["sysname"] == "Linux")
  {
    defaultOutputFolder <<- "~/Downloads/ALA_dump"
  }
  else
  {
    defaultOutputFolder <<- paste0("C:/Users/", systemInfo["user"], "/Downloads/ALA_dump")
  }

  stdFields <<-  c("id", "catalogue_number", "taxon_name", "institution_code", "collection_code",
                   "collection_name", "latitude", "longitude", "coordinate_uncertainty", "collector",
                   "month", "year", "basis_of_record", "verbatim_locality", "data_provider",
                   "dataset_name")
}
