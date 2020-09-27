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
}
