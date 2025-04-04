% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchALAdata.R
\name{fetchALAdata}
\alias{fetchALAdata}
\title{Fetch ALA records for one or more taxa}
\usage{
fetchALAdata(
  taxonList = NULL,
  taxonID = NULL,
  baseOutputPath = defaultOutputFolder,
  theseFields = "",
  doNameCheck = TRUE,
  ALA_registered_email = "",
  verbose = FALSE
)
}
\arguments{
\item{taxonList}{A character vector holding a set of taxon names to be processed. Ignored if taxon identifiers are supplied.}

\item{taxonID}{A character vector of FULL taxon GUIDs. See Details.}

\item{baseOutputPath}{Character string. A path to the base folder into which output will be written. An attempt will be made to create the path to the base folder if does not already exist. A sub-folder named for each taxon in \emph{taxonList} will be created and dowloaded files written into it.}

\item{theseFields}{Character vector. A set of ALA occurrence field names to be returned in the ALA API 'GET' call. The default is a set suited to post-processing of plant occurrence records. See Details below.}

\item{doNameCheck}{Logical. Should a check of taxonomic names be performed? Default is TRUE; if FALSE, then it is assumed that names are valid and accepted as reported by \code{\link{checkTaxonName}}.}

\item{ALA_registered_email}{Character string. Email address registered with ALA for occurrence data downloads.}

\item{verbose}{Logical. Should additional progress messages be produced?}
}
\value{
None
}
\description{
Download records from the ALA database using API calls. For each taxon, four files are written: the raw ALA data, and a file for each of herbarium, human observation and NSW survey records.
}
\details{
{
The function \emph{fetchALAdata} handles the process of fetching occurrence data from ALA for one or more taxa. For each taxon, the function saves four csv files:
\itemize{
\item raw data
\item raw herbarium (voucher) data
\item raw human observation data
\item raw NSW Vegetation Survey data (extracted from the human observation records using a cunning bit of jiggery-pokery)}

These files can be used for whatever purpose you have mind, or they can be passed through the companion function \code{\link{filterALAdata}} to make a first pass at cleaning the data. Let me be perfectly frank: \emph{ALA data is full of detritus and needs to be thoroughly cleaned before use!}

Occurrence fields: The parameter \emph{theseFields} allows you to specify the occurrence fields to be supplied in the resulting data table. The default is set to a list of fields which have proven useful in post-processing plant occurrence records. The list of available fields is long and includes many options which you may wish to use in your own spin on filtering and processing the resulting data table.

See help for \link{showOccFields} for information on available fields and their definitions.

If taxon identifiers (GUIDs) are supplied in parameter \emph{taxonID}, they must be the FULL identifier specified for ALA taxa. For plant taxa, this counter-intuitively has the form of a URL.

For example, the full GUID for \emph{Acacia linifolia} is "https://id.biodiversity.org.au/node/apni/2906316"

If you have used \code{\link{checkTaxonName}}, then the full GUID is available in the field "acceptedFullGUID". Alternatively, you can use galah::galah_identify() to obtained this information.
}
}
\examples{
\dontrun{
## Download or refresh all data for a single taxon
 fetchALAdata("Acacia linifolia", baseOutpath = "/home/peterw/Restore and Renew/newData/")

## For a number of species
 fetchALAdata(c("Acacia linifolia","Wilkiea hugeliana","Banksia ericifolia","Angophora costata"),
              baseOutpath = "/home/peterw/Restore and Renew/newData/")}
}
