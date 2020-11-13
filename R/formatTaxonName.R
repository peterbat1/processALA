

#' Format taxon name string returned by ALA4R::speciesInfo()
#'
#' @param thisNameStr Character. String from the field taxonConcept$nameFormatted in the object returned by ALA4R::speciesInfo()
#'
#' @return A string representing the full taxon name formatted with simple mark-up (e.g. simple HTML <i></i> tags)
#' @export
#'
#' @examples
formatTaxonName <- function(thisNameStr)
{
  split1 <- strsplit(thisNameStr, "=")[[1]]
  split2 <- strsplit(split1, "<|>")

  xx <- vector("character", length(split2))

  for (i in 1:length(xx))
  {
    if (grepl("\"name\"", split2[[i]][1]))
    {
      xx[i] <- paste0("<i>", split2[[i]][2], "</i>")
    }
    else
    {
      if (grepl("base-author\"", split2[[i]][1]))
      {
        xx[i] <- paste0("(", split2[[i]][2], ")")
      }
      else
      {
        if (grepl("\"author\"|\"rank\"", split2[[i]][1]))
        {
          xx[i] <- split2[[i]][2]
        }
      }
    }

  }

  return(gsub("</i> <i>", " ", trimws(paste(xx, collapse = " ")), fixed = TRUE))
}

