% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brapi_post_search_germplasm.R
\name{brapi_post_search_germplasm}
\alias{brapi_post_search_germplasm}
\title{post /search/germplasm}
\usage{
brapi_post_search_germplasm(
  con = NULL,
  accessionNumbers = "",
  collections = "",
  commonCropNames = "",
  externalReferenceIDs = "",
  externalReferenceSources = "",
  genus = "",
  germplasmDbIds = "",
  germplasmNames = "",
  germplasmPUIs = "",
  parentDbIds = "",
  progenyDbIds = "",
  species = "",
  studyDbIds = "",
  studyNames = "",
  synonyms = "",
  page = 0,
  pageSize = 1000
)
}
\arguments{
\item{con}{list; required: TRUE; BrAPI connection object}

\item{accessionNumbers}{vector of type character; required: FALSE; The unique
identifiers for accessions within a genebank; default: "", when
using multiple values supply as c("value1", "value2").}

\item{collections}{vector of type character; required: FALSE; A specific
panel/collection/population name this germplasm belongs to.; default:
"", when using multiple values supply as
c("value1", "value2").}

\item{commonCropNames}{vector of type character; required: FALSE; Common name
for the crop which this program is for; default: "", when using
multiple values supply as c("value1", "value2").}

\item{externalReferenceIDs}{vector of type character; required: FALSE; The
external reference IDs to search for. Could be a simple strings or a URIs.
(use with \code{externalReferenceSources} parameter); default: "",
when using multiple values supply as c("value1",
"value2").}

\item{externalReferenceSources}{vector of type character; required: FALSE;
The identifiers for the source system or database of an external reference
to search for (use with \code{externalReferenceIDs} parameter); default:
"", when using multiple values supply as c("value1",
"value2").}

\item{genus}{vector of type character; required: FALSE; The Genus names to
identify germplasm to search for; default: "", when using
multiple values supply as c("value1", "value2").}

\item{germplasmDbIds}{vector of type character; required: FALSE; The unique
germplasm database idetifier(s) to search for; default: "", when
using multiple values supply as c("value1", "value2").}

\item{germplasmNames}{vector of type character; required: FALSE; The human
readable names to identify germplasm to search for; default: "",
when using multiple values supply as c("value1",
"value2").}

\item{germplasmPUIs}{vector of type character; required: FALSE; The Permanent
Unique Identifiers to identify germplasm to search for; default:
"", when using multiple values supply as c("value1",
"value2").}

\item{parentDbIds}{vector of type character; required: FALSE; Search for
Germplasm with these unique parents database identifier(s); default:
"", when using multiple values supply as c("value1",
"value2").}

\item{progenyDbIds}{vector of type character; required: FALSE; Search for
Germplasm with these unique children database identifier(s); default:
"", when using multiple values supply as c("value1",
"value2").}

\item{species}{vector of type character; required: FALSE; The species names
to identify germplasm to search for; default: "", when using
multiple values supply as c("value1", "value2").}

\item{studyDbIds}{vector of type character; required: FALSE; The unique study
database identifier(s) to search for; default: "", when using
multiple values supply as c("value1", "value2").}

\item{studyNames}{vector of type character; required: FALSE; The study names
to filter search results; default: "", when using multiple
values supply as c("value1", "value2").}

\item{synonyms}{vector of type character; required: FALSE; The alternative
names or IDs used to reference this germplasm; default: "",
when using multiple values supply as c("value1",
"value2").}

\item{page}{integer; required: FALSE; Used to request a specific page of data
to be returned. The page indexing starts at 0 (the first page is
\code{page = 0}). Default is \code{0}.}

\item{pageSize}{integer; required: FALSE; The size of the pages to be
returned. Default is \code{1000}.}
}
\value{
data.frame
}
\description{
Submit a search request for \code{Germplasm}
}
\details{
Submit a search request for \code{Germplasm}. Function will return either
the search results (Status 200 for an immediate response) or a
\code{searchResultsDbId} (Status 202 for both a saved and an asynchronous
search).
}
\examples{
\dontrun{
con <- brapi_db()$testserver
con[["token"]] <- "YYYY"

# Immediate Response Example
brapi_post_search_germplasm(con = con, page = 0, pageSize = 1000)

# Saved or Asynchronous Search Response Example
brapi_post_search_germplasm(con = con,
                            accessionNumbers = c("A0000001",
                                                 "A0000003"),
                            collections = "Fake Foods Collection",
                            commonCropNames = "Tomatillo",
                            genus = "Aspergillus",
                            germplasmDbIds = c("germplasm1",
                                               "germplasm3"),
                            page = 0,
                            pageSize = 1000)
}

}
\references{
\href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Germplasm/2.0#/Germplasm/post_search_germplasm }{BrAPI SwaggerHub}
}
\seealso{
Other brapi-germplasm: 
\code{\link{brapi_get_attributes}()},
\code{\link{brapi_get_attributevalues}()},
\code{\link{brapi_get_germplasm}()},
\code{\link{brapi_get_search_attributes_searchResultsDbId}()},
\code{\link{brapi_get_search_attributevalues_searchResultsDbId}()},
\code{\link{brapi_get_search_germplasm_searchResultsDbId}()},
\code{\link{brapi_post_search_attributes}()},
\code{\link{brapi_post_search_attributevalues}()}

Other Germplasm: 
\code{\link{brapi_get_germplasm}()},
\code{\link{brapi_get_search_germplasm_searchResultsDbId}()}
}
\author{
Maikel Verouden
}
\concept{Germplasm}
\concept{brapi-germplasm}
