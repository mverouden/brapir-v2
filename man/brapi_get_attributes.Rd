% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brapi_get_attributes.R
\name{brapi_get_attributes}
\alias{brapi_get_attributes}
\title{get /attributes}
\usage{
brapi_get_attributes(
  con = NULL,
  attributeCategory = "",
  attributeDbId = "",
  attributeName = "",
  germplasmDbId = "",
  externalReferenceID = "",
  externalReferenceSource = "",
  page = 0,
  pageSize = 1000
)
}
\arguments{
\item{con}{list; required: TRUE; BrAPI connection object}

\item{attributeCategory}{character; required: FALSE; The general category for
the attribute. very similar to Trait class.}

\item{attributeDbId}{character; required: FALSE; The unique id for an
attribute}

\item{attributeName}{character; required: FALSE; The human readable name for
an attribute}

\item{germplasmDbId}{character; required: FALSE; Get all attributes
associated with this unique germplasm database identifier.}

\item{externalReferenceID}{character; required: FALSE; An external reference
ID. Could be a simple string or a URI. (use with \code{externalReferenceSource}
parameter)}

\item{externalReferenceSource}{character; required: FALSE; An identifier for
the source system or database of an external reference (use with
\code{externalReferenceID} parameter)}

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
Get the Germplasm Attributes
}
\details{
Retrieve available attributes.
}
\examples{
\dontrun{
con <- brapi_db()$testserver
brapi_get_attributes(con = con)
}

}
\references{
\href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Germplasm/2.0#/Germplasm\%20Attributes/get_attributes }{BrAPI SwaggerHub}
}
\seealso{
Other brapi-germplasm: 
\code{\link{brapi_get_attributevalues}()},
\code{\link{brapi_get_germplasm}()},
\code{\link{brapi_get_search_attributes_searchResultsDbId}()},
\code{\link{brapi_get_search_attributevalues_searchResultsDbId}()},
\code{\link{brapi_get_search_germplasm_searchResultsDbId}()},
\code{\link{brapi_post_search_attributes}()},
\code{\link{brapi_post_search_attributevalues}()},
\code{\link{brapi_post_search_germplasm}()}

Other Germplasm Attributes: 
\code{\link{brapi_get_search_attributes_searchResultsDbId}()},
\code{\link{brapi_post_search_attributes}()}
}
\author{
Maikel Verouden
}
\concept{Germplasm Attributes}
\concept{brapi-germplasm}
