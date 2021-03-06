% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brapi_get_lists.R
\name{brapi_get_lists}
\alias{brapi_get_lists}
\title{get /lists}
\usage{
brapi_get_lists(
  con = NULL,
  listType = "",
  listName = "",
  listDbId = "",
  listSource = "",
  externalReferenceID = "",
  externalReferenceSource = "",
  page = 0,
  pageSize = 1000
)
}
\arguments{
\item{con}{list; required: TRUE; BrAPI connection object}

\item{listType}{character; required: FALSE; The type of objects contained by
this generic list; default: "", other possible values: "
germplasm"|"markers"|"observations"|"
observationUnits"|"observationVariables"|"programs
"|"samples"|"studies"|"trials"}

\item{listName}{character; required: FALSE; The human readable name of this
generic list}

\item{listDbId}{character; required: FALSE; The unique ID of this generic list.}

\item{listSource}{character; required: FALSE; The source tag of this generic
list}

\item{externalReferenceID}{character; required: FALSE; An external reference
ID. Could be a simple string or a URI. (use with \code{externalReferenceSource}
parameter)}

\item{externalReferenceSource}{character; required: FALSE; An identifier for
the source system or database of an external reference (use with
\code{externalReferenceID} parameter)}

\item{page}{integer; required: FALSE; Used to request a specific page of data
to be returned. The page indexing starts at 0 (the first page is 'page'= 0).
Default is \code{0}.}

\item{pageSize}{integer; required: FALSE; The size of the pages to be returned.
Default is \code{1000}.}
}
\value{
data.frame
}
\description{
Get filtered set of generic lists
}
\details{
Get filtered set of generic lists
}
\examples{
\dontrun{
con <- brapi_db()$testserver
brapi_get_lists(con = con)
}

}
\references{
\href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Lists/get_lists}{BrAPI SwaggerHub}
}
\seealso{
Other brapi-core: 
\code{\link{brapi_get_commoncropnames}()},
\code{\link{brapi_get_lists_listDbId}()},
\code{\link{brapi_get_locations_locationDbId}()},
\code{\link{brapi_get_locations}()},
\code{\link{brapi_get_people_personDbId}()},
\code{\link{brapi_get_people}()},
\code{\link{brapi_get_programs_programDbId}()},
\code{\link{brapi_get_programs}()},
\code{\link{brapi_get_search_lists_searchResultsDbId}()},
\code{\link{brapi_get_search_locations_searchResultsDbId}()},
\code{\link{brapi_get_search_people_searchResultsDbId}()},
\code{\link{brapi_get_search_programs_searchResultsDbId}()},
\code{\link{brapi_get_search_studies_searchResultsDbId}()},
\code{\link{brapi_get_search_trials_searchResultsDbId}()},
\code{\link{brapi_get_seasons_seasonDbId}()},
\code{\link{brapi_get_seasons}()},
\code{\link{brapi_get_serverinfo}()},
\code{\link{brapi_get_studies_studyDbId}()},
\code{\link{brapi_get_studies}()},
\code{\link{brapi_get_studytypes}()},
\code{\link{brapi_get_trials_trialDbId}()},
\code{\link{brapi_get_trials}()},
\code{\link{brapi_post_lists_listDbId_items}()},
\code{\link{brapi_post_lists}()},
\code{\link{brapi_post_locations}()},
\code{\link{brapi_post_people}()},
\code{\link{brapi_post_programs}()},
\code{\link{brapi_post_search_lists}()},
\code{\link{brapi_post_search_locations}()},
\code{\link{brapi_post_search_people}()},
\code{\link{brapi_post_search_programs}()},
\code{\link{brapi_post_search_studies}()},
\code{\link{brapi_post_search_trials}()},
\code{\link{brapi_post_seasons}()},
\code{\link{brapi_post_studies}()},
\code{\link{brapi_post_trials}()},
\code{\link{brapi_put_lists_listDbId}()},
\code{\link{brapi_put_locations_locationDbId}()},
\code{\link{brapi_put_people_personDbId}()},
\code{\link{brapi_put_programs_programDbId}()},
\code{\link{brapi_put_seasons_seasonDbId}()},
\code{\link{brapi_put_studies_studyDbId}()},
\code{\link{brapi_put_trials_trialDbId}()}

Other Lists: 
\code{\link{brapi_get_lists_listDbId}()},
\code{\link{brapi_get_search_lists_searchResultsDbId}()},
\code{\link{brapi_post_lists_listDbId_items}()},
\code{\link{brapi_post_lists}()},
\code{\link{brapi_post_search_lists}()},
\code{\link{brapi_put_lists_listDbId}()}
}
\author{
Maikel Verouden
}
\concept{Lists}
\concept{brapi-core}
