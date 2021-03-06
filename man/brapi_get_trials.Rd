% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brapi_get_trials.R
\name{brapi_get_trials}
\alias{brapi_get_trials}
\title{get /trials}
\usage{
brapi_get_trials(
  con = NULL,
  active = NA,
  commonCropName = "",
  contactDbId = "",
  programDbId = "",
  locationDbId = "",
  searchDateRangeStart = "",
  searchDateRangeEnd = "",
  studyDbId = "",
  trialDbId = "",
  trialName = "",
  trialPUI = "",
  sortBy = "",
  sortOrder = "",
  externalReferenceID = "",
  externalReferenceSource = "",
  page = 0,
  pageSize = 1000
)
}
\arguments{
\item{con}{list; required: TRUE; BrAPI connection object}

\item{active}{logical; required: FALSE; Filter active status true/false.
Default: NA, other possible values: TRUE | FALSE}

\item{commonCropName}{character; required: FALSE; Common name for the crop
associated with this trial}

\item{contactDbId}{character; required: FALSE; Contact entities associated
with this trial}

\item{programDbId}{character; required: FALSE; Program filter to only return
trials associated with given program id.}

\item{locationDbId}{character; required: FALSE; Filter by location}

\item{searchDateRangeStart}{character; required: FALSE; The start of the
overlapping search date range. \code{searchDateRangeStart} must be before
\code{searchDateRangeEnd}.}

\item{searchDateRangeEnd}{character; required: FALSE; The start of the
overlapping search date range. \code{searchDateRangeStart} must be before
\code{searchDateRangeEnd}.

Return a Trial entity if any of the following cases are true:
\itemize{
\item \code{searchDateRangeStart} is before \code{trial.endDate} AND \code{searchDateRangeEnd}
is null
\item \code{searchDateRangeStart} is before \code{trial.endDate} AND \code{searchDateRangeEnd}
is after \code{trial.startDate}
\item \code{searchDateRangeEnd} is after \code{trial.startDate} AND \code{searchDateRangeStart}
is null
\item \code{searchDateRangeEnd} is after \code{trial.startDate} AND \code{searchDateRangeStart}
is before \code{trial.endDate}
}}

\item{studyDbId}{character; required: FALSE; Filter by connected studyDbId}

\item{trialDbId}{character; required: FALSE; Filter by trialDbId}

\item{trialName}{character; required: FALSE; Filter by trial name}

\item{trialPUI}{character; required: FALSE; Filter by trial PUI}

\item{sortBy}{character; required: FALSE; Sort order. Name of the field to
sort by.}

\item{sortOrder}{character; required: FALSE; Sort order direction: asc/desc.
Default: "", other possible values:
"asc"|"ASC"|"desc"|"DESC"}

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
Get a filtered list of Trials
}
\details{
Retrieve a filtered list of breeding Trials. A Trial is a collection
of Studies
}
\examples{
\dontrun{
con <- brapi_db()$testserver
brapi_get_trials(con = con)
}

}
\references{
\href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Trials/get_trials }{BrAPI SwaggerHub}
}
\seealso{
Other brapi-core: 
\code{\link{brapi_get_commoncropnames}()},
\code{\link{brapi_get_lists_listDbId}()},
\code{\link{brapi_get_lists}()},
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

Other Trials: 
\code{\link{brapi_get_search_trials_searchResultsDbId}()},
\code{\link{brapi_get_trials_trialDbId}()},
\code{\link{brapi_post_search_trials}()},
\code{\link{brapi_post_trials}()},
\code{\link{brapi_put_trials_trialDbId}()}
}
\author{
Maikel Verouden
}
\concept{Trials}
\concept{brapi-core}
