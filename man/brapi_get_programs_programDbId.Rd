% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brapi_get_programs_programDbId.R
\name{brapi_get_programs_programDbId}
\alias{brapi_get_programs_programDbId}
\title{get /programs/\{programDbId\}}
\usage{
brapi_get_programs_programDbId(con = NULL, programDbId = "")
}
\arguments{
\item{con}{list; required: TRUE; BrAPI connection object}

\item{programDbId}{character; required: TRUE; Filter by the common crop name.
Exact match.}
}
\value{
data.frame
}
\description{
Get a breeding Program by Id
}
\details{
Get a single breeding Program by Id. This can be used to quickly get
the details of a Program when you have the Id from another entity.
}
\examples{
\dontrun{
con <- brapi_db()$testserver
brapi_get_programs_programDbId(con = con,
                               programDbId = "program1")
}

}
\references{
\href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Programs/get_programs__programDbId_ }{BrAPI SwaggerHub}
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

Other Programs: 
\code{\link{brapi_get_programs}()},
\code{\link{brapi_get_search_programs_searchResultsDbId}()},
\code{\link{brapi_post_programs}()},
\code{\link{brapi_post_search_programs}()},
\code{\link{brapi_put_programs_programDbId}()}
}
\author{
Maikel Verouden
}
\concept{Programs}
\concept{brapi-core}
