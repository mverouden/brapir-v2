% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brapi_get_images_imageDbId.R
\name{brapi_get_images_imageDbId}
\alias{brapi_get_images_imageDbId}
\title{get /images/\{imageDbId\}}
\usage{
brapi_get_images_imageDbId(con = NULL, imageDbId = "")
}
\arguments{
\item{con}{list; required: TRUE; BrAPI connection object}

\item{imageDbId}{character; required: TRUE; Unique database identifier for an
image.}
}
\value{
data.frame
}
\description{
Get the an image meta data summary
}
\details{
Get one image meta data object.
}
\examples{
\dontrun{
con <- brapi_db()$testserver
brapi_get_images_imageDbId(con = con,
                           imageDbId = "image1")
}

}
\references{
\href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Images/get_images__imageDbId_ }{BrAPI SwaggerHub}
}
\seealso{
Other brapi-phenotyping: 
\code{\link{brapi_get_events}()},
\code{\link{brapi_get_images}()},
\code{\link{brapi_get_methods_methodDbId}()},
\code{\link{brapi_get_methods}()},
\code{\link{brapi_get_observationlevels}()},
\code{\link{brapi_get_observations_observationDbId}()},
\code{\link{brapi_get_observations_table}()},
\code{\link{brapi_get_observations}()},
\code{\link{brapi_get_observationunits_observationUnitDbId}()},
\code{\link{brapi_get_observationunits_table}()},
\code{\link{brapi_get_observationunits}()},
\code{\link{brapi_get_ontologies}()},
\code{\link{brapi_get_scales_scaleDbId}()},
\code{\link{brapi_get_scales}()},
\code{\link{brapi_get_search_images_searchResultsDbId}()},
\code{\link{brapi_get_search_observations_searchResultsDbId}()},
\code{\link{brapi_get_search_observationunits_searchResultsDbId}()},
\code{\link{brapi_get_search_variables_searchResultsDbId}()},
\code{\link{brapi_get_traits_traitDbId}()},
\code{\link{brapi_get_traits}()},
\code{\link{brapi_get_variables_observationVariableDbId}()},
\code{\link{brapi_get_variables}()},
\code{\link{brapi_post_images}()},
\code{\link{brapi_post_methods}()},
\code{\link{brapi_post_observations}()},
\code{\link{brapi_post_observationunits}()},
\code{\link{brapi_post_scales}()},
\code{\link{brapi_post_search_images}()},
\code{\link{brapi_post_search_observations}()},
\code{\link{brapi_post_search_observationunits}()},
\code{\link{brapi_post_search_variables}()},
\code{\link{brapi_post_traits}()},
\code{\link{brapi_post_variables}()},
\code{\link{brapi_put_images_imageDbId_imagecontent}()},
\code{\link{brapi_put_images_imageDbId}()},
\code{\link{brapi_put_methods_methodDbId}()},
\code{\link{brapi_put_observations_observationDbId}()},
\code{\link{brapi_put_observationunits_observationUnitDbId}()},
\code{\link{brapi_put_scales_scaleDbId}()},
\code{\link{brapi_put_traits_traitDbId}()},
\code{\link{brapi_put_variables_observationVariableDbId}()}

Other Images: 
\code{\link{brapi_get_images}()},
\code{\link{brapi_get_search_images_searchResultsDbId}()},
\code{\link{brapi_post_images}()},
\code{\link{brapi_post_search_images}()},
\code{\link{brapi_put_images_imageDbId_imagecontent}()},
\code{\link{brapi_put_images_imageDbId}()}
}
\author{
Maikel Verouden
}
\concept{Images}
\concept{brapi-phenotyping}
