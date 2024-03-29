% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brapi_post_search_locations.R
\name{brapi_post_search_locations}
\alias{brapi_post_search_locations}
\title{post /search/locations}
\usage{
brapi_post_search_locations(
  con = NULL,
  abbreviations = "",
  altitudeMax = as.integer(NA),
  altitudeMin = as.integer(NA),
  coordinates = list(),
  countryCodes = "",
  countryNames = "",
  externalReferenceIDs = "",
  externalReferenceSources = "",
  instituteAddresses = "",
  instituteNames = "",
  locationDbIds = "",
  locationNames = "",
  locationTypes = "",
  page = 0,
  pageSize = 1000
)
}
\arguments{
\item{con}{list; required: TRUE; BrAPI connection object}

\item{abbreviations}{vector of type character; required: FALSE; Location
abbreviation(s) to search for; default: "", when using multiple
values supply as c("value1", "value2").}

\item{altitudeMax}{integer; required: FALSE; The maximum altitude to search
for.}

\item{altitudeMin}{integer; required: FALSE; The minimum altitude to search
for.}

\item{coordinates}{list; required: FALSE; A GeoJSON Polygon geometry, as
defined by GeoJSON (RFC 7946), which describes an area to search for other
GeoJSON objects. All contained Points and intersecting Polygons are
returned as search results. The coordinates are decimal values on the
WGS84 geographic coordinate reference system. A coordinate position MUST
be two or more elements. The first two elements are longitude and
latitude, or easting and northing, precisely in that order and using
decimal numbers. Altitude or elevation MAY be included as an optional
third element and is  specified in meters.

The \code{coordinates} list MUST contain the following two elements:
\itemize{
\item \code{geometry} as a list; required: TRUE; A geometry as defined by GeoJSON
(RFC 7946). In this context, only Polygon geometry is allowed
.

The Polygon geometry is described by exactly two elements:
\itemize{
\item \code{coordinates} as a list; required : TRUE; List of linear rings, where
each linear ring is a list of at least four positions with the first
equal to the last. The first linear ring specifies the exterior
ring, and each subsequent ring an interior ring.
\item \code{type} as a character; required: TRUE; Literally specified as "Polygon".
}
\item \code{type} as a character; required: TRUE; Literally specified as "Feature".
}

The easiest way in R to construct the \code{coordinates} list is to use the
\strong{geojsonR} package. The Examples section shows how to create a
\code{coordinates} list object for a polygon geometry.}

\item{countryCodes}{vector of type character; required: FALSE; Country
code(s), specified in the \href{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}{ISO_3166-1_alpha-3}
specification, to search for; default: "", when using multiple
values supply as c("value1", "value2").}

\item{countryNames}{vector of type character; required: FALSE; Full country
name(s) to search for; default: "", when using multiple values
supply as c("value1", "value2").}

\item{externalReferenceIDs}{vector of type character; required: FALSE;
External reference identifier(s) to search for. Could be a simple strings
or a URIs (use with \code{externalReferenceSources} parameter).; default:
"", when using multiple values supply as c("value1",
"value2").}

\item{externalReferenceSources}{vector of type character; required: FALSE;
Source system or database identifier(s) of an external reference(s) to
search for (use with \code{externalReferenceIDs} parameter); default:
"", when using multiple values supply as c("value1",
"value2").}

\item{instituteAddresses}{vector of type character; required: FALSE; The institute
street address(es) to search for; default: "", when using
multiple values supply as c("value1", "value2").}

\item{instituteNames}{vector of type character; required: FALSE; The
institute name(s) to search for; default: "", when using
multiple values supply as c("value1", "value2").}

\item{locationDbIds}{vector of type character; required: FALSE; The unique
location identifier(s) to search for; default: "", when using
multiple values supply as c("value1", "value2").}

\item{locationNames}{vector of type character; required: FALSE; A human
readable location name(s) to search for; default: "", when using
multiple values supply as c("value1", "value2").}

\item{locationTypes}{vector of type character; required: FALSE; The location
type(s) (e.g. Breeding Location, Storage Location, \emph{etc.}) to search for;
default: "", when using multiple values supply as
c("value1", "value2").}

\item{page}{integer; required: FALSE; Which result page is requested. The
page indexing starts at 0 (the first page is 'page'= 0). Default is \code{0}.}

\item{pageSize}{integer; required: FALSE; The size of the pages to be
returned. Default is \code{1000}.}
}
\value{
data.frame
}
\description{
Submit a search request for Locations
}
\details{
Advanced searching for the locations resource. Function will return
either the search results (Status 200 for an immediate response) or a
\code{searchResultsDbId} (Status 202 for both a saved and an asynchronous
search).
}
\examples{
\dontrun{
con <- brapi_db()$testserver
con[["token"]] <- "YYYY"

# Immediate Response Example
brapi_post_search_locations(con = con,
                            abbreviations = "L1")

# Saved or Asynchronous Search Response Example
brapi_post_search_locations(con = con,
                            abbreviations = c("L1", "L2", "L3"),
                            countryCodes = "PER",
                            countryNames = "Peru",
                            locationNames = c("Location 1", "Location 2"),
                            locationTypes = "Breeding location")
}

}
\references{
\href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Locations/post_search_locations }{BrAPI SwaggerHub}
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
\code{\link{brapi_get_trials}()},
\code{\link{brapi_post_lists_listDbId_items}()},
\code{\link{brapi_post_lists}()},
\code{\link{brapi_post_locations}()},
\code{\link{brapi_post_people}()},
\code{\link{brapi_post_programs}()},
\code{\link{brapi_post_search_lists}()},
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

Other Locations: 
\code{\link{brapi_get_locations_locationDbId}()},
\code{\link{brapi_get_locations}()},
\code{\link{brapi_get_search_locations_searchResultsDbId}()},
\code{\link{brapi_post_locations}()},
\code{\link{brapi_put_locations_locationDbId}()}
}
\author{
Maikel Verouden
}
\concept{Locations}
\concept{brapi-core}
