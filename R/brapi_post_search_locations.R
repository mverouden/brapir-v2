#' @title
#' post /search/locations
#'
#' @description
#' Submit a search request for Locations
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param abbreviations vector of type character; required: FALSE; Location
#'    abbreviation(s) to search for; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param altitudeMax integer; required: FALSE; The maximum altitude to search
#'    for.
#' @param altitudeMin integer; required: FALSE; The minimum altitude to search
#'    for.
#' @param coordinates list; required: FALSE; A GeoJSON Polygon geometry, as
#'    defined by GeoJSON (RFC 7946), which describes an area to search for other
#'    GeoJSON object. All contained Points and intersecting Polygons are
#'    returned as search results. The coordinates are decimal values on the
#'    WGS84 geographic coordinate reference system. A coordinate position MUST
#'    be two or more elements. The first two elements are longitude and
#'    latitude, or easting and northing, precisely in that order and using
#'    decimal numbers. Altitude or elevation MAY be included as an optional
#'    third element and is  specified in meters.
#'
#'    The `coordinates` list MUST contain the following two elements:
#'
#'    * `geometry` as a list; required: TRUE; A geometry as defined by GeoJSON
#'      (RFC 7946). In this context, only Polygon geometry is allowed
#'      .
#'
#'      The Polygon geometry is described by exactly two elements:
#'        + `coordinates` as a list; required : TRUE; List of linear rings, where
#'          each linear ring is a list of at least four positions with the first
#'          equal to the last. The first linear ring specifies the exterior
#'          ring, and each subsequent ring an interior ring.
#'        + `type` as a character; required: TRUE; Literally specified as "Polygon".
#'    * `type` as a character; required: TRUE; Literally specified as "Feature".
#'
#'    The easiest way in R to construct the `coordinates` list is to use the
#'    **geojsonR** package. The Examples section shows how to create a
#'    `coordinates` list object for a point and a polygon geometry.
#' @param countryCodes vector of type character; required: FALSE; Country
#'    code(s), specified in the [ISO_3166-1_alpha-3](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3)
#'    specification, to search for; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param countryNames vector of type character; required: FALSE; Full country
#'    name(s) to search for; default: &quot;&quot;, when using multiple values
#'    supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param externalReferenceIDs vector of type character; required: FALSE;
#'    External reference identifier(s) to search for. Could be a simple strings
#'    or a URIs (use with `externalReferenceSources` parameter).; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param externalReferenceSources vector of type character; required: FALSE;
#'    Source system or database identifier(s) of an external reference(s) to
#'    search for (use with `externalReferenceIDs` parameter); default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param instituteAddresses vector of type character; required: FALSE; The institute
#'    street address(es) to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param instituteNames vector of type character; required: FALSE; The
#'    institute name(s) to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param locationDbIds vector of type character; required: FALSE; The unique
#'    location identifier(s) to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param locationNames vector of type character; required: FALSE; A human
#'    readable location name(s) to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param locationTypes vector of type character; required: FALSE; The location
#'    type(s) (e.g. Breeding Location, Storage Location, *etc.*) to search for;
#'    default: &quot;&quot;, when using multiple values supply as
#'    c(&quot;value1&quot;, &quot;value2&quot;).
#' @param page integer; required: FALSE; Which result page is requested. The
#'    page indexing starts at 0 (the first page is 'page'= 0). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Advanced searching for the locations resource. Function will return
#'    either the search results (Status 200 for an immediate response) or a
#'    `searchResultsDbId` (Status 202 for both a saved and an asynchronous
#'    search).
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Locations/post_search_locations }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Locations
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' TO BE ADDED STILL
#' }
#'
#' @export
brapi_post_search_locations <- function(con = NULL,
                                        abbreviations = '',
                                        altitudeMax = as.integer(NA),
                                        altitudeMin = as.integer(NA),
                                        coordinates = list(),
                                        countryCodes = '',
                                        countryNames = '',
                                        externalReferenceIDs = '',
                                        externalReferenceSources = '',
                                        instituteAddresses = '',
                                        instituteNames = '',
                                        locationDbIds = '',
                                        locationNames = '',
                                        locationTypes = '',
                                        page = 0,
                                        pageSize = 1000) {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                           callPath = "/search/locations",
                                           reqArgs = "",
                                           packageName = "BrAPI-Core",
                                           callVersion = 2.0)
  ## Build the Body
  callbody <- brapirv2:::brapi_POST_callBody(usedArgs = usedArgs,
                                             reqArgs = "")

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_POST(url = callurl, body = callbody, usedArgs = usedArgs)
    ## Message about call status
    if (httr::status_code(resp) == 200) {
      message(paste0("Immediate Response.", "\n"))
    } else if (httr::status_code(resp) == 202) {
      message(paste0("Saved or Asynchronous Response has provided a searchResultsDbId.", "\n"))
      message(paste0("Use the GET /search/locations/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/locations call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_locations")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
