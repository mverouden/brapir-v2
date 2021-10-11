#' @title
#' post /search/images
#'
#' @description
#' Submit a search request for `images`
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param descriptiveOntologyTerms vector of type character; required: FALSE; A
#'    list of terms to formally describe the image to search for. Each item
#'    could be a simple Tag, an Ontology reference identifier, or a full
#'    ontology URL.; default: &quot;&quot;, when using multiple values supply as
#'    c(&quot;value1&quot;, &quot;value2&quot;).
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
#' @param imageDbIds vector of type character; required: FALSE; A list of unique
#'    database identifiers of images to search for; default: &quot;&quot;, when
#'    using multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param imageFileNames vector of type character; required: FALSE; Image file
#'    names to search for.; default: &quot;&quot;, when using multiple values
#'    supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param imageFileSizeMax integer; required: FALSE; A maximum image file size
#'    to search for.
#' @param imageFileSizeMin integer; required: FALSE; A minimum image file size
#'    to search for.
#' @param imageHeightMax integer; required: FALSE; A maximum image height to
#'    search for.
#' @param imageHeightMin integer; required: FALSE; A minimum image height to
#'    search for.
#' @param imageLocation list; required: FALSE; A GeoJSON Polygon geometry, as
#'    defined by GeoJSON (RFC 7946), which describes an area to search for other
#'    GeoJSON objects. All contained Points and intersecting Polygons are
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
#'    The easiest way in R to construct the `imageLocation` list is to use the
#'    **geojsonR** package. The Examples section shows how to create a
#'    `imageLocation` list object for a polygon geometry.
#' @param imageNames vector of type character; required: FALSE; Human readable
#'    image names to search for.; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param imageTimeStampRangeEnd character; required: FALSE; Time stamp to end
#'    the range for images to filter on. Coded in the ISO 8601 standard extended
#'    format, where date, time and time zone information needs to be provided
#'    (check for example https://www.w3.org/TR/NOTE-datetime).
#' @param imageTimeStampRangeStart character; required: FALSE; Time stamp to
#'    start the range for images to filter on. Coded in the ISO 8601 standard
#'    extended format, where date, time and time zone information needs to be
#'    provided (check for example https://www.w3.org/TR/NOTE-datetime).
#' @param imageWidthMax integer; required: FALSE; A maximum image width to
#'    search for.
#' @param imageWidthMin integer; required: FALSE; A minimum image width to
#'    search for.
#' @param mimeTypes vector of type character; required: FALSE; A set of image
#'    file types to search for, pattern to use "image/*" e.g. "image/jpg",
#'    "image/jpeg", "image/gif"; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param observationDbIds vector of type character; required: FALSE; A list of
#'    unique observation database identifiers this image is associated with to
#'    search for; default: &quot;&quot;, when using multiple values supply as
#'    c(&quot;value1&quot;, &quot;value2&quot;).
#' @param observationUnitDbIds vector of type character; required: FALSE; A set
#'    of unique observation unit database identifiers to search for.; default:
#'    &quot;&quot;, when using multiple values supply as
#'    c(&quot;value1&quot;, &quot;value2&quot;).
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Submit a search request for `Images`. Function will return
#'    either the search results (Status 200 for an immediate response) or a
#'    `searchResultsDbId` (Status 202 for both a saved and an asynchronous
#'    search).
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Images/post_search_images }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Images
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' ## Create a imageLocation list object
#' library(geojsonR)
#' ## Polygon geometry with an exterior ring only
#' init <- TO_GeoJson$new()
#' ## Individual polygon points are provided as c(longitude, latitude, altitude)
#' polygonData <- list(list(c(-76.476949, 42.447274, 123), # exterior ring (rectangle)
#'                          c(-76.474429, 42.447258, 123),
#'                          c(-76.474428, 42.446193, 123),
#'                          c(-76.476961, 42.446211, 123),
#'                          c(-76.476949, 42.447274, 123)))
#' imageLocation <- list()
#' imageLocation[["geometry"]] <- init$Polygon(data = polygonData,
#'                                             stringify = FALSE)
#' imageLocation[["type"]] <- "Feature"
#'
#' # Immediate Response Example
#' brapi_post_search_images(con = con,
#'                          page = 0,
#'                          pageSize = 1000)
#'
#' # Saved or Asynchronous Search Response Example
#' brapi_post_search_images(con = con,
#'                          imageDbIds = c("image1",
#'                                         "image2"),
#'                          imageWidthMax = 1920,
#'                          imageWidthMin = 100,
#'                          mimeTypes = c("image/jpg",
#'                                        "image/jpeg",
#'                                        "image/gif"))
#' }
#'
#' @export
brapi_post_search_images <- function(con = NULL,
                                     descriptiveOntologyTerms = '',
                                     externalReferenceIDs = '',
                                     externalReferenceSources = '',
                                     imageDbIds = '',
                                     imageFileNames = '',
                                     imageFileSizeMax = as.integer(NA),
                                     imageFileSizeMin = as.integer(NA),
                                     imageHeightMax = as.integer(NA),
                                     imageHeightMin = as.integer(NA),
                                     imageLocation = list(),
                                     imageNames = '',
                                     imageTimeStampRangeEnd = '',
                                     imageTimeStampRangeStart = '',
                                     imageWidthMax = as.integer(NA),
                                     imageWidthMin = as.integer(NA),
                                     mimeTypes = '',
                                     observationDbIds = '',
                                     observationUnitDbIds = '',
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
                                           callPath = "/search/images",
                                           reqArgs = "",
                                           packageName = "BrAPI-Phenotyping",
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
      message(paste0("Use the GET /search/images/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/images call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_images")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
