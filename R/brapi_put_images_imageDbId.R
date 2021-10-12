#' @title
#' put /images/\{imageDbId\}
#'
#' @description
#' Update an image meta data
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param imageDbId character; required: TRUE; The unique database identifier
#'    for the image meta data to be updated.
#' @param additionalInfo list; required: FALSE; Additional arbitrary information.
#'    If provided use the following construct list(additionalProp1 = "string",
#'    additionalProp2 =  "string", additionalProp3 = "string").
#'
#'    The Examples section shows an example on how to construct the
#'    `additionalInfo` argument as a list.
#' @param copyright character; required: FALSE; The copyright information of
#'    this image, e.g. "Copyright 2018 Bob Robertson".
#' @param description character; required: FALSE; The human readable description
#'    of an image.
#' @param descriptiveOntologyTerms vector of type character; required: FALSE; A
#'    list of terms to formally describe the image to search for. Each item
#'    could be a simple Tag, an Ontology reference identifier, or a full
#'    ontology URL.; default: &quot;&quot;, when using multiple values supply as
#'    c(&quot;value1&quot;, &quot;value2&quot;).
#' @param externalReferences data.frame; required: FALSE; A data.frame of
#'    external reference ids. These are references to this piece of data in an
#'    external system. Could be a simple string or a URI. The `externalReferences`
#'    argument data.frame should contain the following columns:
#'
#'    * `referenceID` character; required: TRUE; The external reference ID. Could
#'      be a simple string or a URI.
#'    * `referenceSource` character; required: TRUE; An identifier for the source
#'      system or database of this reference.
#'
#'    The Examples section shows an example of how to construct the
#'    `externalReferences` argument as a data.frame.
#' @param imageFileName character; required: FALSE; The name of the image file,
#'    e.g. "image_0000231.jpg". Might be the same as `imageName`, but could be
#'    different.
#' @param imageFileSize integer; required: FALSE; The size of the image in
#'    Bytes.
#' @param imageHeight integer; required: FALSE; The height of the image in
#'    Pixels.
#' @param imageLocation list; required: FALSE; One geometry as defined by
#'    GeoJSON (RFC 7946). All coordinates are decimal values on the WGS84
#'    geographic coordinate reference system. A coordinate position MUST be two
#'    or more elements. The first two elements are longitude and latitude, or
#'    easting and northing, precisely in that order and using decimal numbers.
#'    Altitude or elevation MAY be included as an optional third element and is
#'    specified in meters.
#'
#'    The `imageLocation` list MUST contain the following two elements:
#'
#'    * `geometry` as a list; required: TRUE; A geometry as defined by GeoJSON
#'      (RFC 7946). In this context, only Point or Polygon geometry are allowed
#'      .
#'
#'      The Point geometry is described by exactly two elements:
#'        + `coordinates` as a vector of type character; required: TRUE; A point
#'          position containing two or more elements. The first two elements
#'          are longitude and latitude, or easting and northing, precisely in
#'          that order and using decimal numbers. Altitude or elevation MAY be
#'          included as an optional third element.
#'        + `type` as a character; required: TRUE; Literally specified as "Point"
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
#'    `imageLocation` list object for a point and a polygon geometry.
#' @param imageName character; required: FALSE; The human readable name of an
#'    image. Might be the same as `imageFileName`, but could be different.
#' @param imageTimeStamp character; required: FALSE; The date and time
#'    when the image was taken. Coded in the ISO 8601 standard extended
#'    format, where date, time and time zone information needs to be provided
#'    (check for example https://www.w3.org/TR/NOTE-datetime).
#' @param imageURL character; required: FALSE; The complete, absolute URI path
#'    to the image file. Images might be stored on a different host or path than
#'    the BrAPI web server.
#' @param imageWidth integer; required: FALSE; The width of the image in Pixels.
#' @param mimeType character; required: FALSE; The file type of the image,
#'    supply using the pattern: "image/", e.g. "image/jpg", "image/jpeg",
#'    "image/png", "image/svg", *etc.*
#' @param observationDbIds vector of type character; required: FALSE; A list of
#'    unique observation database identifiers this image is associated with, if
#'    applicable; default: &quot;&quot;, when using multiple values supply as
#'    c(&quot;value1&quot;, &quot;value2&quot;).
#' @param observationUnitDbId character; required: FALSE; The unique database
#'    identifier of the related observation unit, if relevant.
#'
#' @details Update an image meta data object. Implementation Notes:
#'
#' - This call should be paired with `PUT /images/{imageDbId}/imagecontent` for
#'     full capability
#' - A server may choose to modify the image meta data object based on the
#'     actually image which has been uploaded.
#' - Image data may be stored in a database or file system. Servers should
#'     generate and provide the `imageURL` as an absolute path for retrieving
#'     the image, wherever it happens to live.
#' - `descriptiveOntologyTerms` can be thought of as Tags for the image. These
#'     could be simple descriptive words, or ontology references, or full
#'     ontology URI's.
#' - The `/images` calls support a GeoJSON object structure for describing their
#'     location. The BrAPI spec for GeoJSON only supports two of the possible
#'     geometries: Points and Polygons.
#' - With most images, the Point geometry should be used, and it should indicate
#'     the longitude and latitude of the camera.
#' - For top down images (i.e. from drones, cranes, *etc.*), the Point geometry
#'     may be used to indicate the longitude and latitude of the centroid of the
#'     image content, and the Polygon geometry may be used to indicate the
#'     border of the image content.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Images/put_images__imageDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Images
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' ## Create function argument values
#' additionalInfo <- list(dummyData = "TRUE",
#'                        example = "post_images")
#' copyright <- "Copyright 2021 Bob Robertson"
#' description <- "This is a picture of a tomato"
#' descriptiveOntologyTerms <- c("doi:10.1002/0470841559",
#'                               "Red",
#'                               "ncbi:0300294")
#' externalReferences <-
#'   data.frame(referenceID = c("doi:10.155454/12341234",
#'                              "http://purl.obolibrary.org/obo/ro.owl",
#'                              "75a50e76"),
#'              referenceSource = c("DOI",
#'                                  "OBO Library",
#'                                  "Remote Data Collection Upload Tool"))
#' imageFileName <- "image_0000231.jpg"
#' imageFileSize <- 50000
#' imageHeight <- 550
#'
#' ## Create the imageLocation argument
#' ## Load geojsonR package
#' library(geojsonR)
#' ## Create a imageLocation list object
#' ## Point geometry
#' init <- TO_GeoJson$new()
#' imageLocation <- list()
#' pointData <- c( 5.663288, # longitude
#'                51.988720, # lattitude
#'                      0)   # altitude
#' imageLocation[["geometry"]] <- init$Point(data = pointData,
#'                                           stringify = FALSE)
#' imageLocation[["type"]] <- "Feature"
#'
#' imageName <- "Tomato Image 1"
#' imageTimeStamp <- "2021-10-11T14:11:28.672Z"
#' imageURL <- "https://wiki.brapi.org/images/tomato"
#' imageWidth <- 700
#' mimeType <- "image/jpeg"
#' observationDbIds <- c("observation1",
#'                       "observation4")
#' observationUnitDbId <- "observation_unit1"
#'
#' ## Add new image meta data
#' out <- brapi_post_images(
#'   con = con,
#'   additionalInfo = additionalInfo,
#'   copyright = copyright,
#'   description = description,
#'   descriptiveOntologyTerms = descriptiveOntologyTerms,
#'   externalReferences = externalReferences,
#'   imageFileName = imageFileName,
#'   imageFileSize = imageFileSize,
#'   imageHeight = imageHeight,
#'   imageLocation = imageLocation,
#'   imageName = imageName,
#'   imageTimeStamp = imageTimeStamp,
#'   imageURL = imageURL,
#'   imageWidth = imageWidth,
#'   mimeType = mimeType,
#'   observationDbIds = observationDbIds,
#'   observationUnitDbId = observationUnitDbId)
#'
#' ## Obtain the imageDbId
#' imageDbId <- unique(out$imageDbId)
#'
#' ## Retrieve information about the new image
#' brapi_get_images_imageDbId(con = con, imageDbId = imageDbId)
#'
#' ## Update the image meta data
#' additionalInfo <- list(dummyData = "TRUE",
#'                        example = "put_images_imageDbId")
#' ## Polygon geometry with an exterior and one interior ring
#' init <- TO_GeoJson$new()
#' ## Individual polygon points are provided as c(longitude, latitude, altitude)
#' polygonData <- list(list(c(5.663176, 51.988506, 0), # exterior ring (rectangle)
#'                          c(5.663601, 51.988626, 0),
#'                          c(5.663405, 51.988904, 0),
#'                          c(5.662976, 51.988788, 0),
#'                          c(5.663176, 51.988506, 0)))
#' imageLocation <- list()
#' imageLocation[["geometry"]] <- init$Polygon(data = polygonData,
#'                                               stringify = FALSE)
#' imageLocation[["type"]] <- "Feature"
#' imageTimeStamp <- "2021-10-11T18:05:00.666Z"
#' mimeType <- "image/jpg"
#' brapi_put_images_imageDbId(
#'   con = con,
#'   imageDbId = imageDbId,
#'   additionalInfo = additionalInfo,
#'   imageLocation = imageLocation,
#'   imageTimeStamp = imageTimeStamp,
#'   mimeType = mimeType)
#'
#' ## Check the changes
#' brapi_get_images_imageDbId(con = con, imageDbId = imageDbId)
#' }
#'
#' @export
brapi_put_images_imageDbId <- function(con = NULL,
                                       imageDbId = '',
                                       additionalInfo = list(),
                                       copyright = '',
                                       description = '',
                                       descriptiveOntologyTerms = '',
                                       externalReferences = '',
                                       imageFileName = '',
                                       imageFileSize = as.integer(NA),
                                       imageHeight = as.integer(NA),
                                       imageLocation = list(),
                                       imageName = '',
                                       imageTimeStamp = '',
                                       imageURL = '',
                                       imageWidth = as.integer(NA),
                                       mimeType = '',
                                       observationDbIds = '',
                                       observationUnitDbId = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "imageDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_PUT_callURL(usedArgs = usedArgs,
                                          callPath = "/images/{imageDbId}",
                                          reqArgs = "imageDbId",
                                          packageName = "BrAPI-Phenotyping",
                                          callVersion = 2.0)
  ## Build the Body
  callbody <- brapirv2:::brapi_PUT_callBody(usedArgs = usedArgs,
                                            reqArgs = "imageDbId")

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_PUT(url = callurl, body = callbody, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_put_images_imageDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
