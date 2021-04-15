#' @title
#' post /locations
#'
#' @description
#' Create new Locations
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param abbreviation character; required: FALSE; An abbreviation which
#'    represents this location
#' @param additionalInfo list; required: FALSE; Additional arbitrary information.
#'    If provided use the following construct list(additionalProp1 = "string",
#'    additionalProp2 =  "string", additionalProp3 = "string"). The Examples
#'    Section shows an example on how to construct the `additionalInfo` argument
#'    as a list.
#' @param coordinateDescription character; required: FALSE; Describes the
#'    precision and landmarks of the coordinate values used for this location.
#'    (ex. the site, the nearest town, a 10 kilometers radius circle, +/- 20
#'    meters, etc)
#' @param coordinateUncertainty character; required: FALSE; Uncertainty
#'    associated with the coordinates in meters. Leave the value empty if
#'    the uncertainty is unknown.
#' @param coordinates list(); required: FALSE; One geometry as defined by
#'    GeoJSON (RFC 7946). All coordinates are decimal values on the WGS84
#'    geographic coordinate reference system. A coordinate position MUST be two
#'    or more elements. The first two elements are longitude and latitude, or
#'    easting and northing, precisely in that order and using decimal numbers.
#'    Altitude or elevation MAY be included as an optional third element and is
#'    specified in meters.
#'
#'    The `coordinates` list MUST contain the following two elements:
#'
#'    * `geometry` list; required: TRUE; A geometry as defined by GeoJSON
#'      (RFC 7946). In this context, only Point or Polygon geometry are allowed
#'      .The Point geometry is described by exactly two elements:
#'        + `coordinates` vector of type character; required: TRUE; A point
#'          position containing two or more elements. The first two elements
#'          are longitude and latitude, or easting and northing, precisely in
#'          that order and using decimal numbers. Altitude or elevation MAY be
#'          included as an optional third element.
#'        + `type` character; required: TRUE; Literally specified as "Point"
#'      The Polygon geometry is described by exactly two elements:
#'        + `coordinates` list; required : TRUE; List of linear rings, where
#'          each linear ring is a list of at least four positions with the first
#'          equal to the last. The first linear ring specifies the exterior
#'          ring, and each subsequent ring an interior ring.
#'        + `type` character; required: TRUE; Literally specified as "Polygon".
#'    * `type` character; required: TRUE; Literally specified as "Feature".
#'
#'    The easiest way in R to construct the `coordinates` list is to use the
#'    **geojsonR** package. The Examples Section shows how to create a
#'    `coordinates` list object for a point and a polygon geometry.
#' @param countryCode character; required: FALSE; [ISO_3166-1_alpha-3](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3)
#'    specification MIAPPE V1.1 (DM-17) Geographic location (country) - The
#'    country where the experiment took place, either as a full name or
#'    preferably as a 2-letter code.
#' @param countryName character; required: FALSE; The full name of the country
#'    where this location is MIAPPE V1.1 (DM-17) Geographic location (country) -
#'    The country where the experiment took place, either as a full name or
#'    preferably as a 2-letter code.
#' @param documentationURL character; required: FALSE; A URL to the human
#'    readable documentation of this object.
#' @param environmentType character; required: FALSE; Describes the general type
#'    of environment of the location. (ex. forest, field, nursery, etc).
#' @param exposure character; required: FALSE: Describes the level of protection
#'   /exposure for things like sun light and wind.
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
#'    The Examples Section shows an example of how to construct the
#'    `externalReferences` argument as a data.frame.
#' @param instituteAddress character; required: FALSE; The street address of the
#'    institute representing this location. MIAPPE V1.1 (DM-16) Contact
#'    institution - Name and address of the institution responsible for
#'    the study.
#' @param instituteName character; required: FALSE; Each institute/laboratory
#'    can have several experimental field. MIAPPE V1.1 (DM-16) Contact
#'    institution - Name and address of the institution responsible for the
#'    study.
#' @param locationName character; required: FALSE; A human readable name for
#'    this location. MIAPPE V1.1 (DM-18) Experimental site name - The name of
#'    the natural site, experimental field, greenhouse, phenotyping facility,
#'    *etc.*, where the experiment took place.
#' @param locationType character; required: FALSE; The type of location this
#'    represents (ex. Breeding Location, Storage Location, *etc.*).
#' @param siteStatus character; required: FALSE: Description of the
#'    accessibility of the location (ex. Public, Private)
#' @param slope character; required: FALSE; Describes the approximate slope
#'    (height/distance) of the location.
#' @param topography character; required: FALSE; Describes the topography of the
#'    land at the location. (ex. Plateau, Cirque, Hill, Valley, *etc.*)
#'
#' @details Add new locations to database
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Locations/post_locations }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Locations
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' abbreviation <- "L1"
#' additionalInfo <- list(dummyData = "True", example = "post_locations")
#' coordinateDescription <- "North East corner of greenhouse"
#' coordinateUncertainty <- "20"
#' ## Load geojsonR package
#' library(geojsonR)
#' ## Create a coordinates list object
#' ## Point geometry
#' init <- TO_GeoJson$new()
#' pointGeometry <- list()
#' pointData <- c(-76.506042, # longitude
#'                 42.417373, # lattitude
#'                       123) # altitude
#' pointGeometry[["geometry"]] <- init$Point(data = pointData,
#'                                           stringify = FALSE)
#' pointGeometry[["type"]] <- "Feature"
#' ##
#' ## Polygon geometry with an exterior and one interior ring
#' init <- TO_GeoJson$new()
#' ## Individual polygon points are provided as c(longitude, latitude, altitude)
#' polygonData <- list(list(c(-76.476949, 42.447274, 123), # exterior ring (rectangle)
#'                          c(-76.474429, 42.447258, 123),
#'                          c(-76.474428, 42.446193, 123),
#'                          c(-76.476961, 42.446211, 123),
#'                          c(-76.476949, 42.447274, 123)),
#'                     list(c(-76.476733, 42.446916), # interior ring (triangle)
#'                          c(-76.475810, 42.447154),
#'                          c(-76.476306, 42.446281),
#'                          c(-76.476733, 42.446916)))
#' polygonGeometry <- list()
#' polygonGeometry[["geometry"]] <- init$Polygon(data = polygonData,
#'                                               stringify = FALSE)
#' polygonGeometry[["type"]] <- "Feature"
#' countryCode <- "PER"
#' countryName <- "Peru"
#' documentationURL <- "https://brapi.org"
#' environmentType <- "Nursery"
#' exposure <- "Structure, no exposure"
#' externalReferences <-
#'   data.frame(referenceID = c("doi:10.155454/12341234",
#'                              "http://purl.obolibrary.org/obo/ro.owl",
#'                              "75a50e76"),
#'              referenceSource = c("DOI",
#'                                  "OBO Library",
#'                                  "Remote Data Collection Upload Tool"))
#' instituteAddress <- "71 Pilgrim Avenue Chevy Chase MD 20815",
#' instituteName <- "Plant Science Institute"
#' locationName <- "Location 1"
#' locationType <- "Storage Location"
#' siteStatus <- "Private"
#' slope <- "0"
#' topography <- "Valley"
#'
#' brapi_post_location(con = con,
#'                     abbreviation = abbreviation,
#'                     additionalInfo = additionalInfo,
#'                     coordinateDescription = coordinateDescription,
#'                     coordinateUncertainty = coordinateUncertainty,
#'                     coordinates = pointGeometry,
#'                     countryCode = countryCode,
#'                     countryName = countryName,
#'                     documentationURL = documentationURL,
#'                     environmentType = environmentType,
#'                     exposure = exposure,
#'                     externalReferences = externalReferences,
#'                     instituteAddress = instituteAddress,
#'                     instituteName = instituteName,
#'                     locationName = locationName,
#'                     locationType = locationType,
#'                     siteStatus = siteStatus,
#'                     slope = slope,
#'                     topography = topography)
#' }
#'
#' @export
brapi_post_locations <- function(con = NULL,
                                 abbreviation = '',
                                 additionalInfo = list(),
                                 coordinateDescription = '',
                                 coordinateUncertainty = '',
                                 coordinates = list(),
                                 countryCode = '',
                                 countryName = '',
                                 documentationURL = '',
                                 environmentType = '',
                                 exposure = '',
                                 externalReferences = '',
                                 instituteAddress = '',
                                 instituteName = '',
                                 locationName = '',
                                 locationType = '',
                                 siteStatus = '',
                                 slope = '',
                                 topography = '') {
  ## Create a list of used arguments
  usedArgs <- brapir:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check if usedArgs[["externalReferences"]] is supplied as empty character vector or data.frame
  if (!(inherits(usedArgs[["externalReferences"]], what = "character") && usedArgs[["externalReferences"]] == "" || inherits(usedArgs[["externalReferences"]], what = "data.frame"))) {
    stop('Argument: "externalReferences" should be supplied as an empty character or as a data.frame, see the help page on how the data.frame should be constructed.')
  }
  externalReferences <- usedArgs[["externalReferences"]]
  usedArgs[["externalReferences"]] <- NULL
  ## Check validity of used and required arguments
  brapir:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Put externalReferences back into usedArgs
  usedArgs[["externalReferences"]] <- externalReferences
  ## Obtain the call url
  callurl <- brapir:::brapi_POST_callURL(usedArgs = usedArgs,
                                         callPath = "/locations",
                                         reqArgs = "",
                                         packageName = "BrAPI-Core",
                                         callVersion = 2.0)
  ## Build the Body
  callbody <- brapir:::brapi_POST_callBody(usedArgs = usedArgs,
                                           reqArgs = "")
  ## Adaptation for v2.0 where json body is wrapped in []
  callbody <- list(callbody)

  try({
    ## Make the call and receive the response
    resp <- brapir:::brapi_POST(url = callurl, body = callbody, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapir:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_locations")
  ## Show pagination information from metadata
  brapir:::brapi_serverinfo_metadata(cont)
  return(out)
}
