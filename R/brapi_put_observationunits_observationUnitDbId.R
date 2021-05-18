#' @title
#' put /observationunits/\{observationUnitDbId\}
#'
#' @description
#' Update an existing Observation Unit
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param observationUnitDbId character; required: TRUE; The unique identifier
#'    of the specific observation unit.
#' @param additionalInfo list; required: FALSE; Additional arbitrary information.
#'    If provided use the following construct list(additionalProp1 = "string",
#'    additionalProp2 =  "string", additionalProp3 = "string").
#'
#'    The Examples section shows an example on how to construct the
#'    `additionalInfo` argument as a list.
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
#' @param germplasmDbId character; required: FALSE; Unique germplasm (accession)
#'    identifier for the observation unit.
#' @param germplasmName character; required: FALSE; Human readable germplasm
#'    name for the observation unit. It can be the preferred name and does not
#'    have to be unique.
#' @param locationDbId character; required: FALSE; The identifier, which
#'    uniquely identifies a location, associated with this study.
#' @param locationName character; required: FALSE; The human readable name of a
#'    location associated with this study.
#' @param observationUnitName character; required: FALSE; A human readable name
#'    for an observation unit.
#' @param observationUnitPUI character; required: FALSE; A Permanent Unique
#'    Identifier for an observation unit. MIAPPE V1.1 (DM-72) External ID -
#'    Identifier for the observation unit in a persistent repository, comprises
#'    the name of the repository and the identifier of the observation unit
#'    therein. The EBI Biosamples repository can be used. URI are recommended
#'    when possible.
#' @param observationUnitPosition list; required: FALSE; All positional and
#'    layout information related to this Observation Unit. MIAPPE V1.1 (DM-73)
#'    Spatial distribution - Type and value of a spatial coordinate
#'    (georeference or relative) or level of observation (plot 45, subblock 7,
#'    block 2) provided as a key-value pair of the form type:value. Levels of
#'    observation must be consistent with those listed in the Study section. The
#'    `observationUnitPosition` list can contain the following elements:
#'
#'    * `entryType` character; required: FALSE; The type of entry for this
#'      observation unit, possible values: &quot;CHECK&quot;|&quot;TEST&quot;
#'      |&quot;FILLER&quot;.
#'    * `geoCoordinates` list; required: FALSE; One geometry as defined by
#'      GeoJSON (RFC 7946). All coordinates are decimal values on the WGS84
#'      geographic coordinate reference system. A coordinate position MUST be
#'      two or more elements. The first two elements are longitude and latitude,
#'      or easting and northing, precisely in that order and using decimal
#'      numbers. Altitude or elevation MAY be included as an optional third
#'      element and is specified in meters. The `geoCcoordinates` list MUST
#'      contain the following two elements:
#'
#'      + `geometry` as a list; required: TRUE; A geometry as defined by GeoJSON
#'        (RFC 7946). In this context, only Point or Polygon geometry are
#'        allowed.
#'
#'        The Point geometry is described by exactly two elements:
#'          - `coordinates` as a vector of type character; required: TRUE; A
#'            point position containing two or more elements. The first two
#'            elements are longitude and latitude, or easting and northing,
#'            precisely in that order and using decimal numbers. Altitude or
#'            elevation MAY be included as an optional third element.
#'          - `type` as a character; required: TRUE; Literally specified as
#'            "Point".
#'
#'        The Polygon geometry is described by exactly two elements:
#'          - `coordinates` as a list; required : TRUE; List of linear rings,
#'            where each linear ring is a list of at least four positions with
#'            the first equal to the last. The first linear ring specifies the
#'            exterior ring, and each subsequent ring an interior ring.
#'          - `type` as a character; required: TRUE; Literally specified as
#'            "Polygon".
#'
#'      + `type` as a character; required: TRUE; Literally specified as "Feature".
#'
#'      The easiest way in R to construct the `geoCoordinates` list is to use
#'      the **geojsonR** package. The Examples section shows how to create a
#'      `geoCoordinates` list object for a point and a polygon geometry.
#'
#'    * `observationLevel` list; required: FALSE; The exact level and level code
#'      of an observation unit. MIAPPE V1.1 DM-71 Observation unit type "Type of
#'      observation unit in textual form, usually one of the following: study,
#'      block, sub-block, plot, sub-plot, pot, plant. Use of other observation
#'      unit types is possible but not recommended. The observation unit type
#'      can not be used to indicate sub-plant levels. However, observations can
#'      still be made on the sub-plant level, as long as the details are
#'      indicated in the associated observed variable (see observed variables).
#'      Alternatively, it is possible to use samples for more detailed tracing
#'      of sub-plant units, attaching the observations to them instead.". The
#'      `observationLevel` list can contain the following elements:
#'
#'      + `levelCode` character; required: FALSE; An identifier code for this
#'        level tag. Identify this observation unit by each level of the
#'        hierarchy, where it exists.
#'      + `levelName` character; required: FALSE; A name for this level.
#'      + `levelOrder` integer; required: FALSE; Defines where that level exists
#'        in the hierarchy of levels. Level Order's lower numbers are at the top
#'        of the hierarchy (i.e. field -> 1) and higher numbers are at the bottom
#'        of the hierarchy (i.e. plant -> 9).
#'
#'    * `observationLevelRelationships` data.frame; required: FALSE; Data.frame
#'      of observation levels to indicate the granularity level at which the
#'      measurements are taken. Each row in the data.frame defines the level
#'      code, level name (`levelName`), and the level order, as integer, where
#'      that level exists in the hierarchy of levels. `levelOrders` lower
#'      numbers are at the top of the hierarchy (i.e. field -> 0) and higher
#'      numbers are at the bottom of the hierarchy (ie plant > 6). `levelCode`
#'      is an identifier code for this level tag. Identify this observation unit
#'      by each level of the hierarchy, where it exists.
#'    * `positionCoordinateX`	character; required: FALSE; The X position
#'      coordinate for an observation unit. Different systems may use different
#'      coordinate systems.
#'    * `positionCoordinateXType` character; required: FALSE; The type of
#'      positional coordinate used. Must be one of the following values:
#'
#'      + `LONGITUDE` - ISO 6709 standard, WGS84 geodetic datum.
#'      + `LATITUDE` - ISO 6709 standard, WGS84 geodetic datum.
#'      + `PLANTED_ROW` - The physical planted row number
#'      + `PLANTED_INDIVIDUAL` - The physical counted number, could be
#'        independent or within a planted row.
#'      + `GRID_ROW` - The row index number of a square grid overlay
#'      + `GRID_COL` - The column index number of a square grid overlay
#'      + `MEASURED_ROW` - The distance in meters from a defined 0-th row
#'      + `MEASURED_COL` - The distance in meters from a defined 0-th column
#'
#'    * `positionCoordinateY` charcter; required: FALSE; The Y position
#'      coordinate for an observation unit. Different systems may use different
#'      coordinate systems.
#'    * `positionCoordinateYType` character; required: FALSE; The type of
#'      positional coordinate used. Must be one of the following values:
#'
#'      + `LONGITUDE` - ISO 6709 standard, WGS84 geodetic datum.
#'      + `LATITUDE` - ISO 6709 standard, WGS84 geodetic datum.
#'      + `PLANTED_ROW` - The physical planted row number
#'      + `PLANTED_INDIVIDUAL` - The physical counted number, could be
#'        independent or within a planted row.
#'      + `GRID_ROW` - The row index number of a square grid overlay
#'      + `GRID_COL` - The column index number of a square grid overlay
#'      + `MEASURED_ROW` - The distance in meters from a defined 0-th row
#'      + `MEASURED_COL` - The distance in meters from a defined 0-th column
#'
#' @param programDbId character; required: FALSE; The identifier, which uniquely
#'    identifies a program.
#' @param programName character; required: FALSE; The human readable name of a
#'    program.
#' @param seedLotDbId character; required: FALSE; The unique identifier for the
#'    originating Seed Lot.
#' @param studyDbId character; required: FALSE; The identifier, which uniquely
#'    identifies a study within the given database server.
#' @param studyName character; required: FALSE; The human readable name for a
#'    study.
#' @param treatments data.frame; required: FALSE; Data.frame of treatments
#'    applied to an observation unit. MIAPPE V1.1 (DM-74) Observation Unit
#'    factor value - List of values for each factor applied to the observation
#'    unit. Each row in the `treatments` data.frame should contain the following
#'    columns:
#'
#'    * `factor` character; required: TRUE; The type of treatment/factor. e.g.
#'      'fertilizer', 'inoculation', 'irrigation', *etc.* MIAPPE V1.1 (DM-61)
#'      Experimental Factor type - Name/Acronym of the experimental factor.
#'    * `modality` character; required: TRUE; The treatment/factor description.
#'      e.g. 'low fertilizer', 'yellow rust inoculation', 'high water', *etc.*
#'      MIAPPE V1.1 (DM-62) Experimental Factor description - Free text
#'      description of the experimental factor. This includes all relevant
#'      treatments planned and protocol planned for all the plants targeted by a
#'      given experimental factor.
#' @param trialDbId character; required: FALSE; The identifier, which uniquely
#'    identifies a trial.
#' @param trialName character; required: FALSE; The human readable name of a
#'    trial.
#'
#' @details Update an existing Observation Units
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Observation%20Units/put_observationunits__observationUnitDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Observation Units
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' additionalInfo <- list(dummyData = "TRUE",
#'                        example = "post_observationunits")
#' externalReferences <-
#'   data.frame(referenceID = c("doi:10.155454/12341234",
#'                              "http://purl.obolibrary.org/obo/ro.owl",
#'                              "75a50e76"),
#'              referenceSource = c("DOI",
#'                                  "OBO Library",
#'                                  "Remote Data Collection Upload Tool"))
#' germplasmDbId <- "germplasm2"
#' germplasmName <- "Tomatillo Fantastico"
#' locationDbId <- "location_01"
#' locationName <- "Location 1"
#' observationUnitName <- "Plot 1"
#' observationUnitPUI <- "doi:10.12345/plot/1a9afc14"
#' ## Create the observationUnitPosition list object
#' observationUnitPosition <- list()
#' observationUnitPosition$entryType <- "TEST"
#' ## Create a geoCoordinates list object
#' library(geojsonR)
#' ## Point geometry
#' init <- TO_GeoJson$new()
#' pointGeometry <- list()
#' pointData <- c(-76.46313, # longitude
#'                 42.44423, # lattitude
#'                      123) # altitude
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
#' observationUnitPosition$geoCoordinates <- pointGeometry
#' observationUnitPosition$observationLevel <- list(
#'    levelCode = "plot_1",
#'    levelName = "plot",
#'    levelOrder =  4)
#' observationUnitPosition$observationLevelRelationships <- data.frame(
#'    levelCode  = c("fieldA", "rep1", "block1"),
#'    levelName  = c("field", "rep", "block"),
#'    levelOrder = c(1, 2, 3))
#' observationUnitPosition$positionCoordinateX <- "1"
#' observationUnitPosition$positionCoordinateXType <- "PLANTED_ROW"
#' observationUnitPosition$positionCoordinateY <- "1"
#' observationUnitPosition$positionCoordinateYType <- "PLANTED_INDIVIDUAL"
#' programDbId <- "program1"
#' programName <- "The BrAPI Breeding Program"
#' seedLotDbId <- "seed_lot2"
#' studyDbId <- "study1"
#' studyName <- "Paw paw 2013 yield trial"
#' treatments <- data.frame(
#'    factor   = c("fertilizer", "irrigation"),
#'    modality = c("high fertilizer", "low water"))
#' trialDbId <- "trial1"
#' trialName <- "Peru Yield Trial 1"
#' out <- brapi_post_observationunits(
#'   con = con,
#'   additionalInfo = additionalInfo,
#'   externalReferences = externalReferences,
#'   germplasmDbId = germplasmDbId,
#'   germplasmName = germplasmName,
#'   locationDbId = locationDbId,
#'   locationName = locationName,
#'   observationUnitName = observationUnitName,
#'   observationUnitPUI = observationUnitPUI,
#'   observationUnitPosition = observationUnitPosition,
#'   programDbId = programDbId,
#'   programName = programName,
#'   seedLotDbId = seedLotDbId,
#'   studyDbId = studyDbId,
#'   studyName = studyName,
#'   treatments = treatments,
#'   trialDbId = trialDbId,
#'   trialName = trialName)
#' observationUnitDbId <- unique(out$observationUnitDbId)
#' brapi_put_observationunits_observationUnitDbId(
#'   con = con,
#'   observationUnitDbId = observationUnitDbId,
#'   additionalInfo = list(
#'     dummyData = "TRUE",
#'     example = "put_observationunits_observationUnitDbId"),
#'   observationUnitPosition = list(geoCoordinates = polygonGeometry))
#' }
#'
#' @export
brapi_put_observationunits_observationUnitDbId <- function(con = NULL,
                                                           observationUnitDbId = '',
                                                           additionalInfo = list(),
                                                           externalReferences = '',
                                                           germplasmDbId = '',
                                                           germplasmName = '',
                                                           locationDbId = '',
                                                           locationName = '',
                                                           observationUnitName = '',
                                                           observationUnitPUI = '',
                                                           observationUnitPosition = list(),
                                                           programDbId = '',
                                                           programName = '',
                                                           seedLotDbId = '',
                                                           studyDbId = '',
                                                           studyName = '',
                                                           treatments = '',
                                                           trialDbId = '',
                                                           trialName = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "observationUnitDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_PUT_callURL(usedArgs = usedArgs,
                                          callPath = "/observationunits/{observationUnitDbId}",
                                          reqArgs = "observationUnitDbId",
                                          packageName = "BrAPI-Phenotyping",
                                          callVersion = 2.0)
  ## Build the Body
  callbody <- brapirv2:::brapi_PUT_callBody(usedArgs = usedArgs,
                                            reqArgs = "observationUnitDbId")

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_PUT(url = callurl, body = callbody, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_put_observationunits_observationUnitDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
