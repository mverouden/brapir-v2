#' @title
#' put /observations/\{observationDbId\}
#'
#' @description
#' Update an existing Observation
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param observationDbId character; required: TRUE; The unique identifier of an
#'    observation to update.
#' @param additionalInfo list; required: FALSE; Additional arbitrary information.
#'    If provided use the following construct list(additionalProp1 = "string",
#'    additionalProp2 =  "string", additionalProp3 = "string").
#'
#'    The Examples section shows an example on how to construct the
#'    `additionalInfo` argument as a list.
#' @param collector character; required: FALSE; The name or identifier of the
#'    entity, which collected the observation.
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
#' @param germplasmDbId character; required: FALSE; The identifier which
#'    uniquely identifies a germplasm.
#' @param germplasmName character; required: FALSE; Human readable name of the
#'    germplasm. It can be the preferred name and does not have to be unique.
#' @param observationTimeStamp character; required: FALSE; The date and time
#'    when the observation was made. Coded in the ISO 8601 standard extended
#'    format, where date, time and time zone information needs to be provided
#'    (check for example https://www.w3.org/TR/NOTE-datetime).
#' @param observationUnitDbId character; required: FALSE; The identifier which
#'    uniquely identifies an observation unit.
#' @param observationUnitName character; required: FALSE; A human readable name
#'    for an observation unit.
#' @param observationVariableDbId character; required: FALSE; The identifier
#'    which uniquely identifies an observation variable.
#' @param observationVariableName character; required: FALSE; A human readable
#'    name for an observation variable.
#' @param season list; required: FALSE; Information about the season in which
#'    the observation was made. The list can or should contain the following
#'    elements:
#'
#'    * `season` character; required: FALSE; Name of the season, e.g. 'Spring',
#'              'Q2', 'Season A', *etc.*
#'    * `seasonDbId` character; required: __TRUE__; Uniquely identifier for the
#'                   season in which the observation was made. For backward
#'                   compatibility it can be a string like '2012', '1957-2004'.
#'    * `year` integer; required: FALSE; The 4 digit year of the season in which
#'             the observation was made.
#'
#'    The Examples section shows an example of how to construct the
#'    `season` argument as a list.
#' @param studyDbId character; required: FALSE; The identifier which uniquely
#'    identifies a study within the given database server.
#' @param uploadedBy character; required: FALSE; The name or id of the user, who
#'    uploaded the observation to the database system.
#' @param value character; required: FALSE; The value of the data collected as
#'    an observation.
#'
#' @details Update an existing Observation
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Observations/put_observations__observationDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Observations
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' additionalInfo <- list(dummyData = "TRUE",
#'                        example = "post_observations")
#' collector <- "BrAPIR v2"
#' externalReferences <-
#'   data.frame(referenceID = c("doi:10.155454/12341234",
#'                              "http://purl.obolibrary.org/obo/ro.owl",
#'                              "75a50e76"),
#'              referenceSource = c("DOI",
#'                                  "OBO Library",
#'                                  "Remote Data Collection Upload Tool"))
#' germplasmDbId <- "germplasm2"
#' germplasmName <- "Tomatillo Fantastico"
#' observationTimeStamp <- "2021-05-11T19:00:00+02:00"
#' observationUnitDbId <- "observation_unit2"
#' observationUnitName <- "Plot 2"
#' observationVariableDbId <- "variable1"
#' observationVariableName <- "Plant Height"
#' season <- list(season = "summer",
#'                seasonDbId = "summer_2013",
#'                year = 2013)
#' studyDbId <- "study2"
#' uploadedBy <- "Maikel (BrAPIR)"
#' value <- "50"
#' out <-
#'   brapi_post_observations(con = con,
#'                           additionalInfo = additionalInfo,
#'                           collector = collector,
#'                           externalReferences = externalReferences,
#'                           germplasmDbId = germplasmDbId,
#'                           germplasmName = germplasmName,
#'                           observationTimeStamp = observationTimeStamp,
#'                           observationUnitDbId = observationUnitDbId,
#'                           observationUnitName = observationUnitName,
#'                           observationVariableDbId = observationVariableDbId,
#'                           observationVarialbleName = observationVariableName,
#'                           season = season,
#'                           studyDbId = studyDbId,
#'                           uploadedBy = uploadedBy,
#'                           value = value)
#' observationDbId <- unique(out$observationDbId)
#' brapi_put_observations_observationDbId(con = con,
#'                                        observationDbId = observationDbId,
#'                                        value = "100")
#' }
#'
#' @export
brapi_put_observations_observationDbId <- function(con = NULL,
                                                   observationDbId = '',
                                                   additionalInfo = list(),
                                                   collector = '',
                                                   externalReferences = '',
                                                   germplasmDbId = '',
                                                   germplasmName = '',
                                                   observationTimeStamp = '',
                                                   observationUnitDbId = '',
                                                   observationUnitName = '',
                                                   observationVariableDbId = '',
                                                   observationVariableName = '',
                                                   season = list(),
                                                   studyDbId = '',
                                                   uploadedBy = '',
                                                   value = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "observationDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_PUT_callURL(usedArgs = usedArgs,
                                          callPath = "/observations/{observationDbId}",
                                          reqArgs = "observationDbId",
                                          packageName = "BrAPI-Phenotyping",
                                          callVersion = 2.0)
  ## Build the Body
  callbody <- brapirv2:::brapi_PUT_callBody(usedArgs = usedArgs,
                                            reqArgs = "observationDbId")

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_PUT(url = callurl, body = callbody, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_put_observations_observationDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
