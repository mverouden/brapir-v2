#' @title
#' get /observations
#'
#' @description
#' Get a filtered set of Observations
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param observationDbId character; required: FALSE; Unique observation
#'    identifier to filter on.
#' @param observationUnitDbId character; required: FALSE; Unique observation
#'    unit identifier to filter on.
#' @param germplasmDbId character; required: FALSE; Unique germplasm (accession)
#'    identifier to filter on
#' @param observationVariableDbId character; required: FALSE; Unique observation
#'    variable identifier to filter on.
#' @param studyDbId character; required: FALSE; Unique study identifier to
#'    filter on.
#' @param locationDbId character; required: FALSE; Unique location identifier,
#'    where the observations were collected.
#' @param trialDbId character; required: FALSE; Unique trial identifier to
#'    filter on
#' @param programDbId character; required: FALSE; Unique program identifier to
#'    filter on.
#' @param seasonDbId character; required: TRUE; The unique identifier for a
#'    season. For backward compatibility it can be a string like '2012',
#'    '1957-2004'
#' @param observationUnitLevelName character; required: FALSE; The Observation
#'    Unit Level. Returns only the observation unit of the specified Level.
#'    References ObservationUnit-&gt;observationUnitPosition-&gt;
#'    observationLevel-&gt;levelName
#' @param observationUnitLevelOrder character; required: FALSE; The Observation
#'    Unit Level Order Number. Returns only the observation unit of the
#'    specified Level. References ObservationUnit-&gt;
#'    observationUnitPosition-&gt;observationLevel-&gt;levelOrder
#' @param observationUnitLevelCode character; required: FALSE; The Observation
#'    Unit Level Code. This parameter should be used together with
#'    `observationUnitLevelName` or `observationUnitLevelOrder`. References
#'    ObservationUnit-&gt;observationUnitPosition-&gt;observationLevel-&gt;
#'    levelCode
#' @param observationTimeStampRangeStart character; required: FALSE; Timestamp
#'    to start the range for observations to filter on. Coded in the ISO 8601
#'    standard extended format, where date, time and time zone information needs
#'    to be provided (check for example https://www.w3.org/TR/NOTE-datetime).
#' @param observationTimeStampRangeEnd character; required: FALSE; Timestamp
#'    to end the range for observations to filter on. Coded in the ISO 8601
#'    standard extended format, where date, time and time zone information needs
#'    to be provided (check for example https://www.w3.org/TR/NOTE-datetime).
#' @param externalReferenceID character; required: FALSE; An external reference
#'    ID. Could be a simple string or a URI. (use with `externalReferenceSource`
#'    parameter)
#' @param externalReferenceSource character; required: FALSE; An identifier for
#'    the source system or database of an external reference (use with
#'    `externalReferenceID` parameter)
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Retrieve all observations where there are measurements for the given
#'    observation variables.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Observations/get_observations }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Observations
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_observations(con = con)
#' }
#'
#' @export
brapi_get_observations <- function(con = NULL,
                                   observationDbId = '',
                                   observationUnitDbId = '',
                                   germplasmDbId = '',
                                   observationVariableDbId = '',
                                   studyDbId = '',
                                   locationDbId = '',
                                   trialDbId = '',
                                   programDbId = '',
                                   seasonDbId = '',
                                   observationUnitLevelName = '',
                                   observationUnitLevelOrder = '',
                                   observationUnitLevelCode = '',
                                   observationTimeStampRangeStart = '',
                                   observationTimeStampRangeEnd = '',
                                   externalReferenceID = '',
                                   externalReferenceSource = '', page = 0,
                                   pageSize = 1000) {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/observations",
                                          reqArgs = "",
                                          packageName = "BrAPI-Phenotyping",
                                          callVersion = 2.0)

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_GET(url = callurl, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_get_observations")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
