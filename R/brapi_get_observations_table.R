#' @title
#' get /observations/table
#'
#' @description
#' Get a list of Observations in a table format
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param Accept character; required: TRUE; The requested content type which
#'    should be returned by the server; default: "application/json",
#'    other possible values:
#'
#'    * "text/csv"
#'    * "text/tsv"
#'    * "application/flapjack"
#'
#' @param observationUnitDbId character; required: FALSE; Unique observation
#'    unit data base identifier to filter on.
#' @param germplasmDbId character; required: FALSE; The unique database
#'    identifier of a germplasm (accession) to filter on.
#' @param observationVariableDbId character; required: FALSE; The unique
#'    database identifier of an observation variable to filter on.
#' @param studyDbId character; required: FALSE; The unique database identifier
#'    of a study to filter on.
#' @param locationDbId character; required: FALSE; The unique database
#'    identifier of a location where these observations were collected.
#' @param trialDbId character; required: FALSE; The unique database identifier
#'    of a trial to filter on.
#' @param programDbId character; required: FALSE; The unique database identifier
#'    of a program to filter on.
#' @param seasonDbId character; required: TRUE; The unique identifier for a
#'    season to filter on. For backward compatibility it can be a string like
#'    '2012', '1957-2004'.
#' @param observationLevel character; required: FALSE; The type of the
#'    observation unit. Returns only the observation unit of the specified type;
#'    the parent levels identifier can be accessed through
#'    `observationUnitStructure`.
#' @param searchResultsDbId character; required: FALSE; Permanent unique
#'    database identifier, which references the search results.
#' @param observationTimeStampRangeStart character; required: FALSE; Time stamp
#'    to start the range for observations to filter on. Coded in the ISO 8601
#'    standard extended format, where date, time and time zone information needs
#'    to be provided (check for example https://www.w3.org/TR/NOTE-datetime).
#' @param observationTimeStampRangeEnd character; required: FALSE; Time stamp
#'    to end the range for observations to filter on. Coded in the ISO 8601
#'    standard extended format, where date, time and time zone information needs
#'    to be provided (check for example https://www.w3.org/TR/NOTE-datetime).
#'
#' @details This service is designed to retrieve a table of time dependent
#'    observation values as a matrix of Observation Units and Observation
#'    Variables. This is also sometimes called a Time Series.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Observations/get_observations_table }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Observations
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_observations_table(con = con,
#'                              Accept = "application/json")
#' }
#'
#' @export
brapi_get_observations_table <- function(con = NULL,
                                         Accept = 'application/json',
                                         observationUnitDbId = '',
                                         germplasmDbId = '',
                                         observationVariableDbId = '',
                                         studyDbId = '',
                                         locationDbId = '',
                                         trialDbId = '',
                                         programDbId = '',
                                         seasonDbId = '',
                                         observationLevel = '',
                                         searchResultsDbId = '',
                                         observationTimeStampRangeStart = '',
                                         observationTimeStampRangeEnd = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "Accept")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/observations/table",
                                          reqArgs = "Accept",
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
  class(out) <- c(class(out), "brapi_get_observations_table")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
