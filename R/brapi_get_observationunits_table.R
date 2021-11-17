#' @title
#' get /observationunits/table
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
#'
#' @param observationUnitDbId character; required: FALSE; Unique observation
#'    unit database identifier to filter on.
#' @param germplasmDbId character; required: FALSE; Unique germplasm (accession)
#'    identifier to filter on.
#' @param observationVariableDbId character; required: FALSE; Unique observation
#'    variable identifier to filter on.
#' @param studyDbId character; required: FALSE; Unique study database identifier
#'    to filter on.
#' @param locationDbId character; required: FALSE; Unique location identifier,
#'    where the observations were collected.
#' @param trialDbId character; required: FALSE; Unique trial identifier to
#'    filter on
#' @param programDbId character; required: FALSE; Unique program database
#'    identifier to filter on.
#' @param seasonDbId character; required: TRUE; The unique database identifier
#'    for a season, e.g. the year or Phenotyping campaign of a multi-annual
#'    study (trees, grape, ...). For backward compatibility it can be a string
#'    like '2012', '1957-2004'.
#' @param observationLevel character; required: FALSE; The type of the
#'    observationUnit. Returns only the observation unit of the specified type;
#'    the parent levels ID can be accessed through `observationUnitStructure`.
#'
#' @details This service is designed to retrieve a table for observation values
#'    as a matrix of Observation Units and Observation Variables.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Observation%20Units/get_observationunits_table}{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Observation Units
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_observationunits_table(con = con,
#'                                  Accept = "application/json")
#' }
#'
#' @export
brapi_get_observationunits_table <- function(con = NULL,
                                             Accept = 'application/json',
                                             observationUnitDbId = '',
                                             germplasmDbId = '',
                                             observationVariableDbId = '',
                                             studyDbId = '',
                                             locationDbId = '',
                                             trialDbId = '',
                                             programDbId = '',
                                             seasonDbId = '',
                                             observationLevel = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "Accept")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/observationunits/table",
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
  class(out) <- c(class(out), "brapi_get_observationunits_table")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
