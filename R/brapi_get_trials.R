#' @title
#' get /trials
#'
#' @description
#' Get a filtered list of Trials
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param active logical; required: FALSE; Filter active status true/false.
#'    Default: NA, other possible values: TRUE | FALSE
#' @param commonCropName character; required: FALSE; Common name for the crop
#'    associated with this trial
#' @param contactDbId character; required: FALSE; Contact entities associated
#'    with this trial
#' @param programDbId character; required: FALSE; Program filter to only return
#'    trials associated with given program id.
#' @param locationDbId character; required: FALSE; Filter by location
#' @param searchDateRangeStart character; required: FALSE; The start of the
#'    overlapping search date range. `searchDateRangeStart` must be before
#'    `searchDateRangeEnd`.
#' @param searchDateRangeEnd character; required: FALSE; The start of the
#'    overlapping search date range. `searchDateRangeStart` must be before
#'    `searchDateRangeEnd`.
#'
#' Return a Trial entity if any of the following cases are true:
#'
#' - `searchDateRangeStart` is before `trial.endDate` AND `searchDateRangeEnd`
#'    is null
#' - `searchDateRangeStart` is before `trial.endDate` AND `searchDateRangeEnd`
#'    is after `trial.startDate`
#' - `searchDateRangeEnd` is after `trial.startDate` AND `searchDateRangeStart`
#'    is null
#' - `searchDateRangeEnd` is after `trial.startDate` AND `searchDateRangeStart`
#'    is before `trial.endDate`
#' @param studyDbId character; required: FALSE; Filter by connected studyDbId
#' @param trialDbId character; required: FALSE; Filter by trialDbId
#' @param trialName character; required: FALSE; Filter by trial name
#' @param trialPUI character; required: FALSE; Filter by trial PUI
#' @param sortBy character; required: FALSE; Sort order. Name of the field to
#'    sort by.
#' @param sortOrder character; required: FALSE; Sort order direction: asc/desc.
#'    Default: &quot;&quot;, other possible values:
#'    &quot;asc&quot;|&quot;ASC&quot;|&quot;desc&quot;|&quot;DESC&quot;
#' @param externalReferenceID character; required: FALSE; An external reference
#'    ID. Could be a simple string or a URI. (use with `externalReferenceSource`
#'    parameter)
#' @param externalReferenceSource character; required: FALSE; An identifier for
#'    the source system or database of an external reference (use with
#'    `externalReferenceID` parameter)
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is 'page'= 0
#'    ). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Retrieve a filtered list of breeding Trials. A Trial is a collection
#'    of Studies
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Trials/get_trials }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Trials
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_trials(con = con)
#' }
#'
#' @export
brapi_get_trials <- function(con = NULL,
                             active = NA,
                             commonCropName = '',
                             contactDbId = '',
                             programDbId = '',
                             locationDbId = '',
                             searchDateRangeStart = '',
                             searchDateRangeEnd = '',
                             studyDbId = '',
                             trialDbId = '',
                             trialName = '',
                             trialPUI = '',
                             sortBy = '',
                             sortOrder = '',
                             externalReferenceID = '',
                             externalReferenceSource = '',
                             page = 0,
                             pageSize = 1000) {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                        callPath = "/trials",
                                        reqArgs = "",
                                        packageName = "BrAPI-Core",
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
  class(out) <- c(class(out), "brapi_get_trials")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
