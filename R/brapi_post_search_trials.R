#' @title
#' post /search/trials
#'
#' @description
#' Submit a search request for Trials
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param active logical; required: FALSE; Is this trial currently active;
#'    default: NA, other possible values: TRUE | FALSE
#' @param commonCropNames vector of type character; required: FALSE; Common crop
#'    name(s) for the trials to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param contactDbIds vector of type character; required: FALSE; Unique contact
#'    identifier(s) of trials to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
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
#' @param locationDbIds vector of type character; required: FALSE; The unique
#'    location identifier(s) to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param locationNames vector of type character; required: FALSE; A human
#'    readable location name(s) to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param programDbIds vector of type character; required: FALSE; Unique program
#'    identifier(s) to search for; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param programNames vector of type character; required: FALSE; Program
#'    name(s) to search for; default: &quot;&quot;, when using multiple values
#'    supply as c(&quot;value1&quot;, &quot;value2&quot;).
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
#' @param studyDbIds vector of type character; required: FALSE; Unique study
#'    identifier(s) to search for; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param studyNames vector of type character; required: FALSE; Study name(s)
#'    to search for; default: &quot;&quot;, when using multiple values supply as
#'    c(&quot;value1&quot;, &quot;value2&quot;).
#' @param trialDbIds vector of type character; required: FALSE; Unique trial
#'    identifier(s) to search for; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param trialNames vector of type character; required: FALSE; The human
#'    readable trial name(s)to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param trialPUIs vector of type character; required: FALSE; A permanent trial
#'    identifier(s) to search for. Could be DOI or other URI formatted
#'    identifier.; default: &quot;&quot;, when using multiple values supply as
#'    c(&quot;value1&quot;, &quot;value2&quot;).
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Advanced searching for the programs resource. Function will return
#'    either the search results (Status 200 for an immediate response) or a
#'    `searchResultsDbId` (Status 202 for both a saved and an asynchronous
#'    search).
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Trials/post_search_trials }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Trials
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' # Immediate Response Example
#' brapi_post_search_trials(con = con,
#'                          commonCropNames = "Paw Paw")
#'
#' # Saved or Asynchronous Search Response Example
#' brapi_post_search_trials(con = con,
#'                          active = TRUE,
#'                          commonCropNames = c("Tomatillo", "Paw Paw"),
#'                          contactDbIds = c("trial_contact_1", "trial_contact_3"),
#'                          programDbIds = c("program1" , "program3"),
#'                          trialDbIds = c("trial1", "trial3"))
#' }
#'
#' @export
brapi_post_search_trials <- function(con = NULL,
                                     active = NA,
                                     commonCropNames = '',
                                     contactDbIds = '',
                                     externalReferenceIDs = '',
                                     externalReferenceSources = '',
                                     locationDbIds = '',
                                     locationNames = '',
                                     programDbIds = '',
                                     programNames = '',
                                     searchDateRangeEnd = '',
                                     searchDateRangeStart = '',
                                     studyDbIds = '',
                                     studyNames = '',
                                     trialDbIds = '',
                                     trialNames = '',
                                     trialPUIs = '',
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
                                           callPath = "/search/trials",
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
      message(paste0("Use the GET /search/trials/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/trials call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_trials")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
