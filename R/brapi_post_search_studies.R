#' @title
#' post /search/studies
#'
#' @description
#' Submit a search request for Studies
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param active logical; required: FALSE; Is this study currently active;
#'     default: NA, other possible values: TRUE | FALSE
#' @param commonCropNames vector of type character; required: FALSE; Common crop
#'    name(s) of the studies to search for; default: &quot;&quot;, when using
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
#' @param germplasmDbIds vector of type character; required: FALSE; Unique
#'    germplasm indentifier(s) to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param germplasmNames vector of type character; required: FALSE; Human
#'    readable germplasm name(s) to search for; default: &quot;&quot;, when
#'    using multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param locationDbIds vector of type character; required: FALSE; The unique
#'    location identifier(s) to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param locationNames vector of type character; required: FALSE; A human
#'    readable location name(s) to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param observationVariableDbIds vector of type character; required: FALSE;
#'    Unique observation variable indentifier(s) to search for; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param observationVariableNames vector of type character; required: FALSE;
#'    Observation variable name(s) to search for; default: &quot;&quot;, when
#'    using multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param programDbIds vector of type character; required: FALSE; Unique program
#'    identifier(s) to search for; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param programNames vector of type character; required: FALSE; Program
#'    name(s) to search for; default: &quot;&quot;, when using multiple values
#'    supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param seasonDbIds vector of type character; required: FALSE; Unique season
#'    identifier(s) to search for; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param sortBy character; required: FALSE; Name of one of the fields within
#'    the study object on which results can be sorted; default: &quot;&quot;.
#' @param sortOrder character; required: FALSE; Order results should be sorted,
#'    e.g. &quot;ASC&quot; or &quot;DESC&quot;; default: &quot;&quot;.
#' @param studyCodes vector of type character; required: FALSE; Short human
#'    readable study code(s) to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param studyDbIds vector of type character; required: FALSE; Unique study
#'    identifier(s) to search for; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param studyNames vector of type character; required: FALSE; Study name(s) to
#'    filter search results for; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param studyPUIs vector of type character; required: FALSE; Study permanent
#'    unique identifier(s) to search for. For example, a URI or DOI; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param studyTypes vector of type character; required: FALSE; Study type(s)
#'    being performed to search for, e.g. &quot;Yield Trial&quot;, *etc.*;
#'    default: &quot;&quot;, when using multiple values supply as
#'    c(&quot;value1&quot;, &quot;value2&quot;).
#' @param trialDbIds vector of type character; required: FALSE; Unique trial
#'    identifier(s) to search for; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param trialNames vector of type character; required: FALSE; Human readable
#'    trial name(s) to search for; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Advanced searching for the studies resource. Function will return
#'    either the search results (Status 200 for an immediate response) or a
#'    `searchResultsDbId` (Status 202 for both a saved and an asynchronous
#'    search).
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Studies/post_search_studies }{BrAPI SwaggerHub}
#'
#' @family brapi_2.0
#' @family brapi-core
#' @family Studies
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' TO BE ADDED STILL
#' }
#'
#' @export
brapi_post_search_studies <- function(con = NULL,
                                      active = NA,
                                      commonCropNames = '',
                                      externalReferenceIDs = '',
                                      externalReferenceSources = '',
                                      germplasmDbIds = '',
                                      germplasmNames = '',
                                      locationDbIds = '',
                                      locationNames = '',
                                      observationVariableDbIds = '',
                                      observationVariableNames = '',
                                      programDbIds = '',
                                      programNames = '',
                                      seasonDbIds = '',
                                      sortBy = '',
                                      sortOrder = '',
                                      studyCodes = '',
                                      studyDbIds = '',
                                      studyNames = '',
                                      studyPUIs = '',
                                      studyTypes = '',
                                      trialDbIds = '',
                                      trialNames = '',
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
                                           callPath = "/search/studies",
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
      message(paste0("Use the GET /search/studies/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/studies call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_studies")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
