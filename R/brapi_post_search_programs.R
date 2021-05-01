#' @title
#' post /search/programs
#'
#' @description
#' Submit a search request for Programs
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param abbreviations vector of type character; required: FALSE; Program
#'    abbreviation(s) to search for; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param commonCropNames vector of type character; required: FALSE; Common crop
#'    name(s) to search for; default: &quot;&quot;, when using multiple values
#'    supply as c(&quot;value1&quot;, &quot;value2&quot;).
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
#' @param leadPersonDbIds vector of type character; required: FALSE; Unique lead
#'    person identifier(s) of the program leader(s) to search for; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param leadPersonNames vector of type character; required: FALSE; Program
#'    leader name(s) to search for; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param objectives vector of type character; required: FALSE; Program
#'    objective(s) to search for; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param programDbIds vector of type character; required: FALSE; Unique program
#'    identifier(s) to search for; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param programNames vector of type character; required: FALSE; Program
#'    name(s) to search for; default: &quot;&quot;, when using multiple values
#'    supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param page integer; required: FALSE; Which result page is requested. The
#'    page indexing starts at 0 (the first page is `page = 0`). Default is `0`.
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
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Programs/post_search_programs }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Programs
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
brapi_post_search_programs <- function(con = NULL,
                                       abbreviations = '',
                                       commonCropNames = '',
                                       externalReferenceIDs = '',
                                       externalReferenceSources = '',
                                       leadPersonDbIds = '',
                                       leadPersonNames = '',
                                       objectives = '',
                                       programDbIds = '',
                                       programNames = '',
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
                                           callPath = "/search/programs",
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
      message(paste0("Use the GET /search/programs/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/programs call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
       ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_programs")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
