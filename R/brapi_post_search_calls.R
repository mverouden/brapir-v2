#' @title
#' post /search/calls
#'
#' @description
#' Submit a search request for `Calls`
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param callSetDbIds vector of type character; required: FALSE; The CallSet to
#'    search for specified as unique database callset identifier(s).; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param expandHomozygotes logical; required: FALSE; Should homozygotes be
#'    expanded (TRUE) or collapsed into a single occurrence (FALSE); default: NA
#'    , other possible values: TRUE | FALSE.
#' @param pageToken character; required: FALSE; Used to request a specific page
#'    of data to be returned. Tokenized pages are for large data sets, which can
#'    not be efficiently broken into indexed pages. Use the `nextPageToken` and
#'    `prevPageToken` from a prior response to construct a query and move to the
#'     next or previous page respectively.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#' @param sepPhased character; required: FALSE; The string to use as a separator
#'    for phased allele calls.
#' @param sepUnphased character; required: FALSE; The string to use as a
#'    separator for unphased allele calls.
#' @param unknownString character; required: FALSE; The string used as a
#'    representation for missing data.; default: &quot;&quot;.
#' @param variantDbIds vector of type character; required: FALSE; The unique
#'    variant database identifier(s) to search for.; default: &quot;&quot;,
#'    when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param variantSetDbIds vector of type character; required: FALSE; The unique
#'    variantset database identifier(s) to search for.; default: &quot;&quot;,
#'    when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#'
#' @details Submit a search request for `Calls`. Function will return either the
#'    search results (Status 200 for an immediate response) or a
#'    `searchResultsDbId` (Status 202 for both a saved and an asynchronous
#'    search).
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Calls/post_search_calls }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Calls
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' # Immediate Response Example
#' brapi_post_search_calls(con = con, pageSize = 1000)
#'
#' # Saved or Asynchronous Search Response Example
#' brapi_post_search_calls(con = con,
#'                         callSetDbIds = c("callSet12",
#'                                          "callSet21"),
#'                         expandHomozygotes = TRUE,
#'                         pageSize = 1000,
#'                         variantDbIds = c("variant15",
#'                                          "variant23"))
#' }
#'
#' @export
brapi_post_search_calls <- function(con = NULL,
                                    callSetDbIds = '',
                                    expandHomozygotes = NA,
                                    pageSize = 1000,
                                    pageToken = '',
                                    sepPhased = '',
                                    sepUnphased = '',
                                    unknownString = '',
                                    variantDbIds = '',
                                    variantSetDbIds = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                           callPath = "/search/calls",
                                           reqArgs = "",
                                           packageName = "BrAPI-Genotyping",
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
      message(paste0("Use the GET /search/calls/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/calls call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_calls")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
