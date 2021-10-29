#' @title
#' post /search/callsets
#'
#' @description
#' Submit a search request for `CallSets`
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param callSetDbIds vector of type character; required: FALSE; The CallSet to
#'    search for specified as unique database callset identifier(s).; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param callSetNames vector of type character; required: FALSE; Only return
#'    call sets with these names (case-sensitive, exact match).; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param germplasmDbIds vector of type character; required: FALSE; Vector of
#'    database identifiers which uniquely identify germplasm to search for;
#'    default: &quot;&quot;, when using multiple values supply as
#'    c(&quot;value1&quot;, &quot;value2&quot;).
#' @param germplasmNames vector of type character; required: FALSE; List of
#'    human readable names to identify germplasm to search for; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#' @param sampleDbIds vector of type character; required: FALSE; Return only
#'    call sets generated from the provided unique database Biosample
#'    identifiers; default: &quot;&quot;, when using multiple values supply as
#'    c(&quot;value1&quot;, &quot;value2&quot;).
#' @param sampleNames vector of type character; required: FALSE; Return only
#'    call sets generated from the provided Biosample human readable names.;
#'    default: &quot;&quot;, when using multiple values supply as
#'    c(&quot;value1&quot;, &quot;value2&quot;).
#' @param variantSetDbIds vector of type character; required: FALSE; The unique
#'    VariantSet database identifier(s) to search for; default: &quot;&quot;,
#'    when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#'
#' @details Submit a search request for `CallSets`. Function will return either
#'    the search results (Status 200 for an immediate response) or a
#'    `searchResultsDbId` (Status 202 for both a saved and an asynchronous
#'    search).
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Call%20Sets/post_search_callsets }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Call Sets
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' # Immediate Response Example
#' brapi_post_search_callsets(con = con,
#'                            page = 0,
#'                            pageSize = 1000)
#'
#' # Saved or Asynchronous Search Response Example
#' brapi_post_search_callsets(con = con,
#'                            callSetDbIds = c("callset01",
#'                                             "callset02"),
#'                            page = 0,
#'                            pageSize = 1000,
#'                            sampleDbIds = c("sample3",
#'                                            "sample2"),
#'                            variantSetDbIds = c("variantset1"))
#' }
#'
#' @export
brapi_post_search_callsets <- function(con = NULL,
                                       callSetDbIds = '',
                                       callSetNames = '',
                                       germplasmDbIds = '',
                                       germplasmNames = '',
                                       page = 0,
                                       pageSize = 1000,
                                       sampleDbIds = '',
                                       sampleNames = '',
                                       variantSetDbIds = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                           callPath = "/search/callsets",
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
      message(paste0("Use the GET /search/callsets/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/callsets call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_callsets")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
