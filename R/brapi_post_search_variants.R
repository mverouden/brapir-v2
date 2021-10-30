#' @title
#' post /search/variants
#'
#' @description
#' Submit a search request for `Variants`
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param callSetDbIds vector of type character; required: FALSE; The CallSet to
#'    search for specified as unique database callset identifier(s).; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param start integer; required: FALSE; The beginning of the window (0-based,
#'    inclusive) for which overlapping variants should be returned. Genomic
#'    positions are non-negative integers less than reference length. Requests
#'    spanning the join of circular genomes are represented as two requests one
#'    on each side of the join (position 0).
#' @param end integer; required: FALSE; The end of the window (0-based,
#'    exclusive) for which overlapping variants should be returned.
#' @param referenceDbId character; required: FALSE; Only return variants on this
#'    unique reference database identifier; default: &quot;&quot;.
#' @param variantDbIds vector of type character; required: FALSE; The unique
#'    `Variant` database identifier(s) to search for; default: &quot;&quot;,
#'    when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param variantSetDbIds vector of type character; required: FALSE; The unique
#'    `VariantSet` database identifier(s) to search for; default: &quot;&quot;,
#'    when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param pageToken character; required: FALSE; Used to request a specific page
#'    of data to be returned. Tokenized pages are for large data sets, which can
#'    not be efficiently broken into indexed pages. Use the `nextPageToken` and
#'    `prevPageToken` from a prior response to construct a query and move to the
#'     next or previous page respectively.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Submit a search request for `Variants`. Function will return either
#'    the search results (Status 200 for an immediate response) or a
#'    `searchResultsDbId` (Status 202 for both a saved and an asynchronous
#'    search).
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Variants/post_search_variants }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Variants
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' # Immediate Response Example
#' brapi_post_search_variants(con = con, pageSize = 1000)
#'
#' # Saved or Asynchronous Search Response Example
#' brapi_post_search_variants(con = con,
#'                            callSetDbIds = c("callset01",
#'                                             "callset02"),
#'                            start = 0,
#'                            end = 1000,
#'                            variantDbIds = c("variant01",
#'                                             "variant02"),
#'                            variantSetDbIds = "variantset1",
#'                            pageSize = 1000)
#' }
#'
#' @export
brapi_post_search_variants <- function(con = NULL,
                                       callSetDbIds = '',
                                       start = as.integer(NA),
                                       end = as.integer(NA),
                                       referenceDbId = '',
                                       variantDbIds = '',
                                       variantSetDbIds = '',
                                       pageToken = '',
                                       pageSize = 1000) {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                           callPath = "/search/variants",
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
      message(paste0("Use the GET /search/variants/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/variants call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_variants")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
