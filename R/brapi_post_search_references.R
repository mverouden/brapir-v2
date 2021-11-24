#' @title
#' post /search/references
#'
#' @description
#' Submit a search request for `References`
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param accessions vector of type character; required: FALSE; If specified,
#'    return the references for which the accession matches this string (case-
#'    sensitive, exact match); default: &quot;&quot;, when using multiple values
#'    supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param isDerived logical; required: FALSE; A sequence X is said to be derived
#'    from source sequence Y, if X and Y are of the same length and the per-base
#'    sequence divergence at A/C/G/T bases is sufficiently small. Two sequences
#'    derived from the same official sequence share the same coordinates and
#'    annotations, and can be replaced with the official sequence for certain
#'    use cases. If the reference is derived (TRUE) from a source sequence or
#'    not (FALSE). If unspecified (NA), both will be displayed.
#' @param maxLength integer; required: FALSE; The maximum length of the
#'    reference sequences to be retrieved.
#' @param md5checksums vector of type character; required: FALSE; If specified,
#'    return the references for which the `md5checksum` matches this string
#'    (case-sensitive, exact match); default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param minLength integer; required: FALSE; The minimum length of the
#'    reference sequences to be retrieved.
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#' @param referenceDbIds vector of type character; required: FALSE; The unique
#'    database identifier(s) of a reference or references to search; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param referenceSetDbIds vector of type character; required: FALSE; The unique
#'    database identifier(s) of a referenceset or referencesets to search.;
#'    default: &quot;&quot;, when using multiple values supply as c(&quot;value1
#'    &quot;, &quot;value2&quot;).
#'
#' @details Submit a search request for `References`. Function will return
#'    either the search results (Status 200 for an immediate response) or a
#'    `searchResultsDbId` (Status 202 for both a saved and an asynchronous
#'    search).
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/References/post_search_references }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family References
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' # Immediate Response Example
#' brapi_post_search_references(con = con,
#'                              page = 0,
#'                              pageSize = 1000)
#'
#' # Saved or Asynchronous Search Response Example
#' brapi_post_search_references(con = con,
#'                              accessions = c("A0000001",
#'                                             "A0000002"),
#'                              page = 0,
#'                              pageSize = 1000,
#'                              referenceDbIds = c("reference1",
#'                                                 "reference2"),
#'                              referenceSetDbIds = c("reference_set1",
#'                                                    "reference_set2"))
#' }
#'
#' @export
brapi_post_search_references <- function(con = NULL,
                                         accessions = '',
                                         isDerived = NA,
                                         maxLength = as.integer(NA),
                                         md5checksums = '',
                                         minLength = as.integer(NA),
                                         page = 0,
                                         pageSize = 1000,
                                         referenceDbIds = '',
                                         referenceSetDbIds = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                           callPath = "/search/references",
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
      message(paste0("Use the GET /search/references/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/references call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_references")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
