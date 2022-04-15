#' @title
#' post /search/attributes
#'
#' @description
#' Submit a search request for Germplasm `Attributes`
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param attributeDbIds vector of type character; required: FALSE; The unique
#'    Germplasm Attribute database identifier(s) to search for; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param attributeNames vector of type character; required: FALSE; The human
#'    readable Germplasm Attribute names to search for; default: &quot;&quot;,
#'    when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param dataTypes vector of type character; required: FALSE; The scale data
#'    types to filter search results; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param externalReferenceIDs vector of type character; required: FALSE; The
#'    external reference IDs to search for. Could be a simple strings or a URIs.
#'    (use with `externalReferenceSources` parameter); default: &quot;&quot;,
#'    when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param externalReferenceSources vector of type character; required: FALSE;
#'    The identifiers for the source system or database of an external reference
#'    to search for (use with `externalReferenceIDs` parameter); default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param methodDbIds vector of type character; required: FALSE; Unique method
#'    database indentifier(s) to filter search results; default: &quot;&quot;,
#'    when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param ontologyDbIds vector of type character; required: FALSE; Unique
#'    ontology database identiefier(s) to search for; default: &quot;&quot;,
#'    when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param scaleDbIds vector of type character; required: FALSE; List of unique
#'    scale database identifier(s) to filter search results; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param studyDbId vector of type character; required: FALSE; The unique
#'    database identifier(s) of studies to filter on; default: &quot;&quot;,
#'    when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param traitClasses vector of type character; required: FALSE; List of trait
#'    classes to filter search results; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param traitDbIds vector of type character; required: FALSE; List of trait
#'    unique database identifier(s) to filter search results; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Submit a search request for `Attributes`. Function will return
#'    either the search results (Status 200 for an immediate response) or a
#'    `searchResultsDbId` (Status 202 for both a saved and an asynchronous
#'    search).
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Germplasm/2.0#/Germplasm%20Attributes/post_search_attributes }{BrAPI SwaggerHub}
#'
#' @family brapi-germplasm
#' @family Germplasm Attributes
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' # Immediate Response Example
#' brapi_post_search_attributes(con = con, page = 0, pageSize = 1000)
#'
#' # Saved or Asynchronous Search Response Example
#' brapi_post_search_attributes(con = con,
#'                              attributeDbIds = c("attribute1",
#'                                                  "attribute2"),
#'                              attributeNames = "Rht-B2b",
#'                              externalReferenceIDs = "https://brapi.org/specification",
#'                              externalReferenceSources = "BrAPI Doc",
#'                              traitClasses = "Numeric",
#'                              page = 0,
#'                              pageSize = 1000)
#' }
#'
#' @export
brapi_post_search_attributes <- function(con = NULL,
                                         attributeDbIds = '',
                                         attributeNames = '',
                                         dataTypes = '',
                                         externalReferenceIDs = '',
                                         externalReferenceSources = '',
                                         methodDbIds = '',
                                         ontologyDbIds = '',
                                         scaleDbIds = '',
                                         studyDbId = '',
                                         traitClasses = '',
                                         traitDbIds = '',
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
                                           callPath = "/search/attributes",
                                           reqArgs = "",
                                           packageName = "BrAPI-Germplasm",
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
      message(paste0("Use the GET /search/attributes/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/attributes call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_attributes")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
