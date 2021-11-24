#' @title
#' post /search/samples
#'
#' @description
#' Submit a search request for `Samples`
#'
#' @param con list; required: TRUE; BrAPI connection object
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
#' @param germplasmDbIds vector of type character; required: FALSE; Vector of
#'    database identifiers which uniquely identify germplasm to search for;
#'    default: &quot;&quot;, when using multiple values supply as
#'    c(&quot;value1&quot;, &quot;value2&quot;).
#' @param germplasmNames vector of type character; required: FALSE; Human
#'    readable germplasm name(s) to search for; default: &quot;&quot;, when
#'    using multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param observationUnitDbIds vector of type character; required: FALSE; The
#'    unique database identifier(s) of observation unit(s) to search samples for
#'    ; default: &quot;&quot;, when using multiple values supply as c(&quot;
#'    value1&quot;, &quot;value2&quot;).
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#' @param plateDbIds vector of type character; required: FALSE; The
#'    identifier(s), which uniquely identifies a plate or plates of samples;
#'    default: &quot;&quot;, when using multiple values supply as c(&quot;value1
#'    &quot;, &quot;value2&quot;).
#' @param sampleDbIds vector of type character; required: FALSE; The
#'    identifier(s), which uniquely identifies a sample or samples; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param studyDbIds vector of type character; required: FALSE; List of unique
#'    study database identifier(s) to search for; default: &quot;&quot;, when
#'    using multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param studyNames vector of type character; required: FALSE; List of study
#'    names to filter search results; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#'
#' @details Submit a search request for `Samples`. Function will return either
#'    the search results (Status 200 for an immediate response) or a
#'    `searchResultsDbId` (Status 202 for both a saved and an asynchronous
#'    search).
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Samples/post_search_samples }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Samples
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' # Immediate Response Example
#' brapi_post_search_samples(con = con,
#'                           page = 0,
#'                           pageSize = 1000)
#'
#' # Saved or Asynchronous Search Response Example
#' brapi_post_search_samples(con = con,
#'                           germplasmDbIds = c("germplasm1",
#'                                              "germplasm3"),
#'                           page = 0,
#'                           pageSize = 1000,
#'                           plateDbIds = c("plate1",
#'                                          "vendor_plate1"),
#'                           studyDbIds = "study1")
#' }
#'
#' @export
brapi_post_search_samples <- function(con = NULL,
                                      externalReferenceIDs = '',
                                      externalReferenceSources = '',
                                      germplasmDbIds = '',
                                      germplasmNames = '',
                                      observationUnitDbIds = '',
                                      page = 0,
                                      pageSize = 1000,
                                      plateDbIds = '',
                                      sampleDbIds = '',
                                      studyDbIds = '',
                                      studyNames = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                           callPath = "/search/samples",
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
      message(paste0("Use the GET /search/samples/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/samples call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_samples")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
