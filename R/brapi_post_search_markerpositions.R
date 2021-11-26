#' @title
#' post /search/markerpositions
#'
#' @description
#' Submit a search request for `MarkerPositions`
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param linkageGroupNames vector of type character; required: FALSE; The
#'    uniquely identifiable name(s) of the linkage group(s) to search for;
#'    default: &quot;&quot;, when using multiple values supply as c(&quot;
#'    value1&quot;, &quot;value2&quot;).
#' @param mapDbIds vector of type character; required: FALSE; The unique
#'    database identifier(s) of the map(s) to search for; default: &quot;&quot;,
#'    when using multiple values supply as c(&quot;value1&quot;, &quot;value2
#'    &quot;).
#' @param minPosition integer; required: FALSE; The minimum position.
#' @param maxPosition integer; required: FALSE; The maximum position.
#' @param variantDbIds vector of type character; required: FALSE; Unique
#'    internal database identifier(s) of the variant(s) to search for; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Submit a search request for `MarkerPositions`. Function will return
#'    either the search results (Status 200 for an immediate response) or a
#'    `searchResultsDbId` (Status 202 for both a saved and an asynchronous
#'    search).
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Genome%20Maps/post_search_markerpositions }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Genome Maps
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' # Immediate Response Example
#' brapi_post_search_markerpositions(con = con,
#'                                   page = 0,
#'                                   pageSize = 1000)
#'
#' # Saved or Asynchronous Search Response Example
#' brapi_post_search_markerpositions(con = con,
#'                                   linkageGroupNames = c("Chromosome 1",
#'                                                         "Chromosome 2"),
#'                                   mapDbIds = c("genome_map1",
#'                                                "genome_map2"),
#'                                   minPosition = 0,
#'                                   maxPosition = 60000,
#'                                   variantDbIds = c("variant03",
#'                                                    "variant06"),
#'                                   page = 0,
#'                                   pageSize = 1000)
#' }
#'
#' @export
brapi_post_search_markerpositions <- function(con = NULL,
                                              linkageGroupNames = '',
                                              mapDbIds = '',
                                              minPosition = as.integer(NA),
                                              maxPosition = as.integer(NA),
                                              variantDbIds = '',
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
                                           callPath = "/search/markerpositions",
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
      message(paste0("Use the GET /search/markerpositions/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/markerpositions call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_markerpositions")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
