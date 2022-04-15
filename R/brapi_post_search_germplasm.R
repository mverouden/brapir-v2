#' @title
#' post /search/germplasm
#'
#' @description
#' Submit a search request for `Germplasm`
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param accessionNumbers vector of type character; required: FALSE; The unique
#'    identifiers for accessions within a genebank; default: &quot;&quot;, when
#'    using multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param collections vector of type character; required: FALSE; A specific
#'     panel/collection/population name this germplasm belongs to.; default:
#'     &quot;&quot;, when using multiple values supply as
#'     c(&quot;value1&quot;, &quot;value2&quot;).
#' @param commonCropNames vector of type character; required: FALSE; Common name
#'    for the crop which this program is for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param externalReferenceIDs vector of type character; required: FALSE; The
#'     external reference IDs to search for. Could be a simple strings or a URIs.
#'     (use with `externalReferenceSources` parameter); default: &quot;&quot;,
#'     when using multiple values supply as c(&quot;value1&quot;,
#'     &quot;value2&quot;).
#' @param externalReferenceSources vector of type character; required: FALSE;
#'    The identifiers for the source system or database of an external reference
#'    to search for (use with `externalReferenceIDs` parameter); default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param genus vector of type character; required: FALSE; The Genus names to
#'    identify germplasm to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param germplasmDbIds vector of type character; required: FALSE; The unique
#'    germplasm database idetifier(s) to search for; default: &quot;&quot;, when
#'    using multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param germplasmNames vector of type character; required: FALSE; The human
#'    readable names to identify germplasm to search for; default: &quot;&quot;,
#'    when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param germplasmPUIs vector of type character; required: FALSE; The Permanent
#'    Unique Identifiers to identify germplasm to search for; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param parentDbIds vector of type character; required: FALSE; Search for
#'    Germplasm with these unique parents database identifier(s); default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param progenyDbIds vector of type character; required: FALSE; Search for
#'    Germplasm with these unique children database identifier(s); default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param species vector of type character; required: FALSE; The species names
#'    to identify germplasm to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param studyDbIds vector of type character; required: FALSE; The unique study
#'    database identifier(s) to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param studyNames vector of type character; required: FALSE; The study names
#'    to filter search results; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param synonyms vector of type character; required: FALSE; The alternative
#'    names or IDs used to reference this germplasm; default: &quot;&quot;,
#'    when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Submit a search request for `Germplasm`. Function will return either
#'    the search results (Status 200 for an immediate response) or a
#'    `searchResultsDbId` (Status 202 for both a saved and an asynchronous
#'    search).
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Germplasm/2.0#/Germplasm/post_search_germplasm }{BrAPI SwaggerHub}
#'
#' @family brapi-germplasm
#' @family Germplasm
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' # Immediate Response Example
#' brapi_post_search_germplasm(con = con, page = 0, pageSize = 1000)
#'
#' # Saved or Asynchronous Search Response Example
#' brapi_post_search_germplasm(con = con,
#'                             accessionNumbers = c("A0000001",
#'                                                  "A0000003"),
#'                             collections = "Fake Foods Collection",
#'                             commonCropNames = "Tomatillo",
#'                             genus = "Aspergillus",
#'                             germplasmDbIds = c("germplasm1",
#'                                                "germplasm3"),
#'                             page = 0,
#'                             pageSize = 1000)
#' }
#'
#' @export
brapi_post_search_germplasm <- function(con = NULL,
                                        accessionNumbers = '',
                                        collections = '',
                                        commonCropNames = '',
                                        externalReferenceIDs = '',
                                        externalReferenceSources = '',
                                        genus = '',
                                        germplasmDbIds = '',
                                        germplasmNames = '',
                                        germplasmPUIs = '',
                                        parentDbIds = '',
                                        progenyDbIds = '',
                                        species = '',
                                        studyDbIds = '',
                                        studyNames = '',
                                        synonyms = '',
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
                                           callPath = "/search/germplasm",
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
      message(paste0("Use the GET /search/germplasm/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/germplasm call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_germplasm")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
