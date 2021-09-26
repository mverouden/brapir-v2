#' @title
#' post /search/observationunits
#'
#' @description
#' Submit a search request for `ObservationUnits`
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param externalReferenceIDs vector of type character; required: FALSE; List
#'     of external reference IDs. Could be a simple strings or a URIs. (use with
#'     `externalReferenceSources` parameter); default: &quot;&quot;, when using
#'     multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param externalReferenceSources vector of type character; required: FALSE;
#'    List of identifiers for the source system or database of an external
#'    reference (use with `externalReferenceIDs` parameter); default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param germplasmDbIds vector of type character; required: FALSE; List of
#'    unique database identifiers, which uniquely identify germplasm to search
#'    for; default: &quot;&quot;, when using multiple values supply as c(
#'    &quot;value1&quot;, &quot;value2&quot;).
#' @param germplasmNames vector of type character; required: FALSE; List of
#'    human readable names to identify germplasm to search for; default:
#'     &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'     &quot;value2&quot;).
#' @param includeObservations logical; required: FALSE; Use this parameter to
#'    include a list of observations embedded in each ObservationUnit object.
#'    CAUTION - Use this parameter at your own risk. It may return large,
#'    unpaginated lists of observation data. Only set this value to `TRUE` if
#'    you are sure you need to.
#' @param locationDbIds vector of type character; required: FALSE; The unique
#'    location identifier(s) to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param locationNames vector of type character; required: FALSE; A human
#'    readable location name(s) to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param observationLevelRelationships data.frame; required: FALSE; Data.frame
#'    of observation levels to indicate the granularity level at which the
#'    measurements are taken. Each row in the data.frame defines the level code,
#'    level name (`levelName`), and the level order, as integer, where that
#'    level exists in the hierarchy of levels. `levelOrders` lower numbers are
#'    at the top of the hierarchy (i.e. field -> 0) and higher numbers are at
#'    the bottom of the hierarchy (ie plant > 6). `levelCode` is an identifier
#'    code for this level tag. Identify this observation unit by each level of
#'    the hierarchy, where it exists.
#'
#'    The Examples section shows an example of how to construct the
#'    `observationLevelRelationships` argument as a data.frame.
#' @param observationLevels data.frame; required: FALSE; A data.frame of
#'    Observation levels, which indicate the granularity level at which the
#'    measurements are taken. The `observationLevels` argument data.frame can
#'    contain the following columns:
#'
#'    - `levelName` character; required: FALSE; A name for this level.
#'    - `levelOrder` interger; required: FALSE; `levelOrder` defines, where that
#'      level exists in the hierarchy of levels. `levelOrder`'s lower numbers
#'      are at the top of the hierarchy (i.e. field -> 1) and higher numbers are
#'      at the bottom of the hierarchy (i.e. plant -> 9).
#'
#'    The Examples section shows an example of how to construct the
#'    `observationLevels` argument as a data.frame.
#' @param observationUnitDbIds vector of type character; required: FALSE; The
#'    unique database identifier of an observation unit; default: &quot;&quot;,
#'    when using multiple values supply as c(&quot;value1&quot;,
#'     &quot;value2&quot;).
#' @param observationVariableDbIds vector of type character; required: FALSE;
#'    The database identifiers of Variables to search for; default: &quot;&quot;
#'    , when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param observationVariableNames vector of type character; required: FALSE;
#'    The names of Variables to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#' @param programDbIds vector of type character; required: FALSE; Unique program
#'    database identifiers to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param programNames vector of type character; required: FALSE; A program
#'    names to search for; default: &quot;&quot;, when using multiple values
#'    supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param studyDbIds vector of type character; required: FALSE; List of unique
#'    database study identifiers to search for; default: &quot;&quot;, when
#'    using multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param studyNames vector of type character; required: FALSE; List of study
#'    names to filter search results; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param trialDbIds vector of type character; required: FALSE; The identifiers,
#'    which uniquely identify trials to search for; default: &quot;&quot;, when
#'    using multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param trialNames vector of type character; required: FALSE; The human
#'    readable names of trials to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#'
#' @details Submit a search request for `ObservationUnits`. Function will return
#'    either the search results (Status 200 for an immediate response) or a
#'    `searchResultsDbId` (Status 202 for both a saved and an asynchronous
#'    search).
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Observation%20Units/post_search_observationunits }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Observation Units
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' observationLevelRelationships <- data.frame(
#'   levelCode  = c("fieldA", "rep1", "block1"),
#'   levelName  = c("field", "rep", "block"),
#'   levelOrder = c(1, 2, 3))
#' observationLevels <-
#'   data.frame(levelName = c("field", "block", "plot"),
#'              levelOrder = c(0, 1, 2))
#'
#' # Immediate Response Example
#' brapi_post_search_observationunits(con = con,
#'                                    germplasmDbIds = "germplasm1")
#'
#' # Saved or Asynchronous Search Response Example
#' brapi_post_search_observationunits(
#'   con = con,
#'   externalReferenceIDs = "https://brapi.org/specification",
#'   externalReferenceSources = "BrAPI Doc",
#'   germplasmDbIds = c("germplasm1", "germplasm2"),
#'   includeObservations = FALSE,
#'   programDbIds = "program1")
#' }
#'
#' @export
brapi_post_search_observationunits <- function(con = NULL,
                                               externalReferenceIDs = '',
                                               externalReferenceSources = '',
                                               germplasmDbIds = '',
                                               germplasmNames = '',
                                               includeObservations = NA,
                                               locationDbIds = '',
                                               locationNames = '',
                                               observationLevelRelationships = '',
                                               observationLevels = '',
                                               observationUnitDbIds = '',
                                               observationVariableDbIds = '',
                                               observationVariableNames = '',
                                               page = 0,
                                               pageSize = 1000,
                                               programDbIds = '',
                                               programNames = '',
                                               studyDbIds = '',
                                               studyNames = '',
                                               trialDbIds = '',
                                               trialNames = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                           callPath = "/search/observationunits",
                                           reqArgs = "",
                                           packageName = "BrAPI-Phenotyping",
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
      message(paste0("Use the GET /search/observationunits/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/observationunits call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_observationunits")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
