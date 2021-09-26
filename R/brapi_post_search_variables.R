#' @title
#' post /search/variables
#'
#' @description
#' Submit a search request for Observation `Variables`
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param dataTypes vector of type character; required: FALSE; List of scale
#'    data types to filter search results. Class of the scale entries can be:
#'
#'    * &quot;Code&quot; -  This scale class is exceptionally used to express
#'      complex traits. Code is a nominal scale that combines the expressions of
#'      the different traits composing the complex trait. For example a severity
#'      trait might be expressed by a 2 digit and 2 character code. The first 2
#'      digits are the percentage of the plant covered by a fungus and the 2
#'      characters refer to the delay in development, e.g. &quot;75VD&quot;
#'      means &quot;75 %&quot; of the plant is infected and the plant is very
#'      delayed.
#'    * &quot;Date&quot; - The date class is for events expressed in a time
#'      format, coded in the ISO 8601 standard extended format, where date, time
#'      and time zone information needs to be provided (check for example
#'      [https://www.w3.org/TR/NOTE-datetime](https://www.w3.org/TR/NOTE-datetime).
#'    * &quot;Duration&quot; - The Duration class is for time elapsed between
#'      two events expressed in a time format, e.g. days, hours, months.
#'    * &quot;Nominal&quot; - Categorical scale that can take one of a limited
#'      and fixed number of categories. There is no intrinsic ordering to the
#'      categories.
#'    * &quot;Numerical&quot; - Numerical scales express the trait with real
#'      numbers. The numerical scale defines the unit e.g. centimeter, ton per
#'      hectare, branches.
#'    * &quot;Ordinal&quot; - Ordinal scales are scales composed of ordered
#'      categories.
#'    * &quot;Text&quot; - A free text is used to express the trait.
#'
#'    default: &quot;&quot;, when using multiple values supply as c(
#'    &quot;value1&quot;, &quot;value2&quot;).
#' @param externalReferenceIDs vector of type character; required: FALSE; List
#'    of external reference IDs. Could be a simple strings or a URIs. (use with
#'    `externalReferenceSources` parameter); default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param externalReferenceSources vector of type character; required: FALSE;
#'    List of identifiers for the source system or database of an external
#'    reference (use with `externalReferenceIDs` parameter); default:
#'     &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'     &quot;value2&quot;).
#' @param methodDbIds vector of type character; required: FALSE; List of methods
#'    , supplied as unique database identifiers, to filter search results;
#'    default: &quot;&quot;, when using multiple values supply as c(
#'    &quot;value1&quot;, &quot;value2&quot;).
#' @param observationVariableDbIds vector of type character; required: FALSE;
#'    List of unique observation variable datbase identifiers to search for;
#'    default: &quot;&quot;, when using multiple values supply as c(
#'    &quot;value1&quot;, &quot;value2&quot;).
#' @param observationVariableNames vector of type character; required: FALSE;
#'    List of human readable observation variable names to search for; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param ontologyDbIds vector of type character; required: FALSE; List of
#'    unique ontology database identifiers to search for; default: &quot;&quot;,
#'    when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#' @param scaleDbIds vector of type character; required: FALSE; List of unique
#'    scale database identifiers to filter search results; default: &quot;&quot;
#'    , when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param studyDbId vector of type character; required: FALSE; The unique
#'    database identifiers of studies to filter on; default: &quot;&quot;,
#'    when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#' @param traitClasses vector of type character; required: FALSE; List of trait
#'    classes to filter search results, examples:
#'    &quot;morphological&quot;, &quot;phenological&quot;,
#'    &quot;agronomical&quot;, &quot;physiological&quot;,
#'    &quot;abiotic stress&quot;, &quot;biotic stress&quot;,
#'    &quot;biochemical&quot;, &quot;quality traits&quot;,
#'    &quot;fertility&quot;, *etc.*); default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param traitDbIds vector of type character; required: FALSE; List of trait
#'    unique database identifiers to filter search results; default:
#'    &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;,
#'    &quot;value2&quot;).
#'
#' @details Submit a search request for Observation `Variables`. Function will
#'    return either the search results (Status 200 for an immediate response) or
#'    a `searchResultsDbId` (Status 202 for both a saved and an asynchronous
#'    search).
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Observation%20Variables/post_search_variables }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Observation Variables
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' # Immediate Response Example
#' brapi_post_search_variables(con = con, page = 0, pageSize = 1000)
#'
#' # Saved or Asynchronous Search Response Example
#' brapi_post_search_variables(
#'  con = con,
#'  dataTypes = c("Numerical", "Text"),
#'  externalReferenceIDs = "https://brapi.org/specification",
#'  externalReferenceSources = "BrAPI Doc",
#'  ontologyDbIds = "ontology_variable1",
#'  page = 0,
#'  pageSize = 1000)
#' }
#'
#' @export
brapi_post_search_variables <- function(con = NULL,
                                        dataTypes = '',
                                        externalReferenceIDs = '',
                                        externalReferenceSources = '',
                                        methodDbIds = '',
                                        observationVariableDbIds = '',
                                        observationVariableNames = '',
                                        ontologyDbIds = '',
                                        page = 0,
                                        pageSize = 1000,
                                        scaleDbIds = '',
                                        studyDbId = '',
                                        traitClasses = '',
                                        traitDbIds = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                           callPath = "/search/variables",
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
      message(paste0("Use the GET /search/variables/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/variables call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_variables")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
