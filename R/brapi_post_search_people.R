#' @title
#' post /search/people
#'
#' @description
#' Submit a search request for People
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param emailAddresses vector of type character; required: FALSE; email
#'     address(es) for person(s) to search for; default: &quot;&quot;, when
#'     using multiple values supply as c(&quot;value1&quot;,
#'     &quot;value2&quot;).
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
#' @param firstNames vector of type character; required: FALSE; Person(s) first
#'    name(s) to search for; default: &quot;&quot;, when using multiple values
#'    supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param middleNames vector of type character; required: FALSE; Person(s)
#'     middle name(s) to search for; default: &quot;&quot;, when using multiple
#'     values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param lastNames vector of type character; required: FALSE; Person(s) last
#'     name(s) to search for; default: &quot;&quot;, when using multiple values
#'     supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param mailingAddresses vector of type character; required: FALSE; physical
#'    address(es) of a person(s) to search for; default: &quot;&quot;, when
#'    using multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param personDbIds vector of type character; required: FALSE; Unique
#'    Identifier(s) for person(s) to search for; default: &quot;&quot;, when
#'    using multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param phoneNumbers vector of type character; required: FALSE; phone
#'    number(s) of person(s) to search for; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param userIDs vector of type character; required: FALSE; A systems user
#'    identifier(s) associated with person(s) to search for. Different from
#'    personDbId because you could have a person who is not a user of the
#'    system.; default: &quot;&quot;, when using multiple values supply as
#'    c(&quot;value1&quot;, &quot;value2&quot;).
#' @param page integer; required: FALSE; Which result page is requested. The
#'    page indexing starts at 0 (the first page is `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Advanced searching for the people resource. Function will return
#'    either the search results (Status 200 for an immediate response) or a
#'    `searchResultsDbId` (Status 202 for both a saved and an asynchronous
#'    search).
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/People/post_search_people }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family People
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' # Immediate Response Example
#' brapi_post_search_people(con = con,
#'                          personDbIds = c("program_person_2",
#'                                          "person2"))
#'
#' # Saved or Asynchronous Search Response Example
#' brapi_post_search_people(con = con,
#'                          emailAddresses = "bob@bob.com",
#'                          firstNames = "Bob",
#'                          middleNames = "Danger",
#'                          lastNames = "Robertson",
#'                          personDbIds = c("list_person_1",
#'                                          "person1"))
#' }
#'
#' @export
brapi_post_search_people <- function(con = NULL,
                                     emailAddresses = '',
                                     externalReferenceIDs = '',
                                     externalReferenceSources = '',
                                     firstNames = '',
                                     middleNames = '',
                                     lastNames = '',
                                     mailingAddresses = '',
                                     personDbIds = '',
                                     phoneNumbers = '',
                                     userIDs = '',
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
                                           callPath = "/search/people",
                                           reqArgs = "",
                                           packageName = "BrAPI-Core",
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
      message(paste0("Use the GET /search/people/{searchResultsDbId} call to retrieve the paginated output.", "\n"))
    } else {
      stop(paste0("The POST /search/people call resulted in Server status, ", httr::http_status(resp)[["message"]]))
    }
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_search_people")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
