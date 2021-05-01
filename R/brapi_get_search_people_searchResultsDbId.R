#' @title
#' get /search/people/\{searchResultsDbId\}
#'
#' @description
#' Get the results of a People search request
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param searchResultsDbId character; required: TRUE; Unique identifier which
#'    references the search results
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Returns the result of the advanced searching for the people resource.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/People/get_search_people__searchResultsDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family People
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' TO BE ADDED: AN EXAMPLE WITH A SAVED OR ASYNCHRONOUS SEARCH
#' Still to be completed:
#' brapi_get_search_people_searchResultsDbId(con = con,
#'                                           searchResultsDbId = "")
#' }
#'
#' @export
brapi_get_search_people_searchResultsDbId <- function(con = NULL,
                                                      searchResultsDbId = '',
                                                      page = 0,
                                                      pageSize = 1000) {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "searchResultsDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                        callPath = "/search/people/{searchResultsDbId}",
                                        reqArgs = "searchResultsDbId",
                                        packageName = "BrAPI-Core",
                                        callVersion = 2.0)

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_GET(url = callurl, usedArgs = usedArgs)
    ## Check call status
    while (httr::status_code(resp) == 202) {
      Sys.sleep(5)
      resp <- brapirv2:::brapi_GET(url = callurl, usedArgs = usedArgs)
      status <- jsonlite::fromJSON(httr::content(x = resp,
                                                 as = "text",
                                                 encoding = "UTF-8"))[["metadata"]][["status"]]
      if (length(status) != 0) {
        brapirv2:::brapi_message(msg = paste0(status[["message"]], "\n"))
      }
    }
    if (httr::status_code(resp) == 200) {
      ## Extract the content from the response object in human readable form
      cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
      ## Convert the content object into a data.frame
      out <- brapirv2:::brapi_result2df(cont, usedArgs)
    }
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_get_search_people_searchResultsDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
