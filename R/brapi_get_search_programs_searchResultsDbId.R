#' @title
#' get /search/programs/\{searchResultsDbId\}
#'
#' @description
#' Get the results of a Programs search request
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
#' @details Returns the result of the advanced searching for the programs
#'    resource.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Programs/get_search_programs__searchResultsDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Programs
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' # Saved or Asynchronous Search Response Example
#' out <-
#'  brapi_post_search_programs(con = con,
#'                             commonCropNames = c("Tomatillo",
#'                                                 "Paw Paw"),
#'                             abbreviations = c("P1", "P3"),
#'                             leadPersonNames = "Bob Robertson",
#'                             objectives = c("Global Population Improvement",
#'                                            "XYZ"),
#'                             programDbIds = c("program1", "program3"))
#' searchResultsDbId <- out$searchResultsDbId
#' brapi_get_search_programs_searchResultsDbId(con = con,
#'                                             searchResultsDbId = searchResultsDbId)
#' }
#'
#' @export
brapi_get_search_programs_searchResultsDbId <- function(con = NULL,
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
                                        callPath = "/search/programs/{searchResultsDbId}",
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
  class(out) <- c(class(out), "brapi_get_search_programs_searchResultsDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
