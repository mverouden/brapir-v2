#' @title
#' get /search/attributevalues/\{searchResultsDbId\}
#'
#' @description
#' Get the results of a Germplasm `AttributeValues` search request
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
#' @details Get the results of a Germplasm `AttributeValues` search request.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Germplasm/2.0#/Germplasm%20Attribute%20Values/get_search_attributevalues__searchResultsDbId_}{BrAPI SwaggerHub}
#'
#' @family brapi-germplasm
#' @family Germplasm Attribute Values
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' # Saved or Asynchronous Search Response Example
#' out <-
#'   brapi_post_search_attributevalues(
#'       con = con,
#'       attributeDbIds = c("attribute1",
#'                          "attribute2"),
#'       attributeNames = "Rht-B1b",
#'       attributeValueDbIds = "attribute_val1",
#'       externalReferenceIDs = "https://brapi.org/specification",
#'       externalReferenceSources = "BrAPI Doc",
#'       page = 0,
#'       pageSize = 1000)
#' searchResultsDbId <- out$searchResultsDbId
#' brapi_get_search_attributevalues_searchResultsDbId(con = con, searchResultsDbId = searchResultsDbId)
#' }
#'
#' @export
brapi_get_search_attributevalues_searchResultsDbId <- function(con = NULL,
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
                                          callPath = "/search/attributevalues/{searchResultsDbId}",
                                          reqArgs = "searchResultsDbId",
                                          packageName = "BrAPI-Germplasm",
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
  class(out) <- c(class(out), "brapi_get_search_attributevalues_searchResultsDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
