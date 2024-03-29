#' @title
#' get /search/observations/\{searchResultsDbId\}
#'
#' @description
#' Get the results of a `Observations` search request
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param Accept character; required: TRUE; The requested content type which
#'    should be returned by the server; default: "application/json",
#'    other possible values:
#'
#'    * "text/csv"
#'    * "text/tsv"
#'    * "application/flapjack"
#'
#' @param searchResultsDbId character; required: TRUE; Unique database
#'    identifier which references the search results
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Returns the result of the advanced searching for the
#'    `Observations` resource.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Observations/get_search_observations__searchResultsDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Observations
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' # Saved or Asynchronous Search Response Example
#' out <- brapi_post_search_observations(
#'   con = con,
#'   externalReferenceIDs = "https://brapi.org/specification",
#'   externalReferenceSources = "BrAPI Doc",
#'   germplasmDbIds = c("germplasm1",
#'                      "germplasm2",
#'                      "germplasm3"),
#'   page = 0,
#'   pageSize = 1000)
#' searchResultsDbId <- out$searchResultsDbId
#' brapi_get_search_observations_searchResultsDbId(
#'   con = con,
#'   searchResultsDbId = searchResultsDbId)
#' }
#'
#' @export
brapi_get_search_observations_searchResultsDbId <- function(con = NULL,
                                                            Accept = 'application/json',
                                                            searchResultsDbId = '',
                                                            page = 0,
                                                            pageSize = 1000) {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "Accept, searchResultsDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/search/observations/{searchResultsDbId}",
                                          reqArgs = "Accept, searchResultsDbId",
                                          packageName = "BrAPI-Phenotyping",
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
  class(out) <- c(class(out), "brapi_get_search_observations_searchResultsDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
