#' @title
#' get /search/studies/\{searchResultsDbId\}
#'
#' @description
#' Get the results of a Studies search request
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
#' @details Returns the result of the advanced searching for the studies
#'    resource.
#'
#' See [Search Services for additional implementation details](https://github.com/plantbreeding/API/blob/master/Specification/GeneralInfo/Search_Services.md).
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Studies/get_search_studies__searchResultsDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Studies
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_search_studies_searchResultsDbId(con = con,
#'                                            searchResultsDbId = "")
#' }
#'
#' @export
brapi_get_search_studies_searchResultsDbId <- function(con = NULL,
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
                                        callPath = "/search/studies/{searchResultsDbId}",
                                        reqArgs = "searchResultsDbId",
                                        packageName = "BrAPI-Core",
                                        callVersion = 2.0)

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_GET(url = callurl, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_get_search_studies_searchResultsDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
