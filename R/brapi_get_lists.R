#' @title
#' get /lists
#'
#' @description
#' Get filtered set of generic lists
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param listType character; required: FALSE; The type of objects contained by
#'    this generic list; default: &quot;&quot;, other possible values: &quot;
#'    germplasm&quot;|&quot;markers&quot;|&quot;observations&quot;|&quot;
#'    observationUnits&quot;|&quot;observationVariables&quot;|&quot;programs
#'    &quot;|&quot;samples&quot;|&quot;studies&quot;|&quot;trials&quot;
#' @param listName character; required: FALSE; The human readable name of this
#'    generic list
#' @param listDbId character; required: FALSE; The unique ID of this generic list.
#' @param listSource character; required: FALSE; The source tag of this generic
#'    list
#' @param externalReferenceID character; required: FALSE; An external reference
#'    ID. Could be a simple string or a URI. (use with `externalReferenceSource`
#'    parameter)
#' @param externalReferenceSource character; required: FALSE; An identifier for
#'    the source system or database of an external reference (use with
#'    `externalReferenceID` parameter)
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is 'page'= 0).
#'    Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be returned.
#'    Default is `1000`.
#'
#' @details Get filtered set of generic lists
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Lists/get_lists}{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Lists
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_lists(con = con)
#' }
#'
#' @export
brapi_get_lists <- function(con = NULL,
                            listType = '',
                            listName = '',
                            listDbId = '',
                            listSource = '',
                            externalReferenceID = '',
                            externalReferenceSource = '',
                            page = 0,
                            pageSize = 1000) {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                        callPath = "/lists",
                                        reqArgs = "",
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
  class(out) <- c(class(out), "brapi_get_lists")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
