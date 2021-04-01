#' @title
#' get /people
#'
#' @description
#' Get filtered list of People
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param firstName character; required: FALSE; A persons first name
#' @param lastName character; required: FALSE; A persons last name
#' @param personDbId character; required: FALSE; The unique ID of a person
#' @param userID character; required: FALSE; A systems user ID associated with this person. Different from personDbId because you could have a person who is not a user of the system.
#' @param externalReferenceID character; required: FALSE; An external reference ID. Could be a simple string or a URI. (use with `externalReferenceSource` parameter)
#' @param externalReferenceSource character; required: FALSE; An identifier for the source system or database of an external reference (use with `externalReferenceID` parameter)
#' @param page integer; required: FALSE; Used to request a specific page of data to be returned. The page indexing starts at 0 (the first page is 'page'= 0). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be returned. Default is `1000`.
#'
#' @details Get filtered list of people
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/People/get_people }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family People
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_people(con = con)
#' }
#'
#' @export
brapi_get_people <- function(con = NULL, firstName = '', lastName = '', personDbId = '', userID = '', externalReferenceID = '', externalReferenceSource = '', page = 0, pageSize = 1000) {
  ## Create a list of used arguments
  usedArgs <- brapir:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapir:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapir:::brapi_GET_callURL(usedArgs = usedArgs,
                                        callPath = "/people",
                                        reqArgs = "",
                                        packageName = "BrAPI-Core",
                                        callVersion = 2.0)

  try({
    ## Make the call and receive the response
    resp <- brapir:::brapi_GET(url = callurl, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapir:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_get_people")
  ## Show pagination information from metadata
  brapir:::brapi_serverinfo_metadata(cont)
  return(out)
}
