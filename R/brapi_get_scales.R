#' @title
#' get /scales
#'
#' @description
#' Get the Scales
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param scaleDbId character; required: FALSE; The unique identifier for a
#'    scale.
#' @param observationVariableDbId character; required: FALSE; The unique
#'    identifier for an observation variable.
#' @param externalReferenceID character; required: FALSE; An external reference
#'    ID. Could be a simple string or a URI. (use with `externalReferenceSource`
#'    parameter)
#' @param externalReferenceSource character; required: FALSE; An identifier for
#'    the source system or database of an external reference (use with
#'    `externalReferenceID` parameter)
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Returns a list of Scales available on a server.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Scales/get_scales }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Scales
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_scales(con = con)
#' }
#'
#' @export
brapi_get_scales <- function(con = NULL,
                             scaleDbId = '',
                             observationVariableDbId = '',
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
                                          callPath = "/scales",
                                          reqArgs = "",
                                          packageName = "BrAPI-Phenotyping",
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
  class(out) <- c(class(out), "brapi_get_scales")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
