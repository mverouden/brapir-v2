#' @title
#' get /attributevalues
#'
#' @description
#' Get the Germplasm Attribute Values
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param attributeValueDbId character; required: FALSE; The unique database
#'    identifier for an attribute value.
#' @param attributeDbId character; required: FALSE; The unique database
#'    identifier for an attribute.
#' @param attributeName character; required: FALSE; The human readable name for
#'    an attribute.
#' @param germplasmDbId character; required: FALSE; Get all attributes
#'    associated with this unique germplasm database identifier.
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
#' @details Get the Germplasm Attribute Values
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Germplasm/2.0#/Germplasm%20Attribute%20Values/get_attributevalues }{BrAPI SwaggerHub}
#'
#' @family brapi-germplasm
#' @family Germplasm Attribute Values
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_attributevalues(con = con)
#' }
#'
#' @export
brapi_get_attributevalues <- function(con = NULL,
                                      attributeValueDbId = '',
                                      attributeDbId = '',
                                      attributeName = '',
                                      germplasmDbId = '',
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
                                          callPath = "/attributevalues",
                                          reqArgs = "",
                                          packageName = "BrAPI-Germplasm",
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
  class(out) <- c(class(out), "brapi_get_attributevalues")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
