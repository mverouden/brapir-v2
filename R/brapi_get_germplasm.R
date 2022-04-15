#' @title
#' get /germplasm
#'
#' @description
#' Get a filtered list of Germplasm
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param germplasmPUI character; required: FALSE; Permanent unique identifier
#'    (DOI, URI, *etc.*).
#' @param germplasmDbId character; required: FALSE; Unique internal germplasm
#'    database identifier to filter by.
#' @param germplasmName character; required: FALSE; Name of the germplasm
#' @param commonCropName character; required: FALSE; The common crop name
#'    related to this germplasm.
#' @param accessionNumber character; required: FALSE; Unique identifiers for
#'    accessions within a genebank.
#' @param collection character; required: FALSE; A specific
#'    panel/collection/population name this germplasm belongs to.
#' @param genus character; required: FALSE; Genus name to identify germplasm.
#' @param species character; required: FALSE; Species name to identify germplasm.
#' @param studyDbId character; required: FALSE; Search for Germplasm that are
#'    associated with a particular unique internal Study database identifier.
#' @param synonym character; required: FALSE; Alternative name or ID used to
#'    reference this germplasm.
#' @param parentDbId character; required: FALSE; Search for Germplasm with
#'    this parent.
#' @param progenyDbId character; required: FALSE; Search for Germplasm with this
#'    child.
#' @param externalReferenceID character; required: FALSE; An external reference
#'    ID. Could be a simple string or a URI. (use with `externalReferenceSource`
#'    parameter)
#' @param externalReferenceSource character; required: FALSE; An identifier for
#'    the source system or database of an external reference (use with
#'    `externalReferenceID` parameter).
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Germplasm/2.0#/Germplasm/get_germplasm }{BrAPI SwaggerHub}
#'
#' @family brapi-germplasm
#' @family Germplasm
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_germplasm(con = con)
#' }
#'
#' @export
brapi_get_germplasm <- function(con = NULL,
                                germplasmPUI = '',
                                germplasmDbId = '',
                                germplasmName = '',
                                commonCropName = '',
                                accessionNumber = '',
                                collection = '',
                                genus = '',
                                species = '',
                                studyDbId = '',
                                synonym = '',
                                parentDbId = '',
                                progenyDbId = '',
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
                                          callPath = "/germplasm",
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
  class(out) <- c(class(out), "brapi_get_germplasm")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
