#' @title
#' get /samples
#'
#' @description
#' Get the Samples
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param sampleDbId character; required: FALSE; the unique internal database
#'    identifier for a sample
#' @param observationUnitDbId character; required: FALSE; the unique internal
#'    database identifier for an observation unit, where a sample was taken from.
#' @param plateDbId character; required: FALSE; the unique internal database
#'    identifier for a plate of samples.
#' @param germplasmDbId character; required: FALSE; the unique internal database
#'    identifier for a germplasm.
#' @param studyDbId character; required: FALSE; The unique internal database
#'    study identifier to filter by.
#' @param externalReferenceID character; required: FALSE; An external reference
#'    identifier. Could be a simple string or a URI (use with
#'    `externalReferenceSource` parameter).
#' @param externalReferenceSource character; required: FALSE; An identifier for
#'    the source system or database of an external reference (use with
#'    `externalReferenceID` parameter).
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Used to retrieve list of Samples from a Sample Tracking system based
#'    on some search criteria.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Samples/get_samples }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Samples
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_samples(con = con)
#' }
#'
#' @export
brapi_get_samples <- function(con = NULL,
                              sampleDbId = '',
                              observationUnitDbId = '',
                              plateDbId = '',
                              germplasmDbId = '',
                              studyDbId = '',
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
                                          callPath = "/samples",
                                          reqArgs = "",
                                          packageName = "BrAPI-Genotyping",
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
  class(out) <- c(class(out), "brapi_get_samples")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
