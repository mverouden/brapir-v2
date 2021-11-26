#' @title
#' get /referencesets
#'
#' @description
#' Gets a list of `ReferenceSets`.
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param referenceSetDbId character; required: FALSE; The unique database
#'    identifier of the `ReferenceSet` to be retrieved.
#' @param accession character; required: FALSE; If set, return the reference
#'    sets for which the `accession` matches this string (case-sensitive,
#'    exact match).
#' @param assemblyPUI character; required: FALSE; If set, return the reference
#'    sets for which the `assemblyId` matches this string (case-sensitive, exact
#'    match).
#' @param md5checksum character; required: FALSE; If set, return the reference
#'    sets for which the `md5checksum` matches this string (case-sensitive,
#'    exact match).
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Gets a filtered list of `ReferenceSets`.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Reference%20Sets/get_referencesets }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Reference Sets
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_referencesets(con = con)
#' }
#'
#' @export
brapi_get_referencesets <- function(con = NULL,
                                    referenceSetDbId = '',
                                    accession = '',
                                    assemblyPUI = '',
                                    md5checksum = '',
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
                                          callPath = "/referencesets",
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
  class(out) <- c(class(out), "brapi_get_referencesets")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
