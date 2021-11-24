#' @title
#' get /references/\{referenceDbId\}/bases
#'
#' @description
#' Lists `Reference` bases by database identifier and optional range.
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param referenceDbId character; required: TRUE; The unique identifier of the
#'    `Reference` to be retrieved.
#' @param start integer; required: FALSE; The start position (0-based) of this
#'    query. Defaults to 0. Genomic positions are non-negative integers less
#'    than reference length. Requests spanning the join of circular genomes are
#'    represented as two requests one on each side of the join (position 0).
#' @param end integer; required: FALSE; The end position (0-based, exclusive) of
#'    this query. Defaults to the length of this `Reference`.
#' @param pageToken character; required: FALSE; Used to request a specific page
#'    of data to be returned. Tokenized pages are for large data sets, which can
#'    not be efficiently broken into indexed pages. Use the `nextPageToken` and
#'    `prevPageToken` from a prior response to construct a query and move to the
#'     next or previous page respectively.
#'
#' @details Lists `Reference` bases by ID and optional range.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/References/get_references__referenceDbId__bases }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family References
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_references_referenceDbId_bases(con = con,
#'                                          referenceDbId = "reference1")
#' }
#'
#' @export
brapi_get_references_referenceDbId_bases <- function(con = NULL,
                                                     referenceDbId = '',
                                                     start = as.integer(NA),
                                                     end = as.integer(NA),
                                                     pageToken = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "referenceDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/references/{referenceDbId}/bases",
                                          reqArgs = "referenceDbId",
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
  class(out) <- c(class(out), "brapi_get_references_referenceDbId_bases")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
