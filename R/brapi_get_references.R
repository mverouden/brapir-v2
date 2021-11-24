#' @title
#' get /references
#'
#' @description
#' Gets a filtered list of `Reference` objects.
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param referenceDbId character; required: FALSE; The unique database
#'    identifier of the `Reference` to be retrieved.
#' @param referenceSetDbId character; required: FALSE; The unique database
#'    identifier of the `ReferenceSet` to be retrieved.
#' @param accession character; required: FALSE; If set, return the reference
#'    sets for which the `accession` matches this string (case-sensitive,
#'    exact match).
#' @param md5checksum character; required: FALSE; If specified, return the
#'    references for which the `md5checksum` matches this string (case-sensitive
#'    , exact match).
#' @param isDerived logical; required: FALSE; If the reference is derived (TRUE) from a
#'    source sequence or not (FALSE). If unspecified (NA), both will be
#'    displayed.
#' @param minLength integer; required: FALSE; The minimum length of the
#'    reference sequences to be retrieved.
#' @param maxLength integer; required: FALSE; The maximum length of the
#'    reference sequences to be retrieved.
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details `GET /references` will return a filtered data.frame of `Reference`
#'    objects.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/References/get_references }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family References
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_references(con = con)
#' }
#'
#' @export
brapi_get_references <- function(con = NULL,
                                 referenceDbId = '',
                                 referenceSetDbId = '',
                                 accession = '',
                                 md5checksum = '',
                                 isDerived = NA,
                                 minLength = as.integer(NA),
                                 maxLength = as.integer(NA),
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
                                          callPath = "/references",
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
  class(out) <- c(class(out), "brapi_get_references")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
