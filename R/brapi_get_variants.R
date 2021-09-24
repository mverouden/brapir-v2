#' @title
#' get /variants
#'
#' @description
#' Gets a filtered list of `Variants`.
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param variantDbId character; required: FALSE; The unique database identifier
#'    of the `Variant` to be retrieved.
#' @param variantSetDbId character; required: FALSE; The unique database
#'    identifier of the `VariantSet` to be retrieved.
#' @param pageToken character; required: FALSE; Used to request a specific page
#'    of data to be returned. Tokenized pages are for large data sets, which can
#'    not be efficiently broken into indexed pages. Use the `nextPageToken` and
#'    `prevPageToken` from a prior response to construct a query and move to the
#'     next or previous page respectively.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Gets a filtered list of `Variants`. **THIS ENDPOINT USES TOKEN BASED
#'  PAGING!**
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Variants/get_variants }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Variants
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_variants(con = con)
#' }
#'
#' @export
brapi_get_variants <- function(con = NULL,
                               variantDbId = '',
                               variantSetDbId = '',
                               pageToken = '',
                               pageSize = 1000) {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/variants",
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
  class(out) <- c(class(out), "brapi_get_variants")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
