#' @title
#' get /variants/\{variantDbId\}/calls
#'
#' @description
#' Gets a list of `Calls` associated with a `Variant`.
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param variantDbId character; required: TRUE; The unique database identifier
#'    of the `Variant` to be retrieved.
#' @param expandHomozygotes logical; required: FALSE; Should homozygotes be
#'    expanded (TRUE) or collapsed into a single occurrence (FALSE); default: NA
#'    , other possible values: TRUE | FALSE
#' @param unknownString character; required: FALSE; The string to use as a
#'    representation for missing data.
#' @param sepPhased character; required: FALSE; The string to use as a separator
#'    for phased allele calls.
#' @param sepUnphased character; required: FALSE; The string to use as a
#'    separator for unphased allele calls.
#' @param pageToken character; required: FALSE; Used to request a specific page
#'    of data to be returned. Tokenized pages are for large data sets, which can
#'    not be efficiently broken into indexed pages. Use the `nextPageToken` and
#'    `prevPageToken` from a prior response to construct a query and move to the
#'     next or previous page respectively.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details The variant calls for this particular variant. Each one represents
#'    the determination of genotype with respect to this variant. `Calls` in
#'    this array are implicitly associated with this `Variant`.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Variants/get_variants__variantDbId__calls }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Variants
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_variants_variantDbId_calls(
#'   con = con,
#'   variantDbId = "variant01",
#'   pageSize = 1000)
#' }
#'
#' @export
brapi_get_variants_variantDbId_calls <- function(con = NULL,
                                                 variantDbId = '',
                                                 expandHomozygotes = NA,
                                                 unknownString = '',
                                                 sepPhased = '',
                                                 sepUnphased = '',
                                                 pageToken = '',
                                                 pageSize = 1000) {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  if (exists(usedArgs[["sepPhased"]]) && usedArgs[["sepPhased"]] %in% c("|", "/")) {
    usedArgs[["sepPhased"]] <- paste0("%",
                                      toupper(charToRaw(usedArgs[["sepPhased"]])))
  }
  if (exists(usedArgs[["sepUnphased"]]) && usedArgs[["sepUnphased"]] %in% c("|", "/")) {
    usedArgs[["sepUnphased"]] <- paste0("%",
                                        toupper(charToRaw(usedArgs[["sepUnphased"]])))
  }
  if (exists(usedArgs[["unknownString"]]) && usedArgs[["unknownString"]] %in% c("|", "/")) {
    usedArgs[["unknownString"]] <- paste0("%",
                                          toupper(
                                            charToRaw(
                                              usedArgs[["unknownString"]])))
  }
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "variantDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/variants/{variantDbId}/calls",
                                          reqArgs = "variantDbId",
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
  class(out) <- c(class(out), "brapi_get_variants_variantDbId_calls")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
