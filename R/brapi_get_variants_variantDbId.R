#' @title
#' get /variants/\{variantDbId\}
#'
#' @description
#' Gets a `Variant` by unique database identifier.
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param variantDbId character; required: TRUE; The unique database identifier
#'    of the `Variant` to be retrieved.
#'
#' @details Returns a data.frame version of `Variant` as received from the
#'    server.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Variants/get_variants__variantDbId_ }{BrAPI SwaggerHub}
#'

#' @family brapi-genotyping
#' @family Variants
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_variants_variantDbId(con = con,
#'                                variantDbId = "variant01")
#' }
#'
#' @export
brapi_get_variants_variantDbId <- function(con = NULL,
                                           variantDbId = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "variantDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/variants/{variantDbId}",
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
  class(out) <- c(class(out), "brapi_get_variants_variantDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
