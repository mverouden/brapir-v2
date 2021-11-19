#' @title
#' get /variantsets/\{variantSetDbId\}
#'
#' @description
#' Gets a `variantSet` by unique database identifier.
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param variantSetDbId character; required: TRUE; The unique database
#'    identifier of the `variantSet` to be retrieved.
#'
#' @details This call will return a JSON version of a `VariantSet`.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Variant%20Sets/get_variantsets__variantSetDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Variant Sets
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_variantsets_variantSetDbId(con = con,
#'                                      variantSetDbId = "variantset1")
#' }
#'
#' @export
brapi_get_variantsets_variantSetDbId <- function(con = NULL,
                                                 variantSetDbId = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "variantSetDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/variantsets/{variantSetDbId}",
                                          reqArgs = "variantSetDbId",
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
  class(out) <- c(class(out), "brapi_get_variantsets_variantSetDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
