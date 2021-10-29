#' @title
#' get /callsets/\{callSetDbId\}
#'
#' @description
#' Gets a `CallSet` by ID.
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param callSetDbId character; required: TRUE; The unique database identifier
#'    of the `CallSet` to be retrieved.
#'
#' @details Gets a `CallSet` by database identifier.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Call%20Sets/get_callsets__callSetDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Call Sets
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_callsets_callSetDbId(con = con,
#'                                callSetDbId = "callset01")
#' }
#'
#' @export
brapi_get_callsets_callSetDbId <- function(con = NULL,
                                           callSetDbId = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "callSetDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/callsets/{callSetDbId}",
                                          reqArgs = "callSetDbId",
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
  class(out) <- c(class(out), "brapi_get_callsets_callSetDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
