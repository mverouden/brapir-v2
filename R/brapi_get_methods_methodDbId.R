#' @title
#' get /methods/\{methodDbId\}
#'
#' @description
#' Get the details for a specific Method
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param methodDbId character; required: TRUE; Unique method identifier, of
#'    which to retrieve the details.
#'
#' @details Retrieve details about a specific method
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Methods/get_methods__methodDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Methods
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_methods_methodDbId(con = con,
#'                              methodDbId = "method_variable1")
#' }
#'
#' @export
brapi_get_methods_methodDbId <- function(con = NULL,
                                         methodDbId = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "methodDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/methods/{methodDbId}",
                                          reqArgs = "methodDbId",
                                          packageName = "BrAPI-Phenotyping",
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
  class(out) <- c(class(out), "brapi_get_methods_methodDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
