#' @title
#' get /programs/\{programDbId\}
#'
#' @description
#' Get a breeding Program by Id
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param programDbId character; required: TRUE; Filter by the common crop name.
#'    Exact match.
#'
#' @details Get a single breeding Program by Id. This can be used to quickly get
#'    the details of a Program when you have the Id from another entity.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Programs/get_programs__programDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Programs
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_programs_programDbId(con = con,
#'                                programDbId = "program1")
#' }
#'
#' @export
brapi_get_programs_programDbId <- function(con = NULL,
                                           programDbId = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "programDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                        callPath = "/programs/{programDbId}",
                                        reqArgs = "programDbId",
                                        packageName = "BrAPI-Core",
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
  class(out) <- c(class(out), "brapi_get_programs_programDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
