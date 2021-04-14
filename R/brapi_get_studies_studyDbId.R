#' @title
#' get /studies/\{studyDbId\}
#'
#' @description
#' Get the details for a specific Study
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param studyDbId character; required: TRUE; Identifier of the study. Usually
#'    a number, could be alphanumeric.
#'
#' @details Retrieve the information of the study required for field data
#'    collection
#'
#' An additionalInfo field was added to provide a controlled vocabulary for less
#'    common data fields.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Studies/get_studies__studyDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Studies
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_studies_studyDbId(con = con,
#'                             studyDbId = "study1")
#' }
#'
#' @export
brapi_get_studies_studyDbId <- function(con = NULL,
                                        studyDbId = '') {
  ## Create a list of used arguments
  usedArgs <- brapir:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapir:::brapi_checkArgs(usedArgs, reqArgs = "studyDbId")
  ## Obtain the call url
  callurl <- brapir:::brapi_GET_callURL(usedArgs = usedArgs,
                                        callPath = "/studies/{studyDbId}",
                                        reqArgs = "studyDbId",
                                        packageName = "BrAPI-Core",
                                        callVersion = 2.0)

  try({
    ## Make the call and receive the response
    resp <- brapir:::brapi_GET(url = callurl, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapir:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_get_studies_studyDbId")
  ## Show pagination information from metadata
  brapir:::brapi_serverinfo_metadata(cont)
  return(out)
}
