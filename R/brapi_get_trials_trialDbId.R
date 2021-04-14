#' @title
#' get /trials/\{trialDbId\}
#'
#' @description
#' Get the details of a specific Trial
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param trialDbId character; required: TRUE; The internal trialDbId
#'
#' @details Get the details of a specific Trial
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Trials/get_trials__trialDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Trials
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_trials_trialDbId(con = con, trialDbId = "trial3")
#' }
#'
#' @export
brapi_get_trials_trialDbId <- function(con = NULL,
                                       trialDbId = '') {
  ## Create a list of used arguments
  usedArgs <- brapir:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapir:::brapi_checkArgs(usedArgs, reqArgs = "trialDbId")
  ## Obtain the call url
  callurl <- brapir:::brapi_GET_callURL(usedArgs = usedArgs,
                                        callPath = "/trials/{trialDbId}",
                                        reqArgs = "trialDbId",
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
  class(out) <- c(class(out), "brapi_get_trials_trialDbId")
  ## Show pagination information from metadata
  brapir:::brapi_serverinfo_metadata(cont)
  return(out)
}
