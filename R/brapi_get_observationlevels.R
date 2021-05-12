#' @title
#' get /observationlevels
#'
#' @description
#' Get the Observation Levels
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param studyDbId character; required: FALSE; Filter by an unique study
#'    identifier.
#' @param trialDbId character; required: FALSE; Filter by an unique trial
#'    identifier.
#' @param programDbId character; required: FALSE; Filter by an unique program
#'    identifier.
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Call to retrieve supported observation levels.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Observation%20Units/get_observationlevels }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Observation Units
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_observationlevels(con = con)
#' }
#'
#' @export
brapi_get_observationlevels <- function(con = NULL,
                                        studyDbId = '',
                                        trialDbId = '',
                                        programDbId = '',
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
                                          callPath = "/observationlevels",
                                          reqArgs = "",
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
  class(out) <- c(class(out), "brapi_get_observationlevels")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
