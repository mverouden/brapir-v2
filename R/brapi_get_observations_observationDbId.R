#' @title
#' get /observations/\{observationDbId\}
#'
#' @description
#' Get the details of a specific Observations
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param observationDbId character; required: TRUE; Unique observation
#'    identifier.
#'
#' @details Get the details of a specific Observations
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Observations/get_observations__observationDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Observations
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_observations_observationDbId(con = con,
#'                                        observationDbId = "observation1")
#' }
#'
#' @export
brapi_get_observations_observationDbId <- function(con = NULL,
                                                   observationDbId = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "observationDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/observations/{observationDbId}",
                                          reqArgs = "observationDbId",
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
  class(out) <- c(class(out), "brapi_get_observations_observationDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
