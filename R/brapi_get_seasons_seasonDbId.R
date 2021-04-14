#' @title
#' get /seasons/\{seasonDbId\}
#'
#' @description
#' Get the a single Season
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param seasonDbId character; required: TRUE; The unique identifier for a
#'    season. For backward compatibility it can be a string like '2012',
#'    '1957-2004'
#'
#' @details Get the a single Season
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Seasons/get_seasons__seasonDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Seasons
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_seasons_seasonDbId(con = con,
#'                              seasonDbId = "winter_2012")
#' }
#'
#' @export
brapi_get_seasons_seasonDbId <- function(con = NULL,
                                         seasonDbId = '') {
  ## Create a list of used arguments
  usedArgs <- brapir:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapir:::brapi_checkArgs(usedArgs, reqArgs = "seasonDbId")
  ## Obtain the call url
  callurl <- brapir:::brapi_GET_callURL(usedArgs = usedArgs,
                                        callPath = "/seasons/{seasonDbId}",
                                        reqArgs = "seasonDbId",
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
  class(out) <- c(class(out), "brapi_get_seasons_seasonDbId")
  ## Show pagination information from metadata
  brapir:::brapi_serverinfo_metadata(cont)
  return(out)
}
