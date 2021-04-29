#' @title
#' put /seasons/\{seasonDbId\}
#'
#' @description
#' Update existing Seasons
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param seasonDbId character; required: TRUE; The unique identifier for a
#'    season. For backward compatibility it can be a string like '2012',
#'    '1957-2004'
#' @param seasonName character; required: FALSE; Name of the season. ex.
#'    'Spring', 'Q2', 'Season A', *etc.*
#' @param year integer; required: FALSE; The 4 digit year of the season.
#'
#' @details Update existing Seasons
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Seasons/put_seasons__seasonDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Seasons
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' seasonDbId <- "summer_2022"
#' seasonName <- "spring"
#' year <- 2021
#'
#' out <- brapi_post_seasons(con = con,
#'                           seasonDbId = seasonDbId,
#'                           seasonName = seasonName,
#'                           year = year)
#'
#' seasonName <- "summer"
#' year <- 2022
#'
#' brapi_put_seasons_seasonDbId(con = con,
#'                             seasonDbId = unique(out$seasonDbId),
#'                             seasonName = seasonName,
#'                             year = year)
#' }
#'
#' @export
brapi_put_seasons_seasonDbId <- function(con = NULL,
                                         seasonDbId = '',
                                         seasonName = '',
                                         year = as.integer(NA)) {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "seasonDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_PUT_callURL(usedArgs = usedArgs,
                                          callPath = "/seasons/{seasonDbId}",
                                          reqArgs = "seasonDbId",
                                          packageName = "BrAPI-Core",
                                          callVersion = 2.0)
  ## Build the Body
  callbody <- brapirv2:::brapi_PUT_callBody(usedArgs = usedArgs,
                                            reqArgs = "seasonDbId")

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_PUT(url = callurl, body = callbody, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_put_seasons_seasonDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
