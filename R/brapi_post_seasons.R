#' @title
#' post /seasons
#'
#' @description
#' POST new Seasons
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param seasonDbId character; required: FALSE; The Id which uniquely
#'    identifies a season. For backward compatibility it can be a string like
#'    '2012', '1957-2004'.
#' @param seasonName character; required: FALSE; Name of the season. ex.
#'    'Spring', 'Q2', 'Season A', *etc.*
#' @param year integer; required: FALSE; The 4 digit year of the season.
#'
#' @details Add new season entries to the database
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Seasons/post_seasons }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Seasons
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' seasonDbId <- "Spring_2021"
#' seasonName <- "Spring"
#' year <- 2021
#'
#' brapi_post_seasons(con = con,
#'                    seasonDbId = seasonDbId,
#'                    seasonName = seasonName,
#'                    year = year)
#' }
#'
#' @export
brapi_post_seasons <- function(con = NULL,
                               seasonDbId = '',
                               seasonName = '',
                               year = as.integer(NA)) {
  ## Create a list of used arguments
  usedArgs <- brapir:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapir:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapir:::brapi_POST_callURL(usedArgs = usedArgs,
                                         callPath = "/seasons",
                                         reqArgs = "",
                                         packageName = "BrAPI-Core",
                                         callVersion = 2.0)
  ## Build the Body
  callbody <- brapir:::brapi_POST_callBody(usedArgs = usedArgs,
                                           reqArgs = "")
  ## Adaptation for v2.0 where json body is wrapped in []
  callbody <- list(callbody)

  try({
    ## Make the call and receive the response
    resp <- brapir:::brapi_POST(url = callurl, body = callbody, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapir:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_seasons")
  ## Show pagination information from metadata
  brapir:::brapi_serverinfo_metadata(cont)
  return(out)
}
