#' @title
#' get /seasons
#'
#' @description
#' Get the Seasons
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param seasonDbId character; required: FALSE; The unique identifier for a
#'    season. For backward compatibility it can be a string like '2012',
#'    '1957-2004'
#' @param season character; required: FALSE; The term to describe a given
#'    season. Example &quot;Spring&quot; OR &quot;May&quot; OR &quot;
#'    Planting_Time_7&quot;.
#' @param year integer; required: FALSE; The 4 digit year of a season. Example
#'    &quot;2017&quot;
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    'page'= 0). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Call to retrieve all seasons in the database.
#'
#' A season is made of 2 parts; the primary year and a term which defines
#'    a segment of the year.
#' This could be a traditional season, like &quot;Spring&quot; or &quot;Summer
#'   &quot; or this could be a month, like &quot;May&quot; or &quot;June&quot;
#'   or this could be an arbitrary season name which is meaningful to the
#'   breeding program like &quot;PlantingTime_3&quot; or &quot;Season E&quot;
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Seasons/get_seasons }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Seasons
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_seasons(con = con)
#' }
#'
#' @export
brapi_get_seasons <- function(con = NULL,
                              seasonDbId = '',
                              season = '',
                              year = '',
                              page = 0,
                              pageSize = 1000) {
  ## Create a list of used arguments
  usedArgs <- brapir:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapir:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapir:::brapi_GET_callURL(usedArgs = usedArgs,
                                        callPath = "/seasons",
                                        reqArgs = "",
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
  class(out) <- c(class(out), "brapi_get_seasons")
  ## Show pagination information from metadata
  brapir:::brapi_serverinfo_metadata(cont)
  return(out)
}
