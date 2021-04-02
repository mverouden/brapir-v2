#' @title
#' get /serverinfo
#'
#' @description
#' Get the list of implemented Calls
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param dataType character; required: FALSE; The data format supported by the call.; default: &quot;&quot;, other possible values: &quot;application/json&quot;|&quot;text/csv&quot;|&quot;text/tsv&quot;|&quot;application/flapjack&quot;
#'
#' @details Implementation Notes: Having a consistent structure for the path
#'  string of each call is very important for teams to be able to connect and
#'  find errors. Read more on Github.Here are the rules for the path of each
#'  call that should be returned:
#'
#' * Every word in the call path should match the documentation exactly, both in spelling and capitalization. Note that path strings are all lower case, but path parameters are camel case.
#' * Each path should start relative to &quot;/&quot; and therefore should not include &quot;/&quot;
#' * No leading or trailing slashes (&quot;/&quot;)
#' * Path parameters are wrapped in curly braces (&quot;\{\}&quot;). The name of the path parameter should be spelled exactly as it is specified in the documentation.
#'
#' Examples:
#'
#' * GOOD &quot;call&quot;: &quot;germplasm/\{germplasmDbId\}/pedigree&quot;
#' * BAD &quot;call&quot;: &quot;germplasm/\{id\}/pedigree&quot;
#' * BAD &quot;call&quot;: &quot;germplasm/\{germplasmDBid\}/pedigree&quot;
#' * BAD &quot;call&quot;: &quot;brapi/v2/germplasm/\{germplasmDbId\}/pedigree&quot;
#' * BAD &quot;call&quot;: &quot;/germplasm/\{germplasmDbId\}/pedigree/&quot;
#' * BAD &quot;call&quot;: &quot;germplasm/&lt;germplasmDbId&gt;/pedigree&quot;
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Server%20Info/get_serverinfo }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Server Info
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_serverinfo(con = con)
#' }
#'
#' @export
brapi_get_serverinfo <- function(con = NULL, dataType = '') {
  ## Create a list of used arguments
  usedArgs <- brapir:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapir:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapir:::brapi_GET_callURL(usedArgs = usedArgs,
                                        callPath = "/serverinfo",
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
  class(out) <- c(class(out), "brapi_get_serverinfo")
  ## Show pagination information from metadata
  brapir:::brapi_serverinfo_metadata(cont)
  return(out)
}
