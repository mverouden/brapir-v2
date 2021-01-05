#' @title
#' get /locations
#'
#' @description
#' Get a filtered list of Locations
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param locationType character; required: FALSE; Filter by location type specified.
#' @param locationDbId character; required: FALSE; Filter by location DbId
#' @param externalReferenceID character; required: FALSE; An external reference ID. Could be a simple string or a URI. (use with `externalReferenceSource` parameter)
#' @param externalReferenceSource character; required: FALSE; An identifier for the source system or database of an external reference (use with `externalReferenceID` parameter)
#' @param page integer; required: FALSE; Used to request a specific page of data to be returned. The page indexing starts at 0 (the first page is 'page'= 0). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be returned. Default is `1000`.
#'
#' @details Get a list of locations.
#' * The `countryCode` is as per [ISO_3166-1_alpha-3](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) spec.
#' * coordinates are specified as longitude, latitude (in this specific order).
#' * `altitude` (optional third parameter to coordinates) is in meters.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Locations/get_locations}{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Locations
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_locations(con = con)
#' }
#'
#' @export
brapi_get_locations <- function(con = NULL, locationType = '', locationDbId = '', externalReferenceID = '', externalReferenceSource = '', page = 0, pageSize = 1000) {
  ## Create a list of used arguments
  usedArgs <- brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapi_GET_callURL(usedArgs = usedArgs,
                               callPath = "/locations",
                               reqArgs = "",
                               packageName = "BrAPI-Core",
                               callVersion = 2.0)

  try({
    ## Make the call and receive the response
    resp <- brapi_GET(url = callurl, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_get_locations")
  ## Show pagination information from metadata
  brapi_serverinfo_metadata(cont)
  return(out)
}
