#' @title
#' get /markerpositions
#'
#' @description
#' Get marker position info
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param mapDbId character; required: FALSE; unique database identifier of a
#'    map.
#' @param linkageGroupName character; required: FALSE; The chromosome identifier
#'    or the generic linkage group identifier if the chromosome is not
#'    applicable.
#' @param variantDbId character; required: FALSE; The unique database identifier
#'    for a marker.
#' @param minPosition integer; required: FALSE; The minimum position.
#' @param maxPosition integer; required: FALSE; The maximum position.
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Get marker position information, based on Map, Linkage Group, and
#'    Marker Identifier.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Genome%20Maps/get_markerpositions }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Genome Maps
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_markerpositions(con = con)
#' }
#'
#' @export
brapi_get_markerpositions <- function(con = NULL,
                                      mapDbId = '',
                                      linkageGroupName = '',
                                      variantDbId = '',
                                      minPosition = as.integer(NA),
                                      maxPosition = as.integer(NA),
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
                                          callPath = "/markerpositions",
                                          reqArgs = "",
                                          packageName = "BrAPI-Genotyping",
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
  class(out) <- c(class(out), "brapi_get_markerpositions")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
