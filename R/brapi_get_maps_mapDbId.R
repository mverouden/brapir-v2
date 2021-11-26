#' @title
#' get /maps/\{mapDbId\}
#'
#' @description
#' Get the details of a specific Genomic Map
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param mapDbId character; required: TRUE; The internal database identifier of
#'    the selected map.
#'
#' @details Provides the number of markers on each linkageGroup and the max
#'    position on the linkageGroup.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Genome%20Maps/get_maps__mapDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Genome Maps
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_maps_mapDbId(con = con,
#'                        mapDbId = "genome_map1")
#' }
#'
#' @export
brapi_get_maps_mapDbId <- function(con = NULL,
                                   mapDbId = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "mapDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/maps/{mapDbId}",
                                          reqArgs = "mapDbId",
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
  class(out) <- c(class(out), "brapi_get_maps_mapDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
