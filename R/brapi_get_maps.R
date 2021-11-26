#' @title
#' get /maps
#'
#' @description
#' Get the Genomic Maps
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param commonCropName character; required: FALSE; The common name of the crop.
#' @param mapDbId character; required: FALSE; The unique database identifier for
#'    a genomic map.
#' @param mapPUI character; required: FALSE; The DOI or other permanent
#'    identifier for a genomic map.
#' @param scientificName character; required: FALSE; Full scientific binomial
#'    format name. This includes Genus, Species, and Sub-species.
#' @param type character; required: FALSE; Type of map matches the supplied
#'    string (case-sensitive, exact match) to filter on.
#' @param programDbId character; required: FALSE; Unique database identifier to
#'    filter by program.
#' @param trialDbId character; required: FALSE; Unique database identifier to
#'    filter by trial.
#' @param studyDbId character; required: FALSE; Unique database identifier to
#'    filter by study.
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Get list of maps
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Genome%20Maps/get_maps }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Genome Maps
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_maps(con = con,
#'                type = "Physical Map")
#' }
#'
#' @export
brapi_get_maps <- function(con = NULL,
                           commonCropName = '',
                           mapDbId = '',
                           mapPUI = '',
                           scientificName = '',
                           type = '',
                           programDbId = '',
                           trialDbId = '',
                           studyDbId = '',
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
                                          callPath = "/maps",
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
  class(out) <- c(class(out), "brapi_get_maps")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
