#' @title
#' get /observationunits
#'
#' @description
#' Get a filtered set of Observation Units
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param observationUnitDbId character; required: FALSE; The unique observation
#'    unit identifier to filter on.
#' @param germplasmDbId character; required: FALSE; The unique germplasm
#'    (accession) identifier to filter on.
#' @param studyDbId character; required: FALSE; The unique study identifier to
#'    filter on.
#' @param locationDbId character; required: FALSE; The unique location
#'    identifier, where these observations were collected, to filter on.
#' @param trialDbId character; required: FALSE; The unique trial identifier to
#'    filter on.
#' @param programDbId character; required: FALSE; The unique program identifier
#'    to filter on.
#' @param seasonDbId character; required: FALSE; The year or Phenotyping
#'    campaign of a multi-annual study (trees, grape, $\ldots$) to filter on.
#' @param observationUnitLevelName character; required: FALSE; The observation
#'    unit level name. Returns only the observation unit of the specified Level.
#'    References ObservationUnit-&gt;observationUnitPosition
#'    -&gt;observationLevel-&gt;levelName
#' @param observationUnitLevelOrder character; required: FALSE; The observation
#'    unit level order number. Returns only the observation unit of the
#'    specified level. References ObservationUnit-&glt;observationUnitPosition
#'    -&gt;observationLevel-&gt;levelOrder
#' @param observationUnitLevelCode character; required: FALSE; The observation
#'    unit level code. This parameter should be used together with
#'    `observationUnitLevelName` or `observationUnitLevelOrder`. References
#'    ObservationUnit-&gt;observationUnitPosition-&gt;observationLevel
#'    -&gt;levelCode
#' @param includeObservations logical; required: FALSE; Use this parameter to
#'    include a list of observations embedded in each ObservationUnit object.
#'    CAUTION - Use this parameter at your own risk. It may return large,
#'    unpaginated lists of observation data. Only set this value to `TRUE` if
#'    you are sure you need to.
#' @param externalReferenceID character; required: FALSE; An external reference
#'    ID. Could be a simple string or a URI. (use with `externalReferenceSource`
#'    parameter)
#' @param externalReferenceSource character; required: FALSE; An identifier for
#'    the source system or database of an external reference (use with
#'    `externalReferenceID` parameter)
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Get a filtered set of Observation Units
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Observation%20Units/get_observationunits }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Observation Units
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_observationunits(con = con)
#' }
#'
#' @export
brapi_get_observationunits <- function(con = NULL,
                                       observationUnitDbId = '',
                                       germplasmDbId = '',
                                       studyDbId = '',
                                       locationDbId = '',
                                       trialDbId = '',
                                       programDbId = '',
                                       seasonDbId = '',
                                       observationUnitLevelName = '',
                                       observationUnitLevelOrder = '',
                                       observationUnitLevelCode = '',
                                       includeObservations = NA,
                                       externalReferenceID = '',
                                       externalReferenceSource = '',
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
                                          callPath = "/observationunits",
                                          reqArgs = "",
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
  class(out) <- c(class(out), "brapi_get_observationunits")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
