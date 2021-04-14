#' @title
#' get /studies
#'
#' @description
#' Get a filtered list of Studies
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param commonCropName character; required: FALSE; Common name for the crop
#'    associated with this study
#' @param studyType character; required: FALSE; Filter based on study type
#'    unique identifier
#' @param programDbId character; required: FALSE; Program filter to only return
#'    studies associated with given program id.
#' @param locationDbId character; required: FALSE; Filter by location
#' @param seasonDbId character; required: FALSE; Filter by season or year
#' @param trialDbId character; required: FALSE; Filter by trial
#' @param studyDbId character; required: FALSE; Filter by study DbId
#' @param studyName character; required: FALSE; Filter by study name
#' @param studyCode character; required: FALSE; Filter by study code
#' @param studyPUI character; required: FALSE; Filter by study PUI
#' @param germplasmDbId character; required: FALSE; Filter by germplasm DbId
#' @param observationVariableDbId character; required: FALSE; Filter by
#'    observation variable DbId
#' @param active logical; required: FALSE; Filter active status true/false.
#'    Default: NA, other possible values: TRUE | FALSE
#' @param sortBy character; required: FALSE; Name of the field to sort by.
#' @param sortOrder character; required: FALSE; Sort order direction.
#'    Ascending/Descending.; default: &quot;&quot;, other possible values:
#'    &quot;asc&quot;|&quot;ASC&quot;|&quot;desc&quot;|&quot;DESC&quot;
#' @param externalReferenceID character; required: FALSE; An external reference
#'    ID. Could be a simple string or a URI. (use with `externalReferenceSource`
#'    parameter)
#' @param externalReferenceSource character; required: FALSE; An identifier for
#'    the source system or database of an external reference (use with
#'    `externalReferenceID` parameter)
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is 'page'= 0
#'    ). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details Get list of studies, where StartDate and endDate should be ISO-8601
#'    format for dates.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Studies/get_studies }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Studies
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_studies(con = con)
#' }
#'
#' @export
brapi_get_studies <- function(con = NULL,
                              commonCropName = '',
                              studyType = '',
                              programDbId = '',
                              locationDbId = '',
                              seasonDbId = '',
                              trialDbId = '',
                              studyDbId = '',
                              studyName = '',
                              studyCode = '',
                              studyPUI = '',
                              germplasmDbId = '',
                              observationVariableDbId = '',
                              active = NA,
                              sortBy = '',
                              sortOrder = '',
                              externalReferenceID = '',
                              externalReferenceSource = '',
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
                                        callPath = "/studies",
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
  class(out) <- c(class(out), "brapi_get_studies")
  ## Show pagination information from metadata
  brapir:::brapi_serverinfo_metadata(cont)
  return(out)
}
