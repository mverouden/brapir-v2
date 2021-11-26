#' @title
#' get /referencesets/\{referenceSetDbId\}
#'
#' @description
#' Gets a `ReferenceSet` by ID.
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param referenceSetDbId character; required: TRUE; The unique database
#'    identifier of the `ReferenceSet` to be retrieved.
#'
#' @details Gets a `ReferenceSet` by ID.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Reference%20Sets/get_referencesets__referenceSetDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Reference Sets
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_referencesets_referenceSetDbId(con = con,
#'                                          referenceSetDbId = "reference_set1")
#' }
#'
#' @export
brapi_get_referencesets_referenceSetDbId <- function(con = NULL,
                                                     referenceSetDbId = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "referenceSetDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/referencesets/{referenceSetDbId}",
                                          reqArgs = "referenceSetDbId",
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
  class(out) <- c(class(out), "brapi_get_referencesets_referenceSetDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
