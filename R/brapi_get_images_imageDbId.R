#' @title
#' get /images/\{imageDbId\}
#'
#' @description
#' Get the an image meta data summary
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param imageDbId character; required: TRUE; Unique database identifier for an
#'    image.
#'
#' @details Get one image meta data object.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Images/get_images__imageDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Images
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_images_imageDbId(con = con,
#'                            imageDbId = "image1")
#' }
#'
#' @export
brapi_get_images_imageDbId <- function(con = NULL,
                                       imageDbId = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "imageDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/images/{imageDbId}",
                                          reqArgs = "imageDbId",
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
  class(out) <- c(class(out), "brapi_get_images_imageDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
