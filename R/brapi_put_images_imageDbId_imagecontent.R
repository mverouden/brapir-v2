#' @title
#' put /images/\{imageDbId\}/imagecontent
#'
#' @description
#' Update an image with the image file content
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param imageDbId character; required: TRUE; The unique identifier for a image
#' @param imageFileContent character; required: TRUE; The absolute location of
#'    the image file content to be uploaded to the server.
#'
#' @details Update an image with the image file content. This function should be
#'    paired with the `POST /images` call, as implemented in the
#'    `brapi_post_images()` function, for full capability.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Images/put_images__imageDbId__imagecontent }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Images
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#'
#' ## Create function argument values for brapi_post_images()
#' additionalInfo <- list(dummyData = "TRUE",
#'                        example = "post_images")
#' copyright <- "Copyright 2021 Bob Robertson"
#' description <- "This is a picture"
#' descriptiveOntologyTerms <- c("doi:10.1002/0470841559",
#'                               "Red",
#'                               "ncbi:0300294")
#' externalReferences <-
#'   data.frame(referenceID = c("doi:10.155454/12341234",
#'                              "http://purl.obolibrary.org/obo/ro.owl",
#'                              "75a50e76"),
#'              referenceSource = c("DOI",
#'                                  "OBO Library",
#'                                  "Remote Data Collection Upload Tool"))
#' imageFileName <- "FAIR_data.jpg"
#' imageFileSize <- 39268
#' imageHeight <- 407
#'
#' ## Create the imageLocation argument
#' ## Load geojsonR package
#' library(geojsonR)
#' ## Create a imageLocation list object
#' ## Point geometry
#' init <- TO_GeoJson$new()
#' imageLocation <- list()
#' pointData <- c( 5.663288, # longitude
#'                51.988720, # lattitude
#'                      0)   # altitude
#' imageLocation[["geometry"]] <- init$Point(data = pointData,
#'                                           stringify = FALSE)
#' imageLocation[["type"]] <- "Feature"
#'
#' imageName <- "FAIR Data"
#' imageTimeStamp <- "2021-10-11T14:11:28.672Z"
#' imageURL <- "https://wiki.brapi.org/images/tomato"
#' imageWidth <- 1200
#' mimeType <- "image/jpg"
#' observationDbIds <- c("observation1",
#'                       "observation4")
#' observationUnitDbId <- "observation_unit1"
#'
#' ## Add new image meta data
#' out <- brapi_post_images(
#'   con = con,
#'   additionalInfo = additionalInfo,
#'   copyright = copyright,
#'   description = description,
#'   descriptiveOntologyTerms = descriptiveOntologyTerms,
#'   externalReferences = externalReferences,
#'   imageFileName = imageFileName,
#'   imageFileSize = imageFileSize,
#'   imageHeight = imageHeight,
#'   imageLocation = imageLocation,
#'   imageName = imageName,
#'   imageTimeStamp = imageTimeStamp,
#'   imageURL = imageURL,
#'   imageWidth = imageWidth,
#'   mimeType = mimeType,
#'   observationDbIds = observationDbIds,
#'   observationUnitDbId = observationUnitDbId)
#'
#' ## Obtain the imageDbId
#' imageDbId <- unique(out$imageDbId)
#'
#' ## Retrieve information about the new image
#' brapi_get_images_imageDbId(con = con, imageDbId = imageDbId)
#'
#' ## Update an image with the image file content
#' imageFileContent <- system.file("extdata",
#'                                 "FAIR_data.jpg",
#'                                 package = "brapirv2")
#' brapi_put_images_imageDbId_imagecontent(con = con,
#'                                         imageDbId = imageDbId,
#'                                         imageFileContent = imageFileContent)
#'
#' ## Check the changes
#' brapi_get_images_imageDbId(con = con, imageDbId = imageDbId)
#' }
#'
#' @export
brapi_put_images_imageDbId_imagecontent <- function(con = NULL,
                                                    imageDbId = '',
                                                    imageFileContent = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "imageDbId, imageFileContent")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_PUT_callURL(usedArgs = usedArgs,
                                          callPath = "/images/{imageDbId}/imagecontent",
                                          reqArgs = "imageDbId",
                                          packageName = "BrAPI-Phenotyping",
                                          callVersion = 2.0)

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_PUT(url = callurl,
                                 body = httr::upload_file(path = usedArgs[["imageFileContent"]],
                                                          type = mime::guess_type(file = usedArgs[["imageFileContent"]])),
                                 usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_put_images_imageDbId_imagecontent")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
