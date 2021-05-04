#' @title
#' post /programs
#'
#' @description
#' Add new breeding Programs to the database
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param abbreviation character; required: FALSE; An abbreviation which
#'    represents this program.
#' @param additionalInfo list; required: FALSE; Additional arbitrary information.
#'    If provided use the following construct list(additionalProp1 = "string",
#'    additionalProp2 =  "string", additionalProp3 = "string").
#'
#'    The Examples section shows an example on how to construct the
#'    `additionalInfo` argument as a list.
#' @param commonCropName character; required: FALSE; Common name for the crop,
#'    which this program is for.
#' @param documentationURL character; required: FALSE; A URL to the human
#'    readable documentation of this object.
#' @param externalReferences data.frame; required: FALSE; A data.frame of
#'    external reference ids. These are references to this piece of data in an
#'    external system. Could be a simple string or a URI. The `externalReferences`
#'    argument data.frame should contain the following columns:
#'
#'    * `referenceID` character; required: TRUE; The external reference ID. Could
#'      be a simple string or a URI.
#'    * `referenceSource` character; required: TRUE; An identifier for the source
#'      system or database of this reference.
#'
#'    The Examples section shows an example of how to construct the
#'    `externalReferences` argument as a data.frame.
#' @param leadPersonDbId character; required: FALSE; The unique identifier of
#'    the program leader. Needs to be an existing `personDbId`.
#' @param leadPersonName character; required: FALSE; The name of the program
#'    leader.
#' @param objective character; required: FALSE; The primary objective of the
#'    program.
#' @param programName character; required: FALSE; Human readable name of the
#'    program.
#'
#' @details Add new breeding Programs to the database. The `programDbId` is set
#'    by the server, all other fields are take from the request body,
#'    or a default value is used.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Programs/post_programs }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Programs
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' abbreviation <- "P1"
#' additionalInfo <- list(additionalProp1 = "string",
#'                                 additionalProp2 = "string",
#'                                 additionalProp3 = "string")
#' commonCropName <- "Tomatillo"
#' documentationURL <- "https://wiki.brapi.org"
#' externalReferences <-
#'   data.frame(referenceID = c("doi:10.155454/12341234",
#'                              "http://purl.obolibrary.org/obo/ro.owl",
#'                              "75a50e76"),
#'              referenceSource = c("DOI",
#'                                  "OBO Library",
#'                                  "Remote Data Collection Upload Tool"))
#' leadPersonDbId <- "program_person_1"
#' leadPersonName <- "Bob Robertson"
#' objective <- "Make a better tomatillo"
#' programName <- "Tomatillo_Breeding_Program"
#'
#' brapi_post_programs(con = con,
#'                     abbreviation = abbreviation,
#'                     additionalInfo = additionalInfo,
#'                     commonCropName = commonCropName,
#'                     documentationURL = documentationURL,
#'                     externalReferences = externalReferences,
#'                     leadPersonDbId = leadPersonDbId,
#'                     leadPersonName = leadPersonName,
#'                     objective = objective,
#'                     programName = programName)
#' }
#'
#' @export
brapi_post_programs <- function(con = NULL,
                                abbreviation = '',
                                additionalInfo = list(),
                                commonCropName = '',
                                documentationURL = '',
                                externalReferences = '',
                                leadPersonDbId = '',
                                leadPersonName = '',
                                objective = '',
                                programName = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                         callPath = "/programs",
                                         reqArgs = "",
                                         packageName = "BrAPI-Core",
                                         callVersion = 2.0)
  ## Build the Body
  callbody <- brapirv2:::brapi_POST_callBody(usedArgs = usedArgs,
                                             reqArgs = "")
  ## Adaptation for v2.0 where json body is wrapped in []
  callbody <- list(callbody)

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_POST(url = callurl, body = callbody, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_programs")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
