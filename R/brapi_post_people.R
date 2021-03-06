#' @title
#' post /people
#'
#' @description
#' Create new People
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param additionalInfo list; required: FALSE; Additional arbitrary information.
#'    If provided use the following construct list(additionalProp1 = "string",
#'    additionalProp2 =  "string", additionalProp3 = "string").
#'
#'    The Examples section shows an example on how to construct the
#'    `additionalInfo` argument as a list.
#' @param description character; required: FALSE; A description of this person.
#' @param emailAddress character; required: FALSE; An email address for this
#'    person.
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
#' @param firstName character; required: FALSE; Persons first name.
#' @param middleName character; required: FALSE; Persons middle name.
#' @param lastName character; required: FALSE; Persons last name.
#' @param mailingAddress character; required: FALSE; Physical address of this
#'    person.
#' @param phoneNumber character; required: FALSE; phone number of this person.
#' @param userID character; required: FALSE; A systems user ID associated with
#'    this person. Different from `personDbId` because you could have a person,
#'    who is not a user of the system.
#'
#' @details Create new People entities. `personDbId` is generated and managed by the server.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/People/post_people }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family People
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' additionalInfo <- list(additionalProp1 = "string",
#'                        additionalProp2 = "string",
#'                        additionalProp3 = "string")
#' description <- "Bob likes pina coladas and getting caught in the rain."
#' emailAddress <- "bob@bob.com"
#' externalReferences <-
#'   data.frame(referenceID = c("doi:10.155454/12341234",
#'                              "http://purl.obolibrary.org/obo/ro.owl",
#'                              "75a50e76"),
#'              referenceSource = c("DOI",
#'                                  "OBO Library",
#'                                  "Remote Data Collection Upload Tool"))
#' firstName <- "Bob"
#' middleName <- "Danger"
#' lastName <- "Robertson"
#' mailingAddress <- "123 Street Ave, City, State, Country"
#' phoneNumber <- "+1-555-555-5555"
#' userID <- "bob-23"
#'
#' brapi_post_people(con = con,
#'                   additionalInfo = additionalInfo,
#'                   description = description,
#'                   emailAddress = emailAddress,
#'                   externalReferences = externalReferences,
#'                   firstName = firstName,
#'                   middleName = middleName,
#'                   lastName = lastName,
#'                   mailingAddress = mailingAddress,
#'                   phoneNumber = phoneNumber,
#'                   userID = userID)
#' }
#'
#' @export
brapi_post_people <- function(con = NULL,
                              additionalInfo = list(),
                              description = '',
                              emailAddress = '',
                              externalReferences = '',
                              firstName = '',
                              middleName = '',
                              lastName = '',
                              mailingAddress = '',
                              phoneNumber = '',
                              userID = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                           callPath = "/people",
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
  class(out) <- c(class(out), "brapi_post_people")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
