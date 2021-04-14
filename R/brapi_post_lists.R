#' @title
#' post /lists
#'
#' @description
#' Create New List Objects
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param data vector of type character; required: FALSE; The list of DbIds contained in this list; default: &quot;&quot;, when using multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param additionalInfo list; required: FALSE; Additional arbitrary information. If provided use the following construct list(additionalProp1 = "string", additionalProp2 =  "string", additionalProp3 = "string"). The Examples Section shows an example on how to construct the `additionalInfo` argument as a list.
#' @param dateCreated character; required: FALSE; Timestamp when the entity was first created. Coded in the ISO 8601 standard extended format, where date, time and time zone information needs to be provided (check for example https://www.w3.org/TR/NOTE-datetime).
#' @param dateModified character; required: FALSE; Timestamp when the entity was last updated. Coded in the ISO 8601 standard extended format, where date, time and time zone information needs to be provided (check for example https://www.w3.org/TR/NOTE-datetime).
#' @param externalReferences data.frame; required: FALSE; A data.frame of external reference ids. These are references to this piece of data in an external system. Could be a simple string or a URI. The `externalReferences` argument data.frame should contain the following columns:
#'
#'   * `referenceID` character; required: TRUE; The external reference ID. Could be a simple string or a URI.
#'   * `referenceSource` character; required: TRUE; An identifier for the source system or database of this reference.
#'
#' The Examples Section shows an example of how to construct the `externalReferences` argument as a data.frame.
#' @param listDescription character; required: FALSE; Description of a List.
#' @param listName character; required: FALSE; Human readable name of a List.
#' @param listOwnerName character; required: FALSE; Human readable name of a List Owner (usually a user or person).
#' @param listOwnerPersonDbId character; required: FALSE; The unique identifier for a List Owner (usually a user or person).
#' @param listSize integer; required: FALSE; The number of elements in a List.
#' @param listSource character; required: FALSE; The description of where a List originated from.
#' @param listType character; required: FALSE; default: &quot;&quot;, other possible values: &quot;germplasm&quot;|&quot;markers&quot;|&quot;observations&quot;|&quot;observationUnits&quot;|&quot;observationVariables&quot;|&quot;programs&quot;|&quot;samples&quot;|&quot;studies&quot;|&quot;trials&quot;
#'
#' @details Create new list objects in the database
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI/2.0#/Lists/post_lists }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Lists
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' data <- c("germ1", "germ2", "germ3")
#' additionalInfo <- list(dummyData = "True", example = "post_lists")
#' dateCreated <- "2021-04-12T09:01:03.183+02:00"
#' dateModified <- "2021-04-13T07:00:24.596Z"
#' externalReferences <-
#'   data.frame(referenceID = c("doi:10.155454/12341234",
#'                              "http://purl.obolibrary.org/obo/ro.owl",
#'                              "75a50e76"),
#'              referenceSource = c("DOI",
#'                                  "OBO Library",
#'                                  "Remote Data Collection Upload Tool"))
#' listDescription <- "This is a list of germplasm I would like to investigate next summer season"
#' listName <- "WURgermplasm_Apr_2021"
#' listOwnerName <- "Bob Robertson"
#' listOwnerPersonDbId <- "person1"
#' listSize <- 3
#' listSource <- "WUR GeneBank"
#' listType <- "germplasm"
#' brapi_post_lists(con = con,
#'                  data = data,
#'                  additionalInfo = additionalInfo,
#'                  dateCreated = dateCreated,
#'                  dateModified = dateModified,
#'                  externalReferences = externalReferences,
#'                  listDescription = listDescription,
#'                  listName = listName,
#'                  listOwnerName = listOwnerName,
#'                  listOwnerPersonDbId = listOwnerPersonDbId,
#'                  listSize = listSize,
#'                  listSource = listSource,
#'                  listType = listType)
#' }
#'
#' @export
brapi_post_lists <- function(con = NULL, data = '', additionalInfo = list(), dateCreated = '', dateModified = '', externalReferences = '', listDescription = '', listName = '', listOwnerName = '', listOwnerPersonDbId = '', listSize = 0, listSource = '', listType = '') {
  ## Create a list of used arguments
  usedArgs <- brapir:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check if usedArgs[["externalReferences"]] is supplied as empty character vector or data.frame
  if (!(inherits(usedArgs[["externalReferences"]], what = "character") && usedArgs[["externalReferences"]] == "" || inherits(usedArgs[["externalReferences"]], what = "data.frame"))) {
    stop('Argument: "externalReferences" should be supplied as an empty character or as a data.frame, see the help page on how the data.frame should be constructed.')
  }
  externalReferences <- usedArgs[["externalReferences"]]
  usedArgs[["externalReferences"]] <- NULL
  ## Check validity of used and required arguments
  brapir:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Put externalReferences back into usedArgs
  usedArgs[["externalReferences"]] <- externalReferences
  ## Obtain the call url
  callurl <- brapir:::brapi_POST_callURL(usedArgs = usedArgs,
                                         callPath = "/lists",
                                         reqArgs = "",
                                         packageName = "BrAPI-Core",
                                         callVersion = 2.0)
  ## Build the Body
  callbody <- brapir:::brapi_POST_callBody(usedArgs = usedArgs,
                                           reqArgs = "")
  ## Adaptation for v2.0 where json body is wrapped in []
  callbody <- list(callbody)

  try({
    ## Make the call and receive the response
    resp <- brapir:::brapi_POST(url = callurl, body = callbody, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapir:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_lists")
  ## Show pagination information from metadata
  brapir:::brapi_serverinfo_metadata(cont)
  return(out)
}
