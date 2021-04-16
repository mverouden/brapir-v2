#' @title
#' post /lists/\{listDbId\}/items
#'
#' @description
#' Add Items to a specific List
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param listDbId character; required: TRUE; The unique ID of this generic list
#' @param items vector of type character; required: TRUE; The list of DbIds to
#'    be added to the specified list; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#'
#' @details Add new data to a specific generic lists
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Lists/post_lists__listDbId__items }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Lists
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' data <- ""
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
#' listDescription <-
#'  "This is a list of germplasm I would like to investigate next spring season"
#' listName <- "WURgermplasm_Apr_2021"
#' listOwnerName <- "Bob Robertson"
#' listOwnerPersonDbId <- "person1"
#' listSize <- 0
#' listSource <- "WUR GeneBank"
#' listType <- "germplasm"
#' out <- brapi_post_lists(con = con,
#'                         data = data,
#'                         additionalInfo = additionalInfo,
#'                         dateCreated = dateCreated,
#'                         dateModified = dateModified,
#'                         externalReferences = externalReferences,
#'                         listDescription = listDescription,
#'                         listName = listName,
#'                         listOwnerName = listOwnerName,
#'                         listOwnerPersonDbId = listOwnerPersonDbId,
#'                         listSize = listSize,
#'                         listSource = listSource,
#'                         listType = listType)
#' brapi_post_lists_listDbId_items(con = con,
#'                                 listDbId = unique(out$listDbId),
#'                                 items = c("germ1", "germ2", "germ3"))
#' }
#'
#' @export
brapi_post_lists_listDbId_items <- function(con = NULL,
                                            listDbId = '',
                                            items = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "listDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                         callPath = "/lists/{listDbId}/items",
                                         reqArgs = "listDbId",
                                         packageName = "BrAPI-Core",
                                         callVersion = 2.0)
  ## Build the Body
  callbody <- brapirv2:::brapi_POST_callBody(usedArgs = usedArgs,
                                           reqArgs = "listDbId")
  callbody <- callbody[["items"]]
  ## Add-ons to usedArgs to make an array of strings POST call
  usedArgs[["Accept"]] <- "application/json"
  usedArgs[["Content-Type"]] <- "application/json"

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_POST(url = callurl, body = callbody, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_lists_listDbId_items")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
