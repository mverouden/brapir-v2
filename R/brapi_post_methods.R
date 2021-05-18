#' @title
#' post /methods
#'
#' @description
#' Add new Methods
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param additionalInfo list; required: FALSE; Additional arbitrary information.
#'    If provided use the following construct list(additionalProp1 = "string",
#'    additionalProp2 =  "string", additionalProp3 = "string").
#'
#'    The Examples section shows an example on how to construct the
#'    `additionalInfo` argument as a list.
#' @param bibliographicalReference character; required: FALSE; Bibliographical
#'    reference describing the method. MIAPPE V1.1 (DM-91) Reference associated
#'    to the method - URI/DOI of reference describing the method.
#' @param description character; required: FALSE; Method description. MIAPPE
#'    V1.1 (DM-90) Method description - Textual description of the method, which
#'    may extend a method defined in an external reference with specific
#'    parameters, e.g. growth stage, inoculation precise organ (leaf number).
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
#' @param formula character; required: FALSE; For computational methods, i.e.
#'    when the method consists in assessing the trait by computing measurements,
#'    write the generic formula used for the calculation.
#' @param methodClass character; required: FALSE; Method class (examples:
#'    &quot;Measurement&quot;, &quot;Counting&quot;, &quot;Estimation&quot;,
#'    &quot;Computation&quot;, *etc.*).
#' @param methodName character; required: FALSE; Human readable name for the
#'    method. MIAPPE V1.1 (DM-88) Method Name of the method of observation.
#' @param ontologyReference list; required: FALSE; MIAPPE V1.1 (DM-85) Variable
#'    accession number - Accession number of the variable in the Crop Ontology.
#'    (DM-87) Trait accession number - Accession number of the trait in a
#'    suitable controlled vocabulary (Crop Ontology, Trait Ontology). (DM-89)
#'    Method accession number - Accession number of the method in a suitable
#'    controlled vocabulary (Crop Ontology, Trait Ontology). (DM-93) Scale
#'    accession number - Accession number of the scale in a suitable controlled
#'    vocabulary (Crop Ontology).
#'
#'   The `ontolgyReference` list should/can contain the following elements:
#'
#'   * `documentationLinks` data.frame; required: FALSE; Links to various
#'     ontology documentation. Each row in the `documentationLinks` data.frame
#'     should contain the following columns:
#'
#'       + `URL` character; required: FALSE; A uniform resource locator
#'         specified as a valid uniform resource identifier (URI), e.g.
#'         http://purl.obolibrary.org/obo/ro.owl
#'       + `type` character; required: FALSE; one of &quot;OBO&quot;|
#'         &quot;RDF&quot;|&quot;WEBPAGE&quot;.
#'
#'   * `ontologyDbId` character; required: TRUE; Ontology database unique
#'     identifier.
#'   * `ontologyName` character; required: TRUE; Ontology name, e.g. the Crop
#'     Ontology.
#'   * `version` character; required: FALSE; Ontology version (no specific
#'     format).
#'
#'   The Examples section shows an example of how to construct the
#'   `ontologyReference` argument as a list.
#'
#' @details Create new method objects in the database
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Methods/post_methods }{BrAPI SwaggerHub}
#'
#' @family brapi_2.0
#' @family brapi-phenotyping
#' @family Methods
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' additionalInfo <- list(dummyData = "TRUE",
#'                        example = "post_methods")
#' bibliographicalReference <- "Smith, 1893, Really Cool Paper, Popular Journal"
#' description <- "Discriminant for root finding of quadratic functions"
#' externalReferences <-
#'   data.frame(referenceID = c("doi:10.155454/12341234",
#'                              "http://purl.obolibrary.org/obo/ro.owl",
#'                              "75a50e76"),
#'              referenceSource = c("DOI",
#'                                  "OBO Library",
#'                                  "Remote Data Collection Upload Tool"))
#' formula <- "b^2 - 4ac"
#' methodClass <- "Computation"
#' methodName <- "Discriminant"
#' ontologyReference <- list(
#'   documentationLinks = data.frame(
#'     URL = c("http://purl.obolibrary.org/obo/ro.owl",
#'             "https://en.wikipedia.org/wiki/Discriminant"),
#'     type = c("OBO",
#'              "WEBPAGE")),
#'   ontologyDbId = "ontology_attribute1",
#'   ontologyName = "Ontology.org",
#'   version = "17")
#' ## Add the new method
#' brapi_post_methods(con = con,
#'                    additionalInfo = additionalInfo,
#'                    bibliographicalReference = bibliographicalReference,
#'                    description = description,
#'                    externalReferences = externalReferences,
#'                    formula = formula,
#'                    methodClass = methodClass,
#'                    methodName = methodName,
#'                    ontologyReference = ontologyReference)
#' }
#'
#' @export
brapi_post_methods <- function(con = NULL,
                               additionalInfo = list(),
                               bibliographicalReference = '',
                               description = '',
                               externalReferences = '',
                               formula = '',
                               methodClass = '',
                               methodName = '',
                               ontologyReference = list()) {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                           callPath = "/methods",
                                           reqArgs = "",
                                           packageName = "BrAPI-Phenotyping",
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
  class(out) <- c(class(out), "brapi_post_methods")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
