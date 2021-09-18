#' @title
#' post /traits
#'
#' @description
#' Add new Traits
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param additionalInfo list; required: FALSE; Additional arbitrary information.
#'    If provided use the following construct list(additionalProp1 = "string",
#'    additionalProp2 =  "string", additionalProp3 = "string").
#'
#'    The Examples section shows an example on how to construct the
#'    `additionalInfo` argument as a list.
#' @param alternativeAbbreviations vector of type character; required: FALSE;
#'    Other frequent abbreviations of the trait, if any. These abbreviations do
#'    not have to follow a convention; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param attribute character; required: FALSE; A trait can be decomposed as
#'    &quot;Trait&quot; = &quot;Entity&quot; + &quot;Attribute&quot;, the
#'    attribute is the observed feature (or characteristic) of the entity, e.g.
#'    for &quot;grain colour&quot;, attribute = &quot;colour&quot;.
#' @param entity character; required: FALSE; A trait can be decomposed as
#'    &quot;Trait&quot; = &quot;Entity&quot; + &quot;Attribute&quot;, the entity
#'    is the part of the plant that the trait refers to, e.g. for
#'    &quot;grain colour&quot;, entity = &quot;grain&quot;.
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
#' @param mainAbbreviation character; required: FALSE; Main abbreviation for
#'    trait name, examples: &quot;Carotenoid content&quot; =&gt; &quot;CC&quot;).
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
#' @param status character; required: FALSE; Trait status (examples:
#'    &quot;recommended&quot;, &quot;obsolete&quot;, &quot;legacy&quot;, *etc.*).
#' @param synonyms vector of type character; required: FALSE; Other trait names;
#'    default: &quot;&quot;, when using multiple values supply as c(
#'    &quot;value1&quot;, &quot;value2&quot;).
#' @param traitClass character; required: FALSE; Trait class, examples:
#'    &quot;morphological&quot;, &quot;phenological&quot;,
#'    &quot;agronomical&quot;, &quot;physiological&quot;,
#'    &quot;abiotic stress&quot;, &quot;biotic stress&quot;,
#'    &quot;biochemical&quot;, &quot;quality traits&quot;,
#'    &quot;fertility&quot;, *etc.*).
#' @param traitDescription character; required: FALSE; The description of a
#'    trait.
#' @param traitName character; required: FALSE; The human readable name of a
#'    trait. MIAPPE V1.1 (DM-86) Trait - Name of the (plant or environmental)
#'    trait under observation.
#'
#' @details Create new trait objects in the database
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Traits/post_traits }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Traits
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' additionalInfo <- list(dummyData = "TRUE",
#'                        example = "post_traits")
#' alternativeAbbreviations <- c("PHght", "Hght")
#' attribute <- "height"
#' entity <- "plant"
#' externalReferences <-
#'   data.frame(referenceID = c("doi:10.155454/12341234",
#'                              "http://purl.obolibrary.org/obo/ro.owl",
#'                              "75a50e76"),
#'              referenceSource = c("DOI",
#'                                  "OBO Library",
#'                                  "Remote Data Collection Upload Tool"))
#' mainAbbreviation <- "PH"
#' ontologyReference <- list(
#'   documentationLinks = data.frame(
#'     URL = c("http://purl.obolibrary.org/obo/ro.owl",
#'             "https://www.cropontology.org/terms/CO_323:0000024/"),
#'     type = c("OBO",
#'              "WEBPAGE")),
#'   ontologyDbId = "ontology_variable1",
#'   ontologyName = "Ontology.org",
#'   version = "17")
#' status <- "recommended"
#' synonyms <- c("Height",
#'               "Plant Height",
#'               "Stalk Height",
#'               "Canopy Height")
#' traitClass <- "agronomical"
#' traitDescription <- "The height of plant from ground to top part"
#' traitName <- "Plant height"
#' ## Add the new trait
#' brapi_post_traits(con = con,
#'                   additionalInfo = additionalInfo,
#'                   alternativeAbbreviations = alternativeAbbreviations,
#'                   attribute = attribute,
#'                   entity = entity,
#'                   externalReferences = externalReferences,
#'                   mainAbbreviation = mainAbbreviation,
#'                   ontologyReference = ontologyReference,
#'                   status = status,
#'                   synonyms = synonyms,
#'                   traitClass = traitClass,
#'                   traitDescription = traitDescription,
#'                   traitName = traitName)
#' }
#'
#' @export
brapi_post_traits <- function(con = NULL,
                              additionalInfo = list(),
                              alternativeAbbreviations = '',
                              attribute = '',
                              entity = '',
                              externalReferences = '',
                              mainAbbreviation = '',
                              ontologyReference = list(),
                              status = '',
                              synonyms = '',
                              traitClass = '',
                              traitDescription = '',
                              traitName = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                           callPath = "/traits",
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
  class(out) <- c(class(out), "brapi_post_traits")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
