#' @title
#' put /scales/\{scaleDbId\}
#'
#' @description
#' Update an existing Scale
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param scaleDbId character; required: TRUE; Identifier of the scale to update
#'    details of.
#' @param additionalInfo list; required: FALSE; Additional arbitrary information.
#'    If provided use the following construct list(additionalProp1 = "string",
#'    additionalProp2 =  "string", additionalProp3 = "string").
#'
#'    The Examples section shows an example on how to construct the
#'    `additionalInfo` argument as a list.
#' @param dataType character; required: FALSE; Class of the scale, entries can
#'    be:
#'
#'    * &quot;Code&quot; -  This scale class is exceptionally used to express
#'      complex traits. Code is a nominal scale that combines the expressions of
#'      the different traits composing the complex trait. For example a severity
#'      trait might be expressed by a 2 digit and 2 character code. The first 2
#'      digits are the percentage of the plant covered by a fungus and the 2
#'      characters refer to the delay in development, e.g. &quot;75VD&quot;
#'      means &quot;75 %&quot; of the plant is infected and the plant is very
#'      delayed.
#'    * &quot;Date&quot; - The date class is for events expressed in a time
#'      format, coded in the ISO 8601 standard extended format, where date, time
#'      and time zone information needs to be provided (check for example
#'      [https://www.w3.org/TR/NOTE-datetime](https://www.w3.org/TR/NOTE-datetime).
#'    * &quot;Duration&quot; - The Duration class is for time elapsed between
#'      two events expressed in a time format, e.g. days, hours, months.
#'    * &quot;Nominal&quot; - Categorical scale that can take one of a limited
#'      and fixed number of categories. There is no intrinsic ordering to the
#'      categories.
#'    * &quot;Numerical&quot; - Numerical scales express the trait with real
#'      numbers. The numerical scale defines the unit e.g. centimeter, ton per
#'      hectare, branches.
#'    * &quot;Ordinal&quot; - Ordinal scales are scales composed of ordered
#'      categories.
#'    * &quot;Text&quot; - A free text is used to express the trait.
#' @param decimalPlaces integer; required: FALSE; For numerical, number of
#'    decimal places to be reported.
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
#' @param scaleName character; required: FALSE; Name of the scale. MIAPPE V1.1
#'    (DM-92) Scale Name of the scale associated with the variable.
#' @param validValues list; required: FALSE; Valid values for the scale. The
#'    `validValues` list can contain the following elements:
#'
#'    * `categories` data.frame; required: FALSE;	data.frame of possible values
#'      with optional labels. Each row in the `categories` data.frame must have
#'      the following columns:
#'
#'        + `label` character; required: FALSE; A text label for a category.
#'        + `value` character; required: FALSE; The actual value for a category.
#'
#'    * `max` integer; required: FALSE; Maximum value (used for field data
#'      capture control) for numerical and date scales.
#'    * `min` integer; required: FALSE; Minimum value (used for field data
#'      capture control) for numerical and date scales.
#'
#'   The Examples section shows an example of how to construct the
#'   `validValues` argument as a list.
#'
#' @details Update the details of an existing scale
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Scales/put_scales__scaleDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Scales
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' ## Create function argument values
#' additionalInfo <- list(dummyData = "TRUE",
#'                        example = "post_scales")
#' dataType <- "Numerical"
#' decimalPlaces <- 2
#' externalReferences <-
#'   data.frame(referenceID = c("doi:10.155454/12341234",
#'                              "http://purl.obolibrary.org/obo/ro.owl",
#'                              "75a50e76"),
#'              referenceSource = c("DOI",
#'                                  "OBO Library",
#'                                  "Remote Data Collection Upload Tool"))
#' ontologyReference <- list(
#'   documentationLinks = data.frame(
#'     URL = c("http://purl.obolibrary.org/obo/ro.owl",
#'             "https://en.wikipedia.org/wiki/Discriminant"),
#'     type = c("OBO",
#'              "WEBPAGE")),
#'   ontologyDbId = "ontology_attribute1",
#'   ontologyName = "Ontology.org",
#'   version = "17")
#' scaleName <- "Meters"
#' validValues <- list(
#'   categories = data.frame(
#'     label = c("low", "medium", "high"),
#'     value = c("0", "5", "10")),
#'   max = 9999,
#'   min = 2)
#' ## Add the new scale
#' out <- brapi_post_scales(con = con,
#'                          additionalInfo = additionalInfo,
#'                          dataType = dataType,
#'                          decimalPlaces = decimalPlaces,
#'                          externalReferences = externalReferences,
#'                          ontologyReference = ontologyReference,
#'                          scaleName = scaleName,
#'                          validValues = validValues)
#' ## Obtain the scaleDbId
#' scaleDbId <- unique(out$scaleDbId)
#' ## Retrieve information about the new scale
#' brapi_get_scales_scaleDbId(con = con, scaleDbId = scaleDbId)
#' ## Update the new scale information
#' additionalInfo <- list(dummyData = "TRUE",
#'                        example = "put_scales_scaleDbId")
#' decimalPlaces <- 1
#' validValues <- list(
#'   categories = data.frame(
#'     label = c("low", "medium", "high"),
#'     value = c("0", "5", "10")),
#'   max = 9999,
#'   min = 0)
#' brapi_put_scales_scaleDbId(con = con,
#'                            scaleDbId = scaleDbId,
#'                            additionalInfo = additionalInfo,
#'                            decimalPlaces = decimalPlaces,
#'                            validValues = validValues)
#' ## Check the changes
#' brapi_get_scales_scaleDbId(con = con, scaleDbId = scaleDbId)
#'
#' }
#'
#' @export
brapi_put_scales_scaleDbId <- function(con = NULL,
                                       scaleDbId = '',
                                       additionalInfo = list(),
                                       dataType = '',
                                       decimalPlaces = 0,
                                       externalReferences = '',
                                       ontologyReference = list(),
                                       scaleName = '',
                                       validValues = list()) {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  dataType <- usedArgs[["dataType"]]
  usedArgs[["dataType"]] <- NULL
  brapirv2:::brapi_matchArg(arg = dataType,
                            choices =  c("",
                                         "Code",
                                         "Date",
                                         "Duration",
                                         "Nominal",
                                         "Numerical",
                                         "Ordinal",
                                         "Text"))
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "scaleDbId")
  usedArgs[["dataType"]] <- dataType
  ## Obtain the call url
  callurl <- brapirv2:::brapi_PUT_callURL(usedArgs = usedArgs,
                                          callPath = "/scales/{scaleDbId}",
                                          reqArgs = "scaleDbId",
                                          packageName = "BrAPI-Phenotyping",
                                          callVersion = 2.0)
  ## Build the Body
  callbody <- brapirv2:::brapi_PUT_callBody(usedArgs = usedArgs,
                                            reqArgs = "scaleDbId")

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_PUT(url = callurl, body = callbody, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_put_scales_scaleDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
