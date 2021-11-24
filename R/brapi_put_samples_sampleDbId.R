#' @title
#' put /samples/\{sampleDbId\}
#'
#' @description
#' Update the details of an existing Sample
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param sampleDbId character; required: TRUE; the unique internal database
#'    identifier for a sample.
#' @param additionalInfo list; required: FALSE; Additional arbitrary information.
#'    If provided use the following construct list(additionalProp1 = "string",
#'    additionalProp2 =  "string", additionalProp3 = "string").
#'
#'    The Examples section shows an example on how to construct the
#'    `additionalInfo` argument as a list.
#' @param column integer; required: FALSE; The Column identifier for this
#'    sample's location in the plate.
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
#' @param germplasmDbId character; required: FALSE; Unique database identifier
#'    for a germplasm (accession).
#' @param observationUnitDbId character; required: FALSE; Database identifier,
#'    which unique identifies a observation unit.
#' @param plateDbId character; required: FALSE; The database identifier, which
#'    uniquely identifies a plate of samples.
#' @param plateName character; required: FALSE; The human readable name of a
#'    plate.
#' @param programDbId character; required: FALSE; The datbase identifier, which
#'    uniquely identifies a program.
#' @param row character; required: FALSE; The Row identifier for this sample's
#'    location in the plate.
#' @param sampleBarcode character; required: FALSE; A unique identifier
#'    physically attached to the sample.
#' @param sampleDescription character; required: FALSE; Description of a sample.
#'    MIAPPE V1.1 (DM-79) Sample description - Any information not captured by
#'    the other sample fields, including quantification, sample treatments and
#'    processing.
#' @param sampleGroupDbId character; required: FALSE; The identifier, which
#'    uniquely identifies a group of samples.
#' @param sampleName character; required: FALSE; The name of the sample.
#' @param samplePUI character; required: FALSE; A permanent unique identifier
#'    for the sample (DOI, URL, UUID, *etc.*). MIAPPE V1.1 (DM-81) External ID -
#'    An identifier for the sample in a persistent repository, comprising the
#'    name of the repository and the accession number of the observation unit
#'    therein. Submission to the EBI Biosamples repository is recommended. URI
#'    are recommended when possible.
#' @param sampleTimestamp character; required: FALSE; The date and time a sample
#'    was collected from the field. MIAPPE V1.1 (DM-80) Collection date - The
#'    date and time when the sample was collected / harvested.
#' @param sampleType character; required: FALSE; The type of sample taken. e.g.
#'    'DNA', 'RNA', 'Tissue', *etc.*.
#' @param studyDbId character; required: FALSE; The database identifier, which
#'    uniquely identifies a study.
#' @param takenBy character; required: FALSE; The name or identifier of the
#'    entity, which took the sample from the field.
#' @param tissueType character; required: FALSE; The type of tissue sampled,
#'    e.g. 'Leaf', 'Root', *etc.*. MIAPPE V1.1 (DM-78) Plant anatomical entity -
#'    A description of  the plant part (e.g. leaf) or the plant product (e.g.
#'    resin) from which the sample was taken, in the form of an accession number
#'    to a suitable controlled vocabulary (Plant Ontology).
#' @param trialDbId character; required: FALSE; The database identifier, which
#'    uniquely identifies a trial.
#' @param well character; required: FALSE; The Well identifier for this sample's
#'    location in the plate. Usually a concatenation of Row and Column, or just
#'    a number if the samples are not part of an ordered plate.
#'
#' @details Update the details of an existing Sample
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Samples/put_samples__sampleDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Samples
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' ## Create function argument values
#' additionalInfo <- list(dummyData = "TRUE",
#'                        example = "post_samples")
#' column <- 3
#' externalReferences <-
#'   data.frame(referenceID = c("doi:10.155454/12341234",
#'                              "http://purl.obolibrary.org/obo/ro.owl",
#'                              "75a50e76"),
#'              referenceSource = c("DOI",
#'                                  "OBO Library",
#'                                  "Remote Data Collection Upload Tool"))
#' germplasmDbId <- "germplasm3"
#' observationUnitDbId <- "observation_unit3"
#' plateDbId <- "plate1"
#' plateName <- "Plate 1"
#' programDbId <- "program1"
#' row <- "B"
#' sampleBarcode <- "1234567890"
#' sampleDescription <- "Fake sample"
#' sampleGroupDbId <- "sample_group_02"
#' sampleName <- "Sample 4"
#' samplePUI <- "doi://40.12345/fake/4423"
#' sampleTimestamp <- "2012-12-12T12:12:12Z"
#' sampleType <- "Tissue"
#' studyDbId <- "study1"
#' takenBy <- "M. Verouden"
#' tissueType <- "Stem"
#' trialDbId <- "trial1"
#' well <- "B3"
#' ## Add a new sample
#' out <-
#'  brapi_post_samples(con = con,
#'                     additionalInfo = additionalInfo,
#'                     column = column,
#'                     externalReferences = externalReferences,
#'                     germplasmDbId = germplasmDbId,
#'                     observationUnitDbId = observationUnitDbId,
#'                     plateDbId = plateDbId,
#'                     plateName = plateName,
#'                     programDbId = programDbId,
#'                     row = row,
#'                     sampleBarcode = sampleBarcode,
#'                     sampleDescription = sampleDescription,
#'                     sampleGroupDbId = sampleGroupDbId,
#'                     sampleName = sampleName,
#'                     samplePUI = samplePUI,
#'                     sampleTimestamp = sampleTimestamp,
#'                     sampleType = sampleType,
#'                     studyDbId = studyDbId,
#'                     takenBy = takenBy,
#'                     tissueType = tissueType,
#'                     trialDbId = trialDbId,
#'                     well = well)
#' ## Obtain the sampleDbId
#' sampleDbId <- unique(out$sampleDbId)
#' ## Retrieve information about the new sample
#' brapi_get_samples_sampleDbId(con = con, sampleDbId = sampleDbId)
#' ## Update the new sample information
#' additionalInfo <- list(dummyData = "TRUE",
#'                        example = "put_samples_sampleeDbId")
#' sampleDescription <- "Fake sample with updated information"
#' sampleName <- "Updated Sample 4"
#' brapi_put_samples_sampleDbId(con = con,
#'                              sampleDbId = sampleDbId,
#'                              additionalInfo = additionalInfo,
#'                              sampleDescription = sampleDescription,
#'                              sampleName = sampleName)
#' ## Check the changes
#' brapi_get_samples_sampleDbId(con = con, sampleDbId = sampleDbId)
#' }
#'
#' @export
brapi_put_samples_sampleDbId <- function(con = NULL,
                                         sampleDbId = '',
                                         additionalInfo = list(),
                                         column = as.integer(NA),
                                         externalReferences = '',
                                         germplasmDbId = '',
                                         observationUnitDbId = '',
                                         plateDbId = '',
                                         plateName = '',
                                         programDbId = '',
                                         row = '',
                                         sampleBarcode = '',
                                         sampleDescription = '',
                                         sampleGroupDbId = '',
                                         sampleName = '',
                                         samplePUI = '',
                                         sampleTimestamp = '',
                                         sampleType = '',
                                         studyDbId = '',
                                         takenBy = '',
                                         tissueType = '',
                                         trialDbId = '',
                                         well = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "sampleDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_PUT_callURL(usedArgs = usedArgs,
                                          callPath = "/samples/{sampleDbId}",
                                          reqArgs = "sampleDbId",
                                          packageName = "BrAPI-Genotyping",
                                          callVersion = 2.0)
  ## Build the Body
  callbody <- brapirv2:::brapi_PUT_callBody(usedArgs = usedArgs,
                                            reqArgs = "sampleDbId")

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_PUT(url = callurl, body = callbody, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_put_samples_sampleDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
