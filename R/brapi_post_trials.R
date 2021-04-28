#' @title
#' post /trials
#'
#' @description
#' Create new trials
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param active logical; required: FALSE; Is this trial currently active;
#'     default: NA, other possible values: TRUE | FALSE
#' @param additionalInfo list; required: FALSE; Additional arbitrary information.
#'    If provided use the following construct list(additionalProp1 = "string",
#'    additionalProp2 =  "string", additionalProp3 = "string").
#'
#'    The Examples section shows an example on how to construct the
#'    `additionalInfo` argument as a list.
#' @param commonCropName character; required: FALSE; Common name for the crop
#'    associated with this trial.
#' @param contacts data.frame; required: FALSE; A data.frame of contact entities
#'    associated with this trial. The `contacts` argument data.frame should or
#'    can contain the following columns:
#'
#'    - `contactDbId` character; required: TRUE; The ID which uniquely
#'      identifies this contact. MIAPPE V1.1 (DM-33) Person ID - An identifier
#'      for the data submitter. If that submitter is an individual, ORCID
#'      identifiers are recommended.
#'    - `email`	character; required: FALSE; The contacts email address. MIAPPE
#'      V1.1 (DM-32) Person email - The electronic mail address of the person.
#'    - `instituteName` character; required: FALSE; The name of the institution
#'      which this contact is part of. MIAPPE V1.1 (DM-35) Person affiliation -
#'      The institution the person belongs to.
#'    - `name`	character; required: FALSE; The full name of this contact person.
#'      MIAPPE V1.1 (DM-31) Person name - The name of the person (either full
#'      name or as used in scientific publications).
#'    - `orcid` character; required: FALSE; The Open Researcher and Contributor
#'      ID for this contact person (orcid.org). MIAPPE V1.1 (DM-33) Person ID -
#'      An identifier for the data submitter. If that submitter is an individual,
#'      ORCID identifiers are recommended.
#'    - `type` character;	required: FALSE; The type of person this contact
#'      represents (ex: Coordinator, Scientist, PI, etc.). MIAPPE V1.1 (DM-34)
#'      Person role - Type of contribution of the person to the investigation.
#'
#'    The Examples section shows an example of how to construct the
#'    `contacts` argument as a data.frame.
#' @param datasetAuthorships data.frame; required: FALSE; A data.frame of
#'    License and citation information for the data in this trial. The
#'    `datasetAuthorships` argument data.frame can contain the following
#'    columns:
#'
#'    - `datasetPUI` character; required: FALSE; For example a DOI of a
#'      publication in which the dataset is used.
#'    - `license` character; required: FALSE; MIAPPE V1.1 (DM-7) License -
#'      License for the reuse of the data associated with this investigation.
#'      The Creative Commons licenses cover most use cases and are recommended.
#'    - `publicReleaseDate` character; required: FALSE; MIAPPE V1.1 (DM-6)
#'      Public release date - Date of first public release of the dataset
#'      presently being described.
#'    - `submissionDate` character; required: FALSE; MIAPPE V1.1 (DM-5)
#'      Submission date - Date of submission of the data set presently being
#'      described to a host repository.
#'
#'    The Examples section shows an example of how to construct the
#'    `datasetAuthorships` argument as a data.frame.
#' @param documentationURL character; required: FALSE; A URL to the human
#'    readable documentation of this object.
#' @param endDate character; required: FALSE; The date the trial will end or
#'    ended. Coded in the ISO 8601 standard extended format, where date,
#'    time and time zone information needs to be provided (check for example
#'    [https://www.w3.org/TR/NOTE-datetime](https://www.w3.org/TR/NOTE-datetime)
#'    ).
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
#' @param programDbId character; required: FALSE; A program identifier, to which
#'    this trial belongs to. Exact match.
#' @param programName character; required: FALSE; Human readable name of the
#'    program, to which this trial belongs to. Exact match.
#' @param publications data.frame; required: FALSE; A data.frame of publications
#'    in which the trial is described. MIAPPE V1.1 (DM-9) Associated publication
#'    - An identifier for a literature publication where the investigation is
#'    described. Use of DOIs is recommended. The `publications` data.frame
#'    should contain the following columns:
#'
#'    - `publicationPUI` character; required: FALSE; Preferably a DOI of the
#'      publication
#'    - `publicationReference` character; required: FALSE;	A reference for the
#'      publication, e.g. "Selby, BrAPI - An application programming interface
#'      for plant breeding applications, Bioinformatics,
#'      https://doi.org/10.1093/bioinformatics/190"
#'
#'    The Examples section shows an example of how to construct the
#'    `publications` argument as a data.frame.
#' @param startDate character; required: FALSE; The date the trial will start or
#'    started. Coded in the ISO 8601 standard extended format, where date, time
#'    and time zone information needs to be provided (check for example
#'    [https://www.w3.org/TR/NOTE-datetime](https://www.w3.org/TR/NOTE-datetime)
#'    ).
#' @param trialDescription character; required: FALSE; The human readable
#'    description of a trial. MIAPPE V1.1 (DM-4) Investigation description -
#'    Human-readable text describing the investigation in more detail.
#' @param trialName character; required: FALS; The human readable name of a
#'    trial. MIAPPE V1.1 (DM-3) Investigation title - Human-readable string
#'    summarizing the investigation.
#' @param trialPUI character; required: FALSE; A permanent identifier for a
#'    trial. Could be DOI or other URI formatted identifier.
#'
#' @details Create new breeding Trials. A Trial represents a collection of
#'    related Studies. `trialDbId` is generated by the server.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Trials/post_trials }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Trials
#'
#' @examples
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' active <- TRUE
#' additionalInfo <- list(additionalProp1 = "string",
#'                        additionalProp2 = "string",
#'                        additionalProp3 = "string")
#' commonCropName <- "Tomatillo"
#' contacts <- data.frame(contactDbId = "trial_contact_1",
#'                        email = "a.breeder@brapi.org",
#'                        instituteName = "Plant Science Institute",
#'                        name = "A. Breeder",
#'                        orcid = "http://orcid.org/0000-0002-0607-8728",
#'                        type = "Breeder")
#' culturalPractices <- "Irrigation was applied according needs during summer to prevent water stress."
#' dataLinks <-
#'   data.frame(dataFormat = "Image Archive",
#'              description = "Raw drone images collected for this study",
#'              fileFormat = "application/zip",
#'              name = "image-archive.zip", provenance = "Image Processing Pipeline, built at the University of Antarctica: https://github.com/antarctica/pipeline",
#'              scientificType = "Environmental",
#'              url = "https://brapi.org/image-archive.zip",
#'              version = "1.0.3")
#' datasetAuthorships <-
#'   data.frame(datasetPUI = "doi:10.15454/312953986E3",
#'              license = "https://CreativeCommons.org/licenses/by/4.0/",
#'              publicReleaseDate = "2021-04-28",
#'              submissionDate = "2021-04-28")
#' documentationURL <- "https://wiki.brapi.org"
#' endDate <- "2021-04-28T12:34:13.494Z"
#' externalReferences <-
#'   data.frame(referenceID = c("doi:10.155454/12341234",
#'                              "http://purl.obolibrary.org/obo/ro.owl",
#'                              "75a50e76"),
#'              referenceSource = c("DOI",
#'                                  "OBO Library",
#'                                  "Remote Data Collection Upload Tool"))
#' programDbId <- "program2"
#' programName <- "The Fake Tomatillo Breeding Program"
#' publications <-
#'   data.frame(publicationPUI = "doi:10.15454/312953986E3",
#'              publicationReference = "Selby, BrAPI - An application programming interface for plant breeding applications, Bioinformatics, https://doi.org/10.1093/bioinformatics/190")
#' startDate <- "2021-04-28T12:34:13.494Z"
#' trialDescription <- "General drought resistance trial initiated in Peru before duplication in Africa"
#' trialName <- "Peru Yield Trial 1"
#' trialPUI <- "https://doi.org/101093190"
#'
#' brapi_post_trials(con = con,
#'                   active = active,
#'                   additionalInfo = additionalInfo,
#'                   commonCropName = commonCropName,
#'                   contacts = contacts,
#'                   datasetAuthorships = datasetAuthorships,
#'                   documentationURL = documentationURL,
#'                   endDate = endDate,
#'                   externalReferences = externalReferences,
#'                   programDbId = programDbId,
#'                   programName = programName,
#'                   publications = publications,
#'                   startDate = startDate,
#'                   trialDescription = trialDescription,
#'                   trialName = trialName,
#'                   trialPUI = trialPUI)
#' }
#'
#' @export
brapi_post_trials <- function(con = NULL,
                              active = NA,
                              additionalInfo = list(),
                              commonCropName = '',
                              contacts = '',
                              datasetAuthorships = '',
                              documentationURL = '',
                              endDate = '',
                              externalReferences = '',
                              programDbId = '',
                              programName = '',
                              publications = '',
                              startDate = '',
                              trialDescription = '',
                              trialName = '',
                              trialPUI = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                           callPath = "/trials",
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
  class(out) <- c(class(out), "brapi_post_trials")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
