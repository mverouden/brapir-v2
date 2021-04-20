#' @title
#' post /studies
#'
#' @description
#' Create new Studies.
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param active logical; required: FALSE; Is this study currently active;
#'     default: NA, other possible values: TRUE | FALSE
#' @param additionalInfo list; required: FALSE; Additional arbitrary information.
#'    If provided use the following construct list(additionalProp1 = "string",
#'    additionalProp2 =  "string", additionalProp3 = "string"). The Examples
#'    Section shows an example on how to construct the `additionalInfo` argument
#'    as a list.
#' @param commonCropName character; required: FALSE; Common name for the crop
#'    associated with this study
#' @param contacts data.frame; required: FALSE; A data.frame of contact entities
#'    associated with this study. The `contacts` argument data.frame should or
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
#'    The Examples Section shows an example of how to construct the
#'    `contacts` argument as a data.frame.
#' @param culturalPractices character; required: FALSE; MIAPPE V1.1 (DM-28)
#'    Cultural practices - General description of the cultural practices of the
#'    study.
#' @param dataLinks data.frame; required: FALSE; A data.frame of links to extra
#'    data files associated with this study. Extra data could include notes,
#'    images, and reference data. The `dataLinks` arguments data.frame can
#'    contain the following columns:
#'
#'    - `dataFormat` character; required: FALSE; The structure of the data
#'      within a file. For example - VCF, table, image archive, multispectral
#'      image archives in EDAM ontology (used in Galaxy). MIAPPE V1.1 (DM-38)
#'      Data file description - Description of the format of the data file. May
#'      be a standard file format name, or a description of organization of the
#'      data in a tabular file.
#'    - `description` character; required: FALSE; The general description of
#'      this data link. MIAPPE V1.1 (DM-38) Data file description - Description
#'      of the format of the data file. May be a standard file format name, or a
#'      description of organization of the data in a tabular file.
#'    - `fileFormat` character; required: FALSE; The MIME type of the file (i.e.
#'      text/csv, application/excel, application/zip). MIAPPE V1.1 (DM-38) Data
#'      file description - Description of the format of the data file. May be a
#'      standard file format name, or a description of organization of the data
#'      in a tabular file.
#'    - `name` character; required: FALSE; The name of the external data link.
#'      MIAPPE V1.1 (DM-38) Data file description - Description of the format of
#'      the data file. May be a standard file format name, or a description of
#'      organization of the data in a tabular file.
#'    - `provenance` character; required: FALSE; The description of the origin
#'      or ownership of this linked data. Could be a formal reference to
#'      software, method, or workflow.
#'    - `scientificType` character; required: FALSE; The general type of data.
#'      For example- Genotyping, Phenotyping raw data, Phenotyping reduced data,
#'      Environmental, *etc.*
#'    - `url` character; required: FALSE; URL describing the location of this
#'      data file to view or download. MIAPPE V1.1 (DM-37) Data file link - Link
#'      to the data file (or digital object) in a public database or in a
#'      persistent institutional repository; or identifier of the data file when
#'      submitted together with the MIAPPE submission.
#'    - `version` character; required: FALSE; The version number for this data.
#'      MIAPPE V1.1 (DM-39) Data file version - The version of the data set (the
#'      actual data).
#'
#'    The Examples Section shows an example of how to construct the
#'    `dataLinks` argument as a data.frame.
#' @param documentationURL character; required: FALSE; A URL to the human
#'    readable documentation of this object.
#' @param startDate character; required: FALSE; The date the study will start or
#'    started. Coded in the ISO 8601 standard extended format, where date, time
#'    and time zone information needs to be provided (check for example
#'    [https://www.w3.org/TR/NOTE-datetime](https://www.w3.org/TR/NOTE-datetime)
#'    ). MIAPPE V1.1 (DM-14) Start date of study - Date and, if relevant, time
#'    when the experiment started.
#' @param endDate character; required: FALSE; The date the study will end or
#'    ended. Coded in the ISO 8601 standard extended format, where date,
#'    time and time zone information needs to be provided (check for example
#'    [https://www.w3.org/TR/NOTE-datetime](https://www.w3.org/TR/NOTE-datetime)
#'    ). MIAPPE V1.1 (DM-15) End date of study - Date and, if relevant, time
#'    when the experiment ended.
#' @param environmentParameters data.frame; required: FALSE; A data.frame of
#'    Environmental parameters that were kept constant throughout the study and
#'    did not change between observation units. MIAPPE V1.1 (DM-57) Environment
#'    - Environmental parameters that were kept constant throughout the study
#'    and did not change between observation units or assays. Environment
#'    characteristics that vary over time, i.e. environmental variables, should
#'    be recorded as Observed Variables (see below). The `environmentParameters`
#'    argument data.frame should or can contain the following columns:
#'
#'    - `description` character; required: TRUE; Human-readable value of the
#'      environment parameter (defined above) constant within the experiment.
#'    - `parameterName` character; required: TRUE; Name of the environment
#'      parameter constant within the experiment. MIAPPE V1.1 (DM-58) Environment
#'      parameter - Name of the environment parameter constant within the
#'      experiment.
#'    - `parameterPUI` character; required: FALSE; URI pointing to an ontology
#'      class for the parameter.
#'    - `unit` character; required: FALSE; Unit of the value for this parameter
#'    - `unitPUI` character; required: FALSE; URI pointing to an ontology class
#'      for the unit.
#'    - `value` character; required: FALSE; Numerical or categorical value.
#'      MIAPPE V1.1 (DM-59) Environment parameter value - Value of the
#'      environment parameter (defined above) constant within the experiment.
#'    - `valuePUI` character; required: FALSE; URI pointing to an ontology class
#'      for the parameter value.
#'
#'    The Examples Section shows an example of how to construct the
#'    `environmentParameters` argument as a data.frame.
#' @param experimentalDesign list; required: FALSE; The experimental and
#'    statistical design full description plus a category PUI taken from crop
#'    research ontology or agronomy ontology. The `experimentalDesign` list
#'    argument can contain the following elements:
#'
#'    - `PUI` character; required: FALSE; MIAPPE V1.1 (DM-23) Type of experimental
#'      design - Type of experimental design of the study, in the form of an
#'      accession number from the Crop Ontology.
#'    - `description` character: required: FALSE; MIAPPE V1.1 (DM-22) Description
#'      of the experimental design - Short description of the experimental
#'      design, possibly including statistical design. In specific cases, e.g.
#'      legacy datasets or data computed from several studies, the experimental
#'      design can be "unknown"/"NA", "aggregated/reduced data", or simply 'none'.
#'
#'    The Examples Section shows an example of how to construct the
#'    `experimentalDesign` argument as a list.
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
#'    The Examples Section shows an example of how to construct the
#'    `externalReferences` argument as a data.frame.
#' @param growthFacility list; required: FALSE; Short description of the
#'    facility in which the study was carried out. The `growthFacility` list
#'    argument can contain the following elements:
#'
#'    - `PUI` character; required: FALSE; MIAPPE V1.1 (DM-27) Type of growth
#'      facility - Type of growth facility in which the study was carried out,
#'      in the form of an accession number from the Crop Ontology.
#'    - `description` charater; required: FALSE; MIAPPE V1.1 (DM-26) Description
#'      of growth facility - Short description of the facility in which the
#'      study was carried out.
#'
#'    The Examples Section shows an example of how to construct the
#'    `growthFacility` argument as a list.
#' @param lastUpdate list; required: FALSE; The date and time when this study
#'    was last modified. The `lastUpdate` list argument can contain the
#'    following elements:
#'
#'    - `timestamp` character; required: FALSE; Coded in the ISO 8601 standard
#'      extended format, where date, time and time zone information needs to be
#'      provided (check for example
#'      [https://www.w3.org/TR/NOTE-datetime](https://www.w3.org/TR/NOTE-datetime)
#'      ).
#'    - `version` character; required: FALSE; Version number specifying the last
#'      update.
#'
#'    The Examples Section shows an example of how to construct the
#'    `lastUpdate` argument as a list.
#' @param license character; required: FALSE; The usage license associated with
#'    the study data.
#' @param locationDbId character; required: FALSE; The unique identifier for a
#'    location.
#' @param locationName character; required: FALSE; A human readable name for
#'    this location. MIAPPE V1.1 (DM-18) Experimental site name - The name of
#'    the natural site, experimental field, greenhouse, phenotyping facility,
#'    *etc.* where the experiment took place.
#' @param observationLevels data.frame; required: FALSE; A data.frame of
#'    Observation levels, which indicate the granularity level at which the
#'    measurements are taken. The `observationLevels` argument data.frame can
#'    contain the following columns:
#'
#'    - `levelName` character; required: FALSE; A name for this level.
#'    - `levelOrder` interger; required: FALSE; `levelOrder` defines, where that
#'      level exists in the hierarchy of levels. `levelOrder`'s lower numbers
#'      are at the top of the hierarchy (i.e. field -> 1) and higher numbers are
#'      at the bottom of the hierarchy (i.e. plant -> 9).
#'
#'    The Examples Section shows an example of how to construct the
#'    `observationLevels` argument as a data.frame.
#' @param observationUnitsDescription character; required: FALSE; MIAPPE V1.1
#' (DM-25) Observation unit description - General description of the observation
#' units in the study.
#' @param seasons vector of type character; required: FALSE; seasonsDbId value(s)
#'    over which this study was performed.; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param studyCode character; required: FALSE; A short human readable code for
#'    a study.
#' @param studyDescription character; required: FALSE; The description of this
#'    study. MIAPPE V1.1 (DM-13) Study description - Human-readable text
#'    describing the study.
#' @param studyName character; required: FALSE; The human readable name for a
#'    study. MIAPPE V1.1 (DM-12) Study title - Human-readable text summarizing
#'    the study.
#' @param studyPUI character; required: FALSE; A permanent unique identifier
#'    associated with this study data. For example, a URI or DOI.
#' @param studyName character; required: FALSE; The type of study being
#'    performed. ex. "Yield Trial", *etc.*
#' @param trialDbId character; required: FALSE; The ID which uniquely identifies
#'    a trial.
#' @param trialName character; required: FALSE; The human readable name of a
#'    trial.
#'
#' @details Create new studies, where the `studDbId` is generated by the server.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Core/2.0#/Studies/post_studies }{BrAPI SwaggerHub}
#'
#' @family brapi-core
#' @family Studies
#'
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
#' culturalPractices <- "Irrigation was applied according needs during summer to
#'                       prevent water stress."
#' dataLinks <- data.frame(dataFormat = "Image Archive",
#'                         description = "Raw drone images collected for this study",
#'                         fileFormat = "application/zip",
#'                         name = "image-archive.zip",
#'                         provenance = "Image Processing Pipeline, built at the
#'                                       University of Antarctica:
#'                                       https://github.com/antarctica/pipeline",
#'                         scientificType = "Environmental",
#'                         url = "https://brapi.org/image-archive.zip",
#'                         version = "1.0.3")
#' documentationURL <- "https://wiki.brapi.org"
#' startDate <- "2021-04-19T12:34:13.494Z"
#' endDate <- "2021-04-19T12:34:13.494Z"
#' environmentParameters <- data.frame(description = "the soil type was clay",
#'                                     parameterName =  "soil type",
#'                                     parameterPUI = "PECO:0007155",
#'                                     unit = "pH",
#'                                     unitPUI = "PECO:0007059",
#'                                     value = "clay soil",
#'                                     valuePUI = "ENVO:00002262")
#' experimentalDesign <- list(PUI = "CO_715:0000145",
#'                            description = "Lines were repeated twice at each
#'                                           location using a complete block
#'                                           design. In order to limit competition
#'                                           effects, each block was organized into
#'                                           four sub-blocks corresponding to
#'                                           earliest groups based on a prior
#'                                           information.")
#' externalReferences <-
#'   data.frame(referenceID = c("doi:10.155454/12341234",
#'                              "http://purl.obolibrary.org/obo/ro.owl",
#'                              "75a50e76"),
#'              referenceSource = c("DOI",
#'                                  "OBO Library",
#'                                  "Remote Data Collection Upload Tool"))
#' growthFacility <- list(PUI = "CO_715:0000162",
#'                        description = "field environment condition, greenhouse")
#' lastUpdate <- list(timestamp = "2021-04-19T12:34:13.494Z",
#'                    version = "1.2.3")
#' license <- "MIT License"
#' locationDbId <- "location_01"
#' locationName <- "Location 1"
#' observationLevels <-
#'   data.frame(levelName = c("field", "block", "plot"),
#'              levelOrder = c(0, 1, 2))
#' observationUnitsDescription <- "Observation units consisted in individual
#'                                 plots themselves consisting of a row of 15
#'                                 plants at a density of approximately six
#'                                 plants per square meter."
#' seasons <- c("spring_2012", "summer_2012")
#' studyCode <- "Tomatillo_Yield_Spring_2012"
#' studyDescription <- "This is a yield study for Spring 2012"
#' studyName <- "INRA's Tomatillo Genetic Resources Observation at Peru"
#' studyPUI <- "doi:10.155454/12349537312"
#' studyType <- "Phenotyping"
#' trialDbId <- "trial1"
#' trialName <- "Peru Yield Trial 1"
#'
#' brapi_post_studies(con = con,
#'                    active = active,
#'                    additionalInfo = additionalInfo,
#'                    commonCropName = commonCropName,
#'                    contacts = contacts,
#'                    culturalPractices = culturalPractices,
#'                    dataLinks = dataLinks,
#'                    documentationURL = documentationURL,
#'                    startDate = startDate,
#'                    endDate = endDate,
#'                    environmentParameters = environmentParameters,
#'                    experimentalDesign = experimentalDesign,
#'                    externalReferences = externalReferences,
#'                    growthFacility = growthFacility,
#'                    lastUpdate = lastUpdate,
#'                    license = license,
#'                    locationDbId = locationDbId,
#'                    locationName = locationName,
#'                    observationLevels = observationLevels,
#'                    observationUnitsDescription = observationUnitsDescription,
#'                    seasons = seasons,
#'                    studyCode = studyCode,
#'                    studyDescription = studyDescription,
#'                    studyName = studyName,
#'                    studyPUI = studyPUI,
#'                    studyType = studyType,
#'                    trialDbId = trialDbId,
#'                    trialName = trialName)
#' }
#'
#' @export
brapi_post_studies <- function(con = NULL,
                               active = NA,
                               additionalInfo = list(),
                               commonCropName = '',
                               contacts = '',
                               culturalPractices = '',
                               dataLinks = '',
                               documentationURL = '',
                               startDate = '',
                               endDate = '',
                               environmentParameters = '',
                               experimentalDesign = list(),
                               externalReferences = '',
                               growthFacility = list(),
                               lastUpdate = list(),
                               license = '',
                               locationDbId = '',
                               locationName = '',
                               observationLevels = '',
                               observationUnitsDescription = '',
                               seasons = '',
                               studyCode = '',
                               studyDescription = '',
                               studyName = '',
                               studyPUI = '',
                               studyType = '',
                               trialDbId = '',
                               trialName = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                           callPath = "/studies",
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
  class(out) <- c(class(out), "brapi_post_studies")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
