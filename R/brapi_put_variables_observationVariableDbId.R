#' @title
#' put /variables/\{observationVariableDbId\}
#'
#' @description
#' Update an existing Observation Variable
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param observationVariableDbId character; required: TRUE; Unique database id,
#'    which identifies the variable to be updated.
#' @param additionalInfo list; required: FALSE; Additional arbitrary information.
#'    If provided use the following construct list(additionalProp1 = "string",
#'    additionalProp2 =  "string", additionalProp3 = "string").
#'
#'    The Examples section shows an example on how to construct the
#'    `additionalInfo` argument as a list.
#' @param commonCropName character; required: FALSE; Common name for the crop
#'    associated with the observation variable.
#' @param contextOfUse vector of type character; required: FALSE; Indication of how
#'    trait is routinely used; default: &quot;&quot;, when using multiple values
#'    supply as c(&quot;value1&quot;, &quot;value2&quot;)., e.g. c(
#'    "Trial evaluation", "Nursery evaluation").
#' @param defaultValue character; required: FALSE; Variable default value, e.g.
#'    "red", "2.3", *etc.*
#' @param documentationURL character; required: FALSE; A URL to the human
#'    readable documentation of this object.
#' @param externalReferences data.frame; required: FALSE; A data.frame of
#'    external reference ids. These are references to this piece of data in an
#'    external system. Could be a simple string or a URI. The
#'    `externalReferences` argument data.frame should contain the following
#'    columns:
#'
#'    * `referenceID` character; required: TRUE; The external reference ID.
#'      Could be a simple string or a URI.
#'    * `referenceSource` character; required: TRUE; An identifier for the
#'      source system or database of this reference.
#'
#'    The Examples section shows an example of how to construct the
#'    `externalReferences` argument as a data.frame.
#' @param growthStage character; required: FALSE; Growth stage at which
#'    measurement is made, e.g. "flowering".
#' @param institution character; required: FALSE; Name of institution submitting
#'    the variable.
#' @param language character; required: FALSE; 2 letter ISO 639-1 code for the
#'    language of submission of the variable, e.g. en.
#' @param method list; required: FALSE; Method used for the observation variable
#'    . The `method` list can contain the following elements:
#'
#'    * `additionalInfo` list; required: FALSE; Additional arbitrary information
#'        . If provided use the following construct list(additionalProp1 =
#'        "string", additionalProp2 =  "string", additionalProp3 = "string").
#'
#'      The Examples section shows an example on how to construct the
#'      `additionalInfo` element as a list within the `method` argument.
#'
#'    * `bibliographicalReference` character; required: FALSE; Bibliographical
#'        reference describing the method. MIAPPE V1.1 (DM-91) Reference
#'        associated to the method - URI/DOI of reference describing the method.
#'    * `description` character; required: FALSE; Method description. MIAPPE V1.1
#'        (DM-90) Method description - Textual description of the method, which
#'        may extend a method defined in an external reference with specific
#'        parameters, e.g. growth stage, inoculation precise organ (leaf number).
#'    * `externalReferences` data.frame; required: FALSE; A data.frame of
#'        external reference ids. These are references to this piece of data in
#'        an external system. Could be a simple string or a URI. The
#'        `externalReferences` argument data.frame should contain the following
#'        columns:
#'
#'        + `referenceID` character; required: TRUE; The external reference ID.
#'             Could be a simple string or a URI.
#'        + `referenceSource` character; required: TRUE; An identifier for the
#'             source system or database of this reference.
#'
#'      The Examples section shows an example of how to construct the
#'      `externalReferences` element as a data.frame within the `method`
#'      argument.
#'
#'    * `formula` character; required: FALSE; For computational methods, i.e.
#'        when the method consists in assessing the trait by computing
#'        measurements, write the generic formula used for the calculation.
#'    * `methodClass` character; required: FALSE; Method class (examples:
#'        &quot;Measurement&quot;, &quot;Counting&quot;, &quot;Estimation&quot;,
#'        &quot;Computation&quot;, *etc.*).
#'    * `methodDbId` character; required: FALSE; Method unique identifier. The
#'        following cases exist:
#'
#'         + If not provided a new method will be created (generates the
#'             `methodDbId`).
#'         + When only provided within the `method` list, the method will be
#'             changed to the one existing within the database with that
#'             specific `methodDbId`.
#'         + When provided with other `method` list elements from the `method`
#'             list, the existing method with that specific `methodDbId` will
#'             updated within the database.
#'
#'    * `methodName` character; required: FALSE; Human readable name for the
#'        method. MIAPPE V1.1 (DM-88) Method Name of the method of observation.
#'    * `ontologyReference` list; required: FALSE; MIAPPE V1.1 (DM-85) Variable
#'        accession number - Accession number of the variable in the Crop
#'        Ontology. (DM-87) Trait accession number - Accession number of the
#'        trait in a suitable controlled vocabulary (Crop Ontology,
#'        Trait Ontology). (DM-89) Method accession number - Accession number of
#'        the method in a suitable controlled vocabulary (Crop Ontology, Trait
#'        Ontology). (DM-93) Scale accession number - Accession number of the
#'        scale in a suitable controlled vocabulary (Crop Ontology).
#'
#'      The `ontolgyReference` list should/can contain the following elements:
#'
#'        + `documentationLinks` data.frame; required: FALSE; Links to various
#'            ontology documentation. Each row in the `documentationLinks`
#'            data.frame should contain the following columns:
#'
#'            - `URL` character; required: FALSE; A uniform resource locator
#'                specified as a valid uniform resource identifier (URI), e.g.
#'                http://purl.obolibrary.org/obo/ro.owl
#'            - `type` character; required: FALSE; one of &quot;OBO&quot;|
#'                &quot;RDF&quot;|&quot;WEBPAGE&quot;.
#'
#'        + `ontologyDbId` character; required: TRUE; Ontology database unique
#'            identifier.
#'        + `ontologyName` character; required: TRUE; Ontology name, e.g. the
#'            Crop Ontology.
#'        + `version` character; required: FALSE; Ontology version (no specific
#'            format).
#'
#'      The Examples section shows an example of how to construct the
#'      `ontologyReference` element as a list within the `method` argument.
#' @param observationVariableName character; required: FALSE; Variable name
#'    (usually a short name).
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
#' @param scale list; required: FALSE; Scale metadata used for the observation
#'    variable. The `scale` list can contain the following elements:
#'
#'    * `additionalInfo` list; required: FALSE; Additional arbitrary information
#'        . If provided use the following construct list(additionalProp1 =
#'        "string", additionalProp2 =  "string", additionalProp3 = "string").
#'
#'      The Examples section shows an example on how to construct the
#'      `additionalInfo` element as a list within the `scale` argument.
#'
#'    * `dataType` character; required: FALSE; Class of the scale, entries can
#'        be:
#'
#'        + &quot;Code&quot; -  This scale class is exceptionally used to
#'           express complex traits. Code is a nominal scale that combines the
#'           expressions of the different traits composing the complex trait.
#'           For example a severity trait might be expressed by a 2 digit and 2
#'           character code. The first 2 digits are the percentage of the plant
#'           covered by a fungus and the 2 characters refer to the delay in
#'           development, e.g. &quot;75VD&quot; means &quot;75 %&quot; of the
#'           plant is infected and the plant is very delayed.
#'        + &quot;Date&quot; - The date class is for events expressed in a time
#'           format, coded in the ISO 8601 standard extended format, where date,
#'           time and time zone information needs to be provided (check for
#'           example [https://www.w3.org/TR/NOTE-datetime](https://www.w3.org/TR/NOTE-datetime).
#'        + &quot;Duration&quot; - The Duration class is for time elapsed
#'           between two events expressed in a time format, e.g. days, hours,
#'           months.
#'        + &quot;Nominal&quot; - Categorical scale that can take one of a
#'           limited and fixed number of categories. There is no intrinsic
#'           ordering to the categories.
#'        + &quot;Numerical&quot; - Numerical scales express the trait with real
#'           numbers. The numerical scale defines the unit e.g. centimeter, ton
#'           per hectare, branches.
#'        + &quot;Ordinal&quot; - Ordinal scales are scales composed of ordered
#'           categories.
#'        + &quot;Text&quot; - A free text is used to express the trait.
#'    * `decimalPlaces` integer; required: FALSE; For numerical, number of
#'        decimal places to be reported.
#'    * `externalReferences` data.frame; required: FALSE; A data.frame of
#'        external reference ids. These are references to this piece of data in
#'        an external system. Could be a simple string or a URI. The
#'        `externalReferences` argument data.frame should contain the following
#'        columns:
#'
#'        + `referenceID` character; required: TRUE; The external reference ID.
#'             Could be a simple string or a URI.
#'        + `referenceSource` character; required: TRUE; An identifier for the
#'             source system or database of this reference.
#'
#'      The Examples section shows an example of how to construct the
#'      `externalReferences` element as a data.frame within the `scale`
#'      argument.
#'
#'    * `ontologyReference` list; required: FALSE; MIAPPE V1.1 (DM-85) Variable
#'        accession number - Accession number of the variable in the Crop
#'        Ontology. (DM-87) Trait accession number - Accession number of the
#'        trait in a suitable controlled vocabulary (Crop Ontology,
#'        Trait Ontology). (DM-89) Method accession number - Accession number of
#'        the method in a suitable controlled vocabulary (Crop Ontology, Trait
#'        Ontology). (DM-93) Scale accession number - Accession number of the
#'        scale in a suitable controlled vocabulary (Crop Ontology).
#'
#'      The `ontolgyReference` list should/can contain the following elements:
#'
#'        + `documentationLinks` data.frame; required: FALSE; Links to various
#'            ontology documentation. Each row in the `documentationLinks`
#'            data.frame should contain the following columns:
#'
#'            - `URL` character; required: FALSE; A uniform resource locator
#'                specified as a valid uniform resource identifier (URI), e.g.
#'                http://purl.obolibrary.org/obo/ro.owl
#'            - `type` character; required: FALSE; one of &quot;OBO&quot;|
#'                &quot;RDF&quot;|&quot;WEBPAGE&quot;.
#'
#'        + `ontologyDbId` character; required: TRUE; Ontology database unique
#'            identifier.
#'        + `ontologyName` character; required: TRUE; Ontology name, e.g. the
#'            Crop Ontology.
#'        + `version` character; required: FALSE; Ontology version (no specific
#'            format).
#'
#'      The Examples section shows an example of how to construct the
#'      `ontologyReference` element as a list within the `scale` argument.
#'
#'    * `scaleDbId` character; required: FALSE; Unique identifier of the scale.
#'        The following cases exist:
#'
#'         + If not provided a new scale will be created (generates the
#'             `scaleDbId`).
#'         + When only provided within the `scale` list, the scale will be
#'             changed to the one existing within the database with that
#'             specific `scaleDbId`.
#'         + When provided with other `scale` list elements from the `scale`
#'             list, the existing scale with that specific `scaleDbId` will
#'             updated within the database.
#'
#'    * `scaleName` character; required: FALSE; Name of the scale. MIAPPE V1.1
#'        (DM-92) Scale Name of the scale associated with the variable.
#'    * `validValues` list; required: FALSE; Valid values for the scale. The
#'        `validValues` list can contain the following elements:
#'
#'        + `categories` data.frame; required: FALSE;	data.frame of possible
#'            values with optional labels. Each row in the `categories`
#'            data.frame must have the following columns:
#'
#'            - `label` character; required: FALSE; A text label for a category.
#'            - `value` character; required: FALSE; The actual value for a category.
#'
#'        + `max` integer; required: FALSE; Maximum value (used for field data
#'            capture control) for numerical and date scales.
#'        + `min` integer; required: FALSE; Minimum value (used for field data
#'            capture control) for numerical and date scales.
#'
#'      The Examples section shows an example of how to construct the
#'      `validValues` element as a list within the `scale` argument.
#' @param scientist character; required: FALSE; Name of scientist submitting the
#'    variable.
#' @param status character; required: FALSE; Variable status, examples:
#'    "recommended", "obsolete", "legacy", *etc.*
#' @param submissionTimestamp character; required: FALSE; The date and time
#'    when the variable was added. Coded in the ISO 8601 standard extended
#'    format, where date, time and time zone information needs to be provided
#'    (check for example https://www.w3.org/TR/NOTE-datetime).
#' @param synonyms vector of type character; required: FALSE; Other variable
#'    names; default: &quot;&quot;, when using multiple values supply as c(
#'    &quot;value1&quot;, &quot;value2&quot;).
#' @param trait list; required: FALSE; Trait metadata used for the observation
#'    variable.  The `trait` list can contain the following elements:
#'
#'    * `additionalInfo` list; required: FALSE; Additional arbitrary information
#'        . If provided use the following construct list(additionalProp1 =
#'        "string", additionalProp2 =  "string", additionalProp3 = "string").
#'
#'      The Examples section shows an example on how to construct the
#'      `additionalInfo` element as a list within the `trait` argument.
#'
#'    * `alternativeAbbreviations` vector of type character; required: FALSE;
#'        Other frequent abbreviations of the trait, if any. These abbreviations
#'        do not have to follow a convention; default: &quot;&quot;, when using
#'        multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#'    * `attribute` character; required: FALSE; A trait can be decomposed as
#'        &quot;Trait&quot; = &quot;Entity&quot; + &quot;Attribute&quot;, the
#'        attribute is the observed feature (or characteristic) of the entity,
#'        e.g. for &quot;grain colour&quot;, attribute = &quot;colour&quot;.
#'    * `entity` character; required: FALSE; A trait can be decomposed as
#'        &quot;Trait&quot; = &quot;Entity&quot; + &quot;Attribute&quot;, the
#'        entity is the part of the plant that the trait refers to, e.g. for
#'        &quot;grain colour&quot;, entity = &quot;grain&quot;.
#'    * `externalReferences` data.frame; required: FALSE; A data.frame of
#'        external reference ids. These are references to this piece of data in
#'        an external system. Could be a simple string or a URI. The
#'        `externalReferences` argument data.frame should contain the following
#'        columns:
#'
#'        + `referenceID` character; required: TRUE; The external reference ID.
#'             Could be a simple string or a URI.
#'        + `referenceSource` character; required: TRUE; An identifier for the
#'             source system or database of this reference.
#'
#'      The Examples section shows an example of how to construct the
#'      `externalReferences` element as a data.frame within the `trait`
#'      argument.
#'
#'    * `mainAbbreviation` character; required: FALSE; Main abbreviation for
#'        trait name, e.g. &quot;Carotenoid content&quot; =&gt; &quot;CC&quot;.
#'    * `ontologyReference` list; required: FALSE; MIAPPE V1.1 (DM-85) Variable
#'        accession number - Accession number of the variable in the Crop
#'        Ontology. (DM-87) Trait accession number - Accession number of the
#'        trait in a suitable controlled vocabulary (Crop Ontology,
#'        Trait Ontology). (DM-89) Method accession number - Accession number of
#'        the method in a suitable controlled vocabulary (Crop Ontology, Trait
#'        Ontology). (DM-93) Scale accession number - Accession number of the
#'        scale in a suitable controlled vocabulary (Crop Ontology).
#'
#'      The `ontolgyReference` list should/can contain the following elements:
#'
#'        + `documentationLinks` data.frame; required: FALSE; Links to various
#'            ontology documentation. Each row in the `documentationLinks`
#'            data.frame should contain the following columns:
#'
#'            - `URL` character; required: FALSE; A uniform resource locator
#'                specified as a valid uniform resource identifier (URI), e.g.
#'                http://purl.obolibrary.org/obo/ro.owl
#'            - `type` character; required: FALSE; one of &quot;OBO&quot;|
#'                &quot;RDF&quot;|&quot;WEBPAGE&quot;.
#'
#'        + `ontologyDbId` character; required: TRUE; Ontology database unique
#'            identifier.
#'        + `ontologyName` character; required: TRUE; Ontology name, e.g. the
#'            Crop Ontology.
#'        + `version` character; required: FALSE; Ontology version (no specific
#'            format).
#'
#'      The Examples section shows an example of how to construct the
#'      `ontologyReference` element as a list within the `trait` argument.
#'
#'    * `status` character; required: FALSE; Trait status, e.g.
#'        &quot;recommended&quot;, &quot;obsolete&quot;, &quot;legacy&quot;,
#'         *etc.*.
#'    * `synonyms` vector of type character; required: FALSE; Other trait names;
#'        default: &quot;&quot;, when using multiple values supply as c(
#'        &quot;value1&quot;, &quot;value2&quot;).
#'    * `traitClass` character; required: FALSE; Trait class, examples:
#'        &quot;morphological&quot;, &quot;phenological&quot;,
#'        &quot;agronomical&quot;, &quot;physiological&quot;,
#'        &quot;abiotic stress&quot;, &quot;biotic stress&quot;,
#'        &quot;biochemical&quot;, &quot;quality traits&quot;,
#'        &quot;fertility&quot;, *etc.*).
#'    * `traitDbId` character; required: FALSE; The data base ID which uniquely
#'        identifies a trait. The following cases exist:
#'
#'         + If not provided a new trait will be created (generates the
#'             `traitDbId`).
#'         + When only provided within the `trait` list, the trait will be
#'             changed to the one existing within the database with that
#'             specific `traitDbId`.
#'         + When provided with other `trait` list elements from the `trait`
#'             list, the existing trait with that specific `traitDbId` will
#'             updated within the database.
#'
#'    * `traitDescription` character; required: FALSE; The description of a
#'        trait.
#'    * `traitName` character; required: FALSE; The human readable name of a
#'        trait. MIAPPE V1.1 (DM-86) Trait - Name of the (plant or
#'        environmental) trait under observation.
#'
#' @details Update an existing Observation Variable
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Phenotyping/2.0#/Observation%20Variables/put_variables__observationVariableDbId_ }{BrAPI SwaggerHub}
#'
#' @family brapi-phenotyping
#' @family Observation Variables
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' additionalInfo <- list(dummyData = "TRUE",
#'                        example = "post_variables")
#' commonCropName <- "Tomatillo"
#' contextOfUse <- c("Trial evaluation", "Nursery evaluation")
#' defaultValue <- "2.0"
#' documentationURL <- "https://wiki.brapi.org/documentation.html"
#' externalReferences <-
#'   data.frame(referenceID = c("doi:10.155454/12341234",
#'                              "http://purl.obolibrary.org/obo/ro.owl",
#'                              "75a50e76"),
#'              referenceSource = c("DOI",
#'                                  "OBO Library",
#'                                  "Remote Data Collection Upload Tool"))
#' growthStage <- "flowering"
#' institution <- "The BrAPI Institute"
#' language <- "en"
#' ## Use an existing method
#' # method <- list(methodDbId = "method_variabble1")
#' ## Or create a new method (automatically adds a methodDbId)
#' method <- list(
#'   additionalInfo = list(dummyData = "TRUE",
#'                         example = "post_variables add method"),
#'   bibliographicalReference = "Smith, 1893, Really Cool Paper, Popular Journal",
#'   description = "Discriminant for root finding of quadratic functions",
#'   externalReferences = data.frame(
#'     referenceID = c("doi:10.155454/12341234",
#'                     "http://purl.obolibrary.org/obo/ro.owl",
#'                     "75a50e76"),
#'     referenceSource = c("DOI",
#'                         "OBO Library",
#'                         "Remote Data Collection Upload Tool")),
#'   formula = "b^2 - 4ac",
#'   methodClass = "Computation",
#'   methodName = "Discriminant",
#'   ontologyReference = list(
#'     documentationLinks = data.frame(
#'       URL = c("http://purl.obolibrary.org/obo/ro.owl",
#'               "https://en.wikipedia.org/wiki/Method"),
#'       type = c("OBO",
#'                "WEBPAGE")),
#'     ontologyDbId = "ontology_attribute1",
#'     ontologyName = "Ontology.org",
#'     version = "17"))
#' observationVariableName <- "Variable Name"
#' ontologyReference <- list(
#'   documentationLinks = data.frame(
#'     URL = c("http://purl.obolibrary.org/obo/ro.owl",
#'             "https://en.wikipedia.org/wiki/Variable"),
#'     type = c("OBO",
#'              "WEBPAGE")),
#'   ontologyDbId = "ontology_variable1",
#'   ontologyName = "Ontology.org",
#'   version = "17")
#' ## Use an existing scale
#' # scale <- list(scaleDbId = "scale_variabble1")
#' ## Or create a new scale (automatically adds a scaleDbId)
#' scale <- list(
#'   additionalInfo = list(dummyData = "TRUE",
#'                         example = "post_variables add scale"),
#'   dataType = "Numerical",
#'   decimalPlaces = 2,
#'   externalReferences = data.frame(
#'     referenceID = c("doi:10.155454/12341234",
#'                     "http://purl.obolibrary.org/obo/ro.owl",
#'                     "75a50e76"),
#'     referenceSource = c("DOI",
#'                         "OBO Library",
#'                         "Remote Data Collection Upload Tool")),
#'   ontologyReference = list(
#'     documentationLinks = data.frame(
#'       URL = c("http://purl.obolibrary.org/obo/ro.owl",
#'               "https://en.wikipedia.org/wiki/Scale"),
#'       type = c("OBO",
#'                "WEBPAGE")),
#'     ontologyName = "Ontology.org",
#'     version = "17"),
#'   scaleName = "Meters",
#'   validValues = list(
#'     categories = data.frame(
#'       label = c("low", "medium", "high"),
#'       value = c("0", "5", "10")),
#'     max = 9999,
#'     min = 2))
#' scientist <- "Dr. Bob Robertson"
#' status <- "recommended"
#' submissionTimestamp <- "2021-09-18T10:56:10.855Z"
#' synonyms <- c("Tomatillo Height",
#'               "Stalk Height")
#' ## Use an existing trait
#' # trait <- list(traitDbId = "trait_variabble1")
#' ## Or create a new trait (automatically adds a traitDbId)
#' trait <- list(
#'   additionalInfo = list(dummyData = "TRUE",
#'                          example = "post_variables add trait"),
#'   alternativeAbbreviations = c("PHght", "Hght"),
#'   attribute = "height",
#'   entity = "plant",
#'   externalReferences = data.frame(
#'     referenceID = c("doi:10.155454/12341234",
#'                     "http://purl.obolibrary.org/obo/ro.owl",
#'                     "75a50e76"),
#'     referenceSource = c("DOI",
#'                         "OBO Library",
#'                         "Remote Data Collection Upload Tool")),
#'   mainAbbreviation = "PH",
#'   ontologyReference = list(
#'     documentationLinks = data.frame(
#'       URL = c("http://purl.obolibrary.org/obo/ro.owl",
#'               "https://www.cropontology.org/terms/CO_323:0000024/"),
#'       type = c("OBO",
#'                "WEBPAGE")),
#'     ontologyDbId = "ontology_variable1",
#'     ontologyName = "Ontology.org",
#'     version = "17"),
#'   status = "recommended",
#'   synonyms = c("Height",
#'                "Plant Height",
#'                "Stalk Height",
#'                "Canopy Height"),
#'   traitClass = "agronomical",
#'   traitDescription = "The height of plant from ground to top part",
#'   traitName = "Plant height")
#' ## POST /variables to add a new variable
#' out <- brapi_post_variables(
#'   con = con,
#'   additionalInfo = additionalInfo,
#'   commonCropName = commonCropName,
#'   contextOfUse = contextOfUse,
#'   defaultValue = defaultValue,
#'   documentationURL = documentationURL,
#'   externalReferences = externalReferences,
#'   growthStage = growthStage,
#'   institution = institution,
#'   language = language,
#'   method = method,
#'   observationVariableName = observationVariableName,
#'   ontologyReference = ontologyReference,
#'   scale = scale,
#'   scientist = scientist,
#'   submissionTimestamp = submissionTimestamp,
#'   synonyms = synonyms,
#'   trait = trait)
#' ## Obtain the observationVariableDbId of the new variable
#' observationVariableDbId <- unique(out$observationVariableDbId)
#' methodDbId <- unique(out$method.methodDbId)
#' scaleDbId <- unique(out$scale.scaleDbId)
#' traitDbId <- unique(out$trait.traitDbId)
#' ## Update the information of the new observation variable
#' brapi_put_variables_observationVariableDbId(
#'   con = con,
#'   observationVariableDbId = observationVariableDbId,
#'   additionalInfo = list(
#'     dummyData = "TRUE",
#'     example = "put_variables_observationVariableDbId"),
#'   commonCropName = "Paw Paw",
#'   method = list( # update method
#'     methodDbId = methodDbId,
#'     methodName = "Root finding discriminant"),
#'   scale = list( # replace scale
#'     scaleDbId = "scale_variable2"))
#' }
#'
#' @export
brapi_put_variables_observationVariableDbId <- function(con = NULL,
                                                        observationVariableDbId = '',
                                                        additionalInfo = list(),
                                                        commonCropName = '',
                                                        contextOfUse = '',
                                                        defaultValue = '',
                                                        documentationURL = '',
                                                        externalReferences = '',
                                                        growthStage = '',
                                                        institution = '',
                                                        language = '',
                                                        method = list(),
                                                        observationVariableName = '',
                                                        ontologyReference = list(),
                                                        scale = list(),
                                                        scientist = '',
                                                        status = '',
                                                        submissionTimestamp = '',
                                                        synonyms = '',
                                                        trait = list()) {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "observationVariableDbId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_PUT_callURL(usedArgs = usedArgs,
                                          callPath = "/variables/{observationVariableDbId}",
                                          reqArgs = "observationVariableDbId",
                                          packageName = "BrAPI-Phenotyping",
                                          callVersion = 2.0)
  ## Build the Body
  callbody <- brapirv2:::brapi_PUT_callBody(usedArgs = usedArgs,
                                            reqArgs = "observationVariableDbId")

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_PUT(url = callurl, body = callbody, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_put_variables_observationVariableDbId")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
