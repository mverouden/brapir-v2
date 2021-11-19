#' @title
#' post /variantsets/extract
#'
#' @description
#' Create new `VariantSet` based on search results
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param callSetDbIds vector of type character; required: FALSE; The `CallSet`
#'    unique database identifier(s) to search.; default: &quot;&quot;, when
#'    using multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param expandHomozygotes logical; required: FALSE; Should homozygotes be
#'    expanded (TRUE) or collapsed into a single occurrence (FALSE); default: NA
#'    , other possible values: TRUE | FALSE.
#' @param sepPhased character; required: FALSE; The string used as a separator
#'    for phased allele calls.; default: &quot;&quot;.
#' @param sepUnphased character; required: FALSE; The string used as a separator
#'    for unphased allele calls.; default: &quot;&quot;.
#' @param studyDbIds vector of type character; required: FALSE; List of unique
#'    database study identifiers to search for; default: &quot;&quot;, when
#'    using multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param studyNames vector of type character; required: FALSE; List of study
#'    names to filter search results; default: &quot;&quot;, when using multiple
#'    values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param unknownString character; required: FALSE; The string used as a
#'    representation for missing data.; default: &quot;&quot;.
#' @param variantDbIds vector of type character; required: FALSE; The unique
#'    variant database identifiers to search.; default: &quot;&quot;, when using
#'    multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#' @param variantSetDbIds vector of type character; required: FALSE; The unique
#'    VariantSet database identifiers to search.; default: &quot;&quot;, when
#'    using multiple values supply as c(&quot;value1&quot;, &quot;value2&quot;).
#'
#' @details Will perform a search for `Calls` which match the search criteria in
#'  the request made. The results of the search will be used to create a new
#'  `VariantSet` on the server. The new `VariantSet` will be returned.
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Variant%20Sets/post_variantsets_extract }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Variant Sets
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' con[["token"]] <- "YYYY"
#' brapi_post_variantsets_extract(con = con,
#'                                expandHomozygotes = TRUE,
#'                                sepPhased = "/",
#'                                sepUnphased = "|",
#'                                unknownString = "-",
#'                                variantDbIds = c("variant01",
#'                                                 "variant02"))
#' }
#'
#' @export
brapi_post_variantsets_extract <- function(con = NULL,
                                           callSetDbIds = '',
                                           expandHomozygotes = NA,
                                           sepPhased = '',
                                           sepUnphased = '',
                                           studyDbIds = '',
                                           studyNames = '',
                                           unknownString = '',
                                           variantDbIds = '',
                                           variantSetDbIds = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                           callPath = "/variantsets/extract",
                                           reqArgs = "",
                                           packageName = "BrAPI-Genotyping",
                                           callVersion = 2.0)
  ## Build the Body
  callbody <- brapirv2:::brapi_POST_callBody(usedArgs = usedArgs,
                                             reqArgs = "")

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_POST(url = callurl, body = callbody, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_post_variantsets_extract")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
