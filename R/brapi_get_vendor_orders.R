#' @title
#' get /vendor/orders
#'
#' @description
#' List current available orders
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param orderId character; required: ; The order identifier returned by the
#'    vendor, when the order was successfully submitted with **POST**
#'    **/vendor/orders** as implemented in the `brapi_post_orders()` function.
#' @param submissionId character; required: ; The submission identifier returned
#'    by the vendor, when a set of plates was successfully submitted  with
#'    **POST /vendor/orders** as implemented in the `brapi_post_orders()`
#'    function.
#' @param page integer; required: FALSE; Used to request a specific page of data
#'    to be returned. The page indexing starts at 0 (the first page is
#'    `page = 0`). Default is `0`.
#' @param pageSize integer; required: FALSE; The size of the pages to be
#'    returned. Default is `1000`.
#'
#' @details List current available orders
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Vendor/get_vendor_orders }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Vendor
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_vendor_orders(con = con)
#' }
#'
#' @export
brapi_get_vendor_orders <- function(con = NULL,
                                    orderId = '',
                                    submissionId = '',
                                    page = 0,
                                    pageSize = 1000) {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/vendor/orders",
                                          reqArgs = "",
                                          packageName = "BrAPI-Genotyping",
                                          callVersion = 2.0)

  try({
    ## Make the call and receive the response
    resp <- brapirv2:::brapi_GET(url = callurl, usedArgs = usedArgs)
    ## Extract the content from the response object in human readable form
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    ## Convert the content object into a data.frame
    out <- brapirv2:::brapi_result2df(cont, usedArgs)
  })
  ## Set class of output
  class(out) <- c(class(out), "brapi_get_vendor_orders")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
