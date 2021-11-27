#' @title
#' get /vendor/orders/\{orderId\}/status
#'
#' @description
#' Get the status of a specific Order
#'
#' @param con list; required: TRUE; BrAPI connection object
#' @param orderId character; required: TRUE; The order identifier returned by the
#'    vendor, when the order was successfully submitted with **POST**
#'    **/vendor/orders** as implemented in the `brapi_post_orders()` function.
#'
#' @details Retrieve the current status of an order being processed
#'
#' @return data.frame
#'
#' @author Maikel Verouden
#'
#' @references \href{https://app.swaggerhub.com/apis/PlantBreedingAPI/BrAPI-Genotyping/2.0#/Vendor/get_vendor_orders__orderId__status }{BrAPI SwaggerHub}
#'
#' @family brapi-genotyping
#' @family Vendor
#'
#' @examples
#' \dontrun{
#' con <- brapi_db()$testserver
#' brapi_get_vendor_orders_orderId_status(con = con,
#'                                        orderId = "vendor_order1")
#' }
#'
#' @export
brapi_get_vendor_orders_orderId_status <- function(con = NULL,
                                                   orderId = '') {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "orderId")
  ## Obtain the call url
  callurl <- brapirv2:::brapi_GET_callURL(usedArgs = usedArgs,
                                          callPath = "/vendor/orders/{orderId}/status",
                                          reqArgs = "orderId",
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
  class(out) <- c(class(out), "brapi_get_vendor_orders_orderId_status")
  ## Show pagination information from metadata
  brapirv2:::brapi_serverinfo_metadata(cont)
  return(out)
}
