#' @title
#' post /token
#'
#' @description
#' Login into a BrAPI compatible database.
#'
#' @param con list; required: TRUE; BrAPI connection object
#'
#' @details Although the POST /token call for authentication has been deprecated
#'    after version 1.1 of the BrAPI specification, its functionality is still
#'    retained for compatibility reasons with the
#'    [Breeding Management System (BMS Pro)](https://bmspro.io/) of
#'    [Integrated Breeding Platform](https://integratedbreeding.net/) to allow
#'    users to obtain a valid token from within R and use with additional BrAPIR
#'    v2 functions.
#'
#' @return A list object of class 'brapi_con' with the authentication token filled.
#'
#' @author Maikel Verouden
#'
#' @references \href{https://github.com/plantbreeding/API/blob/V1.1/Specification/Authentication/Authentication.md}{BrAPI v1.1 Specs GitHub}
#'
#' @family brapiutils
#'
#' @examples
#' \dontrun{
#' # Create a connection object for BMS
#' con <- brapi_db()$bms_test
#' class(con) <- c("list", "brapi", "brapi_con")
#' # Add a username between the double quotes
#' con[["user"]] <- ""
#' # Add a password between the double quotes
#' con[["password"]] <- ""
#'
#' # Login
#' con <- brapi_bms_login(con = con)
#' }
#'
#' @export
brapi_bms_login <- function(con = NULL) {
  ## Create a list of used arguments
  usedArgs <- brapirv2:::brapi_usedArgs(origValues = FALSE)
  ## Check if BrAPI server can be reached given the connection details
  brapi_checkCon(con = usedArgs[["con"]], verbose = FALSE)
  ## Check validity of used and required arguments
  brapirv2:::brapi_checkArgs(usedArgs, reqArgs = "")
  ## Disable multicrop when TRUE to create correct callurl
  if (usedArgs[["con"]][["multicrop"]] == TRUE) {
    omc <- usedArgs[["con"]][["multicrop"]]
    ## Temporarily disable multicrop
    usedArgs[["con"]][["multicrop"]] <- FALSE
  } else {
    omc <- FALSE
  }
  ## Obtain the call url
  callurl <- brapirv2:::brapi_POST_callURL(usedArgs = usedArgs,
                                           callPath = "/token",
                                           reqArgs = "",
                                           packageName = "BrAPI-Core",
                                           callVersion = 2.0)
  ## Restore multicrop
  if (omc == TRUE) {
    usedArgs[["con"]][["multicrop"]] <- omc
  }
  ## Build the Body
  callbody <- list(username = usedArgs[["con"]][["user"]],
                   password = usedArgs[["con"]][["password"]],
                   grant_type = usedArgs[["con"]][["granttype"]],
                   client_id = usedArgs[["con"]][["clientid"]])

  try({
    ## Make the call and receive the response
    resp <- httr::POST(url = callurl,
                       body = callbody,
                       encode = ifelse(con$bms == TRUE, "json", "form"))
    ## Check response status
    if (resp[["status_code"]] == 401) {
      ## Status Unauthorized
      httr::stop_for_status(x = resp,
                            task = "authenticate. Check your username and password!")
    } else {
      ## Status other than unauthorized
      if (resp[["status_code"]] != 200) {
        ## Status other than Unauthorized and OK
        httr::stop_for_status(x = resp)
      } else {
        ## Status OK: Extract token out of resp(onse) from POST call
        cont <- httr::content(x = resp)
        usedArgs[["con"]][["token"]] <- cont[["access_token"]]
        usedArgs[["con"]][["expires_in"]] <- cont[["expires_in"]]
        brapirv2:::brapi_message(jsonlite::toJSON(x = cont, pretty = TRUE))
        message("Authenticated!")
      }
    }
  })
  return(usedArgs[["con"]])
}
