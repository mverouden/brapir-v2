#' @title
#' brapi_serverinfo_show
#'
#' @description
#' Show informative messages or not
#'
#' @param show logical; default is TRUE
#'
#' @author Maikel Verouden
#'
#' @family brapiutils
#'
#' @examples
#' \dontrun{
#' ## To show debugging output in the console
#' brapi_serverinfo_show(show = TRUE)
#' }
#'
#' @export
brapi_serverinfo_show <- function(show = TRUE) {
  stopifnot(is.logical(show))
  options(brapi_info = show)
  return(invisible())
}
