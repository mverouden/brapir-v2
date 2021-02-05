### Internal function for parsing the result part of the response content into
### a flattened data.frame object
#' @importFrom utils as.relistable read.csv read.delim
brapi_result2df <- function(cont, usedArgs) {
  ## Parse JSON content into a list that consists of a metadata and result
  ## element
  contList <- jsonlite::fromJSON(txt = cont)
  ## Use only the result element from the content list (contList)
  resultList <- contList[["result"]]
  ## resultList can consist of:
  ## 1) Single Response: In this type of response, the result field is an object
  ##    describing a single record of data. There should NOT be a "data" array/element
  ##    in this case and pagination does not apply. A Single Response is used when
  ##    you are requesting or modifying a single, specific data object by its DbId.
  ## 2) List Response: In this type of response, the result object only contains
  ##    a "data" array/element, which is an arbitrarily long array of objects of
  ##    the same type. Pagination request parameters and pagination metadata
  ##    fields apply to this data array.
  ## 3) Special cases: mix of Single and List response (parse "data" element and
  ##    repeat single response part to match dimensions).
}
