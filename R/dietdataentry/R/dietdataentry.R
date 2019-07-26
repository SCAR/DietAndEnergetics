#' \pkg{dietdataentry}
#'
#' @name dietdataentry
#' @docType package
#' @references \url{http://data.aad.gov.au/trophic}
#' @importFrom assertthat assert_that is.flag is.number is.string
#' @importFrom httr content GET
#' @importFrom lubridate now ymd ymd_hms
#' @importFrom R.cache memoizedCall setCacheRootPath
#' @importFrom readr cols read_csv
#' @importFrom readxl excel_sheets read_excel
#' @importFrom stats as.formula na.omit
#' @importFrom stringdist stringsim
#' @importFrom stringr str_match_all str_split str_trim
#' @importFrom worrms wm_records_common wm_records_name wm_records_taxamatch
#' @importFrom utils head str tail
NULL
