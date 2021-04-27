#' \pkg{dietdataentry}
#'
#' @description Functions for checking and entering data to the SCAR Southern Ocean Diet and Energetics Database
#' 
#' @name dietdataentry
#' @docType package
#' @references \url{https://scar.org/resources/southern-ocean-diet-energetics/}
#' @importFrom assertthat assert_that is.flag is.number is.string
#' @importFrom dplyr %>% bind_rows mutate tibble
#' @importFrom httr content GET
#' @importFrom lubridate now ymd ymd_hms
#' @importFrom R.cache memoizedCall setCacheRootPath
#' @importFrom readr cols read_csv
#' @importFrom readxl excel_sheets read_excel
#' @importFrom rlang .data
#' @importFrom stats as.formula na.omit
#' @importFrom stringdist stringsim
#' @importFrom stringr str_match_all str_split str_trim
#' @importFrom worrms wm_common_id wm_record wm_records_common wm_records_name wm_records_taxamatch
#' @importFrom utils head str tail
NULL
