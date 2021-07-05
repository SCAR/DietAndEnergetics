#' Validate data after parsing
#'
#' @param x list: as returned by [parse_diet()], [parse_isotopes()], etc
#' @param records_type string: the data type "diet", "isotopes", "energetics", "dna_diet", "lipids", or "guess"
#' @param check_taxonomy logical: check the taxonomy against worrms?
#'
#' @return The input data.frame `x`, possibly with column types changed
#'
#' @export
validate_data <- function(x, records_type = "guess", check_taxonomy = FALSE) {
    assert_that(is.string(records_type))
    records_type <- tolower(records_type)
    if (records_type == "guess") {
        if ("prey_name" %in% names(x$records)) {
            ## diet or DNA diet
            records_type <- if ("primer_source_id" %in% names(x$records)) "dna_diet" else "diet"
        } else {
            if ("isotopes_are_adjusted" %in% names(x$records)) {
                records_type <- "isotopes"
            } else if ("measurement_class" %in% names(x$records)) {
                records_type <- "lipids"
            } else {
                records_type <- "energetics"
            }
        }
    }
    records_type <- match.arg(records_type, c("diet", "isotopes", "energetics", "dna_diet", "lipids"))

    ## check column data types
    this_schema <- jsonlite::fromJSON(system.file(file.path("extdata", "schema", paste0(records_type, ".json")), package = "dietdataentry"))
    ## add columns for output that aren't in input spreadsheets
    this_schema$source_details <- list(type = "character")
    this_schema$source_doi <- list(type = "character")

    for (nm in names(x)) {
        if (!nm %in% names(this_schema)) stop("column '", nm, "' is not expected")
        if (!inherits(x[[nm]], this_schema[[nm]]$type)) x[[nm]] <- type_cast(x[[nm]], to = this_schema[[nm]]$type, colname = nm)
    }
    #if (!all(expected %in% names_provided)) stop("columns missing from spreadsheet: ", paste(setdiff(expected, names_provided), collapse = ", "))
    #if (!all(names_provided %in% expected)) warning("ignoring unexpected columns in spreadsheet: ", paste(setdiff(names_provided, expected), sep = ", "))

    if (any(duplicated(x$record_id))) stop("duplicate record_id values")

    if (!all(x$quality_flag %in% c("G","Q"))) stop("quality values")
    if (!all(x$is_secondary_data %in% c("Y","N"))) stop("secondary_data values")

    if (isTRUE(check_taxonomy)) {
        warning("taxonomic check not implemented yet")
    }
    x
}

type_cast <- function(z, to, colname) {
    ## some silent conversions
    if ((inherits(z, "numeric") && to == "integer" && all(is.na(z) | floor(z) == z)) ||
        (inherits(z, "logical") && all(is.na(z)))) {
        return(as(z, to))
    }
    if ((to == "datetime" && inherits(z, "POSIXct")) ||
        (to == "date" && inherits(z, "Date"))) {
        return(z)
    }
    warning("column '", colname, "' of type ", class(z)[1], " should be of type ", to, ", converting")
    if (to == "date") {
        as.Date(z)
    } else {
        as(z, to)
    }
}
