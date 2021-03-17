`%eq%` <- function(x, y) !is.na(x) & !is.na(y) & x==y

cat0 <- function(...) cat(..., sep = "")

is_nonempty <- function(z) if (is.null(z)) FALSE else !is.na(z) & (!is.character(z) | nchar(z)>0)

nonempty_or_unknown <- function(z) if (is_nonempty(z)) z else "unknown"

angle_normalise <- function(x) { (x + pi) %% (2 * pi) - pi } ## normalize angle to range [-pi,pi)
deg_normalise <- function(x) angle_normalise(x / 180 * pi) / pi * 180

recase <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))

tidy_name <- function(z) str_trim(gsub("[[:space:]]+"," ", z))

flag_to_yes_no <- function(flag) {
    if (is.na(flag)) {
        "U"
    } else {
        if (flag) "Y" else "N"
    }
}

## flag in data to strict logical
parse_flag <- function(flag,treat_empty_as=TRUE,error_on_ambiguous=FALSE) {
    if (!is_nonempty(flag)) {
        treat_empty_as
    } else {
        if ((is.logical(flag) && flag) || (is.character(flag) && tolower(flag) %in% c("yes","y","1","true")) || is.numeric(flag) && flag %in% c(1)) {
            TRUE
        } else if ((is.logical(flag) && !flag) || (is.character(flag) && tolower(flag) %in% c("no","n","0","false")) || is.numeric(flag) && flag %in% c(0)) {
            FALSE
        } else {
            if (error_on_ambiguous)
                stop(sprintf("ambiguous flag value"))
            else
                NA
        }
    }
}

parse_quality_flag <- function(flag,lidx) {
    flag <- parse_flag(flag,treat_empty_as=FALSE)
    if (is.na(flag)) {
        stop(sprintf("ambiguous quality entry on row %d",lidx))
    } else {
        if (flag) "Q" else "G" ##Q=questionable, G=good
    }
}

get_existing_names <- function(dbh) {
    if (requireNamespace("dietdataentrydb", quietly = TRUE)) {
        dietdataentrydb::get_existing_names(dbh)
    } else {
        c()
    }
}

format_datecol <- function(z) {
    ## z should be a column from a data.frame
    if (inherits(z, "character")) {
        ## parse
        warning("need to check parsing of text date data into dates")
        z <- ymd(z)
    } else if (inherits(z, c("POSIXct", "POSIXlt"))) {
        ## ok, all good
    } else {
        stop("unexpected class ", class(z), " passed to format_datecol")
    }
    z
}
