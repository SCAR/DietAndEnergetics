## todo:
## remove the as.data.frame from read_excel calls, catch all the single-column subsetting ops
## check all !is.na() usage and change to is_nonempty if appropriate
## coerce timezones in parsed worksheets to GMT - or not? does it matter?

tidy_source_details <- function(d) {
    remove_doi <- function(s) gsub("(.*)[,\\.] doi:.*","\\1.", s)
    dash_to_hyphen <- function(s) gsub("\\-+", "-", gsub(sprintf("\023"), "-", s))
    d <- gsub("[\n\r]+", " ", d)
    ch <- str_match_all(d,"^([^\\(]+)[ ]?(\\(.*)$")
    out <- sapply(ch,function(z) paste(z[1, 2], dash_to_hyphen(remove_doi(z[1, 3])), sep = " "))
    gsub("[[:space:]]+", " ", out)
}

strip_name_specials <- function(z) {
    ## strip any special characters from our name (these are used in search_worms)
    if (is.null(z) || length(z)<0) z else gsub("@.*$", "", z)
}


#' Parse source details from spreadsheet
#'
#' @param filename string: path to Excel file
#' @param verbosity numeric: 0 = silent, > 0 = give progress messages
#'
#' @return A list object with the parsed data
#'
#' @export
parse_sources <- function(filename, verbosity = 1) {
    if (is.data.frame(filename)) {
        sx <- filename
    } else {
        sheet_name <- head(intersect(c("source", "sources"), excel_sheets(filename)), 1)
        if (length(sheet_name) < 1) stop("could not find sheet named \"source\" or \"sources\" in file")
        sx <- as.data.frame(read_excel(filename, sheet = sheet_name), stringsAsFactors = FALSE)
    }
    ## check column names
    check_sheet_columns(names(sx), "sources")
    dudidx <- is.na(sx$source_id)
    if (sum(dudidx)>0) {
        if (verbosity > 0) cat("dropping", sum(dudidx), "rows from sources worksheet with missing source_id\n")
        sx <- sx[!dudidx, ]
    }
    ## check DOI present in DOI column if also in details
    if (any(!is_nonempty(sx$doi) & grepl("doi:", sx$details))) stop("DOI present in details but not DOI column?")
    ## check each DOI actually resolves
    temp <- lapply(sx$doi, function(z) if (is_nonempty(z)) resolve_doi(z) else NA)
    if (any(sapply(temp,is.null))) stop("At least one DOI does not resolve")
    if (any(is_nonempty(sx$citation))) stop("non-missing citation info, which isn't yet handled!")
    ssx <- data.frame(source_id = sx$source_id, details = tidy_source_details(sx$details),
                      notes = sx$source_notes, date_created = now(tzone = "GMT"), doi = sx$doi, stringsAsFactors = FALSE)
    list(sources = ssx, raw = sx)
}

## given the taxon table, helper function to extract rows, taking care of specials
tt_lookup <- function(nm, taxon_table, which = "name") {
    taxon_table[strip_name_specials(taxon_table[[which]]) %eq% strip_name_specials(nm), ]
}

#' Parse energetics data spreadsheet
#'
#' @param filename string: path to Excel file
#' @param dbh optional: database handle
#' @param worms_cache_directory string: path to cache directory for taxonomic data
#' @param verbosity numeric: 0 = silent, > 0 = give progress messages
#' @param refresh_worms_cache logical: if `TRUE`, refresh the taxonomic cache
#'
#' @return A list object with the parsed data
#'
#' @export
parse_energetics <- function(filename, dbh, worms_cache_directory = NULL, verbosity = 1, refresh_worms_cache = FALSE) {
    doparse("energetics", filename = filename, dbh = dbh, worms_cache_directory = worms_cache_directory, verbosity = verbosity, refresh_worms_cache = refresh_worms_cache)
}

#' Parse lipids data spreadsheet
#'
#' @param filename string: path to Excel file
#' @param dbh optional: database handle
#' @param worms_cache_directory string: path to cache directory for taxonomic data
#' @param verbosity numeric: 0 = silent, > 0 = give progress messages
#' @param refresh_worms_cache logical: if `TRUE`, refresh the taxonomic cache
#'
#' @return A list object with the parsed data
#'
#' @export
parse_lipids <- function(filename, dbh, worms_cache_directory = NULL, verbosity = 1, refresh_worms_cache = FALSE) {
    doparse("lipids", filename = filename, dbh = dbh, worms_cache_directory = worms_cache_directory, verbosity = verbosity, refresh_worms_cache = refresh_worms_cache)
}

doparse <- function(dtype, filename, dbh, existing_names, worms_cache_directory, verbosity, refresh_worms_cache) {
    ## check inputs
    assert_that(is.string(dtype))
    dtype <- match.arg(tolower(dtype), c("energetics", "lipids"))
    if (missing(existing_names)) existing_names <- c()

    if (!missing(dbh)) assert_that(inherits(dbh, c("JDBCConnection","OdbcConnection")))
    if (!file.exists(filename)) stop("file ", filename, " does not exist")

    ## start with sources
    sources <- parse_sources(filename, verbosity = verbosity)

    ## data sheet
    sheets <- excel_sheets(filename)
    if ("data" %in% sheets) {
        this_sheet <- "data"
    } else {
        warning("using the first worksheet ('", sheets[1], "')")
        this_sheet <- sheets[1]
    }
    r <- as.data.frame(read_excel(filename, sheet = this_sheet), stringsAsFactors = FALSE)
    names(r) <- tolower(names(r))
    raw_r <- r

    ## trim off any trailing rows that might have been used for calculations
    dudidx <- is.na(r$source_id)
    if (sum(dudidx) > 0) {
        if (verbosity > 0) cat("dropping", sum(dudidx), "rows from data worksheet with missing source_id\n")
        r <- r[!dudidx, ]
    }

    ## basic error checking
    ## check column names
    check_sheet_columns(names(r), dtype)

    if (any(is.na(r$taxon_sample_id))) stop("missing at least one taxon_sample_id")
    if (is.character(r$taxon_sample_id)) {
        warning("taxon_sample_id is character: converting to numeric via as.factor. Check this!")
        r$taxon_sample_id <- as.numeric(as.factor(r$taxon_sample_id))
    }
    if (any(!is_nonempty(r$entered_by))) stop("missing at least one entered_by entry")

    ## convert date columns, if needed, and format
    for (datecol in c("observation_date_start", "observation_date_end")) r[, datecol] <- format_datecol(r[, datecol])

    ## force lower case for various columns
    force_lc <- c("taxon_life_stage", "taxon_breeding_stage", "taxon_sex", "body_part_used", "measurement_name")
    for (k in force_lc) r[, k] <- tolower(r[, k])

    ## check unique values in columns with controlled vocabularies
    if (verbosity > 0) cat("Checking controlled vocabularies ...\n")
    controlled_cols <- c("body_part_used", "taxon_breeding_stage", "taxon_sex", "taxon_life_stage", "measurement_name", "measurement_units")
    for (k in controlled_cols) checkvals(r[, k], k)
    if (dtype == "lipids") checkvals(r$measurement_class, "measurement_class")
    if (verbosity > 0) cat("done.\n")

    ## find existing species names, if we've been given the handle to the database
    if (length(existing_names) < 1 && !missing(dbh)) existing_names <- get_existing_names(dbh)

    ## first parse all taxa
    temp <- check_names(r$taxon_name, r$revised_taxon_name, existing_names = existing_names, cache_directory = worms_cache_directory, verbosity = verbosity, force = refresh_worms_cache)
    unmatched_names <- temp$unmatched_names
    taxon_table <- temp$taxon_table

    if (verbosity > 0) cat("\n")

    ## iterate through rows of r, parsing each record
    all_records <- list()
    for (lidx in seq_len(nrow(r))) {
        processed_cols <- c()
        temppred <- if (is_nonempty(r$revised_taxon_name[lidx])) r$revised_taxon_name[lidx] else r$taxon_name[lidx]
        temppred <- tidy_name(temppred)
        if (verbosity > 1) cat0("adding ", tt_lookup(temppred, taxon_table)$resolved_name, ": ", r$measurement_name[lidx], "\n")
        temp_insert <- list(taxon_name = tt_lookup(temppred, taxon_table)$resolved_name)
        temp_insert$taxon_name_original <- strip_name_specials(if (is_nonempty(r$revised_taxon_name[lidx]) && is_nonempty(r$taxon_name[lidx])) r$taxon_name[lidx] else temppred)
        this_tax <- tt_lookup(temppred, taxon_table)
        if (nrow(this_tax) == 1 && !is.na(this_tax$aphia_id)) {
            temp_insert$taxon_aphia_id <- this_tax$aphia_id
            for (tcol in c("worms_rank", "worms_kingdom", "worms_phylum", "worms_class", "worms_order", "worms_family", "worms_genus")) temp_insert[[paste0("taxon_", tcol)]] <- this_tax[[tcol]]
        } else {
            temp_insert$taxon_aphia_id <- NA_integer_
            for (tcol in c("taxon_worms_rank", "taxon_worms_kingdom", "taxon_worms_phylum", "taxon_worms_class", "taxon_worms_order", "taxon_worms_family", "taxon_worms_genus")) temp_insert[[tcol]] <- NA_character_
        }
        processed_cols <- c(processed_cols, c("taxon_name", "revised_taxon_name", "taxon_aphia_id"))
        ## some fairly straightforward ones
        ## re-add taxon_group_soki once populated
        for (blah in c("taxon_sample_count", "taxon_sample_id", "physical_sample_id", "analytical_replicate_id", "analytical_replicate_count")) {
            if (!blah %in% names(r)) stop("expected column ", blah, " is missing from the sheet")
            temp <- r[lidx, blah]
            if (!is.na(temp) && is.na(as.numeric(temp))) {
                cat(str(temp))
                stop("expecting numeric type for ", blah)
            }
            temp <- as.numeric(temp)
            processed_cols <- c(processed_cols, blah)
            temp_insert[[blah]] <- temp
        }
        for (blah in c("original_record_id", "taxon_life_stage", "taxon_breeding_stage", "body_part_used", "observation_date_notes")) {
            temp <- r[lidx, blah]
            processed_cols <- c(processed_cols, blah)
            if (blah %in% c("taxon_life_stage", "taxon_breeding_stage")) temp <- gsub(", ", ",", temp)
            if (grepl("/", temp) && grepl("stage", blah)) stop("taxon life_stage/breeding_stage col contains / as separator?")
            temp_insert[[blah]] <- temp
        }

        temp_insert$taxon_sex <- nonempty_or_unknown(r$taxon_sex[lidx])
        processed_cols <- c(processed_cols, "taxon_sex")

        ## dates
        temp_insert$observation_date_start <- r$observation_date_start[lidx]
        temp_insert$observation_date_end <- r$observation_date_end[lidx]
        processed_cols <- c(processed_cols, c("observation_date_start", "observation_date_end"))

        ## location
        temp_insert$location <- tidy_name(r$location[lidx])
        if (!is.na(r$south[lidx]) && abs(r$south[lidx]) > 90) stop("location south on line ", lidx, " is beyond 90 (", r$south[lidx], ")")
        temp_insert$south <- r$south[lidx]
        temp_insert$west <- deg_normalise(r$west[lidx])
        if (!is.na(r$north[lidx])) {
            if (abs(r$north[lidx]) > 90) stop("location north on line ", lidx, " is beyond 90 (", r$north[lidx], ")")
            if (r$south[lidx] > r$north[lidx]) stop("location south (", r$south[lidx], ") on line ", lidx, " is greater than the north value (", r$north[lidx], ")")
        }
        temp_insert$north <- r$north[lidx]
        temp_insert$east <- deg_normalise(r$east[lidx])
        processed_cols <- c(processed_cols, c("location", "east", "west", "south", "north"))
        ## altitude
        if (!is.na(r$altitude_min[lidx]) && !is.na(r$altitude_max[lidx]) && r$altitude_min[lidx]>r$altitude_max[lidx]) stop("altitude_min (", r$altitude_min[lidx], ") is greater than altitude_max (", r$altitude_max[lidx], ") on line ", lidx)
        temp_insert$altitude_min <- r$altitude_min[lidx]
        temp_insert$altitude_max <- r$altitude_max[lidx]
        processed_cols <- c(processed_cols, c("altitude_min", "altitude_max"))
        ## depth
        tempmin <- abs(r$depth_min[lidx])
        tempmax <- abs(r$depth_max[lidx])
        if (!is.na(tempmin) && !is.na(tempmax) && tempmin > tempmax) stop("depth_min (", tempmin, ") is greater than depth_max (", tempmax, ") on line ", lidx)
        temp_insert$depth_min <- tempmin
        temp_insert$depth_max <- tempmax
        processed_cols <- c(processed_cols, c("depth_min", "depth_max"))

        ## actual measurements data
        temp_insert$samples_were_pooled <- flag_to_yes_no(parse_flag(r$samples_were_pooled[lidx], treat_empty_as = FALSE))
        processed_cols <- c(processed_cols, "samples_were_pooled")
        if (!is_nonempty(r$measurement_name[lidx])) stop("empty measurement_name at row", lidx)
        if (is_nonempty(r$measurement_name[lidx]) && !grepl("ratio", r$measurement_name[lidx]) && !is_nonempty(r$measurement_units[lidx])) stop("missing units at line", lidx, "? (measurement_name '", r$measurement_name[lidx], "')")
        if (is_nonempty(r$measurement_variability_value[lidx]) && !is_nonempty(r$measurement_variability_type[lidx])) warning("empty measurement_variability_type at row", lidx)
        dcols <- c("measurement_name", "measurement_min_value", "measurement_max_value", "measurement_mean_value", "measurement_variability_value", "measurement_variability_type", "measurement_units", "measurement_method")
        if (dtype == "lipids") dcols <- c(dcols, "measurement_class")
        for (blah in dcols) temp_insert[[blah]] <- r[lidx, blah]
        processed_cols <- c(processed_cols, dcols)

        ## source_id
        temp_insert$source_id <- r$source_id[lidx]
        processed_cols <- c(processed_cols, "source_id")

        ## quality and other flags
        temp_insert$quality_flag <- parse_quality_flag(r$is_dodgy[lidx], lidx)
        temp_insert$entered_by <- r$entered_by[lidx]
        temp_insert$is_secondary_data <- flag_to_yes_no(parse_flag(r$is_secondary_data[lidx], treat_empty_as = FALSE))
        temp_insert$is_public_flag <- flag_to_yes_no(parse_flag(r$is_public[lidx], treat_empty_as = TRUE))
        processed_cols <- c(processed_cols, c("is_dodgy", "entered_by", "is_secondary_data", "is_public"))

        ## notes
        temp_insert$notes <- r$notes[lidx]
        processed_cols <- c(processed_cols, "notes")

        temp_insert$last_modified <- now(tzone = "GMT")

        ##cat(str(temp_insert))
        if (!all(names(r) %in% processed_cols)) stop("unprocessed columns: ", paste(setdiff(names(r), processed_cols), collapse = ", "))
        temp <- setdiff(processed_cols, c(names(r), "taxon_aphia_id"))
        if (length(temp) > 0) stop("processed columns that apparently don't exist in the spreadsheet: ", paste(temp, collapse = ", "))

        all_records <- c(all_records, list(temp_insert))
    }
    if (verbosity > 0) cat(length(all_records), "data records parsed.\n")
    ## to data frame
    all_records <- do.call(rbind, lapply(all_records, as.data.frame, stringsAsFactors = FALSE))
    if (length(unmatched_names) > 0) warning("unmatched names: ", paste(unmatched_names, collapse = ", "))
    cat("Unique dates found in these records:\n  ")
    temp <- unique(na.omit(c(all_records$observation_date_start, all_records$observation_date_end)))
    cat(paste(format(sort(temp)), collapse = ", "))

    cord <- columns_order(dtype)
    if (!all(names(all_records) %in% cord)) stop("unexpected column names: ", paste(setdiff(names(all_records), cord), collapse = ", "))
    all_records <- all_records[, intersect(cord, names(all_records))]

    list(sources = sources$sources, records = all_records, raw = list(sources = sources$raw, records = raw_r))
}


#' Parse isotopes data spreadsheet
#'
#' @param filename string: path to Excel file
#' @param dbh optional: database handle
#' @param existing_names character: vector of existing taxon names
#' @param worms_cache_directory string: path to cache directory for taxonomic data
#' @param verbosity numeric: 0 = silent, > 0 = give progress messages
#' @param refresh_worms_cache logical: if `TRUE`, refresh the taxonomic cache
#'
#' @return A list object with the parsed data
#' @examples
#' \dontrun{
#'   ## use the example file bundled with the package
#'   filename <- system.file("extdata/example_isotope_data.xls",
#'                           package = "dietdataentry")
#'   x <- parse_isotopes(filename)
#'
#'   ## same, but using a local cache for the taxon lookups, which will be faster
#'   x <- parse_isotopes(filename, worms_cache_directory = "~/.Rcache")
#' }
#'
#' @export
parse_isotopes <- function(filename, dbh, existing_names, worms_cache_directory = NULL, verbosity = 1, refresh_worms_cache = FALSE) {
    ## check inputs
    if (!missing(dbh)) assert_that(inherits(dbh, c("JDBCConnection", "OdbcConnection")))
    if (!file.exists(filename)) stop("file ", filename, " does not exist")
    if (missing(existing_names)) existing_names <- c()

    ## start with sources
    sources <- parse_sources(filename, verbosity = verbosity)
    valid_source_ids <- sources$sources$source_id

    ## isotopes data sheet
    sheets <- excel_sheets(filename)
    if ("isotopes" %in% sheets) {
        this_sheet <- "isotopes"
    } else if ("trait" %in% sheets) {
        this_sheet <- "trait"
    } else {
        warning("could not find worksheet named \"isotopes\" or \"trait\", using the first worksheet (\"", sheets[1], "\")")
        this_sheet <- sheets[1]
    }
    r <- as.data.frame(read_excel(filename, sheet = this_sheet), stringsAsFactors = FALSE)
    raw_r <- r

    ## trim off any trailing rows that might have been used for calculations
    dudidx <- is.na(r$source_id)
    if (sum(dudidx) > 0) {
        if (verbosity > 0) cat("dropping", sum(dudidx), "rows from isotopes worksheet with missing source_id\n")
        r <- r[!dudidx, ]
    }

    ## rename columns for backwards compatibility with older templates
    names(r) <- gsub("gender", "sex", names(r))
    names(r)[names(r) == "observation_date_min"] <- "observation_date_start"
    names(r)[names(r) == "observation_date_max"] <- "observation_date_end"

    ## basic error checking
    ## check column names
    check_sheet_columns(names(r), "isotopes")

    if (any(is.na(r$taxon_sample_id))) stop("missing at least one taxon_sample_id")
    if (is.character(r$taxon_sample_id)) {
        warning("taxon_sample_id is character: converting to numeric via as.factor. Check this!")
        r$taxon_sample_id <- as.numeric(as.factor(r$taxon_sample_id))
    }
    if (any(!is_nonempty(r$entered_by))) stop("missing at least one entered_by entry")

    ## convert date columns, if needed, and format
    for (datecol in c("observation_date_start","observation_date_end")) r[, datecol] <- format_datecol(r[, datecol])

    ## force lower case for various columns
    force_lc <- c("taxon_life_stage", "taxon_breeding_stage", "taxon_sex", "isotopes_body_part_used", "taxon_size_notes", "taxon_mass_notes", "isotopes_carbonates_treatment", "isotopes_lipids_treatment")
    for (k in force_lc) r[, k] <- tolower(r[, k])

    ## check unique values in columns with controlled vocabularies
    if (verbosity > 0) cat("Checking controlled vocabularies ...\n")
    controlled_cols <- c("isotopes_body_part_used", "taxon_breeding_stage", "taxon_sex", "taxon_life_stage", "taxon_mass_notes", "taxon_mass_units", "taxon_size_notes", "taxon_size_units", "delta_13C_variability_type", "delta_15N_variability_type", "C_N_ratio_variability_type", "C_N_ratio_type", "delta_34S_variability_type", "delta_15N_glutamic_acid_variability_type", "delta_15N_phenylalanine_variability_type", "isotopes_lipids_treatment","isotopes_carbonates_treatment") ##"isotopes_pretreatment"
    for (k in controlled_cols) checkvals(r[, k], k)
    if (verbosity > 0) cat("done.\n")

    ## find existing species names, if we've been given the handle to the database
    if (length(existing_names) < 1 && !missing(dbh)) existing_names <- get_existing_names(dbh)

    ## first parse all taxa
    temp <- check_names(r$taxon_name, r$revised_taxon_name, existing_names = existing_names, cache_directory = worms_cache_directory, verbosity = verbosity, force = refresh_worms_cache)
    unmatched_names <- temp$unmatched_names
    taxon_table <- temp$taxon_table

    if (verbosity > 0) cat("\n")

    ## iterate through rows of r, parsing each record
    all_records <- list()
    for (lidx in seq_len(nrow(r))) {
        processed_cols <- c()
        temppred <- if (is_nonempty(r$revised_taxon_name[lidx])) r$revised_taxon_name[lidx] else r$taxon_name[lidx]
        temppred <- tidy_name(temppred)
        if (verbosity > 1) cat0("adding ", tt_lookup(temppred, taxon_table)$resolved_name, "\n")
        temp_insert <- list(taxon_name = tt_lookup(temppred, taxon_table)$resolved_name)
        temp_insert$taxon_name_original <- strip_name_specials(if (is_nonempty(r$revised_taxon_name[lidx]) && is_nonempty(r$taxon_name[lidx])) r$taxon_name[lidx] else temppred)
        this_tax <- tt_lookup(temppred, taxon_table)
        if (nrow(this_tax) == 1 && !is.na(this_tax$aphia_id)) {
            temp_insert$taxon_aphia_id <- this_tax$aphia_id
            for (tcol in c("worms_rank", "worms_kingdom", "worms_phylum", "worms_class", "worms_order", "worms_family", "worms_genus")) temp_insert[[paste0("taxon_", tcol)]] <- this_tax[[tcol]]
        } else {
            temp_insert$taxon_aphia_id <- NA_integer_
            for (tcol in c("taxon_worms_rank", "taxon_worms_kingdom", "taxon_worms_phylum", "taxon_worms_class", "taxon_worms_order", "taxon_worms_family", "taxon_worms_genus")) temp_insert[[tcol]] <- NA_character_
        }
        processed_cols <- c(processed_cols, c("taxon_name", "revised_taxon_name", "taxon_aphia_id"))
        ## some fairly straightforward ones
        ## re-add taxon_group_soki once populated
        for (blah in c("original_record_id", "taxon_life_stage", "taxon_breeding_stage", "taxon_sample_count", "taxon_sample_id", "taxon_size_min", "taxon_size_max", "taxon_size_mean", "taxon_size_sd", "taxon_size_units", "taxon_size_notes", "taxon_mass_min", "taxon_mass_max", "taxon_mass_mean", "taxon_mass_sd", "taxon_mass_units", "taxon_mass_notes",  "physical_sample_id", "analytical_replicate_id", "analytical_replicate_count")) {
            temp <- r[lidx, blah]
            if (blah %in% c("taxon_life_stage", "taxon_breeding_stage")) temp <- gsub(", ", ",", temp)
            if (grepl("/", temp) && grepl("stage", blah)) stop("taxon life_stage/breeding_stage col contains / as separator?")
            temp_insert[[blah]] <- temp
            processed_cols <- c(processed_cols, blah)
        }

        temp_insert$taxon_sex <- nonempty_or_unknown(r$taxon_sex[lidx])
        processed_cols <- c(processed_cols, "taxon_sex")

        ## dates
        temp_insert$observation_date_start <- r$observation_date_start[lidx]
        temp_insert$observation_date_end <- r$observation_date_end[lidx]
        processed_cols <- c(processed_cols, c("observation_date_start", "observation_date_end"))

        ## location
        temp_insert$location <- tidy_name(r$location[lidx])
        if (!is.na(r$south[lidx]) && abs(r$south[lidx]) > 90) stop("location south on line ", lidx, " is beyond 90 (", r$south[lidx], ")")
        temp_insert$south <- r$south[lidx]
        temp_insert$west <- deg_normalise(r$west[lidx])

        if (!is.na(r$north[lidx])) {
            if (abs(r$north[lidx]) > 90) stop("location north on line ", lidx, " is beyond 90 (", r$north[lidx], ")")
            if (r$south[lidx] > r$north[lidx]) stop("location south (", r$south[lidx], ") on line ", lidx, " is greater than the north value (", r$north[lidx], ")")
        }
        temp_insert$north <- r$north[lidx]
        temp_insert$east <- deg_normalise(r$east[lidx])
        processed_cols <- c(processed_cols,c("location","east","west","south","north"))
        ## altitude
        if (!is.na(r$altitude_min[lidx]) && !is.na(r$altitude_max[lidx]) && r$altitude_min[lidx]>r$altitude_max[lidx]) stop("altitude_min (", r$altitude_min[lidx], ") is greater than altitude_max (", r$altitude_max[lidx], ") on line ", lidx)
        temp_insert$altitude_min <- r$altitude_min[lidx]
        temp_insert$altitude_max <- r$altitude_max[lidx]
        processed_cols <- c(processed_cols, c("altitude_min", "altitude_max"))
        ## depth
        tempmin <- abs(r$depth_min[lidx])
        tempmax <- abs(r$depth_max[lidx])
        if (!is.na(tempmin) && !is.na(tempmax) && tempmin > tempmax) stop("depth_min (", tempmin, ") is greater than depth_max (", tempmax, ") on line ", lidx)
        temp_insert$depth_min <- tempmin
        temp_insert$depth_max <- tempmax
        processed_cols <- c(processed_cols, c("depth_min", "depth_max"))

        ## isotope data
        temp_insert$samples_were_pooled <- flag_to_yes_no(parse_flag(r$samples_were_pooled[lidx], treat_empty_as = FALSE))
        processed_cols <- c(processed_cols, "samples_were_pooled")
        for (blah in c("delta_13C_mean", "delta_13C_variability_value", "delta_13C_variability_type", "delta_15N_mean", "delta_15N_variability_value", "delta_15N_variability_type", "C_N_ratio_mean", "C_N_ratio_variability_value", "C_N_ratio_variability_type", "C_N_ratio_type", "delta_34S_mean","delta_34S_variability_value","delta_34S_variability_type", "delta_15N_glutamic_acid_mean","delta_15N_glutamic_acid_variability_value","delta_15N_glutamic_acid_variability_type", "delta_15N_phenylalanine_mean","delta_15N_phenylalanine_variability_value","delta_15N_phenylalanine_variability_type", "isotopes_adjustment_notes", "isotopes_body_part_used", "isotopes_pretreatment", "isotopes_lipids_treatment", "isotopes_carbonates_treatment")) {
            temp_insert[[blah]] <- r[lidx, blah]
            processed_cols <- c(processed_cols, blah)
        }
        if (is_nonempty(r$C_N_ratio_mean[lidx]) && !is_nonempty(r$C_N_ratio_type[lidx])) stop("C:N ratio given but empty C_N_ratio_type on line ", lidx)

        ## source_id
        this_source_id <- r$source_id[lidx]
        if (is.na(this_source_id)) warning("row ", lidx, " is missing source_id")
        if (!this_source_id %in% valid_source_ids) warning("row ", lidx, " has source_id ", this_source_id, ", which does not match the sources sheet")
        temp_insert$source_id <- this_source_id
        processed_cols <- c(processed_cols, "source_id")

        ## quality and other flags
        temp_insert$isotopes_are_adjusted <- flag_to_yes_no(parse_flag(r$isotopes_are_adjusted[lidx], treat_empty_as = FALSE))
        processed_cols <- c(processed_cols, "isotopes_are_adjusted")

        temp_insert$quality_flag <- parse_quality_flag(r$is_dodgy[lidx], lidx)
        temp_insert$entered_by <- r$entered_by[lidx]
        temp_insert$is_secondary_data <- flag_to_yes_no(parse_flag(r$is_secondary_data[lidx], treat_empty_as = FALSE))
        temp_insert$is_public_flag <- flag_to_yes_no(parse_flag(r$is_public[lidx], treat_empty_as = TRUE))
        processed_cols <- c(processed_cols, c("is_dodgy", "entered_by", "is_secondary_data", "is_public"))

        ## notes
        temp_insert$notes <- r$notes[lidx]
        processed_cols <- c(processed_cols, "notes")

        temp_insert$last_modified <- now(tzone = "GMT")

        if (!all(names(r) %in% processed_cols)) warning("unprocessed columns: ", paste(setdiff(names(r), processed_cols), collapse = ", "))
        temp <- setdiff(processed_cols, c(names(r), "taxon_aphia_id"))
        if (length(temp) > 0) stop("processed columns that apparently don't exist in the spreadsheet: ", paste(temp, collapse = ", "))

        all_records <- c(all_records, list(temp_insert))
    }
    if (verbosity > 0) cat(length(all_records), "isotope data records parsed.\n")
    ## to data frame
    all_records <- do.call(rbind, lapply(all_records, as.data.frame, stringsAsFactors = FALSE))
    if (length(unmatched_names) > 0) warning("unmatched names: ", paste(unmatched_names, collapse = ", "))
    cat("Unique dates found in these records:\n  ")
    temp <- unique(na.omit(c(all_records$observation_date_start, all_records$observation_date_end)))
    cat(paste(format(sort(temp)), collapse = ", "))

    cord <- columns_order("isotopes")
    if (!all(names(all_records) %in% cord)) stop("unexpected column names: ", paste(setdiff(names(all_records), cord), collapse = ", "))
    all_records <- all_records[, intersect(cord, names(all_records))]

    list(sources = sources$sources, records = all_records, raw = list(sources = sources$raw, records = raw_r))
}

#' Parse DNA diet data spreadsheet
#'
#' @param filename string: path to Excel file
#' @param dbh optional: database handle
#' @param existing_names character: vector of existing taxon names
#' @param worms_cache_directory string: path to cache directory for taxonomic data
#' @param verbosity numeric: 0 = silent, > 0 = give progress messages
#' @param data_df data.frame: pass the data data.frame directly rather than providing the filename
#' @param sources_df data.frame: pass the sources data.frame directly rather than providing the filename
#' @param refresh_worms_cache logical: if `TRUE`, refresh the taxonomic cache
#'
#' @return A list object with the parsed data
#'
#' @export
parse_dna_diet <- function(filename, dbh, existing_names, worms_cache_directory = NULL, verbosity = 1, data_df, sources_df, refresh_worms_cache = FALSE) {
    ## check inputs
    if (!missing(dbh)) assert_that(inherits(dbh, c("JDBCConnection", "OdbcConnection")))
    if (!missing(filename) && !file.exists(filename)) stop("file ", filename, " does not exist")
    ## can supply data and sources data.frames directly
    if (!missing(data_df)) assert_that(is.data.frame(data_df))
    if (!missing(sources_df)) assert_that(is.data.frame(sources_df))
    if (missing(existing_names)) existing_names <- c()

    ## start with sources
    sources <- parse_sources(if (!missing(sources_df)) sources_df else filename, verbosity = verbosity)
    valid_source_ids <- sources$sources$source_id

    ## dna diet data sheet
    if (!missing(data_df)) {
        r <- data_df
    } else {
        sheets <- excel_sheets(filename)
        if ("DNA" %in% sheets) {
            this_sheet <- "DNA"
        } else {
            warning("could not find worksheet named \"DNA\", using the first worksheet (\"", sheets[1], "\")")
            this_sheet <- sheets[1]
        }
        r <- as.data.frame(read_excel(filename, sheet = this_sheet), stringsAsFactors = FALSE)
    }
    raw_r <- r

    ## trim off any trailing rows that might have been used for calculations
    dudidx <- is.na(r$source_id)
    if (sum(dudidx) > 0) {
        if (verbosity > 0) cat0("dropping ", sum(dudidx), " rows from diet worksheet with missing source_id\n")
        r <- r[!dudidx, ]
    }

    ## basic error checking
    ## check column names
    check_sheet_columns(names(r), "dna_diet")

    if (any(is.na(r$predator_sample_id))) stop("missing at least one predator_sample_id")
    if (is.character(r$predator_sample_id)) {
        warning("predator_sample_id is character: converting to numeric via as.factor. Check this!")
        r$predator_sample_id <- as.numeric(as.factor(r$predator_sample_id))
    }
    if (any(!is_nonempty(r$entered_by))) stop("missing at least one entered_by entry")

    ## convert date columns, if needed, and format
    for (datecol in c("observation_date_start", "observation_date_end")) r[, datecol] <- format_datecol(r[, datecol])

    ## force lower case for various columns
    force_lc <- c("predator_life_stage", "predator_breeding_stage", "qualitative_dietary_importance", "predator_size_notes", "predator_mass_notes", "predator_sex", "sample_type", "analysis_type", "target_food_group") ##,"other_methods_applied"?
    for (k in force_lc) r[, k] <- tolower(r[, k])

    force_uc <- c("sequence")
    for (k in force_uc) r[, k] <- toupper(r[, k])

    ## check unique values in columns with controlled vocabularies
    if (verbosity > 0) cat("Checking controlled vocabularies ...\n")
    controlled_cols <- c("predator_life_stage", "predator_sex", "predator_breeding_stage", "predator_size_units", "predator_mass_units", "qualitative_dietary_importance", "predator_size_notes", "predator_mass_notes", "sample_type", "DNA_extraction_method", "analysis_type", "sequencing_platform", "target_gene", "target_food_group") ##"sequence_source_id"? ,"other_methods_applied" probably not controlled
    ## TODO: "forward_primer","reverse_primer","blocking_primer"
    for (k in controlled_cols) checkvals(r[, k], k)
    if (verbosity > 0) cat("done.\n")

    ## find existing species names, if we've been given the handle to the database
    if (length(existing_names) < 1 && !missing(dbh)) existing_names <- get_existing_names(dbh)

    ## first parse all taxa
    ru <- unique(r[, c("predator_name", "revised_predator_name")])
    temp <- check_names(ru$predator_name, ru$revised_predator_name, existing_names = existing_names, cache_directory = worms_cache_directory, verbosity = verbosity, force = refresh_worms_cache)
    unmatched_names <- temp$unmatched_names
    taxon_table <- temp$taxon_table
    ru <- unique(r[, c("prey_name", "revised_prey_name")])
    temp <- check_names(ru$prey_name, ru$revised_prey_name, existing_table = taxon_table, existing_names = existing_names, cache_directory = worms_cache_directory, verbosity = verbosity, force = refresh_worms_cache)
    unmatched_names <- sort(unique(c(unmatched_names, temp$unmatched_names)))
    taxon_table <- unique(rbind(taxon_table, temp$taxon_table))

    if (verbosity > 0) cat("\n")
    ## iterate through rows of r, parsing each record
    all_records <- list()
    for (lidx in seq_len(nrow(r))) {
        processed_cols <- c()
        temppred <- if (is_nonempty(r$revised_predator_name[lidx])) r$revised_predator_name[lidx] else r$predator_name[lidx]
        tempprey <- if (is_nonempty(r$revised_prey_name[lidx])) r$revised_prey_name[lidx] else r$prey_name[lidx]
        temppred <- tidy_name(temppred)
        tempprey <- tidy_name(tempprey)
        if (verbosity > 1) cat0("adding ", tt_lookup(temppred, taxon_table)$resolved_name, " -> ", tt_lookup(tempprey, taxon_table)$resolved_name, "\n")
        temp_insert <- list(predator_name = tt_lookup(temppred, taxon_table)$resolved_name)
        temp_insert$predator_name_original <- strip_name_specials(if (is_nonempty(r$revised_predator_name[lidx]) && is_nonempty(r$predator_name[lidx])) r$predator_name[lidx] else temppred)
        this_tax <- tt_lookup(temppred, taxon_table)
        if (nrow(this_tax) == 1 && !is.na(this_tax$aphia_id)) {
            temp_insert$predator_aphia_id <- this_tax$aphia_id
            for (tcol in c("worms_rank", "worms_kingdom", "worms_phylum", "worms_class", "worms_order", "worms_family", "worms_genus")) temp_insert[[paste0("predator_", tcol)]] <- this_tax[[tcol]]
        } else {
            temp_insert$predator_aphia_id <- NA_integer_
            for (tcol in c("predator_worms_rank", "predator_worms_kingdom", "predator_worms_phylum", "predator_worms_class", "predator_worms_order", "predator_worms_family", "predator_worms_genus")) temp_insert[[tcol]] <- NA_character_
        }
        processed_cols <- c(processed_cols, c("predator_name", "revised_predator_name", "predator_aphia_id"))
        ## some fairly straightforward ones
        ## re-add "predator_group_soki" once repopulated
        for (blah in c("original_record_id", "predator_life_stage", "predator_breeding_stage", "predator_sample_count", "predator_size_min", "predator_size_max", "predator_size_mean", "predator_size_sd", "predator_size_units", "predator_size_notes", "predator_mass_min", "predator_mass_max", "predator_mass_mean", "predator_mass_sd", "predator_mass_units", "predator_mass_notes", "predator_sample_id", "physical_sample_id", "analytical_replicate_id", "analytical_replicate_count")) {
            temp <- r[lidx, blah]
            if (blah %in% c("predator_life_stage", "predator_breeding_stage")) temp <- gsub(", ", ",", temp)
            if (grepl("/", temp) && grepl("stage", blah)) stop("predator life_stage/breeding_stage col contains / as separator?")
            temp_insert[[blah]] <- temp
            processed_cols <- c(processed_cols, blah)
        }

        temp_insert$predator_sex <- nonempty_or_unknown(r$predator_sex[lidx])
        processed_cols <- c(processed_cols, "predator_sex")
        if (is_nonempty(tempprey)) {
            temp_insert$prey_name <- tt_lookup(tempprey, taxon_table)$resolved_name
            temp_insert$prey_name_original <- strip_name_specials(if (is_nonempty(r$revised_prey_name[lidx]) && is_nonempty(r$prey_name[lidx])) r$prey_name[lidx] else tempprey)
            this_tax <- tt_lookup(tempprey, taxon_table)
            if (nrow(this_tax) == 1 && !is.na(this_tax$aphia_id)) {
                temp_insert$prey_aphia_id <- this_tax$aphia_id
                for (tcol in c("worms_rank", "worms_kingdom", "worms_phylum", "worms_class", "worms_order", "worms_family", "worms_genus")) temp_insert[[paste0("prey_", tcol)]] <- this_tax[[tcol]]
            } else {
                temp_insert$prey_aphia_id <- NA_integer_
                for (tcol in c("prey_worms_rank", "prey_worms_kingdom", "prey_worms_phylum", "prey_worms_class", "prey_worms_order", "prey_worms_family", "prey_worms_genus")) temp_insert[[tcol]] <- NA_character_
            }
        } else {
            warning("empty prey_name at row ", lidx)
            temp_insert$prey_name <- NA_character_
            temp_insert$prey_name_original <- NA_character_
            temp_insert$prey_aphia_id <- NA_integer_ 
            for (tcol in c("prey_worms_rank", "prey_worms_kingdom", "prey_worms_phylum", "prey_worms_class", "prey_worms_order", "prey_worms_family", "prey_worms_genus")) temp_insert[[tcol]] <- NA_character_
        }
        processed_cols <- c(processed_cols, c("prey_name", "revised_prey_name", "prey_aphia_id"))
        if (is.null(temp_insert$prey_name) || is.na(temp_insert$prey_name) || nchar(temp_insert$prey_name)<1) warning("empty prey_name at row ", lidx)

        ## re-add "prey_group_soki" once repopulated
        for (blah in c("prey_is_aggregate")) {
            if (!blah %in% names(r)) stop("expected column ", blah, " is missing from the sheet")
            temp <- r[lidx, blah]
            processed_cols <- c(processed_cols, blah)
            if (is.na(temp)) {
                temp_insert[[blah]] <- NA
            } else {
                if (blah == "prey_is_aggregate") temp <- flag_to_yes_no(parse_flag(temp))
                temp_insert[[blah]] <- temp
            }
        }

        ## dates
        temp_insert$observation_date_start <- r$observation_date_start[lidx]
        temp_insert$observation_date_end <- r$observation_date_end[lidx]
        processed_cols <- c(processed_cols, c("observation_date_start", "observation_date_end"))

        ## location
        this_location <- tidy_name(r$location[lidx])
        temp_insert$location <- this_location
        if (!is.na(r$south[lidx]) && abs(r$south[lidx]) > 90) stop("location south on line ", lidx, " is beyond 90 (", r$south[lidx], ")")
        temp_insert$south <- r$south[lidx]
        temp_insert$west <- deg_normalise(r$west[lidx])

        if (!is.na(r$north[lidx])) {
            if (abs(r$north[lidx]) > 90) stop("location north on line ", lidx, " is beyond 90 (", r$north[lidx], ")")
            if (r$south[lidx] > r$north[lidx]) stop("location south (", r$south[lidx], ") on line ", lidx, " is greater than the north value (", r$north[lidx], ")")
        }
        temp_insert$north <- r$north[lidx]
        temp_insert$east <- deg_normalise(r$east[lidx])
        processed_cols <- c(processed_cols, c("location", "east", "west", "south", "north"))
        ## altitude
        if (!is.na(r$altitude_min[lidx]) && !is.na(r$altitude_max[lidx]) && r$altitude_min[lidx]>r$altitude_max[lidx]) stop("altitude_min (", r$altitude_min[lidx], ") is greater than altitude_max (", r$altitude_max[lidx], ") on line ", lidx)
        temp_insert$altitude_min <- r$altitude_min[lidx]
        temp_insert$altitude_max <- r$altitude_max[lidx]
        processed_cols <- c(processed_cols, c("altitude_min", "altitude_max"))
        ## depth
        tempmin <- abs(r$depth_min[lidx])
        tempmax <- abs(r$depth_max[lidx])
        if (!is.na(tempmin) && !is.na(tempmax) && tempmin > tempmax) stop("depth_min (", tempmin, ") is greater than depth_max (", tempmax, ") on line ", lidx)
        temp_insert$depth_min <- tempmin
        temp_insert$depth_max <- tempmax
        processed_cols <- c(processed_cols, c("depth_min", "depth_max"))

        ## methodological stuff, diet measures
        mcols <- c("sample_type", "DNA_extraction_method", "analysis_type", "sequencing_platform", "target_gene", "target_food_group", "forward_primer", "reverse_primer", "blocking_primer", "primer_source_id", "sequence_source_id", "sequence", "other_methods_applied", "sequences_total", "DNA_concentration", "fraction_sequences_by_prey", "fraction_occurrence", "qualitative_dietary_importance")
        for (blah in mcols) temp_insert[[blah]] <- r[lidx, blah]
        processed_cols <- c(processed_cols, mcols)

        ## source_id
        this_source_id <- r$source_id[lidx]
        if (is.na(this_source_id)) warning("row ", lidx, " is missing source_id")
        if (!this_source_id %in% valid_source_ids) warning("row ", lidx, " has source_id ", this_source_id, ", which does not match the sources sheet")
        temp_insert$source_id <- this_source_id
        processed_cols <- c(processed_cols, "source_id")

        ## quality and other flags
        temp_insert$quality_flag <- parse_quality_flag(r$is_dodgy[lidx], lidx)
        temp_insert$entered_by <- r$entered_by[lidx]
        temp_insert$is_secondary_data <- flag_to_yes_no(parse_flag(r$is_secondary_data[lidx], treat_empty_as = FALSE))
        temp_insert$is_public_flag <- flag_to_yes_no(parse_flag(r$is_public[lidx], treat_empty_as = TRUE))
        processed_cols <- c(processed_cols, c("is_dodgy", "entered_by", "is_secondary_data", "is_public"))

        ## notes
        temp_insert$notes <- r$notes[lidx]
        processed_cols <- c(processed_cols, "notes")

        temp_insert$last_modified <- now(tzone = "GMT")

        ##cat(str(temp_insert))
        if (!all(names(r) %in% processed_cols)) stop("unprocessed columns: ", paste(setdiff(names(r), processed_cols), collapse = ", "))
        temp <- setdiff(processed_cols, c(names(r), c("predator_aphia_id", "prey_aphia_id")))
        if (length(temp) > 0) stop("processed columns that apparently don't exist in the spreadsheet: ", paste(temp, collapse = ", "))

        all_records <- c(all_records, list(temp_insert))
    }
    if (verbosity > 0) cat(length(all_records), "diet data records parsed.\n")
    all_records <- do.call(rbind, lapply(all_records, as.data.frame, stringsAsFactors = FALSE))
    if (length(unmatched_names) > 0) warning("unmatched names: ", paste(unmatched_names, collapse = ", "))

    cord <- columns_order("dna_diet")
    if (!all(names(all_records) %in% cord)) stop("unexpected column names: ", paste(setdiff(names(all_records), cord), collapse = ", "))
    all_records <- all_records[, intersect(cord, names(all_records))]

    list(sources = sources$sources, records = all_records, raw = list(sources = sources$raw, records = raw_r))
}

#' Parse diet data spreadsheet
#'
#' @param filename string: path to Excel file
#' @param dbh database handle: optional
#' @param existing_names character: vector of existing taxon names
#' @param worms_cache_directory string: path to cache directory for taxonomic data
#' @param verbosity numeric: 0 = silent, > 0 = give progress messages
#' @param refresh_worms_cache logical: if `TRUE`, refresh the taxonomic cache
#'
#' @return A list object with the parsed data
#'
#' @examples
#' \dontrun{
#'   ## use the example file bundled with the package
#'   filename <- system.file("extdata/example_diet_data.xls",
#'                           package = "dietdataentry")
#'   x <- parse_diet(filename)
#'
#'   ## same, but using a local cache for the taxon lookups, which will be faster
#'   x <- parse_diet(filename, worms_cache_directory = "~/.Rcache")
#' }
#' @export
parse_diet <- function(filename, dbh, existing_names, worms_cache_directory = NULL, verbosity = 1, refresh_worms_cache = FALSE) {
    ## check inputs
    if (!missing(dbh)) assert_that(inherits(dbh, c("JDBCConnection","OdbcConnection")))
    if (!file.exists(filename)) stop("file ", filename, " does not exist")
    if (missing(existing_names)) existing_names <- c()

    ## start with sources
    sources <- parse_sources(filename, verbosity = verbosity)
    valid_source_ids <- sources$sources$source_id

    ## diet data sheet
    sheets <- excel_sheets(filename)
    if ("trophic" %in% sheets) {
        this_sheet <- "trophic"
    } else if ("data" %in% sheets) {
        this_sheet <- "data"
    } else {
        warning("could not find worksheet named \"trophic\" or \"data\", using the first worksheet ('", sheets[1], "')")
        this_sheet <- sheets[1]
    }
    r <- as.data.frame(read_excel(filename, sheet = this_sheet), stringsAsFactors = FALSE)
    raw_r <- r

    ## trim off any trailing rows that might have been used for calculations
    dudidx <- is.na(r$source_id)
    if (sum(dudidx) > 0) {
        if (verbosity > 0) cat0("dropping ", sum(dudidx), " rows from diet worksheet with missing source_id\n")
        r <- r[!dudidx, ]
    }

    ## rename columns for backwards compatibility with older templates
    names(r) <- gsub("gender", "sex", names(r))
    names(r)[names(r) == "observation_date_min"] <- "observation_date_start"
    names(r)[names(r) == "observation_date_max"] <- "observation_date_end"

    ## basic error checking
    ## check column names
    check_sheet_columns(names(r), "diet")

    if (any(is.na(r$predator_sample_id))) stop("missing at least one predator_sample_id")
    if (is.character(r$predator_sample_id)) r$predator_sample_id <- as.numeric(as.factor(r$predator_sample_id))
    if (any(!is_nonempty(r$entered_by))) stop("missing at least one entered_by entry")

    ## convert date columns, if needed, and format
    for (datecol in c("observation_date_start", "observation_date_end")) r[, datecol] <- format_datecol(r[, datecol])

    ## force lower case for various columns
    force_lc <- c("prey_life_stage", "predator_life_stage", "predator_breeding_stage", "identification_method", "qualitative_dietary_importance", "prey_size_notes", "prey_mass_notes", "predator_size_notes", "predator_mass_notes", "predator_sex", "prey_sex", "prey_items_included", "accumulated_hard_parts_treatment")
    for (k in force_lc) r[, k] <- sub(" dna ", " DNA ", tolower(r[, k])) ## but DNA uppercase
    for (k in "identification_method") r[, k] <- sub("(coi)", "(COI)", r[, k], fixed = TRUE) ## also (COI)
    for (k in "identification_method") r[, k] <- sub("(18s)", "(18S)", r[, k], fixed = TRUE) ## and (18S)

    ## check unique values in columns with controlled vocabularies
    if (verbosity > 0) cat("Checking controlled vocabularies ...\n")
    controlled_cols <- c("predator_life_stage", "prey_life_stage", "predator_sex", "prey_sex", "identification_method", "consumption_rate_units", "predator_breeding_stage", "predator_size_units", "predator_mass_units", "prey_size_units", "prey_mass_units", "qualitative_dietary_importance", "prey_size_notes", "predator_size_notes", "prey_mass_notes", "predator_mass_notes", "prey_items_included", "accumulated_hard_parts_treatment")
    for (k in controlled_cols) checkvals(r[, k], k)
    if (verbosity > 0) cat("done.\n")

    ## find existing species names, if we've been given the handle to the database
    if (length(existing_names) < 1 && !missing(dbh)) existing_names <- get_existing_names(dbh)

    ## first parse all taxa
    ru <- unique(r[, c("predator_name", "revised_predator_name")])
    temp <- check_names(ru$predator_name, ru$revised_predator_name, existing_names = existing_names, cache_directory = worms_cache_directory, verbosity = verbosity, force = refresh_worms_cache, soki_group_as = "predator")
    unmatched_names <- temp$unmatched_names
    taxon_table <- temp$taxon_table
    ru <- unique(r[, c("prey_name", "revised_prey_name")])
    temp <- check_names(ru$prey_name, ru$revised_prey_name, existing_table = taxon_table, existing_names = existing_names, cache_directory = worms_cache_directory, verbosity = verbosity, force = refresh_worms_cache, soki_group_as = "prey")
    unmatched_names <- sort(unique(c(unmatched_names, temp$unmatched_names)))
    taxon_table <- unique(rbind(taxon_table, temp$taxon_table))

    if (verbosity > 0) cat("\n")

    ## iterate through rows of r, parsing each record
    all_records <- list()

    for (lidx in seq_len(nrow(r))) {
        processed_cols <- c()
        temppred <- if (is_nonempty(r$revised_predator_name[lidx])) r$revised_predator_name[lidx] else r$predator_name[lidx]
        tempprey <- if (is_nonempty(r$revised_prey_name[lidx])) r$revised_prey_name[lidx] else r$prey_name[lidx]
        temppred <- tidy_name(temppred)
        tempprey <- tidy_name(tempprey)
        if (verbosity > 1) cat0("adding ", tt_lookup(temppred, taxon_table)$resolved_name, " -> ", tt_lookup(tempprey, taxon_table)$resolved_name, "\n")
        temp_insert <- list(predator_name = tt_lookup(temppred, taxon_table)$resolved_name)
        temp_insert$predator_name_original <- strip_name_specials(if (is_nonempty(r$revised_predator_name[lidx]) && is_nonempty(r$predator_name[lidx])) r$predator_name[lidx] else temppred)
        this_tax <- tt_lookup(temppred, taxon_table)
        for (tcol in c("worms_rank", "worms_kingdom", "worms_phylum", "worms_class", "worms_order", "worms_family", "worms_genus", "group_soki")) {
            if (nrow(this_tax) == 1 && !is.na(this_tax$aphia_id)) {
                temp_insert$predator_aphia_id <- this_tax$aphia_id
                temp_insert[[paste0("predator_", tcol)]] <- this_tax[[tcol]]
            } else {
                temp_insert$predator_aphia_id <- NA_integer_
                temp_insert[[paste0("predator_", tcol)]] <- NA_character_
            }
        }
        processed_cols <- c(processed_cols,c("predator_name", "revised_predator_name", "predator_aphia_id"))
        ## some fairly straightforward ones
        for (blah in c("original_record_id", "predator_life_stage", "predator_breeding_stage", "predator_sample_count", "predator_size_min", "predator_size_max", "predator_size_mean", "predator_size_sd", "predator_size_units", "predator_size_notes", "predator_mass_min", "predator_mass_max", "predator_mass_mean", "predator_mass_sd", "predator_mass_units", "predator_mass_notes", "predator_sample_id")) {
            if (!blah %in% names(r)) stop("expected column ", blah, " is missing from the sheet")
            temp <- r[lidx, blah]
            processed_cols <- c(processed_cols, blah)
            if (blah %in% c("predator_life_stage", "predator_breeding_stage")) temp <- gsub(", ", ",", temp)
            if (grepl("/", temp) && grepl("stage", blah)) stop("predator life_stage/breeding_stage col contains / as separator?")
            temp_insert[[blah]] <- temp
        }

        temp_insert$predator_sex <- nonempty_or_unknown(r$predator_sex[lidx])
        processed_cols <- c(processed_cols, "predator_sex")

        temp_insert$prey_sex <- nonempty_or_unknown(r$prey_sex[lidx])
        processed_cols <- c(processed_cols, "prey_sex")
        if (is_nonempty(tempprey)) {
            temp_insert$prey_name <- tt_lookup(tempprey, taxon_table)$resolved_name
            temp_insert$prey_name_original <- strip_name_specials(if (is_nonempty(r$revised_prey_name[lidx]) && is_nonempty(r$prey_name[lidx])) r$prey_name[lidx] else tempprey)
            this_tax <- tt_lookup(tempprey, taxon_table)
            for (tcol in c("worms_rank", "worms_kingdom", "worms_phylum", "worms_class", "worms_order", "worms_family", "worms_genus", "group_soki")) {
                if (nrow(this_tax) == 1 && !is.na(this_tax$aphia_id)) {
                    temp_insert$prey_aphia_id <- this_tax$aphia_id
                    temp_insert[[paste0("prey_", tcol)]] <- this_tax[[tcol]]
                } else {
                    temp_insert$prey_aphia_id <- NA_integer_
                    temp_insert[[paste0("prey_", tcol)]] <- NA_character_
                }
            }
        } else {
            warning("empty prey_name at row ", lidx)
            temp_insert$prey_name <- NA_character_
            temp_insert$prey_name_original <- NA_character_
            temp_insert$prey_aphia_id <- NA_integer_
            for (tcol in c("prey_worms_rank", "prey_worms_kingdom", "prey_worms_phylum", "prey_worms_class", "prey_worms_order", "prey_worms_family", "prey_worms_genus", "prey_group_soki")) temp_insert[[tcol]] <- NA_character_
        }
        processed_cols <- c(processed_cols, c("prey_name", "revised_prey_name", "prey_aphia_id"))

        for (blah in c("prey_is_aggregate", "prey_life_stage", "prey_sample_count", "prey_size_min", "prey_size_max", "prey_size_mean", "prey_size_sd", "prey_size_units", "prey_size_notes", "prey_mass_min", "prey_mass_max", "prey_mass_mean", "prey_mass_sd", "prey_mass_units", "prey_mass_notes")) {
            if (!blah %in% names(r)) stop("expected column ", blah, " is missing from the sheet")
            temp <- r[lidx, blah]
            processed_cols <- c(processed_cols, blah)
            if (is.na(temp)) {
                temp_insert[[blah]] <- NA
            } else {
                if (blah %in% c("prey_life_stage", "prey_breeding_stage")) temp <- gsub(", ", ",", temp)
                if (grepl("/", temp) && grepl("stage", blah)) stop("prey life_stage/breeding_stage col contains / as separator?")
                if (blah == "prey_is_aggregate") {
                    temp <- flag_to_yes_no(parse_flag(temp))
                }
                temp_insert[[blah]] <- temp
            }
        }

        ## dates
        temp_insert$observation_date_start <- r$observation_date_start[lidx]
        temp_insert$observation_date_end <- r$observation_date_end[lidx]
        processed_cols <- c(processed_cols, c("observation_date_start", "observation_date_end"))

        ## location
        this_location <- tidy_name(r$location[lidx])
        temp_insert$location <- this_location
        if (!is.na(r$south[lidx])) {
            if (abs(r$south[lidx]) > 90) stop("location south on line ", lidx, " is beyond 90 (", r$south[lidx], ")")
        }
        temp_insert$south <- r$south[lidx]
        temp_insert$west <- deg_normalise(r$west[lidx])

        if (!is.na(r$north[lidx])) {
            if (abs(r$north[lidx]) > 90) stop("location north on line ", lidx, " is beyond 90 (", r$north[lidx], ")")
            if (r$south[lidx]>r$north[lidx]) stop("location south (", r$south[lidx], ") on line ", lidx, " is greater than the north value (", r$north[lidx], ")")
        }
        temp_insert$north <- r$north[lidx]
        temp_insert$east <- deg_normalise(r$east[lidx])
        processed_cols <- c(processed_cols, c("location", "east", "west", "south", "north"))
        ## altitude
        if (!is.na(r$altitude_min[lidx]) && !is.na(r$altitude_max[lidx]) && r$altitude_min[lidx] > r$altitude_max[lidx]) stop("altitude_min (", r$altitude_min[lidx], ") is greater than altitude_max (", r$altitude_max[lidx], ") on line ", lidx)
        temp_insert$altitude_min <- r$altitude_min[lidx]
        temp_insert$altitude_max <- r$altitude_max[lidx]
        processed_cols <- c(processed_cols,c("altitude_min", "altitude_max"))
        ## depth
        tempmin <- abs(r$depth_min[lidx])
        tempmax <- abs(r$depth_max[lidx])
        if (!is.na(tempmin) && !is.na(tempmax) && tempmin>tempmax) stop("depth_min (", tempmin, ") is greater than depth_max (", tempmax, ") on line ", lidx)
        temp_insert$depth_min <- tempmin
        temp_insert$depth_max <- tempmax
        processed_cols <- c(processed_cols,c("depth_min", "depth_max"))

        if (!is_nonempty(r$identification_method[lidx])) warning("missing identification method at row ", lidx)
        temp_insert$identification_method <- r$identification_method[lidx]
        processed_cols <- c(processed_cols, "identification_method")

        ## diet measures
        mcols <- c("fraction_diet_by_weight", "fraction_diet_by_prey_items", "fraction_occurrence", "qualitative_dietary_importance", "consumption_rate_min", "consumption_rate_max", "consumption_rate_mean", "consumption_rate_sd", "consumption_rate_units", "consumption_rate_notes")
        for (blah in mcols) temp_insert[[blah]] <- r[lidx, blah]
        processed_cols <- c(processed_cols, mcols)

        temp_insert$prey_items_included <- nonempty_or_unknown(r$prey_items_included[lidx])
        processed_cols <- c(processed_cols, "prey_items_included")
        temp_insert$accumulated_hard_parts_treatment <- nonempty_or_unknown(r$accumulated_hard_parts_treatment[lidx])
        processed_cols <- c(processed_cols, "accumulated_hard_parts_treatment")

        ## source_id
        this_source_id <- r$source_id[lidx]
        if (is.na(this_source_id)) warning("row ", lidx, " is missing source_id")
        if (!this_source_id %in% valid_source_ids) warning("row ", lidx, " has source_id ", this_source_id, ", which does not match the sources sheet")
        temp_insert$source_id <- r$source_id[lidx]
        processed_cols <- c(processed_cols, "source_id")

        ## quality and other flags
        temp_insert$quality_flag <- parse_quality_flag(r$is_dodgy[lidx], lidx)
        temp_insert$entered_by <- r$entered_by[lidx]
        temp_insert$is_secondary_data <- flag_to_yes_no(parse_flag(r$is_secondary_data[lidx], treat_empty_as = FALSE))
        temp_insert$is_public_flag <- flag_to_yes_no(parse_flag(r$is_public[lidx], treat_empty_as = TRUE))
        processed_cols <- c(processed_cols, c("is_dodgy", "entered_by", "is_secondary_data", "is_public"))

        ## notes
        temp_insert$notes <- as.character(r$notes[lidx])
        processed_cols <- c(processed_cols, "notes")

        temp_insert$last_modified <- now(tzone = "GMT")

        ##cat(str(temp_insert))
        if (!all(names(r) %in% processed_cols)) stop("unprocessed columns: ", paste(setdiff(names(r), processed_cols), collapse = ", "))
        temp <- setdiff(processed_cols, c(names(r), c("predator_aphia_id", "prey_aphia_id")))
        if (length(temp) > 0) stop("processed columns that apparently don't exist in the spreadsheet: ", paste(temp, collapse = ", "))

        all_records <- c(all_records, list(temp_insert))
    }
    if (verbosity > 0) cat(length(all_records), "diet data records parsed.\n")
    if (length(unmatched_names) > 0) warning("unmatched names: ", paste(unmatched_names, collapse = ", "))
    all_records <- do.call(rbind, lapply(all_records, as.data.frame, stringsAsFactors = FALSE))
    cord <- columns_order("diet")
    if (!all(names(all_records) %in% cord)) stop("unexpected column names: ", paste(setdiff(names(all_records), cord), collapse = ", "))
    all_records <- all_records[, intersect(cord, names(all_records))]

    list(sources = sources$sources, records = all_records, raw = list(sources = sources$raw, records = raw_r))
}

check_sheet_columns <- function(names_provided, sheet_type) {
    assert_that(is.character(names_provided))
    assert_that(is.string(sheet_type))
    sheet_type <- match.arg(tolower(sheet_type), c("diet", "dna_diet", "isotopes", "energetics", "sources", "lipids"))
    expected <- switch(sheet_type,
                       diet =,
                       dna_diet =,
                       isotopes =,
                       lipids =,
                       energetics =,
                       sources = {
                           this_schema <- jsonlite::fromJSON(system.file(file.path("extdata", "schema", paste0(sheet_type, ".json")), package = "dietdataentry"))
                           names(this_schema)[vapply(this_schema, function(z) z$expected, FUN.VALUE = TRUE)]
                       },
                       stop("expected columns for sheet_type ", sheet_type, " not coded yet")
                       )
    if (!all(expected %in% names_provided)) stop("columns missing from spreadsheet: ", paste(setdiff(expected, names_provided), collapse = ", "))
    if (!all(names_provided %in% expected)) warning("ignoring unexpected columns in spreadsheet: ", paste(setdiff(names_provided, expected), sep = ", "))
    invisible(TRUE)
}

columns_order <- function(data_type) {
    data_type <- match.arg(tolower(data_type), c("diet", "dna_diet", "isotopes", "energetics", "sources", "lipids"))
    out <- switch(data_type,
                  diet =,
                  dna_diet =,
                  isotopes =,
                  lipids =,
                  energetics =,
                  sources = names(jsonlite::fromJSON(system.file(file.path("extdata", "schema", paste0(data_type, ".json")), package = "dietdataentry"))),
                  stop("column ordering for data type ", data_type, " not coded yet")
                  )
    if (data_type == "lipids") {
        ## lipids identical to energetics except has "measurement_class" added
        idx <- which(out == "measurement_name")
        out <- c(out[seq_len(idx)], "measurement_class", out[seq(from = idx + 1, to = length(out), by = 1)])
    }
    out
}

check_names <- function(this_names, this_names_revised, existing_table, existing_names = c(), cache_directory, verbosity, force = FALSE, marine_only = TRUE, soki_group_as = NULL) {
    unmatched_names <- c()
    if (missing(existing_table)) {
        taxon_table <- data.frame(name = character(), resolved_name = character(), aphia_id = integer(), worms_rank = character(), worms_kingdom = character(), worms_phylum = character(), worms_class = character(), worms_order = character(), worms_family = character(), worms_genus = character(), stringsAsFactors = FALSE)
        if (!is.null(soki_group_as)) taxon_table$group_soki <- character()
    } else {
        taxon_table <- existing_table
        expected <- c("aphia_id", "name", "resolved_name", "worms_rank", "worms_kingdom", "worms_phylum", "worms_class", "worms_order", "worms_family", "worms_genus")
        if (!is.null(soki_group_as)) expected <- c(expected, "group_soki")
        if (!setequal(names(taxon_table), expected)) stop("unexpected names in existing_table: ", paste(setdiff(names(taxon_table), expected), collapse = ", "))
    }
    if (missing(cache_directory)) cache_directory <- NULL
    if (!is.null(soki_group_as)) {
        suppressWarnings(group_rules <- soki_ruleset(worms_cache_directory = cache_directory))
    }
    for (k in seq_along(this_names)) {
        this_taxon <- if (is_nonempty(this_names_revised[k])) this_names_revised[k] else this_names[k]
        if (is.na(this_taxon)) next
        this_taxon <- tidy_name(this_taxon)
        if (!this_taxon %in% taxon_table$name) {
            if (verbosity > 0) cat0("\n", this_taxon, ":\n")
            ## taxonomy check via worms
            this_filter <- NULL
            ## some special cases
            if (tolower(this_taxon)=="pachyptila desolata") {
                thisrecord <- search_worms(this_taxon,cache_directory=cache_directory,acceptable_status=c("unaccepted"),follow_valid=FALSE,force=force)
            } else {
                if (tolower(this_taxon) %in% c("heterorhabdus","heterorhabdus sp."))
                    this_taxon <- "Heterorhabdus@phylum:Arthropoda"
                else if (tolower(this_taxon) %in% c("ctenophora"))
                    this_taxon <- "Ctenophora@phylum:Ctenophora"
                else if (tolower(this_taxon) %in% c("grateloupia","grateloupia sp."))
                    this_taxon <- "Grateloupia@phylum:Rhodophyta"
                else if (tolower(this_taxon) %in% c("lumbricidae"))
                    this_taxon <- "Lumbricidae@marine_only:FALSE"
                thisrecord <- search_worms(this_taxon, cache_directory=cache_directory, filter=this_filter, force=force, marine_only=marine_only)
            }
            this_resolved_name <- this_taxon
            if (nrow(thisrecord)!=1 || is.na(thisrecord$AphiaID)) {
                ## no matches - was it a "sp." name?
                temp_name <- this_taxon
                if (length(str_split(temp_name," ")[[1]])==2) {
                    temp_name <- gsub(" (spp|spp\\.|sp|sp\\.)$","",temp_name)
                }
                temp_record <- search_worms(temp_name,cache_directory=cache_directory,filter=this_filter,force=force)
                if (nrow(temp_record)==1) {
                    ## ok, matched on this
                    thisrecord <- temp_record
                    this_resolved_name <- strip_name_specials(temp_name)
                }
            }
            if (nrow(thisrecord)!=1 || is.na(thisrecord$AphiaID)) {
                unmatched_names <- unique(c(unmatched_names,this_taxon))
                if (verbosity > 0) cat(" Not matched.\n")
                if (length(str_split(this_resolved_name," ")[[1]])==2) {
                    this_resolved_name <- gsub(" spp\\.?$"," sp.",this_resolved_name)
                    this_resolved_name <- gsub(" sp$"," sp.",this_resolved_name)
                    this_resolved_name <- strip_name_specials(this_resolved_name)
                }
                mt <- data.frame(name = this_taxon, resolved_name = this_resolved_name, aphia_id = NA_integer_, worms_rank = NA_character_, worms_kingdom = NA_character_, worms_phylum = NA_character_, worms_class = NA_character_, worms_order = NA_character_, worms_family = NA_character_, worms_genus = NA_character_, stringsAsFactors = FALSE)
                if (!is.null(soki_group_as)) mt$group_soki <- NA_character_
                taxon_table <- rbind(taxon_table, mt)
            } else {
                this_resolved_name <- strip_name_specials(this_resolved_name)
                if (verbosity > 0) {
                    cat0(" AphiaID: ", thisrecord$AphiaID, "\n")
                    cat0(" Matched at rank: ", thisrecord$rank, "\n")
                    cat0(" [", thisrecord$kingdom, " ", thisrecord$phylum, " ", thisrecord$class, " ", thisrecord$order, " ", thisrecord$family, " ", thisrecord$genus, "]\n")
                }
                if (tolower(thisrecord$rank) %in% c("species", "subspecies")) this_resolved_name <- thisrecord$scientificname
                ## only use returned name if it was matched at species or subspecies level
                this <- data.frame(name = this_taxon, resolved_name = this_resolved_name, aphia_id = thisrecord$AphiaID, worms_rank = thisrecord$rank, worms_kingdom = thisrecord$kingdom, worms_phylum = thisrecord$phylum, worms_class = thisrecord$class, worms_order = thisrecord$order, worms_family = thisrecord$family, worms_genus = thisrecord$genus, stringsAsFactors=FALSE)
                if (!is.null(soki_group_as)) {
                    this$group_soki <- group_rules(thisrecord, soki_group_as, worms_cache_directory = cache_directory)
                    if (verbosity > 0) cat(" SOKI group: ", this$group_soki, "\n")
                }
                taxon_table <- rbind(taxon_table, this)
            }
            if (verbosity > 0) cat0(" Using resolved name: ", this_resolved_name, "\n")

            ## check against existing names
            if (length(existing_names)>0) {
                temp_score <- stringsim(this_resolved_name, existing_names)
                if (any(temp_score == 1)) {
                    if (verbosity > 0) cat("  ++  exact match in existing trophic names\n")
                    temp_idx <- c()##which(temp_score>0.99)
                } else {
                    if (verbosity > 0) cat("  -- no exact match in existing trophic names\n")
                    temp_idx <- tail(order(temp_score),5)
                    temp_idx <- temp_idx[temp_score[temp_idx]>0.5]
                    temp_idx <- temp_idx[order(temp_score[temp_idx],decreasing=TRUE)] ## closest match first
                }
                if (length(temp_idx) > 0 && verbosity > 0) {
                    cat("     closest matches:\n       ")
                    cat0(paste(unique(existing_names[temp_idx]), collapse = ", ", sep = ", "), "\n")
                }
            }
        }
    }
    list(taxon_table = taxon_table, unmatched_names = unmatched_names) ## drop the dummy row before returning
}


