#' Append new SO diet data to existing tables
#'
#' @param new_data list: as returned by [parse_diet()], [parse_isotopes()], etc
#' @param existing_data data.frame: the existing diet (or isotopes, etc) data frame, as returned by [sohungry::so_diet()], [sohungry::so_isotopes()], etc
#' @param existing_sources data.frame: the existing sources data frame, as returned by [sohungry::so_sources()]
#' @param records_type string: "diet", "dna_diet", "isotopes", "lipids", or "energetics". Note that "isotopes" only supports measurement-value ("mv") format, as do "lipids" and "energetics"
#' @param action_if_existing string: what to do if we find one or more records in `new_data` that looks like an existing one in `existing_data`? Either "stop", "warn", or "ignore"
#' @param verbosity numeric: if greater than 0, show progress messages
#'
#' @return A list with updated records and sources data
#'
#' @export
so_append_data <- function(new_data, existing_data, existing_sources, records_type, action_if_existing = "stop", verbosity = 1) {
    assert_that(is.string(records_type))
    records_type <- match.arg(records_type, c("diet", "dna_diet", "isotopes", "lipids", "energetics"))
    assert_that(is.string(action_if_existing))
    action_if_existing <- match.arg(action_if_existing, c("stop", "warn", "ignore"))

    mv_format_types <- c("isotopes", "lipids", "energetics")
    if (records_type %in% mv_format_types && !all(c("measurement_name", "measurement_min_value", "measurement_max_value", "measurement_mean_value", "measurement_variability_value", "measurement_variability_type", "measurement_units", "measurement_method") %in% names(existing_data))) {
        stop("only measurement-value ('mv') format supported")
    }
    new_sources <- new_data$sources
    new_data <- new_data$records

    new_data$is_public_flag[is.na(new_data$is_public_flag)] <- "Y"
    assert_that(all(new_data$is_public_flag %in% c("N", "Y")), msg = "is_public_flag must be Y or N in new_data$records")
    if (any(new_data$is_public_flag != "Y")) {
        message("Excluding ", sum(new_data$is_public_flag != "Y"), " non-public rows from new_data$records")
        new_data <- dplyr::filter(new_data, .data$is_public_flag == "Y")
    }

    ## any unused sources
    if (!all(new_sources$source_id %in% new_data$source_id)) {
        message("Excluding ", sum(!new_sources$source_id %in% new_data$source_id), " new_data$sources that are not used in new_data$records")
        new_sources <- new_sources[new_sources$source_id %in% new_data$source_id, ]
    }
    ## any missing sources
    if (!all(new_data$source_id %in% new_sources$source_id)) stop("Sources in new_data$records missing from new_data$sources")

    ## figure out source id remappings
    source_mapping <- data.frame()
    new_source_id <- max(existing_sources$source_id, na.rm = TRUE)
    appended_sources <- existing_sources
    for (s in seq_len(nrow(new_sources))) {
        ## does it exist?
        existing_source_id <- find_existing_source(new_sources[s, ], existing_sources)
        if (!is.null(existing_source_id)) {
            ## yes
            warning("existing source found, need to check sample_id uniqueness!")
            source_mapping <- rbind(source_mapping, data.frame(from = new_sources$source_id[s], to = existing_source_id))
            if (verbosity > 0) message("Using existing source_id ", existing_source_id, " for ", new_sources$details[s])
        } else {
            ## no
            temp <- new_sources[s, ]
            new_source_id <- new_source_id + 1L
            temp$source_id <- new_source_id
            source_mapping <- rbind(source_mapping, data.frame(from = new_sources$source_id[s], to = new_source_id))
            appended_sources <- rbind(appended_sources, temp[, names(appended_sources)])
            if (verbosity > 0) message("Adding source_id ", new_source_id, " for ", new_sources$details[s])
        }
    }

    ## remap source IDs
    if (!"source_id" %in% names(new_data)) stop("missing source_id from new_data$records")
    for (idcol in c("source_id", "primer_source_id", "sequence_source_id")) {
        if (idcol %in% names(new_data)) new_data[[idcol]] <- remap_source_ids(new_data[[idcol]], source_mapping)
    }

    ## add source_details and source_doi to new_data
    new_data <- new_data[, setdiff(names(new_data), c("source_details", "source_doi"))]
    new_data <- left_join_check(new_data, dplyr::select(appended_sources, "source_id", source_details = "details", source_doi = "doi"), by = "source_id")

    if (action_if_existing %in% c("stop", "warn")) {
        is_existing <- rep(FALSE, nrow(new_data))
        for (ii in seq_len(nrow(new_data))) {
            this_record <- new_data[ii, ]
            if (records_type %in% c("isotopes", "energetics", "lipids")) {
                checkex <- existing_data %>% dplyr::filter(.data$taxon_name_original == this_record$taxon_name_original | .data$taxon_name == this_record$taxon_name)
                for (chk in c("taxon_sex", "taxon_life_stage", "taxon_breeding_stage", "taxon_sample_count")) {
                    if (!is.na(this_record[[chk]])) checkex <- checkex %>% dplyr::filter(.data[[chk]] == this_record[[chk]])
                }
                for (chk in c("south", "north", "east", "west")) {
                    if (!is.na(this_record[[chk]])) checkex <- checkex %>% dplyr::filter(dplyr::near(.data[[chk]], this_record[[chk]]))
                }
                is_existing[ii] <- nrow(checkex) > 0
            } else {
                checkex <- existing_data %>% dplyr::filter(.data$predator_name_original == this_record$predator_name_original | .data$predator_name == this_record$predator_name) %>%
                    dplyr::filter(.data$prey_name_original == this_record$prey_name_original | .data$prey_name == this_record$prey_name)
                for (chk in c("predator_sex", "predator_life_stage", "prey_sex", "prey_life_stage", "predator_breeding_stage", "predator_sample_count")) {
                    if (!is.na(this_record[[chk]])) checkex <- checkex %>% dplyr::filter(.data[[chk]] == this_record[[chk]])
                }
                for (chk in c("south", "north", "east", "west")) {
                    if (!is.na(this_record[[chk]])) checkex <- checkex %>% dplyr::filter(dplyr::near(.data[[chk]], this_record[[chk]]))
                }
                is_existing[ii] <- nrow(checkex) > 0
            }
        }
        if (any(is_existing)) {
            ef <- if (action_if_existing == "stop") stop else warning
            ef(sum(is_existing), " records in new_data appear to already exist in existing_data")
        }
    }

    ## non-mv format
    ##col_order <- intersect(columns_order("isotopes"), names(blah))
    ##if (!all(names(blah) %in% col_order)) stop("columns dropped?")
    ##blah <- blah[,col_order]
    ##
    ##if (is_public) blah <- blah %>% select(-is_public_flag,-entered_by) ## but leave these in if dumping non-public data
    ##
    ##write.csv(blah,file="c:/i/projects/trophic_networks/so_web/scar_dump/scar_isotopes.csv",row.names=FALSE,na="")

    ##  measurement-value format
    new_record_id <- floor(max(existing_data$record_id, na.rm = TRUE))
    if (records_type %in% mv_format_types) {
        if (records_type == "isotopes") {
            mmt_cols <- c("taxon_size_min", "taxon_size_max", "taxon_size_mean", "taxon_size_sd", "taxon_size_units", "taxon_size_notes",
                          "taxon_mass_min", "taxon_mass_max", "taxon_mass_mean", "taxon_mass_sd", "taxon_mass_units", "taxon_mass_notes",
                          "delta_13C_mean", "delta_13C_variability_value", "delta_13C_variability_type",
                          "delta_15N_mean", "delta_15N_variability_value", "delta_15N_variability_type",
                          "delta_34S_mean", "delta_34S_variability_value", "delta_34S_variability_type",
                          "delta_15N_glutamic_acid_mean", "delta_15N_glutamic_acid_variability_value", "delta_15N_glutamic_acid_variability_type",
                          "delta_15N_phenylalanine_mean", "delta_15N_phenylalanine_variability_value", "delta_15N_phenylalanine_variability_type",
                          "C_N_ratio_mean", "C_N_ratio_variability_value", "C_N_ratio_variability_type", "C_N_ratio_type")
        } else {
            stop("mv not coded for ", records_type, " yet")
        }

        blah2 <- dplyr::select_at(new_data, setdiff(names(new_data), mmt_cols)) %>% mutate(measurement_name = NA_character_, measurement_min_value = NA_real_, measurement_max_value = NA_real_, measurement_mean_value = NA_real_, measurement_variability_value = NA_real_, measurement_variability_type = NA_character_, measurement_units = NA_character_, measurement_method = NA_character_)
        newxi <- tibble()

        for (k in seq_len(nrow(new_data))) {
            newrows <- tibble()
            if (!is.na(new_data$taxon_size_mean[k])) {
                new_record_id <- new_record_id + 1
                newrow <- blah2[k, ] %>% mutate(measurement_name = new_data$taxon_size_notes[k],
                                                measurement_min_value = new_data$taxon_size_min[k],
                                                measurement_max_value = new_data$taxon_size_max[k],
                                                measurement_mean_value = new_data$taxon_size_mean[k],
                                                measurement_variability_value = new_data$taxon_size_sd[k],
                                                measurement_variability_type = "SD",
                                                measurement_units = new_data$taxon_size_units[k],
                                                isotopes_body_part_used = NA_character_,
                                                isotopes_pretreatment = NA_character_,
                                                isotopes_are_adjusted = NA_character_,
                                                isotopes_adjustment_notes = NA_character_,
                                                isotopes_carbonates_treatment = NA_character_,
                                                isotopes_lipids_treatment = NA_character_,
                                                record_id = new_record_id)
                newrows <- bind_rows(newrows, newrow)
            }

            if (!is.na(new_data$taxon_mass_mean[k])) {
                new_record_id <- new_record_id + 1
                newrow <- blah2[k, ] %>% mutate(measurement_name = if (is.na(new_data$taxon_mass_notes[k])) "wet weight" else new_data$taxon_mass_notes[k],
                                                measurement_min_value = new_data$taxon_mass_min[k],
                                                measurement_max_value = new_data$taxon_mass_max[k],
                                                measurement_mean_value = new_data$taxon_mass_mean[k],
                                                measurement_variability_value = new_data$taxon_mass_sd[k],
                                                measurement_variability_type = "SD",
                                                measurement_units = new_data$taxon_mass_units[k],
                                                isotopes_body_part_used = "whole organism",
                                                isotopes_pretreatment = NA_character_,
                                                isotopes_are_adjusted = NA_character_,
                                                isotopes_adjustment_notes = NA_character_,
                                                isotopes_carbonates_treatment = NA_character_,
                                                isotopes_lipids_treatment = NA_character_,
                                                record_id = new_record_id)
                newrows <- bind_rows(newrows, newrow)
            }

            ## standard ones
            for (mgv in c("delta_13C", "delta_15N", "delta_34S", "delta_15N_glutamic_acid", "delta_15N_phenylalanine")) {
                if (!is.na(new_data[[paste0(mgv, "_mean")]][k])) {
                    new_record_id <- new_record_id + 1
                    newrow <- blah2[k, ] %>% mutate(measurement_name = mgv,
                                                    measurement_mean_value = new_data[[paste0(mgv, "_mean")]][k],
                                                    measurement_variability_value = new_data[[paste0(mgv, "_variability_value")]][k],
                                                    measurement_variability_type = new_data[[paste0(mgv, "_variability_type")]][k],
                                                    measurement_units = "per mil",
                                                    record_id = new_record_id)
                    newrows <- bind_rows(newrows, newrow)
                }
            }

            if (!is.na(new_data$C_N_ratio_mean[k])) {
                this_mmt <- switch(new_data$C_N_ratio_type[k],
                                   atomic = "C:N atomic ratio",
                                   mass = "C:N mass ratio",
                                   unknown = "C:N ratio (unknown basis)",
                                   stop("CN ratio type?"))
                if (is.na(new_data$C_N_ratio_type[k])) stop("check CN ratio")
                new_record_id <- new_record_id + 1
                newrow <- blah2[k,] %>% mutate(measurement_name = this_mmt,
                                               measurement_mean_value = new_data$C_N_ratio_mean[k],
                                               measurement_variability_value = new_data$C_N_ratio_variability_value[k],
                                               measurement_variability_type = new_data$C_N_ratio_variability_type[k],
                                               measurement_units = "",
                                               record_id = new_record_id)
                newrows <- bind_rows(newrows, newrow)
            }
            if (nrow(newrows)>0) newxi <- bind_rows(newxi, newrows)
        }

        ## add soki group
        if (records_type %in% c("isotopes")) {
            newxi$taxon_group_soki <- NA_character_
        } else {
            stop("soki group(s) needed")
        }

        col_order <- c(intersect(columns_order("isotopes"), names(newxi)), "source_details", "source_doi")
        if (!all(names(newxi) %in% col_order)) {
            stop("columns dropped?")
        } else {
            newxi <- newxi[, col_order]
        }
        new_data <- newxi
        ##write.csv(newxi, file="c:/i/projects/trophic_networks/so_web/scar_dump/scar_isotopes_mv.csv", row.names=FALSE, na="")

        ## energetics
        ##ids <- na.omit(unique(xe$taxon_aphia_id))
        ##temp <- do.call(rbind,lapply(ids,function(z)search_worms(ids=z,cache_directory="~/.Rcache")))
        ##
        ##temp <- temp %>% select(AphiaID,rank,kingdom,phylum,class,order,family,genus) %>%
        ##    rename_(.dots=setNames(names(.),paste0("taxon_worms_",names(.)))) %>%
        ##    rename(taxon_aphia_id=taxon_worms_AphiaID) %>% unique
        ##
        ##blah <- xe %>% rename(taxon_group_soki=taxon_group) %>% left_join(temp,by="taxon_aphia_id") %>% dplyr::arrange(source_id,record_id,taxon_name)
        ##
        ##col_order <- intersect(columns_order("energetics"), names(blah))
        ##if (!all(names(blah) %in% col_order)) stop("columns dropped?")
        ##blah <- blah[,col_order]
        ##
        ##if (is_public) blah <- blah %>% select(-is_public_flag,-entered_by) ## but leave these in if dumping non-public data
        ##write.csv(blah,file="c:/i/projects/trophic_networks/so_web/scar_dump/scar_energetics.csv",row.names=FALSE,na="")
        ##
#### lipids
        ##ids <- na.omit(unique(xl$taxon_aphia_id))
        ##temp <- do.call(rbind,lapply(ids,function(z)search_worms(ids=z,cache_directory="~/.Rcache")))
        ##
        ##temp <- temp %>% select(AphiaID,rank,kingdom,phylum,class,order,family,genus) %>%
        ##    rename_(.dots=setNames(names(.),paste0("taxon_worms_",names(.)))) %>%
        ##    rename(taxon_aphia_id=taxon_worms_AphiaID) %>% unique
        ##
        ##blah <- xl %>% rename(taxon_group_soki=taxon_group) %>% left_join(temp,by="taxon_aphia_id") %>% dplyr::arrange(source_id,record_id,taxon_name)
        ##
        ##col_order <- intersect(columns_order("lipids"), names(blah))
        ##if (!all(names(blah) %in% col_order)) stop("columns dropped?")
        ##blah <- blah[,col_order]
        ##
        ##if (is_public) blah <- blah %>% select(-is_public_flag,-entered_by) ## but leave these in if dumping non-public data
        ##write.csv(blah,file="c:/i/projects/trophic_networks/so_web/scar_dump/scar_lipids.csv",row.names=FALSE,na="")
        ##
    } else if (records_type %in% c("diet", "dna_diet")) {
        new_data$prey_is_aggregate[is.na(new_data$prey_is_aggregate)] <- "N"
        assert_that(all(new_data$prey_is_aggregate %in% c("N", "Y")), msg = "prey_is_aggregate must be Y or N in new_data$records")

        ##skip_vars <- c("predator_total_count","predator_taxon_id","prey_taxon_id","geometrypoint","rowid","prey_mass_total","prey_mass_per_sample","habitat_type","gaz_id","predator_rank","prey_rank")
        ##blah <- blah[,!names(blah) %in% skip_vars]
        new_data$predator_group_soki <- NA_character_
        new_data$prey_group_soki <- NA_character_

        new_data$record_id <- new_record_id + seq_len(nrow(new_data))

        ## dates are char, so leave as is
        col_order <- c(intersect(columns_order(records_type), names(new_data)), "source_details", "source_doi")
        if (!all(names(new_data) %in% col_order)) {
            print(setdiff(col_order, names(new_data)))
            print(setdiff(names(new_data), col_order))
            stop("columns dropped?")
        }
        new_data <- new_data[, col_order]
    } else {
        stop(records_type, " not coded")
    }

    ## append new records to existing
    new_data <- new_data[, setdiff(names(new_data), c("is_public_flag", "entered_by"))]
    new_data <- validate_data(new_data, records_type = records_type)
    existing_data <- validate_data(existing_data, records_type = records_type)
##    if (!setequal(names(new_data), names(existing_data))) {
##        stop("column names of new_data differ from those of existing_data")
##    }
#    print(dplyr::glimpse(existing_data))
#    print(dplyr::glimpse(new_data))
    appended_data <- bind_rows(existing_data, new_data)

    ## check data type of each column
    validate_data(appended_data, records_type = records_type)

    list(sources = appended_sources, records = appended_data)
}

remap_source_ids <- function(ids, source_mapping) {
    new_ids <- ids
    for (uid in unique(na.omit(ids))) {
        new_id <- if (sum(uid == source_mapping$from) == 1) source_mapping$to[uid == source_mapping$from] else NA_integer_
        new_ids[which(ids == uid)] <- new_id
    }
    new_ids
}

## does source already exist in the existing_sources dataframe?
find_existing_source <- function(source, existing_sources) {
    ## check on DOIs first
    clean_dois <- clean_doi(existing_sources$doi)
    if (is_nonempty(source$doi)) {
        idx <- which(source$doi == clean_dois)
        if (length(idx) == 1) {
            return(existing_sources$source_id[idx])
        } else if (length(idx) > 1) {
            stop("multiple matching existing sources for DOI ", source$doi)
        }
    }
    ## check by details
    if (is_nonempty(source$details)) {
        idx <- which(str_trim(tolower(source$details)) == str_trim(tolower(existing_sources$details)))
        if (length(idx) == 1) {
            return(existing_sources$source_id[idx])
        } else if (length(idx) > 1) {
            stop("multiple matching existing sources for details ", source$details)
        }
    }
    NULL
}

left_join_check <- function(x, ...) {
    out <- do.call(dplyr::left_join, list(x, ...))
    if (nrow(out) != nrow(x)) stop("left_join added rows")
    out
}
