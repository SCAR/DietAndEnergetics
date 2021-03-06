#' Search for valid names
#'
#' This function is a wrapper around \code{\link[worrms]{wm_records_name}} and related functions, that does its best to find a single valid match for our query.
#'
#' @param scientific string: scientific name. This may also include special filters like `"Ctenophora@phylum:Ctenophora"` or `"Lumbricidae@marine_only:FALSE"`, see examples below
#' @param common string: common name
#' @param ids numeric or string: AphiaID to match on
#' @param force logical: if `TRUE`, force a cache refresh
#' @param follow_valid logical: if `TRUE` follow an invalid name to its valid name
#' @param like logical: if `TRUE`, add a percent sign after the name (SQL LIKE function)
#' @param try_fuzzy logical: if `TRUE`, try fuzzy matching on the query if an exact match fails to find a valid record
#' @param cache_directory string: path to a cache directory
#' @param acceptable_status character: vector of status values that we consider to be acceptable
#' @param follow character: vector of status values that we will follow through to try and find a valid name
#' @param filter list: for internal use
#' @param ... : additional parameters passed to \code{link[worrms]{wm_records_name}}
#'
#' @return A data.frame (tibble) as returned by \code{\link[worrms]{wm_records_name}}
#'
#' @examples
#' \dontrun{
#'   search_worms("Pachyptila turtur")
#'
#'   search_worms(common = "Antarctic silverfish")
#'
#'   ## valid name is Aequiyoldia eightsii
#'   search_worms("Yoldia eightsii")
#'
#'   ## if we really wanted the unaccepted name record, we could do
#'   search_worms("Yoldia eightsii", acceptable_status = "unaccepted")
#'   ## or
#'   search_worms("Yoldia eightsii@status:unaccepted")
#'   ## but that's not really the point of the search_worms function, which is
#'   intended to resolve names to their valid versions
#'   ## if we wanted to see all possible matches for a name, use
#'   ## worms_records("Yoldia eightsii")
#'
#'   ## typo
#'   search_worms("Vibllia stebbingi")
#'
#'   ## typo with fuzzy matching
#'   search_worms("Vibllia stebbingi", try_fuzzy = TRUE)
#'
#'   ## the actual name
#'   search_worms("Vibilia stebbingi")
#'
#'   ## nomen dubium example
#'   search_worms("Teuthida")
#'
#'   ## multiple exact matches
#'   search_worms("Ctenophora")
#'   ## we can restrict this search, e.g.
#'   ## search for Ctenophora that has phylum name Ctenophora
#'   search_worms("Ctenophora@phylum:Ctenophora")
#'   ## or a name at a certain rank
#'   search_worms("Ctenophora@rank:phylum")
#'
#'   ## earthworms, these don't appear by default because they are not a marine taxon
#'   search_worms("Lumbricidae")
#'   ## allow non-marine taxa
#'   search_worms("Lumbricidae@marine_only:FALSE")
#'   ## or equivalently
#'   search_worms("Lumbricidae", marine_only = FALSE)
#'
#' includes [Myopsida + Oegopsida] which are not demonstrated to form a clade
#' }
#' @export
search_worms <- function(scientific = NULL, common = NULL, ids = NULL, force = FALSE, follow_valid = TRUE, like = FALSE, try_fuzzy = FALSE, cache_directory = worms_cache_dir(), acceptable_status = c("accepted", "temporary name", "uncertain@unacceptreason:unassessed", "uncertain@unacceptreason:requiring further investigation", "uncertain@unacceptreason:NA", "taxon inquirendum", "nomen dubium"), follow = c("unaccepted", "alternate representation"), filter = NULL, ...) {
    ## "nomen dubium" needed for Genaxinus bongraini, Teuthida but don't want it for some others
    do_cache <- TRUE
    if (missing(filter)) filter <- NULL
    if (!missing(cache_directory)) {
        if (is.null(cache_directory)) {
            do_cache <- FALSE
        } else {
            if (!dir.exists(cache_directory)) stop("the specified cache_directory ", cache_directory, " does not exist")
            setCacheRootPath(path = cache_directory)
        }
    } else if (length(cache_directory) < 1 || !nzchar(cache_directory)) {
        do_cache <- FALSE
    }
    if ((!missing(scientific) && !is.na(scientific) && scientific == "Gammaridea") ||
        (!missing(ids) && !is.na(ids) && all(ids == 1207))) {
        warning("temporarily using Gammaridea even though it is unaccepted")
        acceptable_status <- "unaccepted"
    }
    cleanup <- function(name) {
        name <- gsub("[\r\n]+","",name)
        name <- gsub("[[:space:]]+"," ",name)
        name <- gsub("^[[:space:]]+","",name)
        gsub("[[:space:]]+$","",name)
    }
    fix_types <- function(x) {
        char2num <- c("AphiaID","valid_AphiaID")
        for (cc in char2num) {
            if (cc %in% names(x) && inherits(x[,cc],"character"))
                x[,cc] <- as.numeric(x[,cc])
        }
        x
    }

    ## memoized wm_record
    mworms_record <- function(...) unique(do.call(mwormsi_record, ...))
    mwormsi_record <- function(...) {
        if (do_cache) {
            tryCatch(memoizedCall(wm_record, ...), ## memoized version that will cache results to disk
                     error = function(e) if (grepl("No Content|Not Found", conditionMessage(e), ignore.case = TRUE)) return(data.frame()) else stop(conditionMessage(e)))
        } else {
            args <- list(...)
            args <- args[!names(args) %in% c("force", "fuzzy")] ## drop the "force" argument, which only applied to memoizedCall
            tryCatch(do.call(wm_record, args), ## no caching of results
                     error = function(e) if (grepl("No Content|Not Found", conditionMessage(e), ignore.case = TRUE)) return(data.frame()) else stop(conditionMessage(e)))
        }
    }

    ## search on scientific name
    mworms <- function(...) unique(do.call(mwormsi, ...)) ## do.call to handle args as a list. sometimes multiple identical rows, so use unique
    mwormsi <- function(...) {
        if (do_cache) {
            tryCatch(memoizedCall(wm_records_name, ...), ## memoized version that will cache results to disk
                     error = function(e) if (grepl("No Content|Not Found", conditionMessage(e), ignore.case = TRUE)) return(data.frame()) else stop(conditionMessage(e)))
        } else {
            args <- list(...)
            args <- args[!names(args) %in% "force"] ## drop the "force" argument, which only applied to memoizedCall
            tryCatch(do.call(wm_records_name, args), ## no caching of results
                     error = function(e) if (grepl("No Content|Not Found", conditionMessage(e), ignore.case = TRUE)) return(data.frame()) else stop(conditionMessage(e)))
        }
    }

    ## search on common name
    cworms <- function(...) unique(do.call(cwormsi, ...))
    cwormsi <- function(...) {
        if (do_cache) {
            tryCatch(memoizedCall(wm_records_common, ...), ## memoized version that will cache results to disk
                     error = function(e) if (grepl("No Content", conditionMessage(e), ignore.case = TRUE)) return(data.frame()) else stop(conditionMessage(e)))
        } else {
            args <- list(...)
            args <- args[!names(args) %in% "force"] ## drop the "force" argument, which only applied to memoizedCall
            tryCatch(do.call(wm_records_common, args), ## no caching of results
                     error = function(e) if (grepl("No Content", conditionMessage(e), ignore.case = TRUE)) return(data.frame()) else stop(conditionMessage(e)))
        }
    }

    ## fuzzy matching
    fworms <- function(...) unique(do.call(fwormsi, ...))
    fwormsi <- function(...) {
        if (do_cache) {
            tryCatch({
                res <- memoizedCall(wm_records_taxamatch, ...) ## memoized version that will cache results to disk
                if (length(res) > 0) res <- res [[1]]
            }, error = function(e) if (grepl("No Content", conditionMessage(e), ignore.case = TRUE)) return(data.frame()) else stop(conditionMessage(e)))
        } else {
            args <- list(...)
            args <- args[!names(args) %in% "force"] ## drop the "force" argument, which only applied to memoizedCall
            tryCatch({
                res <- do.call(wm_records_taxamatch, args) ## no caching of results
                if (length(res) > 0) res <- res [[1]]
            }, error = function(e) if (grepl("No Content", conditionMessage(e), ignore.case = TRUE)) return(data.frame()) else stop(conditionMessage(e)))
        }
    }

    handle_results <- function(x, search_term, dots, follow_valid_names = TRUE) {
        if (nrow(x) < 1) return(x)
        ## do we have a filter
        filtokidx <- rep(TRUE, nrow(x))
        if (!is.null(filter)) {
            for (k in names(filter)) filtokidx <- filtokidx & x[, k] %eq% filter[[k]]
        }
        if ("rank" %in% names(dots)) filtokidx <- filtokidx & tolower(x$rank) %eq% dots$rank
        statusok_idx <- rep(TRUE, nrow(x))
        if (!is.null(acceptable_status)) {
            statusok_idx <- tolower(x$status) %in% tolower(acceptable_status)
            conditional_status <- acceptable_status[grepl(":", acceptable_status)]
            for (cs in conditional_status) {
                bits <- str_match_all(cs, "(.+)@(.+):(.+)")[[1]]
                if (bits[1, 4] %eq% "NA") {
                    statusok_idx <- statusok_idx | (tolower(x$status) %eq% tolower(bits[1, 2]) & is.na(x[[bits[1, 3]]]))
                } else {
                    statusok_idx <- statusok_idx | (tolower(x$status) %eq% tolower(bits[1, 2]) & x[[bits[1, 3]]] %eq% bits[1, 4])
                }
            }
        }
        ## special case: status is "alternate representation" but the record points to itself as the valid name (!!)
        statusok_idx <- statusok_idx | (!is.na(x$status) & tolower(x$status) == "alternate representation" & !is.na(x$valid_AphiaID) & x$valid_AphiaID == x$AphiaID)
        if (!is.null(search_term)) {
            ok <- x[filtokidx & statusok_idx & (!is.na(x$scientificname) & tolower(x$scientificname) == tolower(search_term)), ]
        } else {
            ok <- x[filtokidx & statusok_idx, ]
        }
        if (nrow(ok)<1) {
            ## relax restrictions to include nomen dubium names
            if (!is.null(search_term)) {
                ok <- x[tolower(x$status) %in% acceptable_status & (!is.na(x$scientificname) & tolower(x$scientificname)==tolower(search_term)),]
            } else {
                ok <- x[tolower(x$status) %in% acceptable_status,]
            }
        }
        if (nrow(ok)==1) {
            ## single result that matched our query
            fix_types(ok)
        } else if (nrow(ok) < 1) {
            ## no valid names. can we redirect to a valid name?
            ## first look for an exact match to our query name
            redirect <- tolower(x$status) %in% follow & x$scientificname %eq% search_term & !(is.na(x$valid_AphiaID) | x$valid_AphiaID==0)
            if (sum(redirect) < 1) {
                ## no exact match on search term
                redirect <- tolower(x$status) %in% follow & !(is.na(x$valid_AphiaID) | x$valid_AphiaID==0)
            }
            if (sum(redirect) == 1) {
                if (follow_valid_names) {
                    warning("valid name is ", x$valid_name[redirect], " (AphiaID ", x$valid_AphiaID[redirect], "), searching on this")
                    dots$scientific <- dots$name <- NULL
                    dots$id <- x$valid_AphiaID[redirect]
                    x <- mworms_record(dots)
                    handle_results(x, NULL, dots, follow_valid_names = FALSE)
                } else {
                    data.frame()
                }
            } else if (sum(redirect) < 1) {
                data.frame() ## no results
                ## try fuzzy here?
            } else if (sum(redirect) > 1) {
                ## ambiguous
                tmp <- if (is.null(search_term)) "" else paste0("name \"", search_term, "\" is ")
                warning(tmp, "ambiguous")
                data.frame()
            }
        } else {
            ## must have been more than one exact match?
            warning("multiple exact matches")
            fix_types(ok)
        }
    }
    dots <- list(...)
    if (!is.null(scientific)) {
        if (grepl("cf[ \\.]",scientific)) stop("name contains cf, not coded")
        if (grepl(":", scientific)) {
            bits <- str_match_all(scientific, "(.+)@(.+):(.+)")[[1]]
            if (nrow(bits)<1 || ncol(bits)<4) stop("thing@thing:thing filter did not match")
            if (bits[1, 3]=="status") {
                acceptable_status <- c(bits[1, 4], acceptable_status)
                scientific <- bits[1, 2]
            } else if (bits[1, 3] == "marine_only") {
                dots$marine_only <- tolower(bits[1, 4]) %in% c("true", "t", "1")
                scientific <- bits[1, 2]
            } else if (bits[1, 3] == "rank") {
                dots$rank <- tolower(bits[1, 4])
                scientific <- bits[1, 2]
            } else {
                if (!is.null(filter)) stop("filter already specified, can't use thing@thing:thing notation")
                filter <- list(bits[1, 4])
                names(filter) <- bits[1, 3]
                scientific <- bits[1, 2]
            }
        }
        search_term <- cleanup(scientific)
        dots$name <- scientific
        fxn <- mworms
    } else if (!is.null(common)) {
        search_term <- cleanup(common)
        dots$name <- common
        fxn <- cworms
    } else if (!is.null(ids)) {
        search_term <- NULL
        fxn <- mworms_record
        dots$id <- ids
    } else {
        stop("should not be here")
    }
    dots$force <- force
    dots$fuzzy <- like ## this is the percent thing for mworms and cworms
    x <- fxn(dots)
    if (nrow(x)<1) {
        ## no matches
        if (try_fuzzy) {
            ## try fuzzy search
            dots$fuzzy <- TRUE
            x <- fworms(dots)
            search_term <- NULL
        }
    }
    handle_results(x, search_term, dots, follow_valid_names = follow_valid)
}



#' Get common name for Aphia ID
#'
#' @param ids numeric or string: AphiaID to match on
#' @param cache_directory string: path to a cache directory
#'
#' @return A tibble
#'
#' @export
worms_common <- function(ids, cache_directory = worms_cache_dir()) {
    do_cache <- TRUE
    if (!missing(cache_directory)) {
        if (is.null(cache_directory)) {
            do_cache <- FALSE
        } else {
            if (!dir.exists(cache_directory)) stop("the specified cache_directory ", cache_directory, " does not exist")
            setCacheRootPath(path = cache_directory)
        }
    } else {
        do_cache <- FALSE
    }
    ## memoized
    mwormsi_common_id <- function(...) {
        if (do_cache) {
            tryCatch(memoizedCall(wm_common_id, ...), ## memoized version that will cache results to disk
                     error = function(e) if (grepl("No Content|Not Found", conditionMessage(e), ignore.case = TRUE)) return(data.frame()) else stop(conditionMessage(e)))
        } else {
            args <- list(...)
            tryCatch(do.call(wm_common_id, args), ## no caching of results
                     error = function(e) if (grepl("No Content|Not Found", conditionMessage(e), ignore.case = TRUE)) return(data.frame()) else stop(conditionMessage(e)))
        }
    }
    unique(mwormsi_common_id(id = ids))
}

#' Get full hierarchy for Aphia ID
#'
#' @param id numeric or string: AphiaID to match on
#' @param cache_directory string: path to a cache directory
#'
#' @return A tibble
#'
#' @export
worms_hierarchy <- function(id, cache_directory = worms_cache_dir()) {
    do_cache <- TRUE
    if (!missing(cache_directory)) {
        if (is.null(cache_directory)) {
            do_cache <- FALSE
        } else {
            if (!dir.exists(cache_directory)) stop("the specified cache_directory ", cache_directory, " does not exist")
            setCacheRootPath(path = cache_directory)
        }
    } else {
        do_cache <- FALSE
    }
    ## memoized
    mwormsi_hier <- function(...) {
        if (do_cache) {
            tryCatch(memoizedCall(wm_classification, ...), ## memoized version that will cache results to disk
                     error = function(e) if (grepl("No Content|Not Found", conditionMessage(e), ignore.case = TRUE)) return(data.frame()) else stop(conditionMessage(e)))
        } else {
            args <- list(...)
            tryCatch(do.call(wm_classification, args), ## no caching of results
                     error = function(e) if (grepl("No Content|Not Found", conditionMessage(e), ignore.case = TRUE)) return(data.frame()) else stop(conditionMessage(e)))
        }
    }
    mwormsi_hier(id = id)
}

#' Set the worms cache directory
#'
#' @param cache_directory string: path to a cache directory, or `NULL` to clear the current setting
#'
#' @export
worms_cache_dir <- function(cache_directory) {
    opts <- options()$dietdataentry
    if (is.null(opts)) opts <- list()
    if (missing(cache_directory)) return(opts$worms_cache_directory)
    opts$worms_cache_directory <- cache_directory
    options(dietdataentry = opts)
}

