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
#'
#'   ## earthworms, these don't appear by default because they are not a marine taxon
#'   search_worms("Lumbricidae")
#'   ## allow non-marine taxa
#'   search_worms("Lumbricidae@marine_only:FALSE")
#' }
#' @export

search_worms <- function(scientific = NULL, common = NULL, ids = NULL, force = FALSE, follow_valid = TRUE, like = FALSE, try_fuzzy = FALSE, cache_directory, acceptable_status = c("accepted", "nomen dubium", "temporary name"), follow = c("unaccepted", "alternate representation"), filter = NULL, ...) {
    do_cache <- TRUE
    if (missing(filter)) filter <- NULL
    if (!missing(cache_directory)) {
        if (is.null(cache_directory)) {
            do_cache <- FALSE
        } else {
            if (!dir.exists(cache_directory)) stop("the specified cache_directory ",cache_directory," does not exist")
            setCacheRootPath(path=cache_directory)
        }
    } else {
        do_cache <- FALSE
    }
    if (!missing(scientific) && !is.na(scientific) && scientific=="Gammaridea") {
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

    handle_results <- function(x,search_term,dots,follow_valid_names=TRUE) {
        if (nrow(x)<1) return(x)
        ## do we have a filter
        filtokidx <- rep(TRUE,nrow(x))
        if (!is.null(filter)) {
            for (k in names(filter)) {
                ##cat(k, "matching to:", filter[[k]], "\n")
                ##cat(str(x[, k]))
                filtokidx <- filtokidx & x[,k] %eq% filter[[k]]
                ##cat(filtokidx)
            }
        }
        statusok_idx <- rep(TRUE, nrow(x))
        if (!is.null(acceptable_status)) statusok_idx <- tolower(x$status) %in% tolower(acceptable_status)
        if (!is.null(search_term)) {
            ok <- x[filtokidx & statusok_idx & (!is.na(x$scientificname) & tolower(x$scientificname)==tolower(search_term)),]
        } else {
            ok <- x[filtokidx & statusok_idx,]
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
            if (sum(redirect)==1) {
                if (follow_valid_names) {
                    warning("valid name is ",x$valid_name[redirect],", searching on this")
                    ##dots[["scientific"]] <- x$valid_name[redirect]
                    dots[["name"]] <- x$valid_name[redirect]
                    x <- mworms(dots)
                    handle_results(x,NULL,dots,follow_valid_names=FALSE)
                } else {
                    data.frame()
                }
            } else if (sum(redirect)<1) {
                data.frame() ## no results
                ## try fuzzy here?
            } else if (sum(redirect)>1) {
                ## ambiguous
                tmp <- if (is.null(search_term)) "" else paste0("name \"",search_term,"\" is ")
                warning(tmp,"ambiguous")
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
            } else if (bits[1, 3]=="marine_only") {
                dots$marine_only <- tolower(bits[1, 4]) %in% c("true", "t", "1")
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
    handle_results(x,search_term,dots,follow_valid_names=follow_valid)
}
