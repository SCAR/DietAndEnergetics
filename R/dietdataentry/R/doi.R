clean_doi <- function(doi) {
    assert_that(is.character(doi))
    doi <- gsub("https?://dx.doi.org/","", tolower(doi))
    doi <- gsub("^doi:", "", doi)
    str_trim(doi)
}


#' Resolve a DOI
#'
#' @references http://www.doi.org/
#' @param doi string: DOI to resolve
#'
#' @return The URL (as a string) that the DOI resolves
#'
#' @examples
#' \dontrun{
#'   resolve_doi("10.26179/5d1aec22f41d5")
#' }
#' @export
resolve_doi <- function(doi) {
    assert_that(is.string(doi)) ## single DOI string
    doi <- clean_doi(doi)
    cat("resolving doi: ", doi, "\n")
    if (is.na(doi)) {
        NULL
    } else if (grepl("^http[s]?://", doi, ignore.case=TRUE)) {
        ## is a URL, just pass it back
        doi
    } else {
        doi <- content(GET(paste0("http://dx.doi.org/api/handles/",doi)))
        out <- as.character(Filter(Negate(is.null), sapply(doi$values, function(z) if (z$type=="URL") z$data$value)))
        if (length(out)<1) NULL else out
    }
}


