context("taxonomy")
test_that("search_worms works", {

    res <- search_worms("Pachyptila turtur")
    expect_equal(nrow(res), 1)
    expect_equal(res$status, "accepted")

    expect_warning(res <- search_worms(common = "Antarctic silverfish"), "valid name is")
    expect_equal(nrow(res), 1)
    expect_equal(res$scientificname, "Pleuragramma antarctica")
    expect_equal(res$status, "accepted")

    ## valid name is Aequiyoldia eightsii
    expect_warning(res <- search_worms("Yoldia eightsii"), "valid name is")

    ## if we really wanted the unaccepted name record, we could do
    res <- search_worms("Yoldia eightsii@status:unaccepted")
    expect_equal(nrow(res), 1)
    expect_equal(res$status, "unaccepted")

    ## typo
    res <- search_worms("Vibllia stebbingi")
    expect_equal(nrow(res), 0)

    ## typo with fuzzy matching
    res <- search_worms("Vibllia stebbingi", try_fuzzy = TRUE)
    expect_equal(nrow(res), 1)
    expect_equal(res$scientificname, "Vibilia stebbingi")
    expect_equal(res$status, "accepted")

    ## nomen dubium example
    res <- search_worms("Teuthida")
    expect_equal(nrow(res), 1)
    expect_equal(res$status, "nomen dubium")

    ## multiple exact matches
    expect_warning(res <- search_worms("Ctenophora"), "multiple exact matches")
    expect_equal(nrow(res), 2)
    ## we can restrict this search, e.g.
    ## search for Ctenophora that has phylum name Ctenophora
    res <- search_worms("Ctenophora@phylum:Ctenophora")
    expect_equal(nrow(res), 1)

    ## earthworms, these don't appear by default because they are not a marine taxon
    res <- search_worms("Lumbricidae")
    expect_equal(nrow(res), 0)
    ## allow non-marine taxa
    res <- search_worms("Lumbricidae@marine_only:FALSE")
    expect_equal(nrow(res), 1)
})
