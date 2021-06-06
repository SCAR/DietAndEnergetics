context("taxonomic groups")
test_that("soki_ruleset works", {
    worms_cache_directory <- NULL
    expect_warning(rules <- soki_ruleset(worms_cache_directory = worms_cache_directory), "Gammaridea")
    expect_equal(rules(search_worms("Eurythenes obesus", cache_directory = worms_cache_directory), "prey", worms_cache_directory = worms_cache_directory), "Amphipoda (amphipods)")

    expect_equal(rules(search_worms("Aptenodytes forsteri", cache_directory = worms_cache_directory), "predator", worms_cache_directory = worms_cache_directory), "<i>Aptenodytes forsteri</i> (emperor penguin)")

    expect_equal(rules(search_worms("Aptenodytes forsteri", cache_directory = worms_cache_directory), "prey", worms_cache_directory = worms_cache_directory), "Spheniscidae (penguins)")
})
