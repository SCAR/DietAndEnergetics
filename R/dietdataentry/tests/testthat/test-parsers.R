context("data file parsers")

test_that("diet data parser works", {
    filename <- system.file("extdata/example_diet_data.xls", package = "dietdataentry")
    expect_warning(x <- parse_diet(filename))
    expect_is(x, "list")
    expect_named(x, c("sources", "records", "raw"))
})

test_that("isotopes data parser works", {
    filename <- system.file("extdata/example_isotope_data.xls", package = "dietdataentry")
    expect_warning(x <- parse_isotopes(filename), "unmatched names: Eudyptes chrysocome filholi")
    expect_is(x, "list")
    expect_named(x, c("sources", "records", "raw"))
})
