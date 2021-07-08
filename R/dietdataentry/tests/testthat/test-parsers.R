context("data file parsers")

test_that("diet data parser works", {
    filename <- system.file("extdata/example_diet_data.xls", package = "dietdataentry")
    expect_warning(x <- parse_diet(filename))
    expect_is(x, "list")
    expect_named(x, c("sources", "records", "raw"))
    expect_is(x$sources, "data.frame")
    expect_is(x$records, "data.frame")
})

test_that("isotopes data parser works", {
    filename <- system.file("extdata/example_isotope_data.xls", package = "dietdataentry")
    chk <- capture.output(x <- parse_isotopes(filename))
    expect_true(any(grepl("unmatched names:[[:space:]]+Eudyptes chrysocome filholi", chk)))
    expect_is(x, "list")
    expect_named(x, c("sources", "records", "raw"))
    expect_is(x$sources, "data.frame")
    expect_is(x$records, "data.frame")
})

test_that("lipids data parser works", {
    filename <- system.file("extdata/example_lipids_data.xls", package = "dietdataentry")
    x <- parse_lipids(filename)
    expect_is(x, "list")
    expect_named(x, c("sources", "records", "raw"))
    expect_is(x$sources, "data.frame")
    expect_is(x$records, "data.frame")
})

test_that("energetics data parser works", {
    filename <- system.file("extdata/example_energetics_data.xls", package = "dietdataentry")
    x <- parse_energetics(filename)
    expect_is(x, "list")
    expect_named(x, c("sources", "records", "raw"))
    expect_is(x$sources, "data.frame")
    expect_is(x$records, "data.frame")
})
