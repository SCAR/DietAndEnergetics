context("DOI resolution")

test_that("resolve_doi works", {
    expect_equal(resolve_doi("10.1007/s00300-012-1222-3"), "http://link.springer.com/10.1007/s00300-012-1222-3")
    expect_equal(resolve_doi(" 10.1007/s00300-012-1222-3"), "http://link.springer.com/10.1007/s00300-012-1222-3")
    expect_equal(resolve_doi("doi:10.1007/s00300-012-1222-3"), "http://link.springer.com/10.1007/s00300-012-1222-3")
    expect_equal(resolve_doi(URLencode("doi:10.1007/s00300-012-1222-3")), "http://link.springer.com/10.1007/s00300-012-1222-3")
    expect_equal(resolve_doi("http://dx.doi.org/10.1007/s00300-012-1222-3"), "http://link.springer.com/10.1007/s00300-012-1222-3")
    expect_equal(resolve_doi(URLencode("http://dx.doi.org/10.1007/s00300-012-1222-3")), "http://link.springer.com/10.1007/s00300-012-1222-3")
    expect_equal(resolve_doi("10.26179/5d1aec22f41d5"), "http://data.aad.gov.au/metadata/records/SCAR_Diet_Energetics")
})
