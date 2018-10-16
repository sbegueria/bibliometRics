testthat::context("Just a simple test")


testthat::test_that("bibliometric works", {

  wos_file = system.file("sbegueria.txt", package = "bibliometRics")
  testthat::expect_silent({
    bib <- read.wos(wos_file)
  })
  base = read.baselines()
  discipline = 'GEOSCIENCES'
  testthat::expect_silent({
    bibliometric(bib, base, 'GEOSCIENCES')
  })
})
