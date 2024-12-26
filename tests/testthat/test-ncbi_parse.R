options("Ncpu" = 2L)

test_that("ncbi_parse() works with a BioSample", {
  data(examples)
  biosample_uid <- ncbi_get_uid(examples$biosample[1], db = "biosample")
  res <- ncbi_get_meta(biosample_uid)
  testthat::expect_true(inherits(res, "list"))
  testthat::expect_s3_class(res$main, c("tbl_df", "tbl", "data.frame"))
  expect_equal(res$main$biosample, "SAMN02714232")
})

test_that("ncbi_parse() works with all BioSamples", {
  data(examples)
  biosample_uid <- ncbi_get_uid(examples$biosample, db = "biosample")
  res <- ncbi_get_meta(biosample_uid)
  testthat::expect_true(inherits(res, "list"))
  testthat::expect_equal(length(res), 2)
  testthat::expect_equal(names(res), c("main", "antibiogram"))
  testthat::expect_s3_class(res$main, c("tbl_df", "tbl", "data.frame"))
  testthat::expect_s3_class(res$antibiogram, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(res$main), length(examples$biosample))
})

