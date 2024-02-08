test_that("ncbi_parse() works with a BioSample", {
  data(examples)
  biosample_uid <- ncbi_get_uid(examples$biosample[1], db = "biosample")
  res <- ncbi_get_meta(biosample_uid)
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))
  expect_equal(res$biosample, "SAMN02714232")
})

test_that("ncbi_parse() works with all BioSamples", {
  data(examples)
  biosample_uid <- ncbi_get_uid(examples$biosample, db = "biosample")
  res <- ncbi_get_meta(biosample_uid)
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(res), length(examples$biosample))
})

