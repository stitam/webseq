test_that("ncbi_parse() works with BioSamples", {
  data(examples)
  biosample_uid <- ncbi_get_uid(examples$biosample, db = "biosample")
  meta_xml <- suppressWarnings(
    ncbi_get_meta(examples$biosample[1], db = "biosample", parse = FALSE)
  )
  res <- ncbi_parse(meta = meta_xml$meta, db = "biosample", format = "xml")
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))
  expect_equal(res$biosample[1], "SAMN02714232")
})
