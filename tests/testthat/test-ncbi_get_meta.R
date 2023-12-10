test_that("ncbi_get_meta() works with Microthrix parvicella", {
  meta <- ncbi_get_meta("Microthrix parvicella", db = "biosample")
  expect_s3_class(meta$meta, c("tbl_df", "tbl", "data.frame"))
})



