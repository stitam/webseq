test_that("ncbi_parse() works with BioSamples", {
  data(examples)
  biosample_uid <- ncbi_get_uid(examples$biosample, db = "biosample")
  meta_xml <- rentrez::entrez_fetch(
    db = "biosample",
    id = biosample_uid$uid,
    rettype = "full",
    retmode = "xml"
  )
  res <- ncbi_parse(meta = meta_xml, db = "biosample", format = "xml")
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))
  expect_equal(res$biosample[1], "SAMN02714232")
})
