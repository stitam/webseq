test_that("ncbi_meta() works with BioSample ID-s", {
  biosample_uid <- ncbi_get_uid("SAMN02714232", db = "biosample")
  res <- ncbi_meta(biosample_uid)
  
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))
})

test_that("ncbi_meta() works with BioSample UID-s", {
  assembly_uid <- ncbi_get_uid("GCF_000695855.3")
  biosample_uid <- link_uids(assembly_uid, from = "assembly", to = "biosample")
  res <- ncbi_meta(biosample_uid)
  
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))
})