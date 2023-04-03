test_that("BioSample metadata from BioSample ID", {
  biosample_uid <- ncbi_get_uid("SAMN02714232", db = "biosample")
  res <- ncbi_meta_biosample(biosample_uid$uid)
  
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))
})

test_that("BioSample metadata from Assembly ID", {
  assembly_uid <- ncbi_get_uid("GCF_000695855.3")
  biosample_uid <- ncbi_link_uids(
    assembly_uid$uid, from = "assembly", to = "biosample")
  res <- ncbi_meta_biosample(biosample_uid$result)
  
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))
})