test_that("get_meta() works with Assembly UID-s", {
  assembly_uid <- get_uid("GCF_000695855.3", db = "assembly")
  res <- get_meta(assembly_uid$uid, db = "assembly")
  
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))
})

test_that("get_meta() works with BioSample UID-s", {
  biosample_uid <- get_uid("SAMN02714232", db = "biosample")
  res <- get_meta(biosample_uid$uid, db = "biosample")
  
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))
})
