test_that("ncbi_get_uid works for a single query",{
  res <- ncbi_get_uid("GCA_003012895.2", db = "assembly")
  
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))
  expect_true("4253631" %in% res$uid)
})

test_that("ncbi_get_uid works with a complex term", {
  res <- ncbi_get_uid("Autographiviridae OR Podoviridae", db = "assembly")
  
  expect_s3_class(res, c("tbl_df", "tbl", "data.frame"))
  expect_true(nrow(res) > 3000)
})
