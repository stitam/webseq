test_that("ncbi_get_meta() works with Microthrix parvicella", {
  meta <- suppressWarnings(
    ncbi_get_meta("Microthrix parvicella", db = "biosample")
  )
  expect_s3_class(meta$meta, c("tbl_df", "tbl", "data.frame"))
})

test_that("ncbi_get_meta() works with all supported dbs", {
  data(examples)
  
  expect_true(all(names(examples) %in% ncbi_dbs()))
  
  for (i in names(examples)) {
    res <- suppressWarnings(ncbi_get_meta(examples[[i]], db = i, parse = FALSE))
    
    expect_true(all(class(res) == c("ncbi_meta", "list")))
    expect_true("meta" %in% names(res))
    expect_true(class(res$meta) == "list")
    expect_true(class(res$meta[[1]]) == "character")
  }
})
