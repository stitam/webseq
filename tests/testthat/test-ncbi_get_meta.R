options("Ncpu" = 2L)

test_that("ncbi_get_meta() works with history", {
  # with history, use history, one batch
  uids <- ncbi_get_uid("Microthrix parvicella", db = "biosample")
  meta <- suppressWarnings(ncbi_get_meta(uids))
  testthat::expect_true(inherits(meta, "list"))
  testthat::expect_s3_class(
    meta$main, 
    c("ncbi_meta", "tbl_df", "tbl", "data.frame")
  )
  
  # with history, use history, multiple batches
  uids <- ncbi_get_uid(
    "Microthrix parvicella", 
    db = "biosample",
    batch_size = 5
  )
  meta <- suppressWarnings(ncbi_get_meta(uids))
  testthat::expect_true(inherits(meta, "list"))
  testthat::expect_s3_class(
    meta$main, 
    c("ncbi_meta", "tbl_df", "tbl", "data.frame")
  )
  testthat::expect_equal(nrow(meta$main), length(uids$uid))
})

test_that("ncbi_get_meta() works without history", {
  # with history, do not use history, one batch
  uids <- ncbi_get_uid(
    "Microthrix parvicella",
    db = "biosample",
    use_history = TRUE
  )
  meta <- suppressWarnings(ncbi_get_meta(uids, use_history = FALSE))
  testthat::expect_true(inherits(meta, "list"))
  testthat::expect_s3_class(
    meta$main, 
    c("ncbi_meta", "tbl_df", "tbl", "data.frame")
  )
  
  # with history, do not use history, multiple batches
  meta <- suppressWarnings(ncbi_get_meta(
    uids,
    use_history = FALSE,
    batch_size = 5
  ))
  testthat::expect_true(inherits(meta, "list"))
  testthat::expect_s3_class(
    meta$main, 
    c("ncbi_meta", "tbl_df", "tbl", "data.frame")
  )
  
  # without history, attempt to use history but fall back, one batch
  uids <- ncbi_get_uid(
    "Microthrix parvicella",
    db = "biosample",
    use_history = FALSE
  )
  meta <- suppressWarnings(ncbi_get_meta(uids))
  testthat::expect_true(inherits(meta, "list"))
  testthat::expect_s3_class(
    meta$main, 
    c("ncbi_meta", "tbl_df", "tbl", "data.frame")
  )
  # only ids, one batch
  meta <- suppressWarnings(ncbi_get_meta(
    uids$uid,
    db = "biosample"
  ))
  testthat::expect_true(inherits(meta, "list"))
  testthat::expect_s3_class(
    meta$main, 
    c("ncbi_meta", "tbl_df", "tbl", "data.frame")
  )
  
  # only ids, multiple batches
  meta <- suppressWarnings(ncbi_get_meta(
    uids$uid,
    db = "biosample",
    batch_size = 5
  ))
  testthat::expect_true(inherits(meta, "list"))
  testthat::expect_s3_class(
    meta$main, 
    c("ncbi_meta", "tbl_df", "tbl", "data.frame")
  )
})

test_that("ncbi_get_meta() works with all supported dbs", {
  data(examples)
  
  expect_true(all(names(examples) %in% ncbi_dbs()))
  
  for (i in names(examples)) {
    uids <- ncbi_get_uid(examples[[i]], db = i)
    meta <- suppressWarnings(ncbi_get_meta(uids, parse = FALSE))
    expect_s3_class(meta, c("ncbi_meta", "list"))
  }
})

test_that("ncbi_get_meta() fails if input is invalid", {
  expect_error(suppressWarnings(
    ncbi_get_meta("funky", db = "assembly")
  ))
})
