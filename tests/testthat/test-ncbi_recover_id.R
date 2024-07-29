data(examples)

test_that("ncbi_recover_id() works with ncbi_uid objects", {
  uids <- ncbi_get_uid(examples$assembly, db = "assembly", batch_size = 1)
  ids <- ncbi_recover_id(uids)
  
  expect_equal(examples$assembly, ids)
  
})

test_that("ncbi_recover_id() works with UIDs", {
  uids <- ncbi_get_uid(examples$assembly, db = "assembly", batch_size = 1)
  ids <- ncbi_recover_id(uids$uid, db = "assembly")
  
  expect_equal(examples$assembly, ids)
})

test_that("ncbi_recover_id() works with duplicates", {
  uids <- c(2597423, 2597423)
  ids <- ncbi_recover_id(uids, db = "biosample")
  
  expect_equal(ids, c("SAMN02597423", "SAMN02597423"))
})

test_that("ncbi_recover_id() works with nuccore", {
  uid <- ncbi_get_uid("OP617744.1", db = "nuccore")
  id <- ncbi_recover_id(uid)
  expect_equal(id,"OP617744.1")
})
