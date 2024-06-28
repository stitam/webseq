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
