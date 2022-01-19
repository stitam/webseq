test_that("get_uid works for a single query",{
  res <- get_uid("GCA_003012895.2")
  
  expect_type(res, "character")
  expect_equal(res, "4253631")
})

test_that("get_uid works with a complex term", {
  res <- get_uid("Autographiviridae OR Podoviridae", db = "assembly")
  
  expect_type(res, "character")
  expect_true(length(res) > 3000)
})