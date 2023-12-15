data(examples)

test_that("ncbi_get_uid() works for a single query",{
  
  res <- ncbi_get_uid(examples$assembly[1], db = "assembly")
  
  expect_true(all(c("ncbi_uid", "list") %in% class(res)))
  expect_true(res$uid == 5197591)
  expect_equal(res$db, "assembly")
  expect_equal(nrow(res$web_history), 1)
})

test_that("ncbi_get_uid() works for a single query without web history",{
  
  res <- ncbi_get_uid(
    examples$assembly[1], db = "assembly", use_history = FALSE)
  
  expect_true(all(c("ncbi_uid", "list") %in% class(res)))
  expect_true(res$uid == 5197591)
  expect_equal(nrow(res$web_history), 0)
  
})

test_that("ncbi_get_uid() handles NA", {
  expect_error(ncbi_get_uid(term = NA, db = "assembly"))
  
  res <- suppressMessages(ncbi_get_uid(
    term = c(NA, examples$assembly[1], NA, examples$assembly[2]),
    db = "assembly",
    verbose = TRUE
  ))
  
  res_messages <- testthat::capture_messages(ncbi_get_uid(
    term = c(NA, examples$assembly[1], NA, examples$assembly[2]),
    db = "assembly",
    verbose = TRUE
  ))
  
  expect_true(all(c("ncbi_uid", "list") %in% class(res)))
  expect_equal(length(res$uid), 2)
  
  expect_true(res_messages[1] == "Removing NA-s from search terms.\n")
  expect_true(res_messages[2] == "Querying UIDs for batch 1. ")
  expect_true(res_messages[3] == "rentrez::entrez_search() query successful.\n")
})

test_that("ncbi_get_uid() handles invalid terms", {
  res <- suppressMessages(
    ncbi_get_uid(term = "noname", db = "assembly", verbose = TRUE)
  )
  res_messages <- capture_message(
    res <- ncbi_get_uid(term = "noname", db = "assembly", verbose = TRUE)
  )
  
  expect_true(all(c("ncbi_uid", "list") %in% class(res)))
  expect_true(is.na(res$uid))
  expect_equal(nrow(res$web_history), 0)
})

test_that("ncbi_get_uid works with a complex term", {
  res <- ncbi_get_uid("Autographiviridae OR Podoviridae", db = "assembly")

  expect_true(length(res$uid) > 3000)
})
