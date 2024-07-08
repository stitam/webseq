data(examples)

test_that("ncbi_link_uid() works with ncbi_uid objects", {
  assembly_uid <- ncbi_get_uid(examples$assembly, db = "assembly")
  biosample_uid <- ncbi_link_uid(
    assembly_uid, from = "assembly", to = "biosample"
  )
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in% class(biosample_uid)))
  expect_equal(dim(biosample_uid), c(3,2))
  expect_equal(biosample_uid$biosample, c(19523416, 2952905, 1730125))
})

test_that("ncbi_link_uid() can use an ncbi_uid object's db element", {
  assembly_uid <- ncbi_get_uid(examples$assembly, db = "assembly")
  biosample_uid <- ncbi_link_uid(assembly_uid,  to = "biosample")
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in% class(biosample_uid)))
  expect_equal(dim(biosample_uid), c(3,2))
  expect_equal(biosample_uid$biosample, c(19523416, 2952905, 1730125))
})

test_that("ncbi_link_uid() fails if 'from' or 'to' db in invalid", {
  assembly_uid <- ncbi_get_uid(examples$assembly, db = "assembly")
  expect_error(
    ncbi_link_uid(assembly_uid,  from = "bioproject", to = "biosample")
  )
  expect_error(
    ncbi_link_uid(assembly_uid,  from = "assembly", to = "nonamedb")
  )
})

test_that("ncbi_link_uid() works with vectors of uids", {
  assembly_uid <- ncbi_get_uid(examples$assembly, db = "assembly")
  biosample_uid <- ncbi_link_uid(
    assembly_uid$uid, from = "assembly", to ="biosample"
  )
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in% class(biosample_uid)))
  expect_equal(dim(biosample_uid), c(3,2))
  expect_equal(biosample_uid$biosample, c(19523416, 2952905, 1730125))
})

test_that("ncbi_link_uid() returns NA when input is NA", {
  res <- ncbi_link_uid(NA_real_, from = "assembly", to = "biosample")
  expect_true(all(c("tbl_df", "tbl", "data.frame") %in% class(res)))
  expect_equal(dim(res), c(1,2))
  expect_equal(res$assembly, NA_real_)
  expect_equal(res$biosample, NA_real_)
})

test_that("ncbi_link_uid() throws an error when there are invalid queries", {
  query <- "GCF_000002435.2"
  expect_error(ncbi_link_uid(query, from = "assembly", to = "biosample"))
})

test_that("ncbi_link_uid() returns NA when link cannot be found", {
  query <- "SAMD00057211"
  buid <- ncbi_get_uid(query, db = "biosample")
  auid <- ncbi_link_uid(buid, to = "assembly")
  expect_equal(dim(auid), c(1,2))
  expect_equal(auid$biosample, 9791874)
  expect_equal(auid$assembly, NA_real_)
})

test_that("ncbi_link_uid() returns more rows when there are multiple links", {
  query <- "PRJEB54063"
  puid <- ncbi_get_uid(query, db = "bioproject")
  buid <- ncbi_link_uid(puid, to = "biosample")
  expect_equal(dim(buid), c(2,2))
  expect_equal(buid$bioproject, c(883889, 883889))
  expect_equal(buid$biosample, c(31267349, 31250566))
})

test_that("ncbi_link_uid() returns results for all valid queries", {
  auid <-ncbi_get_uid(examples$assembly, db = "assembly") 
  query <- c(auid$uid[1], 123, auid$uid[2], NA, auid$uid[3], 0)
  res <- ncbi_link_uid(query, from = "assembly", to = "biosample")
  expect_equal(dim(res), c(6,2))
  expect_equal(res$assembly, query)
  expect_equal(res$biosample, c(19523416, NA, 2952905, NA, 1730125, NA))
})

# issue 72
test_that("ncbi_link_uid() converts UIDs to numeric without coercion to NA", {
  nuccore_uid <- ncbi_link_uid(488139305, from = "protein", to = "nuccore")
  
  expect_equal(sum(is.na(nuccore_uid$nuccore)), 0)
})
