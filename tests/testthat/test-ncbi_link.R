data(examples)

test_that("ncbi_link() assembly to biosample from GCA",{
  res <- ncbi_link("GCA_001698945.1", from = "assembly", to = "biosample")
  
  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  expect_equal(dim(res), c(1,2))
  expect_equal(res$assembly, "GCA_001698945.1")
  expect_equal(res$biosample, "SAMN03175161")
})

test_that("ncbi_link() assembly to biosample from GCF",{
  res <- ncbi_link("GCF_001698945.1", from = "assembly", to = "biosample")
  
  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
  expect_equal(dim(res), c(1,2))
  expect_equal(res$assembly, "GCF_001698945.1")
  expect_equal(res$biosample, "SAMN03175161")
})

test_that("ncbi_link() assembly to biosample invalid queries", {
  query <- c(examples$assembly, "noname", NA)
  res <- ncbi_link(query, from = "assembly", to = "biosample")
  
  expect_equal(res$assembly, query)
  expect_equal(res$assembly[4:5], c("noname", NA_character_))
  expect_equal(res$biosample[4:5], c(NA_character_, NA_character_))
})

test_that("ncbi_link() biosample to assembly", {
  query <- c(examples$biosample)
  res <- ncbi_link(query, from = "biosample", to = "assembly")
  
  expect_equal(res$biosample, query)
  expect_equal(
    res$assembly[which(res$biosample == "SAMN02714232")],
    "GCF_000695855.3"
  )
  expect_equal(
    res$assembly[which(res$biosample == "SAMN36356470")],
    NA_character_
  )
})
