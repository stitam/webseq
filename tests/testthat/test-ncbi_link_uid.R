data(examples)

test_that("ncbi_link_uid() converts using input history", {
  # input is ncbi_uid object with history
  assembly_uid <- ncbi_get_uid(examples$assembly, db = "assembly")
  biosample_uid <- ncbi_link_uid(
    assembly_uid, from = "assembly", to = "biosample", use_history = TRUE
  )
  expect_true(all(c("ncbi_uid", "list") %in% class(biosample_uid)))
  expect_true(all(biosample_uid$uid %in% c("2952905", "1730125")))
  expect_equal(nrow(biosample_uid$web_history), 1)
})

test_that("ncbi_link_uid() converts without using history", {
  # input is ncbi_uid_object with history but we don't want to use it
  assembly_uid <- ncbi_get_uid(examples$assembly, db = "assembly")
  biosample_uid <- ncbi_link_uid(
    assembly_uid, from = "assembly", to ="biosample", use_history = FALSE
  )
  expect_true(all(c("ncbi_uid", "list") %in% class(biosample_uid)))
  expect_true(all(biosample_uid$uid %in% c("2952905", "1730125")))
  expect_equal(nrow(biosample_uid$web_history), 0)
  
  # input is ncbi_uid object without history
  assembly_uid <- ncbi_get_uid(
    examples$assembly, db = "assembly", use_history = FALSE
  )
  biosample_uid <- ncbi_link_uid(
    assembly_uid, from = "assembly", to ="biosample", use_history = FALSE
  )
  expect_true(all(c("ncbi_uid", "list") %in% class(biosample_uid)))
  expect_true(all(biosample_uid$uid %in% c("2952905", "1730125")))
  expect_equal(nrow(biosample_uid$web_history), 0)
  
  # input is ncbi_uid object without history but we want to use it
  assembly_uid <- ncbi_get_uid(
    examples$assembly, db = "assembly", use_history = FALSE
  )
  biosample_uid <- ncbi_link_uid(
    assembly_uid, from = "assembly", to ="biosample", use_history = TRUE
  )
  expect_true(all(c("ncbi_uid", "list") %in% class(biosample_uid)))
  expect_true(all(biosample_uid$uid %in% c("2952905", "1730125")))
  expect_equal(nrow(biosample_uid$web_history), 1)
  
  # input is a numeric vector
  assembly_uid <- ncbi_get_uid(
    examples$assembly, db = "assembly", use_history = FALSE
  )
  biosample_uid <- ncbi_link_uid(
    assembly_uid$uid, from = "assembly", to ="biosample", use_history = FALSE
  )
  expect_true(all(c("ncbi_uid", "list") %in% class(biosample_uid)))
  expect_true(all(biosample_uid$uid %in% c("2952905", "1730125")))
  expect_equal(nrow(biosample_uid$web_history), 0)
  
  # input is a numeric vector but we want to use history
  assembly_uid <- ncbi_get_uid(
    examples$assembly, db = "assembly", use_history = FALSE
  )
  biosample_uid <- ncbi_link_uid(
    assembly_uid$uid, from = "assembly", to ="biosample", use_history = TRUE
  )
  expect_true(all(c("ncbi_uid", "list") %in% class(biosample_uid)))
  expect_true(all(biosample_uid$uid %in% c("2952905", "1730125")))
  expect_equal(nrow(biosample_uid$web_history), 1)
})

test_that("ncbi_link_uid() can use an ncbi_uid object's db element", {
  assembly_uid <- ncbi_get_uid(examples$assembly, db = "assembly")
  biosample_uid <- ncbi_link_uid(assembly_uid,  to = "biosample")
  expect_true(all(c("ncbi_uid", "list") %in% class(biosample_uid)))
  expect_true(all(biosample_uid$uid %in% c("2952905", "1730125")))
})

test_that("ncbi_link_uid() returns NA if input is invalid", {
  expect_equal(suppressWarnings(
    ncbi_link_uid("funky", from = "assembly", to = "biosample")$uid
  ), NA_real_)
})

# issue 72
test_that("ncbi_link_uid() converts UIDs to numeric without coercion to NA", {
  nuccore_uid <- ncbi_link_uid(488139305, from = "protein", to = "nuccore")
  
  expect_equal(sum(is.na(nuccore_uid$uid)), 0)
})
