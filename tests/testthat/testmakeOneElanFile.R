library(mocapGrip)
context("makeOneElanFile")

test_that("makeOneElanFile returns warnings for nonexistent file", {
  expect_warning(mocapGrip:::makeOneElanFile("There/is/simply/no/way/this/file/exists"))
  expect_equal(mocapGrip:::makeOneElanFile("There/is/simply/no/way/this/file/exists"), 1)
})
