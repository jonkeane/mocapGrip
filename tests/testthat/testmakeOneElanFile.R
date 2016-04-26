library(mocapGrip)
context("makeOneElanFile")

test_that("makeOneElanFile returns warnings for nonexistent file", {
  expect_warning(mocapGrip:::makeOneElanFile("There/is/simply/no/way/this/file/exists"))
  expect_equal(mocapGrip:::makeOneElanFile("There/is/simply/no/way/this/file/exists"), 1)
})

test_that("makeOneElanFile returns an error if the path to the video file is misformed", {
  expect_error(mocapGrip:::makeOneElanFile("/etc/"))
})
