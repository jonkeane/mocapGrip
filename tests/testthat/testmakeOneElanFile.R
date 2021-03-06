library(mocapGrip)
context("makeOneElanFile")

test_that("makeOneElanFile returns warnings for nonexistent file", {
  expect_warning(testOut <- makeOneElanFile("There/is/simply/no/way/this/file/exists"))
  expect_equal(testOut, 1)
})

test_that("makeOneElanFile returns an error if the path to the video file is misformed", {
  expect_error(makeOneElanFile("/etc/"))
})
