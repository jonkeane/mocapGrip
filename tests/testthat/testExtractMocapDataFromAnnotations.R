library(mocapGrip)
context("extract MocapData from annotations")

test_that("extractMocapDataFromAnnotations errors appropriately", {
  expect_error(extractMocapDataFromAnnotations("./", "./"))
  expect_error(extractMocapDataFromAnnotations(".toTest.Rmd", "./"))
})
