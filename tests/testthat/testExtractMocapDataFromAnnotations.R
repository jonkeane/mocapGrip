library(mocapGrip)
context("extract MocapData from annotations")

test_that("extractMocapDataFromAnnotations errors appropriately", {
  expect_error(extractMocapDataFromAnnotations("./", "./"))
  expect_error(extractMocapDataFromAnnotations("./toTest.Rmd", "./"))
})

test_that("extractMocapDataFromAnnotations runs fine on minimal data", {
  expect_message(extractMocapDataFromAnnotations("../../inst/extData/minimalSubset/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_00?.eaf",
                                                destDir = "../../inst/extData/minimalSubset/extractedData/"))
})
