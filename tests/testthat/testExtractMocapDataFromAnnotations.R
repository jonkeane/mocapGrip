library(mocapGrip)
context("extract MocapData from annotations")

test_that("extractMocapDataFromAnnotations errors appropriately", {
  expect_error(extractMocapDataFromAnnotations("./", "./"))
  expect_error(extractMocapDataFromAnnotations("./toTest.Rmd", "./"))
})

test_that("extractMocapDataFromAnnotations runs fine on minimal data", {
  expect_message(extractMocapDataFromAnnotations("./extData/minimalSubset/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_00?.eaf",
                                                destDir = "./extData/minimalSubset/extractedData/"))
  expect_equal(read.csv("./extData/minimalSubset/extractedData/GRI_070/GRI_070-SESSION_001-TRIAL_002.csv"),
               read.csv("./extractedDataGoldStd/GRI_070/GRI_070-SESSION_001-TRIAL_002.csv"))
  expect_equal(read.csv("./extData/minimalSubset/extractedData/GRI_070/GRI_070-SESSION_001-TRIAL_005.csv"),
               read.csv("./extractedDataGoldStd/GRI_070/GRI_070-SESSION_001-TRIAL_005.csv"))
  expect_equal(read.csv("./extData/minimalSubset/extractedData/GRI_070/GRI_070-SESSION_001-TRIAL_009.csv"),
               read.csv("./extractedDataGoldStd/GRI_070/GRI_070-SESSION_001-TRIAL_009.csv"))
})

