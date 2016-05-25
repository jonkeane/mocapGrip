library(mocapGrip)
context("extract MocapData from annotations")

test_that("extractMocapDataFromAnnotations errors appropriately", {
  expect_error(extractMocapDataFromAnnotations("./", "./"))
  expect_error(extractMocapDataFromAnnotations("./toTest.Rmd", "./"))
})

test_that("path fixing works", {
  expect_output(fixPaths("./GRIP/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_00?.eaf"))
})

# these tests fail with check, but not with test.
# test_that("extractMocapDataFromAnnotations runs fine on minimal data", {
#   expect_message(extractMocapDataFromAnnotations("./GRIP/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_00?.eaf",
#                                                 destDir = "./extractedData/"))
#   expect_equal(read.csv("./extractedData/GRI_070/GRI_070-SESSION_001-TRIAL_002.csv"),
#                read.csv("./extractedDataGoldStd/GRI_070/GRI_070-SESSION_001-TRIAL_002.csv"))
#   expect_equal(read.csv("./extractedData/GRI_070/GRI_070-SESSION_001-TRIAL_005.csv"),
#                read.csv("./extractedDataGoldStd/GRI_070/GRI_070-SESSION_001-TRIAL_005.csv"))
#   expect_equal(read.csv("./extractedData/GRI_070/GRI_070-SESSION_001-TRIAL_009.csv"),
#                read.csv("./extractedDataGoldStd/GRI_070/GRI_070-SESSION_001-TRIAL_009.csv"))
#   # clean up
#   unlink(c("./extractedData/GRI_070/GRI_070-SESSION_001-TRIAL_002.csv",
#            "./extractedData/GRI_070/GRI_070-SESSION_001-TRIAL_005.csv",
#            "./extractedData/GRI_070/GRI_070-SESSION_001-TRIAL_009.csv"))
# })



