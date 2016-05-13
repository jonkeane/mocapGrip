library(mocapGrip)
context("writeCSVsFromData")

test_that("writeCSVsFromData will overwrite", {
  expect_message(writeCSVsFromData(pureReplication))
})
test_that("writeCSVsFromData checks for existing files", {
  expect_error(writeCSVsFromData(pureReplication, overwrite=FALSE))
})
test_that("writeCSVsFromData will overwrite", {
  expect_message(writeCSVsFromData(pureReplication, overwrite=TRUE))
  unlink(c("./action.csv", "./estimation.csv")) # clean up
})
test_that("writeCSVsFromData accepts prefixes", {
  expect_message(writeCSVsFromData(pureReplication, namePrefix = "prefix"))
  unlink(c("./prefixaction.csv", "./prefixestimation.csv")) # clean up
})

context("displayModelsToRun")
test_that("display models works", {
  expect_message(displayAnalysesToRun(pureReplication))
})

context("checkDataer errors where appropriate")
test_that("checkDataer errors when there is an errant dataSet",{
  fakeReplication <- pureReplication
  fakeReplication$foo <- list()
  expect_error(checkData(fakeReplication, modelMd = modelMetadata))
})

test_that("checkDataer errors when there is an errant name in an existing dataSet",{
  fakeReplication <- pureReplication
  fakeReplication$action$foo <- list()
  expect_error(checkData(fakeReplication, modelMd = modelMetadata))
})
test_that("checkDataer errors when there is an errant type in an existing dataSet",{
  fakeReplication <- pureReplication[names(pureReplication) == "action"]
  fakeReplication$action$data <- list() # wrong
  fakeReplication$action$warnings <- list()
  fakeReplication$action$analysesToRun <- character()
  fakeReplication$action$analyses <- list()
  expect_error(checkData(fakeReplication, modelMd = modelMetadata))

  fakeReplication <- pureReplication[names(pureReplication) == "action"]
  fakeReplication$action$data <- data.frame()
  fakeReplication$action$warnings <- character() # wrong
  fakeReplication$action$analysesToRun <- character()
  fakeReplication$action$analyses <- list()
  expect_error(checkData(fakeReplication, modelMd = modelMetadata))

  fakeReplication <- pureReplication[names(pureReplication) == "action"]
  fakeReplication$action$data <- data.frame()
  fakeReplication$action$warnings <- list()
  fakeReplication$action$analysesToRun <- list() # wrong
  fakeReplication$action$analyses <- list()
  expect_error(checkData(fakeReplication, modelMd = modelMetadata))

  fakeReplication <- pureReplication[names(pureReplication) == "action"]
  fakeReplication$action$data <- data.frame()
  fakeReplication$action$warnings <- list()
  fakeReplication$action$analysesToRun <- character()
  fakeReplication$action$analyses <- character() # wrong
  expect_error(checkData(fakeReplication, modelMd = modelMetadata))
})

test_that("checkDataer silently returns the data object it was presented",{
  expect_silent(checkData(pureReplication, modelMd = modelMetadata))
  expect_equal(checkData(pureReplication, modelMd = modelMetadata), pureReplication)
})
