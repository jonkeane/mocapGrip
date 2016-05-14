library(mocapGrip)
context("parse extracted annotations")

load(file.path('./dataForParsingTests/GRI057-001-002action.RData')) # actionData
load(file.path('./dataForParsingTests/GRI057-001-002release.RData')) # releaseData
load(file.path('./dataForParsingTests/GRI057-001-013estimation.RData')) # estimationData
load(file.path('./dataForParsingTests/GRI057-001-013estimationMaxGrip.RData')) # estimationMaxGripData


test_that("readExtractedMocapData matches precomputed action data", {
  expect_output(dataToBeTested <- readExtractedMocapData(path = "./dataForParsingTests/extractedData/", dataSets = c("action")))
  # expect_equal(dataToBeTested$action$data$duration, actionData$duration)
  # expect_equal(dataToBeTested$action$data$maxGrip, actionData$maxGrip)
  # expect_equal(dataToBeTested$action$data$maxGripTime, actionData$maxGripTime)
  # expect_equal(dataToBeTested$action$data$condition, actionData$condition)
  # expect_equal(dataToBeTested$action$data$type, actionData$type)
  # expect_equal(dataToBeTested$action$data$period, actionData$period)
  # expect_equal(dataToBeTested$action$data$gripType, actionData$gripType)
  # expect_equal(dataToBeTested$action$data$obsisSubj, actionData$obsisSubj)
  # expect_equal(dataToBeTested$action$data$obsisSession, actionData$obsisSession)
  # expect_equal(dataToBeTested$action$data$obsisTrial, actionData$obsisTrial)
  # expect_equal(dataToBeTested$action$data$stick, actionData$stick)
  # expect_equal(dataToBeTested$action$data$stickcm, actionData$stickcm)
  # expect_equal(dataToBeTested$action$data$stickcmScaled, actionData$stickcmScaled)
  # expect_equal(dataToBeTested$action$data$orientation, actionData$orientation)
  colsToTest <- names(actionData)
  colsToTest <- colsToTest[!{colsToTest %in% c("file")}]
  lapply(colsToTest, function(co) expect_equal(dataToBeTested$action$data[co], actionData[co]))
  expect_silent(checkData(dataToBeTested, modelMd = modelMetadata))
})

test_that("readExtractedMocapData matches precomputed release data", {
  expect_output(dataToBeTested <- readExtractedMocapData(path = "./dataForParsingTests/extractedData/", dataSets = c("release")))
  colsToTest <- names(releaseData)
  colsToTest <- colsToTest[!{colsToTest %in% c("file")}]
  lapply(colsToTest, function(co) expect_equal(dataToBeTested$release$data[co], releaseData[co]))
  expect_silent(checkData(dataToBeTested, modelMd = modelMetadata))
})

test_that("readExtractedMocapData matches precomputed estimation data", {
  expect_output(dataToBeTested <- readExtractedMocapData(path = "./dataForParsingTests/extractedData/", dataSets = c("estimation")))
  colsToTest <- names(estimationData)
  colsToTest <- colsToTest[!{colsToTest %in% c("file")}]
  lapply(colsToTest, function(co) expect_equal(dataToBeTested$estimation$data[co], estimationData[co]))
  expect_silent(checkData(dataToBeTested, modelMd = modelMetadata))
})

test_that("readExtractedMocapData matches precomputed estimation data", {
  expect_output(dataToBeTested <- readExtractedMocapData(path = "./dataForParsingTests/extractedData/", dataSets = c("estMaxGrip")))
  colsToTest <- names(estimationMaxGripData)
  colsToTest <- colsToTest[!{colsToTest %in% c("file")}]
  lapply(colsToTest, function(co) expect_equal(dataToBeTested$estMaxGrip$data[co], estimationMaxGripData[co]))
  expect_silent(checkData(dataToBeTested, modelMd = modelMetadata))
})

context("addNewDataSets")
dataIncludeFull <- readExtractedMocapData(path = "./dataForParsingTests/extractedData/", dataSets = c("action"), includeFullData = TRUE)
dataIncludeFullwithEst <- readExtractedMocapData(path = "./dataForParsingTests/extractedData/", dataSets = c("action", "estimation"), includeFullData = TRUE)
test_that("addNewDataSets errors or warns appropriately", {
  expect_error(addNewDataSets(pureReplication, dataSets = c("action")))
  expect_warning(dataNotExtended <- addNewDataSets(dataIncludeFull, dataSets = c("action")))
  expect_equal(dataNotExtended, dataIncludeFull)
})

test_that("addNewDataSets returns an extended object", {
  expect_silent(dataExtended <- addNewDataSets(dataIncludeFull, dataSets = c("estimation")))
  expect_equal(dataExtended, dataIncludeFullwithEst)
})
