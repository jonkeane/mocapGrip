library(mocapGrip)
context("parse extract annotations")

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
})

test_that("readExtractedMocapData matches precomputed release data", {
  expect_output(dataToBeTested <- readExtractedMocapData(path = "./dataForParsingTests/extractedData/", dataSets = c("release")))
  colsToTest <- names(releaseData)
  colsToTest <- colsToTest[!{colsToTest %in% c("file")}]
  lapply(colsToTest, function(co) expect_equal(dataToBeTested$release$data[co], releaseData[co]))
})

test_that("readExtractedMocapData matches precomputed estimation data", {
  expect_output(dataToBeTested <- readExtractedMocapData(path = "./dataForParsingTests/extractedData/", dataSets = c("estimation")))
  colsToTest <- names(estimationData)
  colsToTest <- colsToTest[!{colsToTest %in% c("file")}]
  lapply(colsToTest, function(co) expect_equal(dataToBeTested$estimation$data[co], estimationData[co]))
})

test_that("readExtractedMocapData matches precomputed estimation data", {
  expect_output(dataToBeTested <- readExtractedMocapData(path = "./dataForParsingTests/extractedData/", dataSets = c("estMaxGrip")))
  colsToTest <- names(estimationMaxGripData)
  colsToTest <- colsToTest[!{colsToTest %in% c("file")}]
  lapply(colsToTest, function(co) expect_equal(dataToBeTested$estMaxGrip$data[co], estimationMaxGripData[co]))
})

