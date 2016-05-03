library(mocapGrip)
context("parse extract annotations")

load(file.path('./dataForParsingTests/GRI057-001-002action.RData')) # actionData
load(file.path('./dataForParsingTests/GRI057-001-002release.RData')) # releaseData
load(file.path('./dataForParsingTests/GRI057-001-013estimation.RData')) # estimationData
load(file.path('./dataForParsingTests/GRI057-001-013estimationMaxGrip.RData')) # estimationMaxGripData


test_that("readExportedMocapData matches precomputed action data", {
  expect_warning(dataToBeTested <- readExportedMocapData(path = "./dataForParsingTests/extractedData/", types = c("action")))
  # expect_equal(dataToBeTested[["action"]]$duration, actionData$duration)
  # expect_equal(dataToBeTested[["action"]]$maxGrip, actionData$maxGrip)
  # expect_equal(dataToBeTested[["action"]]$maxGripTime, actionData$maxGripTime)
  # expect_equal(dataToBeTested[["action"]]$condition, actionData$condition)
  # expect_equal(dataToBeTested[["action"]]$type, actionData$type)
  # expect_equal(dataToBeTested[["action"]]$period, actionData$period)
  # expect_equal(dataToBeTested[["action"]]$gripType, actionData$gripType)
  # expect_equal(dataToBeTested[["action"]]$obsisSubj, actionData$obsisSubj)
  # expect_equal(dataToBeTested[["action"]]$obsisSession, actionData$obsisSession)
  # expect_equal(dataToBeTested[["action"]]$obsisTrial, actionData$obsisTrial)
  # expect_equal(dataToBeTested[["action"]]$stick, actionData$stick)
  # expect_equal(dataToBeTested[["action"]]$stickcm, actionData$stickcm)
  # expect_equal(dataToBeTested[["action"]]$stickcmScaled, actionData$stickcmScaled)
  # expect_equal(dataToBeTested[["action"]]$orientation, actionData$orientation)
  colsToTest <- names(actionData)
  colsToTest <- colsToTest[!{colsToTest %in% c("file")}]
  lapply(colsToTest, function(co) expect_equal(dataToBeTested[["action"]][co], actionData[co]))
})

test_that("readExportedMocapData matches precomputed release data", {
  expect_warning(dataToBeTested <- readExportedMocapData(path = "./dataForParsingTests/extractedData/", types = c("release")))
  colsToTest <- names(releaseData)
  colsToTest <- colsToTest[!{colsToTest %in% c("file")}]
  lapply(colsToTest, function(co) expect_equal(dataToBeTested[["release"]][co], releaseData[co]))
})

test_that("readExportedMocapData matches precomputed estimation data", {
  expect_warning(dataToBeTested <- readExportedMocapData(path = "./dataForParsingTests/extractedData/", types = c("estimation")))
  colsToTest <- names(estimationData)
  colsToTest <- colsToTest[!{colsToTest %in% c("file")}]
  lapply(colsToTest, function(co) expect_equal(dataToBeTested[["estimation"]][co], estimationData[co]))
})

test_that("readExportedMocapData matches precomputed estimation data", {
  expect_warning(dataToBeTested <- readExportedMocapData(path = "./dataForParsingTests/extractedData/", types = c("estMaxGrip")))
  colsToTest <- names(estimationMaxGripData)
  colsToTest <- colsToTest[!{colsToTest %in% c("file")}]
  lapply(colsToTest, function(co) expect_equal(dataToBeTested[["estMaxGrip"]][co], estimationMaxGripData[co]))
})

