library(mocapGrip)

data <- readExtractedMocapData(path = "./dataForParsingTests/extractedData/", dataSets = c("action"), includeFullData = TRUE)
fullData <- data$fullData

context("maxGripFinder")
test_that("maxGripFinder warns appropriately with no data", {
  expect_warning(mocapGrip:::maxGripFinder(subset(fullData, condition == "-1")))
})

context("meanMedianFinder")
test_that("meanMedianFinder warns appropriately with no data", {
  expect_warning(mocapGrip:::meanMedianFinder(subset(fullData, condition == "-1")))
})

context("meanMedianSubsetFinder")
test_that("meanMedianSubsetFinder warns appropriately with no data", {
  expect_warning(mocapGrip:::meanMedianSubsetFinder(subset(fullData, condition == "-1"), start=0, stop=1))
})
test_that("meanMedianSubsetFinder warns appropriately with lots of occlusions", {
  expect_warning(mocapGrip:::meanMedianSubsetFinder(subset(fullData, period == "GRIP"), start=0, stop=1, percOcclusion = 0))
})
test_that("meanMedianSubsetFinder errors or warns when start or stop is too large, or the order is wrong", {
  expect_error(mocapGrip:::meanMedianSubsetFinder(subset(fullData, period == "GRIP"), start=100, stop=1))
  expect_warning(mocapGrip:::meanMedianSubsetFinder(subset(fullData, period == "GRIP"), start=1, stop=101))
  expect_error(mocapGrip:::meanMedianSubsetFinder(subset(fullData, period == "GRIP"), start=2, stop=1))
  expect_error(mocapGrip:::meanMedianSubsetFinder(subset(fullData, period == "GRIP"), start=-1, stop=1))
  expect_error(mocapGrip:::meanMedianSubsetFinder(subset(fullData, period == "GRIP"), start=1, stop=-1))
  expect_warning(mocapGrip:::meanMedianSubsetFinder(subset(fullData, period == "GRIP"), start=100, stop=101, timeType = "msecs"))
  expect_error(mocapGrip:::meanMedianSubsetFinder(subset(fullData, period == "GRIP"), start=-1, stop=-1, timeType = "msecs"))
  expect_error(mocapGrip:::meanMedianSubsetFinder(subset(fullData, period == "GRIP"), start=-1, stop=1, timeType = "percent"))
  expect_error(mocapGrip:::meanMedianSubsetFinder(subset(fullData, period == "GRIP"), start=2, stop=1, timeType = "percent"))
  expect_error(mocapGrip:::meanMedianSubsetFinder(subset(fullData, period == "GRIP"), start=0, stop=-1, timeType = "percent"))
  expect_error(mocapGrip:::meanMedianSubsetFinder(subset(fullData, period == "GRIP"), start=0, stop=2, timeType = "percent"))
  expect_error(mocapGrip:::meanMedianSubsetFinder(subset(fullData, period == "GRIP"), start=0, stop=1, timeType = "foo"))
})
test_that("meanMedianSubsetFinder runs silently", {
  expect_silent(mocapGrip:::meanMedianSubsetFinder(subset(fullData, period == "GRIP"), start=0, stop=1, timeType = "percent"))
  expect_silent(mocapGrip:::meanMedianSubsetFinder(subset(fullData, period == "GRIP"), start=0, stop=1, timeType = "msecs"))
})

context("ceilingFinder")
test_that("ceilingFinder warns appropriately with no data", {
  expect_warning(mocapGrip:::ceilingFinder(subset(fullData, condition == "-1"), bandWidth=1))
})
test_that("ceilingFinder warns appropriately with lots of occlusions", {
  expect_warning(mocapGrip:::ceilingFinder(subset(fullData, period == "GRIP"), bandWidth=1, percOcclusion = 0))
})
test_that("ceilingFinder warns or errors appropriately with wrong specifications", {
  expect_error(mocapGrip:::ceilingFinder(subset(fullData, period == "GRIP"), bandWidth=2, bandType = "percent"))
  expect_error(mocapGrip:::ceilingFinder(subset(fullData, period == "GRIP"), bandWidth=-1, bandType = "percent"))
  expect_warning(mocapGrip:::ceilingFinder(subset(fullData, period == "GRIP"), bandWidth=1000, bandType = "mm"))
  expect_error(mocapGrip:::ceilingFinder(subset(fullData, period == "GRIP"), bandWidth=1, bandType = "foo"))
})
