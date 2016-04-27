library(mocapGrip)
context("distance calculationss")


print(getwd())
print(list.files())

load(file.path('extractedMarkerData.Rdata')) # markerDataHead
load(file.path('dist57.RData')) # dist57head
load(file.path('meanData.Rdata')) # meanDataHead

test_that("calculateDistances returns the correct distances", {
  expect_equal(mocapGrip:::calculateDistances(markerDataHead, c(5,7)), dist57head)
})

test_that("meanOnAxis returns the correct distances", {
  expect_equal(mocapGrip:::meanOnAxis(markerDataHead, c(0, 1, 2, 3, 4), axis ="Y"), meanDataHead)
})
