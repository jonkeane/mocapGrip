library(mocapGrip)
context("distance calculationss")

# Doesn't work on travis see http://stackoverflow.com/questions/36877168/using-load-to-load-data-from-an-rdata-file-during-a-testthat-test-on-travis
# For now these have been moved to the /data directory.
# This also means the data will always be exported if needed, which is not great, but not the worst.
load(file.path('extractedMarkerData.RData')) # markerDataHead
load(file.path('dist57.RData')) # dist57head
load(file.path('meanData.RData')) # meanDataHead

test_that("calculateDistances returns the correct distances", {
  expect_equal(mocapGrip:::calculateDistances(markerDataHead, c(5,7)), dist57head)
})

test_that("meanOnAxis returns the correct distances", {
  expect_equal(mocapGrip:::meanOnAxis(markerDataHead, c(0, 1, 2, 3, 4), axis ="Y"), meanDataHead)
})
