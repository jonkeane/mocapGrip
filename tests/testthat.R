library(testthat)
library(mocapGrip)

load(file.path('extractedMarkerData.Rdata')) # markerDataHead
load(file.path('dist57.RData')) # dist57head
load(file.path('meanData.Rdata')) # meanDataHead

test_check("mocapGrip")
