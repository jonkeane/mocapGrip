library(mocapGrip)
context("report writer")

modelStructure <- jsonlite::fromJSON(system.file("modelStructure.json", package = "mocapGrip", mustWork=TRUE))

test_that("replaceText works with a selection of texts", {
  expect_equal(mocapGrip:::replaceText(list("##", "$title"), modelStructure$action),c("##", "Maximum Grip aperture (on reach to grasp)"))
  expect_equal(mocapGrip:::replaceText(list("##", "$title", "$intro"), modelStructure$action),c("##", "Maximum Grip aperture (on reach to grasp)", "The maximum grip aperture is the maximum distance between the markers on the thumb and index finger during the period between when the hand left the table and when it touched the stick (this period is labeled as *grip* in our annotation system)."))
})

test_that("reaplceText works with lists in the replacements", {
  expect_equal(mocapGrip:::replaceText(list("$predictorVariables"), modelStructure$action),c("\n* the size of the stick in centimeters (centered at 8 cm, where 1 unit difference is 1 cm difference in stick size)\n* the configuration of the fins (closed, none, open, where closed is the reference level)\n* the interaction between the size of the stick and configuration of fins."))
})
