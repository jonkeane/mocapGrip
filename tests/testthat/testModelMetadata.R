library(mocapGrip)

formulaGood <- "paste0(outcome, '~', predictor1, '*', predictor2, '+', '(', '1+', predictor1, '*', predictor2, '|', grouping1, ')')"

context("modelMetadataCheck, variablExplanations")
test_that("modelMetadata is a list.", {
  expect_error(checkmodelMetadata(character()))
})
test_that("modelMetadata has the right names at the first level.", {
  expect_warning(checkmodelMetadata(list("variableExplanations" = list(),
                                          "models" = list("analyses" = list(), "modelStructures" = list()),
                                          "dataSets" = list())))
})
test_that("modelMetadata's variableExplanations are characters.", {
  expect_error(checkmodelMetadata(list("variableExplanations" = list("foo" = 2),
                                        "models" = list("analyses" = list(), "modelStructures" = list()),
                                        "dataSets" = list())))
})

context("modelMetadataCheck, model, analyses")
test_that("modelMetadata's model section has variablesToUse", {
  expect_error(checkmodelMetadata(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("variablesToUse" = list("foo"))),
                                        "dataSets" = list())))
})
test_that("modelMetadata's model section has analyses", {
  expect_error(checkmodelMetadata(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("modelStructures" = list()),
                                        "dataSets" = list())))
})
test_that("modelMetadata's model's analyses  have lenght>1 (warn if not).", {
  expect_warning(checkmodelMetadata(list("variableExplanations" = list("foo" = "bar"),
                                          "models" = list("analyses" = list(),
                                                          "modelStructures" = list("foo" = formulaGood)),
                                          "dataSets" = list())))
})
test_that("modelMetadata's model's analyses' sections have variables to use.", {
  expect_error(checkmodelMetadata(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list()),
                                                        "modelStructures" = list()),
                                        "dataSets" = list())))
})

context("modelMetadataCheck, model, modelMetadatas")
test_that("modelMetadata's model's modelStructures have lenght>1 (warn if not)", {
  expect_warning(checkmodelMetadata(list("variableExplanations" = list("foo" = "bar"),
                                          "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                          "modelStructures" = list()),
                                          "dataSets" = list())))
})
test_that("modelMetadata's analyses' modelStructure sections are strings.", {
  expect_error(checkmodelMetadata(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                        "modelStructures" = list("foo" = list())),
                                        "dataSets" = list())))
})
test_that("modelMetadata's analyses' modelStructure sections are formated appropriately", {
  expect_error(checkmodelMetadata(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                        "modelStructures" = list("foo" = "bar")),
                                        "dataSets" = list())))
})
test_that("modelMetadataCheck has only the specified sections.", {
  expect_error(checkmodelMetadata(
    list("variableExplanations" = list("foo" = "bar"),
         "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                         "modelStructures" = list("foo" = formulaGood)),
         "dataSets" = list("foo" = list("narrative" = list(),
                                        "defaultAnalysis" = character())),
         "notInmodelMetadata" = list())))
})

context("modelMetadataCheck, dataSets")
test_that("warn if dataSets is empty.", {
  expect_warning(checkmodelMetadata(list("variableExplanations" = list("foo" = "bar"),
                                          "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                          "modelStructures" = list("foo" = formulaGood)),
                                          "dataSets" = list())))
})
test_that("error if each dataSet is not a list", {
  expect_error(checkmodelMetadata(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                        "modelStructures" = list("foo" = formulaGood)),
                                        "dataSets" = list("foo" = "bar"))))
})
test_that("error if there is no narrative in the dataSet", {
  expect_error(checkmodelMetadata(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                        "modelStructures" = list("foo" = formulaGood)),
                                        "dataSets" = list("foo" = list("defaultAnalysis" = character())))))
})
test_that("error if there is no defaultAnalysis in the dataSet", {
  expect_error(checkmodelMetadata(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                        "modelStructures" = list("foo" = formulaGood)),
                                        "dataSets" = list("foo" = list("defaultAnalysis" = character())))))
})
test_that("error if there is no title in the dataSet", {
  expect_error(checkmodelMetadata(list("variableExplanations" = list("foo" = "bar"),
                           "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                           "modelStructures" = list("foo" = formulaGood)),
                           "dataSets" = list("foo" = list("narrative" = list("intro" = character()),
                                                          "defaultAnalysis" = character())))))
})
test_that("error if there is no intro in the dataSet", {
  expect_error(checkmodelMetadata(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                        "modelStructures" = list("foo" = formulaGood)),
                                        "dataSets" = list("foo" = list("narrative" = list("title" = character()),
                                                                       "defaultAnalysis" = character())))))
})
test_that("error if the title is not a character", {
  expect_error(checkmodelMetadata(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                        "modelStructures" = list("foo" = formulaGood)),
                                        "dataSets" = list("foo" = list("narrative" = list("title" = 1,
                                                                                          "intro" = character()),
                                                                       "defaultAnalysis" = character())))))
})
test_that("error if the intro is not a character", {
  expect_error(checkmodelMetadata(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                        "modelStructures" = list("foo" = formulaGood)),
                                        "dataSets" = list("foo" = list("narrative" = list("title" = character(),
                                                                                          "intro" = 1),
                                                                       "defaultAnalysis" = character())))))
})
test_that("error if the defaultAnalysis is not a character", {
  expect_error(checkmodelMetadata(list("variableExplanations" = list("foo" = "bar"),
                           "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                           "modelStructures" = list("foo" = formulaGood)),
                           "dataSets" = list("foo" = list("narrative" = list("title" = character(),
                                                                             "intro" = character()),
                                                          "defaultAnalysis" = 1)))))
})

goodStructure <- list("variableExplanations" = list("foo" = "bar"),
                      "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                      "modelStructures" = list("foo" = formulaGood)),
                      "dataSets" = list("foo" = list("narrative" = list("title" = character(),
                                                                        "intro" = character()),
                                                     "defaultAnalysis" = character())))

test_that("checkmodelMetadata returns the same object it is given.", {
  expect_equal(checkmodelMetadata(goodStructure), goodStructure)
})

context("the supplied modelMetadatas comply with checks")
test_that("modelMetadata Checkes out.", {
  expect_silent(checkmodelMetadata(modelMetadata))
})
test_that("modelMetadata Checkes out.", {
  expect_silent(checkmodelMetadata(jsonlite::fromJSON("./testAddmodelMetadata.json")))
})
