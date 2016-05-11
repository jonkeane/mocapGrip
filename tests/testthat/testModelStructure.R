library(mocapGrip)

formulaGood <- "paste0(outcome, '~', predictor1, '*', predictor2, '+', '(', '1+', predictor1, '*', predictor2, '|', grouping1, ')')"

context("modelStructureCheck, variablExplanations")
test_that("modelStructure is a list.", {
  expect_error(checkModelStructure(character()))
})
test_that("modelStructure has the right names at the first level.", {
  expect_warning(checkModelStructure(list("variableExplanations" = list(),
                                          "models" = list("analyses" = list(), "modelStructures" = list()),
                                          "dataSets" = list())))
})
test_that("modelStructure's variableExplanations are characters.", {
  expect_error(checkModelStructure(list("variableExplanations" = list("foo" = 2),
                                        "models" = list("analyses" = list(), "modelStructures" = list()),
                                        "dataSets" = list())))
})

context("modelStructureCheck, model, analyses")
test_that("modelStructure's model section has variablesToUse", {
  expect_error(checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("variablesToUse" = list("foo"))),
                                        "dataSets" = list())))
})
test_that("modelStructure's model section has analyses", {
  expect_error(checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("modelStructures" = list()),
                                        "dataSets" = list())))
})
test_that("modelStructure's model's analyses  have lenght>1 (warn if not).", {
  expect_warning(checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
                                          "models" = list("analyses" = list(),
                                                          "modelStructures" = list("foo" = formulaGood)),
                                          "dataSets" = list())))
})
test_that("modelStructure's model's analyses' sections have variables to use.", {
  expect_error(checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list()),
                                                        "modelStructures" = list()),
                                        "dataSets" = list())))
})

context("modelStructureCheck, model, modelStructures")
test_that("modelStructure's model's modelStructures have lenght>1 (warn if not)", {
  expect_warning(checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
                                          "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                          "modelStructures" = list()),
                                          "dataSets" = list())))
})
test_that("modelStructure's analyses' modelStructure section's are strings.", {
  expect_error(checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                        "modelStructures" = list("foo" = list())),
                                        "dataSets" = list())))
})
test_that("modelStructure's analyses' modelStructure section's are formated appropriately", {
  expect_error(checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                        "modelStructures" = list("foo" = "bar")),
                                        "dataSets" = list())))
})
test_that("modelStructureCheck has only the specified sections.", {
  expect_error(checkModelStructure(
    list("variableExplanations" = list("foo" = "bar"),
         "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                         "modelStructures" = list("foo" = formulaGood)),
         "dataSets" = list("foo" = list("narrative" = list(),
                                        "defaultAnalysis" = character())),
         "notInModelStructure" = list())))
})

context("modelStructureCheck, dataSets")
test_that("warn if dataSets is empty.", {
  expect_warning(checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
                                          "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                          "modelStructures" = list("foo" = formulaGood)),
                                          "dataSets" = list())))
})
test_that("error if each dataSet is not a list", {
  expect_error(checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                        "modelStructures" = list("foo" = formulaGood)),
                                        "dataSets" = list("foo" = "bar"))))
})
test_that("error if there is no narrative in the dataSet", {
  expect_error(checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                        "modelStructures" = list("foo" = formulaGood)),
                                        "dataSets" = list("foo" = list("defaultAnalysis" = character())))))
})
test_that("error if there is no defaultAnalysis in the dataSet", {
  expect_error(checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                        "modelStructures" = list("foo" = formulaGood)),
                                        "dataSets" = list("foo" = list("defaultAnalysis" = character())))))
})
test_that("error if there is no title in the dataSet", {
  expect_error(checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
                           "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                           "modelStructures" = list("foo" = formulaGood)),
                           "dataSets" = list("foo" = list("narrative" = list("intro" = character()),
                                                          "defaultAnalysis" = character())))))
})
test_that("error if there is no intro in the dataSet", {
  expect_error(checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                        "modelStructures" = list("foo" = formulaGood)),
                                        "dataSets" = list("foo" = list("narrative" = list("title" = character()),
                                                                       "defaultAnalysis" = character())))))
})
test_that("error if the title is not a character", {
  expect_error(checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                        "modelStructures" = list("foo" = formulaGood)),
                                        "dataSets" = list("foo" = list("narrative" = list("title" = 1,
                                                                                          "intro" = character()),
                                                                       "defaultAnalysis" = character())))))
})
test_that("error if the intro is not a character", {
  expect_error(checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
                                        "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                                        "modelStructures" = list("foo" = formulaGood)),
                                        "dataSets" = list("foo" = list("narrative" = list("title" = character(),
                                                                                          "intro" = 1),
                                                                       "defaultAnalysis" = character())))))
})
test_that("error if the defaultAnalysis is not a character", {
  expect_error(checkModelStructure(list("variableExplanations" = list("foo" = "bar"),
                           "models" = list("analyses" = list("testAnalysis" = list("variablesToUse" = list())),
                                           "modelStructures" = list("foo" = formulaGood)),
                           "dataSets" = list("foo" = list("narrative" = list("title" = character(),
                                                                             "intro" = character()),
                                                          "defaultAnalysis" = 1)))))
})


context("the supplied modelStructure complies with checks")
test_that("modelStructure Checkes out.", {
  expect_silent(checkModelStructure(modelStructure))
})
