library(mocapGrip)
context("testing grip clipping")

utils::unzip("./GRIP/mocapData.zip", exdir = "./GRIP/")

print(dir(recursive = TRUE))

# makeElanFiles will error because the movie files are not real, but empty files
# (and because ffmpeg is not on travis)
test_that("makeElanFiles warns (because of NAs)", {
  expect_warning(makeElanFiles(c("./GRIP/Clipped Video/070/GRI_070-SESSION_001-TRIAL_002.mov",
                                 "./GRIP/Clipped Video/070/GRI_070-SESSION_001-TRIAL_005.mov",
                                 "./GRIP/Clipped Video/070/GRI_070-SESSION_001-TRIAL_009.mov")))
})

test_that("elanFiles are created", {
  expect_true(file.exists("./GRIP/elanFilesOut/GRI_070/GRI_070-SESSION_001-TRIAL_002.eaf"))
  expect_true(file.exists("./GRIP/elanFilesOut/GRI_070/GRI_070-SESSION_001-TRIAL_005.eaf"))
  expect_true(file.exists("./GRIP/elanFilesOut/GRI_070/GRI_070-SESSION_001-TRIAL_009.eaf"))
})

test_that("tsconf files are created", {
  expect_true(file.exists("./GRIP/elanFilesOut/GRI_070/GRI_070-SESSION_001-TRIAL_002_tsconf.xml"))
  expect_true(file.exists("./GRIP/elanFilesOut/GRI_070/GRI_070-SESSION_001-TRIAL_005_tsconf.xml"))
  expect_true(file.exists("./GRIP/elanFilesOut/GRI_070/GRI_070-SESSION_001-TRIAL_009_tsconf.xml"))
})


context("extract MocapData from annotations")
test_that("extractMocapDataFromAnnotations errors appropriately", {
  expect_error(extractMocapDataFromAnnotations("./", "./"))
  expect_error(extractMocapDataFromAnnotations("./toTest.Rmd", "./"))
})

test_that("path fixing works", {
  expect_output(fixPaths("./GRIP/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_00?.eaf"))
  # clean up (these should return the backups to the originals instead.)
  file.rename("./GRIP/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_002_tsconf.xml.bak",
              "./GRIP/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_002_tsconf.xml")
  file.rename("./GRIP/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_002.eaf.bak",
              "./GRIP/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_002.eaf")
  file.rename("./GRIP/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_005_tsconf.xml.bak",
              "./GRIP/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_005_tsconf.xml")
  file.rename("./GRIP/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_005.eaf.bak",
              "./GRIP/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_005.eaf")
  file.rename("./GRIP/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_009_tsconf.xml.bak",
              "./GRIP/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_009_tsconf.xml")
  file.rename("./GRIP/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_009.eaf.bak",
              "./GRIP/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_009.eaf")
})

test_that("extractMocapDataFromAnnotations runs fine on minimal data", {
  expect_message(extractMocapDataFromAnnotations("./GRIP/elanFilesCompleted/GRI_070/GRI_070-SESSION_001-TRIAL_00?.eaf", destDir = "./GRIP/extractedData/"))
  expect_equal(read.csv("./GRIP/extractedData/GRI_070/GRI_070-SESSION_001-TRIAL_002.csv"),
               read.csv("./extractedDataGoldStd/GRI_070/GRI_070-SESSION_001-TRIAL_002.csv"))
  expect_equal(read.csv("./GRIP/extractedData/GRI_070/GRI_070-SESSION_001-TRIAL_005.csv"),
               read.csv("./extractedDataGoldStd/GRI_070/GRI_070-SESSION_001-TRIAL_005.csv"))
  expect_equal(read.csv("./GRIP/extractedData/GRI_070/GRI_070-SESSION_001-TRIAL_009.csv"),
               read.csv("./extractedDataGoldStd/GRI_070/GRI_070-SESSION_001-TRIAL_009.csv"))
})

# clean up
unlink("./GRIP/elanFilesOut/GRI_070", recursive = TRUE)
unlink("./GRIP/mocapCSVs/GRI_070", recursive = TRUE)
unlink("./GRIP/mocapData", recursive = TRUE)
unlink("./Rplots.pdf")

# clean up
unlink("./GRIP/extractedData/GRI_070/", recursive = TRUE)
