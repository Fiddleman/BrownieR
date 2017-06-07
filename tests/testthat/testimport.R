library("NeuroIS")
context("Test of all Import Functions")

test_that("Detected DataType is real DataType", {
  expect_equal(detectDataType("datafortestingimport/ExpData_raw.csv"), "exp")
  expect_equal(detectDataType("datafortestingimport/WebData.expdata"), "web")
  expect_equal(detectDataType("datafortestingimport/PhysioData.csv"), "physio")
  expect_equal(detectDataType("datafortestingimport/nonheader.expdata"), "unkown")
})

test_that("Test of indexFiles, is index correctly created?", {
  files <-list.files("datafortestingimport", pattern = "*.csv|.expdata", full.names=T)
  files <- append(files, c("exp", "exp", "unkown","physio", "web", "web"))
  test_index <- array(files, dim = c(6,2))
  file_index <- test_index[test_index[,2] != "unkown",]
  expect_equal(indexFiles("datafortestingimport"), file_index)
})


test_that("Test of import",{
  expect_equal(import("datafortestingimport", prefix = "TEST"), NULL)
})