library("NeuroIS")
context("Test of all Import Functions")

test_that("Detected DataType is real DataType", {
  expect_equal(detectDataType("datafortestingimport/ExpData_raw.csv"), "exp")
  expect_equal(detectDataType("datafortestingimport/WebData.expdata"), "web")
  expect_equal(detectDataType("datafortestingimport/PhysioData.csv"), "physio")
  expect_equal(detectDataType("datafortestingimport/nonheader.expdata"), "unkown")
})

test_that("Test of createFileIndex, is index correctly created?", {
  files <-list.files("datafortestingimport", pattern = "*.csv|.expdata", full.names=T)
  files <- append(files, c("exp", "exp", "unkown","physio", "web", "web"))
  test_index <- array(files, dim = c(6,2))
  file_index <- a_files_types[a_files_types[,2] != "unkown",]
  expect_equal(createFileIndex("datafortestingimport"), test_index)
})


test_that("Test of importData",{
  expect_equal(importData("datafortestingimport", prefix = "TEST"), NULL)
})