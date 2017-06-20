library("NeuroIS")
context("Test of Physio-Transform")

test_that("Test of removeEmptyColums, (empty colums in dataset)", {
  import("datafortestingimport/", "t")
  data <- as.data.frame(unclass(t_physio), stringsAsFactors = F)
  data  <- removeEmptyColums(data)
  expect_equal(names(data),c("ReceiveTime", "SampleTime", "PlugSeqNo", "Input1", "Input2", "Input3", "SUBJECT_ID_SUBJECT"))
})

test_that("Test of removeEmptyColums (no empty colums in dataset)", {
  import("datafortestingimport/", "t")
  data <- as.data.frame(unclass(t_physio), stringsAsFactors = F)
  data <- removeEmptyColums(data)
  data <- removeEmptyColums(data)
  expect_equal(names(data),c("ReceiveTime", "SampleTime", "PlugSeqNo", "Input1", "Input2", "Input3", "SUBJECT_ID_SUBJECT"))
})