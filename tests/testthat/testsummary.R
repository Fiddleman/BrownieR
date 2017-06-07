library("NeuroIS")
context("Test of Summary Function")

test_that("Test of summary() WITHOUT objectives", {
  import("datafortestingimport", prefix = "TEST")
  summary_names <- c("URL", "Impressions", "Sessions", "Duration", "Imp_per_Session")
  expect_equal(names(summary(TEST_web)), summary_names)
})

test_that("Test of summary() WITH objectives",{
  import("datafortestingimport", prefix = "TEST")
  objectives <- c(Goals="http://im.iism.kit.edu/")
  summary_obj_names = c("Label", "URL", "Unique_Conversions", "All_Conversions", "All_Conversion_Rate", "Unique_Conversions_Rate")
  expect_equal(names(summary(TEST_web, objectives)), summary_obj_names)
})


