library("NeuroIS")
context("Test of all plotting related functions")

test_that("Test url_to_filename", {
  expect_equal(url_to_filename("https://www.amazon.de/bücher-buch-lesen/b/ref=nav_shopall_bo?ie=UTF8&node=186606"), "screenshots/www.amazon.de-bücher-buch-lesen-b-ref=nav_shopall_bo.png")
})

test_that("Test take_screenshot", {
  take_screenshot("https://www.amazon.de/bücher-buch-lesen/b/ref=nav_shopall_bo?ie=UTF8&node=186606", width = 50)
  expect_true(file.exists("screenshots/www.amazon.de-bücher-buch-lesen-b-ref=nav_shopall_bo.png"))
})

test_that("Test trans_for_webplot", {
  import("datafortestingimport", prefix = "TEST")
  expect_equal(names(trans_for_webplot(TEST_web, url = "http://im.iism.kit.edu/home.php", type = "motion", subject = 1)), c("X", "Y", "subject"))
})

test_that("Test plot function", {
  import("datafordatafortestingplot", prefix ="PLOT")
})

## Playground and fragments for write test classes:

# take_screenshot(sum_PL[1,1])
# plot(PL_web, type="motion", sum_PL[1,1], subject = 1, alpha = 1.0, color = "purple")
# take_screenshot(sum_PL[2,1])
# plot(PL_web, type="motion", sum_PL[2,1], subject = 1, alpha = 1.0, color = "purple")