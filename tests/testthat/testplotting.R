library("NeuroIS")
context("Test of all plotting related functions")

test_that("Test url_to_filename", {
  expect_equal(url_to_filename("https://www.amazon.de/bücher-buch-lesen/b/ref=nav_shopall_bo?ie=UTF8&node=186606"), "screenshots/www.amazon.de-bücher-buch-lesen-b-ref=nav_shopall_bo.png")
})

test_that("Test take_screenshot", {
  take_screenshot("https://www.amazon.de/bücher-buch-lesen/b/ref=nav_shopall_bo?ie=UTF8&node=186606", width = 50)
  expect_true(file.exists("screenshots/www.amazon.de-bücher-buch-lesen-b-ref=nav_shopall_bo.png"))
})