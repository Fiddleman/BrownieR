library("NeuroIS")
context("Test of all Screenshot Functions")

test_that("Test url_to_filename Function", {
  expect_equal(url_to_filename("https://www.amazon.de/bücher-buch-lesen/b/ref=nav_shopall_bo?ie=UTF8&node=186606"), "screenshots/www.amazon.de-bücher-buch-lesen-b-ref=nav_shopall_bo.png")
})