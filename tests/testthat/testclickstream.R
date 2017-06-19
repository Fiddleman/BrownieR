library("NeuroIS")
context("Test of as_clickstream function")

test_that("correct length of clickstream without any reachable goal", {
  import("datafortestingplot/", "amz")
  clickstream <- as_clickstream(amz_web, objective = c("not to reach" = "http"), "last")
  a <- lengths(clickstream)
  names(a) <- NULL
  expect_equal(a,c(19,11,20))
})
test_that("correct length of clickstreams with reachable goal", {
  import("datafortestingplot/", "amz")
  objective  <- c("Harry Potter Special angesehen!" = summary(amz_web)[41,1])
  clickstream <- as_clickstream(amz_web, objective, "first")
  a <- lengths(clickstream)
  names(a) <- NULL
  expect_equal(a,c(19,11,16))
})

