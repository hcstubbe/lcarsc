test_that("Checking for locked entries works", {
  expect_equal(check_lock(data.frame(locked_row = TRUE, editing_user = "test_user"), session = list(ns = shiny::NS("test")), silent = TRUE), TRUE)
  expect_equal(check_lock(data.frame(locked_row = FALSE, editing_user = "test_user"), session = list(ns = shiny::NS("test")), silent = TRUE), FALSE)
})
