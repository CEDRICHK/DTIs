test_that("cbind.fill() fills empty rows with specified value", {
  data1 <- c(1,2,3)
  data2 <- list(1,2,3)
  data3 <- cbind(c(1,2,3))
  expected_output <- cbind.fill(data1, data2, data3)
  output <- cbind.fill(data1, data2, data3)
  expect_equal(output, expected_output)
})
