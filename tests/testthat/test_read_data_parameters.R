library(eventDetector)

context("Detecting the proper parameters")

test_that("read_data gives error for wrong section", {
  expect_error(read_data("new_protocol_no_field.csv"), "Couldn't find section named Calibration")
})

test_that("read_data gives error for wrong channel", {
  expect_error(read_data("new_protocol_no_field.csv", section = "cy5", channel = "Ch 4"),
               "Couldn't find channel named Ch 4 Should be one of Ch 2, Ch 3")
})

test_that("read_data correctly converts the time", {
  expect_equal(read_data("new_protocol_no_field.csv", section = "cy5", convert_time = 2)$Time, seq(0.5, 6, by = 0.5))
  expect_equal(read_data("new_protocol_no_field.csv", section = "cy5", convert_time = NA)$Time, seq(1,12))
})

