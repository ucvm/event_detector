library(eventDetector)

field_well = c("A1_1", "A1_1", "A1_1", "A1_1", "A1_1", "A1_1", "A1_2", "A1_2", "A1_2", "A1_2", "A1_2", "A1_2")
no_field_well = c("A1", "A1", "A1", "A1", "A1", "A1", "A1", "A1", "A1", "A1", "A1", "A1")
intensity = c(124.43, 125.89, 127.30, 128.53, 127.95, 131.54, 131.14, 129.09, 132.20, 130.54, 132.63, 132.10)
label = c("1", "2","3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
time = c(1.666667e-05, 3.333333e-05, 5.000000e-05, 6.666667e-05, 8.333333e-05, 1.000000e-04,
         1.166667e-04, 1.333333e-04, 1.500000e-04, 1.666667e-04, 1.833333e-04, 2.000000e-04)

context("Correct Output")


test_that("Test that old protocol with no field works", {
  d = read_data("old_protocol_no_field.csv", section = "Calibration", channel = "Ch 2")
  expect_identical(d$Well, no_field_well)
  expect_identical(d$Label, label)
  expect_equal(d$Time, time)
  expect_identical(d$Intensity, intensity)

})


test_that("Test that old protocol with field works", {
  d = read_data("old_protocol_with_field.csv", section = "Calibration", channel = "Ch 2")
  expect_identical(d$Well, field_well)
  expect_identical(d$Label, label)
  expect_equal(d$Time, time)
  expect_identical(d$Intensity, intensity)

})

test_that("Test that new protocol with field works", {
  d = read_data("new_protocol_with_field.csv", section = "cy5", channel = "Ch 3")
  expect_identical(d$Well, field_well)
  expect_identical(d$Label, label)
  expect_equal(d$Time, time)
  expect_identical(d$Intensity, intensity)

})

test_that("Test that new protocol with no field works", {
  d = read_data("new_protocol_no_field.csv", section = "cy5", channel = "Ch 3")
  expect_identical(d$Well, no_field_well)
  expect_identical(d$Label, label)
  expect_equal(d$Time, time)
  expect_identical(d$Intensity, intensity)

})
