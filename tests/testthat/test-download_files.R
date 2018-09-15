
context("Downloading files")

test_that("we can turn hypthated strings to snakes",{
  expect_equal(hyphenated_to_snake("an-example_string"), "an_example_string")
  expect_equal(hyphenated_to_snake("stringWithNoHyphens"), "stringWithNoHyphens")
  expect_equal(hyphenated_to_snake("string-with_mixture.of.things"), "string_with_mixture.of.things")
})

test_that("we can get the file type from a url",{
  expect_equal(url_file_type("example.xls"), ".xls")
  expect_equal(url_file_type("example.file.csv"), ".csv")
  expect_equal(url_file_type("no_file_type"), "")
})

test_that("file names are collected from urls",{
  expect_equal(url_to_file_name("http://some_site.com/download/some-file.csv"), "some_file.csv")
})
