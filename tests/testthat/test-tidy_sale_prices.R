context("Tidy sale prices")

test_that("date_from_report_string is accurate",{
  expect_equal(date_from_report_string("Year ending Dec 1995"),
               as.Date("1995-12-01"))
  expect_equal(date_from_report_string("Year ending Feb 2000"),
               as.Date("2000-02-01"))
  expect_equal(date_from_report_string("Year ending Nov 2020"),
               as.Date("2020-11-01"))
})

test_that("tidy_sale_prices_sheet has the correct columns and rows",{
  med_sales <- readxl::read_xls(test_sales_fp, "Median")
  tidy_med_sales <- tidy_sale_price_sheet(med_sales, "median")
  expect_equal(colnames(tidy_med_sales), c("code","area","report_date_string","value","metric"))
  expect_equal(nrow(tidy_med_sales), 45 * 89)
  expect_type(tidy_med_sales$value, "double")
})

test_that("tidy_sale_prices has all the data",{
  tidy_sp <- tidy_sale_price(test_sales_fp)
  expect_equal(colnames(tidy_sp), c("code","area","report_date_string","value","metric","report_date"))
  expect_equal(nrow(tidy_sp), 45 * 89 * 4)
})

test_that("example tidied values are accurate",{
  tidy_sp <- tidy_sale_price(test_sales_fp)
  med_1 <- tidy_sp$value[tidy_sp$code == "E09000028" & tidy_sp$metric == "median_sale_price" & tidy_sp$report_date_string == "Year ending Sep 1998"]
  sales_1 <- tidy_sp$value[tidy_sp$code == "E12000006" & tidy_sp$metric == "n_sales" & tidy_sp$report_date_string == "Year ending Mar 2006"]
  mean_1 <- tidy_sp$value[tidy_sp$code == "E09000019" & tidy_sp$metric == "mean_sale_price" & tidy_sp$report_date_string == "Year ending Jun 2004"]

  expect_equal(round(med_1), 95000)
  expect_equal(round(sales_1), 127019)
  expect_equal(round(mean_1), 310234)
})
