context("Tidy income")

test_that("can get date headers",{
  expect_equal(date_headers(test_income_sheet),
               c("1999-00",
                 "2000-01",
                 "2001-02",
                 "2002-03",
                 "2003-04",
                 "2004-05",
                 "2005-06",
                 "2006-07",
                 "2007-08",
                 "2009-10",
                 "2010-11",
                 "2011-12",
                 "2012-13",
                 "2013-14",
                 "2014-15",
                 "2015-16"))
  })

test_that("can remove nas",{
  expect_equal(remove_na_and_name(c("A", NA, "B"), ""), c("A","B"))
  expect_equal(remove_na_and_name(c(1, NA, 2, NA, 3), ""), c(1,2,3))
  expect_equal(remove_na_and_name(c(NA, NA, "A"), "A"), character())
})

test_that("can remove names",{
  expect_equal(remove_na_and_name(c("A", "Name", "B"), "Name"), c("A","B"))
  expect_equal(remove_na_and_name(c(1, 4, 2, 4, 3), 4), c(1,2,3))
  expect_equal(remove_na_and_name(c(NA, NA, "Name"), "Name"), character())
})

test_that("area codes are correct",{
  cds <- get_codes(test_income_sheet)
  expect_equal(cds[1], "E09000001")
  expect_equal(cds[3], "E09000003")
  expect_equal(cds[34], "E12000001")
  expect_false(any(is.na(cds)))
})

test_that("area names are correct",{
  cds <- get_areas(test_income_sheet)
  expect_equal(cds[1], "City of London")
  expect_equal(cds[3], "Barnet")
  expect_equal(cds[34], "North East")
  expect_false(any(is.na(cds)))
})

test_that("year_parts has the right rows and columns",{
  yr <- date_headers(test_income_sheet)
  yp <- year_part(yr[1], test_income_sheet)
  expect_equal(colnames(yp), c("code","area","year","number_of_individuals", "income_mean", "income_median"))
  expect_equal(nrow(yp), length(get_codes(test_income_sheet)))
})

test_that("tidy_income has the right rows and columns",{
  ti <- tidy_income(test_income_sheet)
  expect_equal(colnames(ti), c("code","area","year","metric","value"))
  expect_equal(nrow(ti), length(get_codes(test_income_sheet)) * length(date_headers(test_income_sheet)) * 3)
  expect_type(ti$value, "double")
})
