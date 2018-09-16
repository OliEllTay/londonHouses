# https://data.london.gov.uk/download/average-income-tax-payers-borough/392e86d4-f1d3-4f06-a6a5-7fcd0fd65948/income-of-tax-payers.xls
test_income_fp <- file.path(system.file("testdata", package = "londonHouses"), "income_of_tax_payers.xls")
test_income_sheet <- readxl::read_xls(test_income_fp, "Total Income")
