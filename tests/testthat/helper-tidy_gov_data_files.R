# https://data.london.gov.uk/download/average-income-tax-payers-borough/392e86d4-f1d3-4f06-a6a5-7fcd0fd65948/income-of-tax-payers.xls
test_income_fp <- file.path(system.file("testdata", package = "londonHouses"), "income_of_tax_payers.xls")
test_income_sheet <- readxl::read_xls(test_income_fp, "Total Income")

# https://data.london.gov.uk/download/average-house-prices/f01b1cc7-6daa-4256-bd6c-94d8c83ee000/land-registry-house-prices-borough.xls
test_sales_fp <- file.path(system.file("testdata", package = "londonHouses"), "land_registry_house_prices_borough.xls")
