
tidy_sale_price_sheet <- function(sale_sheet, metric_name){
  sale_sheet %>%
    dplyr::filter(stats::complete.cases(sale_sheet)) %>%
    dplyr::rename(code = .data$Code,
                  area = .data$Area) %>%
    tidyr::gather(key = "report_date_string",
                  value = "value",
                  -(.data$code:.data$area)) %>%
    dplyr::mutate(metric = metric_name)
}

date_from_report_string <- function(rep_str){
  strip_start <- sub("Year ending ", "", rep_str)
  d_str <- paste0("01 ", tolower(strip_start))
  as.Date(d_str, format = "%d %b %Y")
}

tidy_sale_price <- function(file_path){

  median_sp <- readxl::read_xls(file_path, "Median")
  mean_sp <- readxl::read_xls(file_path, "Mean")
  sales_sp <- readxl::read_xls(file_path, "Sales")
  lq_sp <- readxl::read_xls(file_path, "Lower Quartile")

  tidy_sheets <- list(
    tidy_sale_price_sheet(median_sp, "median_sale_price"),
    tidy_sale_price_sheet(mean_sp, "mean_sale_price"),
    tidy_sale_price_sheet(sales_sp, "n_sales"),
    tidy_sale_price_sheet(lq_sp, "lower_quartile")
  )

  tidy_sheets %>%
    purrr::map_dfr(~ .x) %>%
    dplyr::mutate(report_date = date_from_report_string(.data$report_date_string))
}

