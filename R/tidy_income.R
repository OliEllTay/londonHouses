
date_headers <- function(income_sheet){
  cn <- colnames(income_sheet)
  blanks <- grepl("X__", cn)
  cn[!blanks]
}

remove_na_and_name <- function(col, name = ""){
  col_no_rm <- col[!is.na(col)]
  if(name != ""){
    col_no_rm <- col_no_rm[!grepl(name, col_no_rm, fixed = TRUE)]
  }
  col_no_rm
}

get_codes <- function(income_sheet){
  all_col <- income_sheet[["X__1"]]
  remove_na_and_name(all_col, "Code")
}

get_areas <- function(income_sheet){
  all_col <- income_sheet[["X__2"]]
  remove_na_and_name(all_col, "Area")
}

year_part <- function(yr, income_sheet){
  yr_dates <- date_headers(income_sheet)
  yr_ind <- which(yr == yr_dates)
  yr_n_ind <- remove_na_and_name(income_sheet[[(3 * yr_ind)]], "Number of Individuals")
  yr_mean <- remove_na_and_name(income_sheet[[(3 * yr_ind + 1)]], "Mean £")
  yr_median <- remove_na_and_name(income_sheet[[(3 * yr_ind + 2)]], "Median £")
  dplyr::data_frame(
    code = get_codes(income_sheet),
    area = get_areas(income_sheet),
    year = yr,
    number_of_individuals = yr_n_ind,
    income_mean = yr_mean,
    income_median = yr_median
  )
}

tidy_income <- function(income_sheet){
  #' Tidy Income XLS
  #'
  #' Turn data.gov london income by borough sheet into a tidy data format
  #'
  #' @param income_sheet Total Income sheet from a london borough income data .xls file.
  yrs <- date_headers(income_sheet)
  year_parts <- purrr::map_dfr(.x = yrs, .f = year_part, income_sheet = income_sheet)
  tidyr::gather(year_parts,
                key = "metric",
                value = "value",
                -(code:year)) %>%
    dplyr::mutate(value = as.numeric(value))
}


