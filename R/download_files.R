
hyphenated_to_snake <- function(string){
  # to files-like-this into ones_like_this
  gsub("-", "_", string)
}

url_file_type <- function(url){
  # file type from url - gets bit from last dot
  last_dot <- regexpr("\\.[^\\.]*$", url) # position of last dot
  ifelse(last_dot == -1, "", substr(url, last_dot, nchar(url)))
}

url_to_file_name <- function(url){
  # https://stackoverflow.com/questions/5214677/r-find-the-last-dot-in-a-string
  last_dot <- regexpr("\\.[^\\.]*$", url) # position of last dot
  last_slash <- regexpr("\\/[^\\/]*$", url) # position of last /
  hyph <- substr(url, (last_slash + 1), (last_dot - 1))
  file_type <- url_file_type(url)
  paste0(hyphenated_to_snake(hyph), file_type)
}

download_data_file <- function(url){
  #' Download data.gov file
  #'
  #' Fetch file and save in /data
  #'
  #' @param url URL of the file to be downloaded
  save_file_name <- url_to_file_name(url)
  if(!dir.exists(here::here("data-raw"))){
    dir.create(here::here("data-raw"))
  }
  save_location <- here::here("data-raw", save_file_name)
  utils::download.file(url, save_location)
  return(save_location)
}

