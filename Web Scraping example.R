library(rvest)
library(tidyselect)
library(xml2)
library(tidyverse)
library(purrr)
library(readr)

extract_rating <- function(rating_text) {
  as.integer(sub(".*?(\\d) out of 5.*", "\\1", rating_text))
}

page_scrape_ubereats <- function(page) {
  page <- read_html(page)
  
  date_of_review <- page %>%
  html_nodes("time") %>%
  html_attr("datetime") %>%
  as_datetime()

get_rating <- page %>%
  html_nodes("img") %>%
  html_attr("alt") %>%
  .[grepl("Rated", .)] %>%
  extract_rating()

tibble(
  date = date_of_review,
  rating = get_rating
)
}

urls <- paste0("https://www.trustpilot.com/review/ubereats.com?page=", 1:5)
all_reviews <- lapply(urls, page_scrape_ubereats)

reviews_data <- bind_rows(all_reviews)
view(reviews_data)
