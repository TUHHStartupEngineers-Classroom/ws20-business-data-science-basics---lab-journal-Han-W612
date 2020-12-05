# Challenge_Chapter 3

#1 Get some data via an API
#2 Data Scraping (contain the model names and prices)

library(tidyverse)
library(rvest)
library(xopen)
library(jsonlite)
library(glue)
library(stringi)

# names
url_urban          <- "https://www.rosebikes.de/fahrräder/urban"
xopen(url_urban)

html_urban         <- read_html(url_urban)

bikes_urban_models <- html_urban %>%
  
  # Get the nodes
  html_nodes(css = ".catalog-category-bikes__title") %>%
  html_text() %>%
  str_remove_all(pattern = "\n")

bikes_urban_models
#price
url_urban          <- "https://www.rosebikes.de/fahrräder/urban"
html_urban         <- read_html(url_urban)

bikes_urban_price         <- html_urban %>%

html_nodes(css = "div.catalog-category-bikes__price-title") %>%
  html_text() %>%
  str_remove_all(pattern = "\n")

bikes_urban_price

tibble(bikes_urban_models, bikes_urban_price)
