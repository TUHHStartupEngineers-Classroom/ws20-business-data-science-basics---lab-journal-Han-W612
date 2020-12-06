# 1.0 LIBRARIES ----

library(tidyverse)
library(vroom)
library(data.table)
library(tictoc)

# 2.0 DATA IMPORT ----
# 2.1 Patent 
col_types_patent <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

patent_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent,
  na         = c("", "NA", "NULL")
)

# 2.2 Assignee
col_types_assignee <- list(
  id = col_character(),
  type = col_character(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent,
  na         = c("", "NA", "NULL")
)

# 2.3 Patent_assignee

col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent,
  na         = c("", "NA", "NULL")
)

# 2.4 USPC

col_types_uspc <- list(
  uuid = col_character(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_character(),
  sequence = col_integer()
)

uspc_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
) 

# Set data.table

setDT(patent_tbl)
setDT(assignee_tbl)
setDT(patent_assignee_tbl)
setDT(uspc_tbl)

# Task1_Patent Dominance - top 10 US companies

setnames(assignee_tbl,"id","assignee_id")
tic()
combined_data_1 <- merge(x = assignee_tbl, y = patent_assignee_tbl, 
                          by    = "assignee_id", 
                          all.x = TRUE, 
                          all.y = FALSE)
toc()

combined_data %>% glimpse()

setkey(combined_data_1, "type")
key(combined_data_1)

setorderv(combined_data_1, c("type", "organization"))




# Task2_Recent patent activity - top 10 US companies in 2019

# Task3_Innovation in Tech - top 5 USPTO tech main classess


