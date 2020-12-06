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

patent_tbl%>% glimpse()

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

assignee_tbl%>% glimpse()

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

patent_assignee_tbl%>% glimpse()

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

uspc_tbl%>% glimpse()

# Set data.table

setDT(patent_tbl) #once I try to convert patent data to data.table, the Rstudio gets stuck.
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

combined_data_1 %>% glimpse()

setkey(combined_data_1, "type")
key(combined_data_1)

setorderv(combined_data_1, c("type", "organization"))

combined_data_US<- combined_data_1[ (type == 2)]
combined_data_US

tic()
top_10_US_companies <- combined_data_US[!is.na(organization), .N, by = organization]
toc()
setkey(top_10_US_companies, "organization")
key(top_10_US_companies)
setorderv(top_10_US_companies, c("N", "organization"), order = -1)
as_tibble(top_10_US_companies, .rows = 10)

### A tibble: 10 x 2
### organization                                     N
###<chr>                                        <int>
###  1 International Business Machines Corporation 139092
###2 General Electric Company                     47122
###3 Intel Corporation                            42157
###4 Hewlett-Packard Development Company, L.P.    35573
###5 Microsoft Corporation                        30086
###6 Micron Technology, Inc.                      28001
###7 QUALCOMM Incorporated                        24703
###8 Texas Instruments Incorporated               24182
###9 Xerox Corporation                            23174
###10 Apple Inc.                                   21821

# Task2_Recent patent activity - top 10 US companies in 2019

#once I try to convert patent data to data.table, the Rstudio gets stuck, the following code can not run as well.
patent_2019_tbl<- patent_tbl[ lubridate::year(date) == "2019"]

setnames(patent_2019_tb,"id","patent_id")

combined_data_c2 <- merge(x = combined_data_c1, y = patent_2019_tb, 
                          by    = "patent_id", 
                          all.x = TRUE, 
                          all.y = FALSE)


# Task3_Innovation in Tech - top 5 USPTO tech main classess


