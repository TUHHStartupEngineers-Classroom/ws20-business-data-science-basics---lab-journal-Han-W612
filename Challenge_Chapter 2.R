#Challenge_Chapter2 - 
# TASK
# 1 Analyze the sales by location (state) with a bar plot. 
# 2 Analyze the sales by location and year (facet_wrap).

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library("writexl")

# 2.0 Importing Files ----
bikes_tbl      <- read_excel(path = "DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----
#Clicking on the file in the environment tab.

# 4.0 Joining Data ----
left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl %>% glimpse()

# 5.0 Wrangling Data ----
bike_orderlines_wrangled_city_separated_tbl <- bike_orderlines_joined_tbl %>%
  # 5.1 Separate category name
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  separate(col    = location,
           into   = c("City", "State"),
           sep    = ", ") %>% 
  # 5.2 Add the total price (price * quantity) 
  mutate(total.price = price * quantity) %>%
  # 5.3 Optional: Reorganize. Using select to grab or remove unnecessary columns
  select(-...1, -gender) %>%
  
  select(-ends_with(".id")) %>%

  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  # 5.4 Rename columns because we actually wanted underscores instead of the dots
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
# 6.1 Sales by location ----

# Step 1 - Manipulate
sales_by_location_tbl <- bike_orderlines_wrangled_city_separated_tbl %>%
  
  select(State, City, total_price) %>%
  group_by(State) %>% 
  summarize(state_sales = sum(total_price)) %>%
  
  mutate(sales_formatted = scales::dollar(state_sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
sales_by_location_tbl
'''
results
# A tibble: 12 x 3
   State                         state_sales sales_formatted
   <chr>                               <dbl> <chr>          
 1 Baden-Württemberg                 6521090 6.521.090 €    
 2 Bavaria                           6742819 6.742.819 €    
 3 Berlin                            1128433 1.128.433 €    
 4 Bremen                           10653499 10.653.499 €   
 5 Hamburg                           3874756 3.874.756 €    
 6 Hesse                             1558901 1.558.901 €    
 7 Lower Saxony                      4107115 4.107.115 €    
 8 Mecklenburg-Western Pomerania      618974 618.974 €      
 9 North Rhine-Westphalia           21200613 21.200.613 €   
10 Saxony                            2230245 2.230.245 €    
11 Saxony-Anhalt                      569614 569.614 €      
12 Schleswig-Holstein                3224749 3.224.749 €   
'''
# Step 2 - Visualize
sales_by_location_tbl %>%
  ggplot(aes(x = State, y = state_sales)) +
  geom_col(fill = "#2DC6D6") + 
  geom_label(aes(label = sales_formatted)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +

  labs(
    title    = "State Revenue by year",
    subtitle = "-",
    x = "", 
    y = "Revenue"
  )
# 6.2 Sales by location and year ----

# Step 1 - Manipulate
sales_by_state_year_tbl <- bike_orderlines_wrangled_city_separated_tbl %>%
  select(order_date, total_price, State) %>%
  mutate(year = year(order_date)) %>%

  group_by(State, year) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(sales_formatted = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_state_year_tbl 

'''
results:
# A tibble: 60 x 4
   State              year   sales sales_formatted
   <chr>             <dbl>   <dbl> <chr>          
 1 Baden-Württemberg  2015 1031924 1.031.924 €    
 2 Baden-Württemberg  2016 1561658 1.561.658 €    
 3 Baden-Württemberg  2017 1224152 1.224.152 €    
 4 Baden-Württemberg  2018 1114327 1.114.327 €    
 5 Baden-Württemberg  2019 1589029 1.589.029 €    
 6 Bavaria            2015 1301461 1.301.461 €    
 7 Bavaria            2016 1129852 1.129.852 €    
 8 Bavaria            2017 1411851 1.411.851 €    
 9 Bavaria            2018 1168783 1.168.783 €    
10 Bavaria            2019 1730872 1.730.872 €    
# … with 50 more rows
'''

# Step 2 - Visualize
sales_by_state_year_tbl %>%
  
  ggplot(aes(x = year, y = sales, fill = State)) +
  geom_col() + 
  facet_wrap(~ State) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and state",
    subtitle = "Each state presents differently",
    fill = "State" # Changes the legend name
  )


# 7.0 Writing Files ----

# 7.1 Excel ----
install.packages("writexl")
library("writexl")
bike_orderlines_wrangled_tbl %>%
  write_xlsx("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")
# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
  write_csv("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")
# 7.3 RDS ----
bike_orderlines_wrangled_tbl %>% 
  write_rds("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

