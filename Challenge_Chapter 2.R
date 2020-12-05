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

