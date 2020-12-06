# Challenge_Chapter 5
# Challenge 1

# 1. Load libraries and data

library(tidyverse)
library(ggplot2)

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# 2. Data manipluation

covid_cumulative_cases_tbl <- covid_data_tbl %>%
  select(countriesAndTerritories, cases, month, year) %>%
  filter(year == "2020") %>%
  filter(countriesAndTerritories %in% c("Germany",
                                        "United_Kingdom",
                                        "France",
                                        "Spain",
                                        "Lithuania",
                                        "United_States_of_America")) %>%
  group_by(countriesAndTerritories,month)%>%
  summarize(cumulative_cases = sum(cases)) %>%
  ungroup()
covid_cumulative_cases_tbl

# 3. Data visualization

covid_cumulative_cases_tbl %>%
  # Canvas
  ggplot(aes(x=month, y=cumulative_cases, color=countriesAndTerritories)) +
  
  # Geometries
  geom_smooth(method = "loess", span = 0.2) +
  
  # Formatting
  scale_x_continuous(breaks = covid_cumulative_cases_tbl$month,
                     labels = month(covid_cumulative_cases_tbl$month, label = T)) +
  
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, 
                                                    prefix = "",
                                                    suffix = "M")) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2"))+
  labs(title = "COVID-19 confirmed cases worldwide",
       x = "Year 2020",
       y = "Cumulative Cases")

# Challenge 2

library(maps)
library(tidyverse)
library(ggplot2)
library(scales)
library(data.table)
library(vroom)
library(tictoc)

world <- map_data("world")

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

mortality_rate_tbl <- covid_data_tbl %>%
  select(countriesAndTerritories, deaths, popData2019) %>%
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
  ))%>%
  group_by(countriesAndTerritories)%>%
  summarize(total_deaths = sum(deaths), population = max(popData2019), mortality_rate = deaths/popData2019) %>%
  ungroup()
mortality_rate_tbl

setDT(mortality_rate_tbl)
setDT(world)

covid_map <- left_join(x = world, y = mortality_rate_tbl, by=c("region" = "countriesAndTerritories")) 

covid_map %>% glimpse()

covid_map %>%
  # Canvas
  ggplot(aes(x=long, y=lat)) +
  # Geometries
  geom_map(aes(map_id=region, fill=mortality_rate), map = world) +
  # Formatting 
  scale_fill_continuous(low = "black",
                        high = "red",
                        labels = scales::percent) +
  labs(title = "Confirmed COVID-19 deaths relative to the size of the population")


