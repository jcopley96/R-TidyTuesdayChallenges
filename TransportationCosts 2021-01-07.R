library(tidyverse)
library(tidytuesdayR)
library(countrycode)
library(scales)
library(glue)
theme_set(theme_light())

#read in the data from tidyTuesdayGithub URL
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

View(transit_cost)

#clean up initial Table
transit_cost_cleaned <- transit_cost %>%
  filter(!is.na(e)) %>%
  mutate_at(vars(start_year, end_year, real_cost), as.numeric) %>%
  mutate(country_code = ifelse(country == "UK", "GB", country),
         country = countrycode(country_code, "iso2c", "country.name"),
         tunnel_per = tunnel / length,
         City = city,
         Country = country,
         rr = ifelse(rr, "Railroad", "Not Railroad"))

#basic calculation checks to see what data looks like
transit_cost_cleaned %>%
  count(City, country, sort = TRUE)

transit_cost_cleaned %>%
  filter(country == "United States") %>%
  mutate(line = fct_reorder(line, year)) %>%
  ggplot(aes(xmin = start_year, xmax = end_year, y = line, color = City, size = real_cost)) +
  geom_errorbarh(height = .1) +
  labs(x = "Year", y = "", color = "City",
       title="United States Transportation Projects by Cost and City")

transit_cost_cleaned %>%
  filter(!is.na(cost_km_millions),
         tunnel_per == 1) %>%
  mutate(country = fct_lump(country, 10)) %>%
  add_count(country) %>%
  mutate(country = glue("{ country } ({ n })"),
         country = fct_reorder(country, cost_km_millions)) %>%
  ggplot(aes(cost_km_millions, country)) +
  geom_boxplot() +
  scale_x_continuous(labels = dollar) +
  labs(x = "Cost / KM (Millions of USD)",
       y = "")


#transit_cost_cleaned %>%
#  ggplot(aes(cost_km_millions)) +
#  geom_histogram() +
#  scale_x_continuous(labels = dollar) +
#  labs(x = "Cost / KM (Millions of USD)")
 
transit_cost_cleaned %>%
  filter(!is.na(cost_km_millions),
         tunnel_per == 1) %>%
  mutate(country = fct_lump(country, 10)) %>%
  add_count(country) %>%
  mutate(country = glue("{ country } ({ n })"),
         country = fct_reorder(country, cost_km_millions)) %>%
  ggplot(aes(cost_km_millions, country)) +
  geom_boxplot() +
  scale_x_continuous(labels = dollar) +
  labs(x = "Cost / KM (Millions of USD)",
       y = "Country (number of projects)",
       title = "Distribution of Transportation costs per KM by Country")

transit_cost_cleaned %>%
  filter(country == "China",
         City == "Shanghai",
         !is.na(start_year),
         !is.na(end_year)) %>%
  mutate(City = fct_lump(City, 5)) %>%
  mutate(line = fct_reorder(line, year)) %>%
  ggplot(aes(xmin = start_year, xmax = end_year, y = line,
             color = City,
             size = real_cost)) +
  geom_errorbarh(height = .1) +
  labs(x = "Year",
       y = "",
       color = "City",
       title = "Shanghai Transportation Projects Costs and Time")

##transit_cost_cleaned %>%
#  filter(tunnel_per == 1,
#         end_year <= 2020,
#         country == "China") %>%
#  group_by(year = (year %/% 5) * 5) %>%
#  summarize(median_cost_km = median(cost_km_millions),
#            n = n()) %>%
#  ggplot(aes(year, median_cost_km)) +
# geom_line() +
#  geom_point(aes(size = n))


#"Chinese transit authorities spent $200M USD per KM of track between 2000-2015, and cost has since risen"
transit_cost_cleaned %>%
  filter(tunnel_per == 1,
         end_year <= 2020,
         country == "China") %>%
  mutate(year = (year %/% 5) * 5,
         City = fct_lump(City, 5)) %>%
  ggplot(aes(year, cost_km_millions, group = year)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(aes(color = City), height = 0, width = 1) +
  expand_limits(y = 0) +
  labs(y = "Cost / km (Real USD, Millions)",
       x = "Year",
       title = "Cost distribution / km in China",
       subtitle ="Chinese transit authorities spent $200M USD per KM of
       track between 2000-2015, and cost has since risen")

transit_cost_cleaned %>%
  filter(tunnel_per == 1,
         end_year <= 2020,
         country == "China") %>%
  mutate(City = fct_lump(City, 4)) %>%
  ggplot(aes(stations / length, cost_km_millions, size = length,
             color = City)) +
  geom_point() +
  expand_limits(x = 0, y = 0) +
  labs(x = "Stations / km", "Cost / kilometer",
       y = "Cost / km",
       title = "There does not appear to be a strong relationship between stations 
       and cost for Chinese transportation Projects")

