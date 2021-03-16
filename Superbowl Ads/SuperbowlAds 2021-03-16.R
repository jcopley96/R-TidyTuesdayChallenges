library(tidyverse)
library(tidytuesdayR)
theme_set(theme_light())
library(scales)

 #read in the data from tidyTuesdayGithub URL
youtube_raw<- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')
View(youtube_raw)

youtube <- youtube_raw %>%
  select(-favorite_count)

youtube %>%
  count(brand, sort = TRUE) %>%
  head(20) %>%
  mutate(brand = fct_reorder(brand, n)) %>%
  ggplot(aes(n, brand)) +
  geom_col()

youtube %>%
  ggplot(aes(year, fill = brand)) +
  geom_bar() +
  facet_wrap(~ brand) +
  theme(legend.position = "none")

youtube %>%
  ggplot(aes(view_count)) +
  geom_histogram(binwidth = .5) +
  scale_x_log10(labels = comma) +
  labs(x = "Number of views",
       y = " ")

youtube %>%
  gather(metric, value, contains("_count")) %>%
  ggplot(aes(value)) +
  geom_histogram(binwidth = .5) +
  scale_x_log10(labels = comma) +
  facet_wrap(~metric)

youtube %>%
  filter(!is.na(view_count)) %>%
  mutate(brand = fct_reorder(brand, view_count)) %>%
  ggplot(aes(view_count, brand, fill = funny)) +
  geom_boxplot() +
  scale_x_log10(labels = comma)

youtube %>%
  filter(!is.na(view_count)) %>%
  mutate(brand = fct_reorder(brand, view_count)) %>%
  ggplot(aes(view_count, brand, fill = funny)) +
  geom_boxplot() +
  scale_x_log10(labels = comma)

#one clear outlier in terms of viewcount
youtube %>%
  ggplot(aes(year, view_count, group = year)) +
  geom_boxplot()



youtube %>%
  filter(!is.na(view_count)) %>%
  group_by(year) %>%
  summarize(n = n(),
            median_views = median(view_count)) %>%
  filter(n > 5) %>%
  ggplot(aes(year, median_views)) +
  geom_line() +
  geom_point(aes(size = n)) +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none") +
  labs(y = "Median Number Of Views", 
       x= "Year of Super Bowl",
       title = "Median Viewership of Super Bowl Ads Change Over Time")

#Setting up next series of analyses looking at categories for supoerbowl ads
ad_categories <- youtube %>%
  gather(category, value, funny:use_sex) %>%
  mutate(category = str_to_title(str_replace_all(category, "_", " ")))

ad_categories  %>%
  ggplot(aes(category, view_count, fill = value)) +
  geom_boxplot() +
  scale_y_log10()

ad_categories  %>%
  filter(!is.na(view_count)) %>%
  group_by(category, value) %>%
  summarize(n = n(),
            median_view_count = median(view_count)) %>%
  ggplot(aes(median_view_count, category, fill = value)) +
  geom_col(position = "dodge")


ad_categories  %>%
  filter(!is.na(view_count)) %>%
  group_by(category) %>%
  summarize(correlation = cor(value, log(view_count + 1))) %>%
  arrange(desc(correlation))


#quick check to see if any ad category was statistically significant in outperforming in # ads 
lm(log2(view_count) ~ danger + patriotic + funny + 
     show_product_quickly + celebrity +
     animals + use_sex,
   data = youtube) %>%
  summary()


coefficients <- gathered_categories %>%
  group_by(category) %>%
  summarize(model = list(glm(value ~ year, family = "binomial"))) %>%
  mutate(td = map(model, broom::tidy)) %>%
  unnest(td) %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(estimate))


gathered_categories %>%
  group_by(category,
           year = 2 * (year %/% 2)) %>%
  summarize(pct = mean(value),
            n = n()) %>%
  inner_join(coefficients, by = "category") %>%
  filter(p.value <= .01) %>%
  ggplot(aes(year, pct, color = category)) +
  geom_line() +
  scale_y_continuous(labels = percent) +
  facet_wrap(~ category) +
  theme(legend.position = "none") +
  labs(x = "Year of Super Bowl",
       y = "% of ads with this trait")

