library(tidyverse)
library(broom)
theme_set(theme_light())

media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")


franchises <- media_franchises %>%
  group_by(franchise, original_media, year_created, creators, owners) %>%
  summarize(categories = n(),
            total_revenue = sum(revenue),
            most_profitable = revenue_category[which.max(revenue)]) %>%
  ungroup()

library(glue)

media_franchises %>%
  semi_join(franchises %>% top_n(20, total_revenue), by = "franchise") %>%
  mutate(franchise = glue("{franchise} ({year_created})")) %>%
  mutate(franchise = fct_reorder(franchise, revenue, sum), 
         revenue_category = fct_reorder(revenue_category, revenue, sum)) %>%
  ggplot(aes(franchise, revenue, fill = revenue_category)) +
  geom_col() +
  scale_y_continuous(labels = scales ::dollar) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "What are the most profitable franchises of all time?",
       fill = "Category",
       x = "",
       y = "Revenue (Billions)")

media_franchises %>%
  group_by(owners) %>%
  filter(n_distinct(franchise) > 2) %>%
  ungroup() %>%
  mutate(franchise = fct_reorder(franchise, revenue, sum),
         owners = fct_reorder(franchise,  -revenue, sum),
         revenue_category = fct_reorder(revenue_category, revenue, sum)) %>%
  ggplot(aes(franchise, revenue, fill = revenue_category)) +
  geom_col() +
  facet_wrap(~owners, scales = "free_y") +
  coord_flip() +
  labs(title = "What owners own at least 3 franchises",
       fill = "Category",
       x = "",
       y = "Revenue (Billions)")
  
franchises %>%
  ggplot(aes(year_created, total_revenue)) +
  geom_point(aes(size = total_revenue, color = original_media)) +
  geom_text(aes(label = franchise), check_overlap =  TRUE, vjust = 1, hjust =1) +
  expand_limits(x= 1900)+
  labs(title = "When were the 'Great' franchises created") +
  guides(size = guide_legend(legend = FALSE))


media_franchises %>%
  group_by(original_media) %>%
  filter(sum(revenue) >=90) %>%
  ungroup() %>%
  mutate(revenue_category = fct_reorder(revenue_category, revenue, sum),
         original_media = fct_reorder(original_media, -revenue, sum)) %>%
  ggplot(aes(revenue_category, revenue)) +
  geom_col() +
  scale_y_continuous(labels = scales::dollar) +
  coord_flip() +
  facet_wrap(~original_media) +
  labs( y = "Revenue Category",
        y = "Revenue (Billions)" ,
        title = "What kinds of media lead to what types of revenue")





  




