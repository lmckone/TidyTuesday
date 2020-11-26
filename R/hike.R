library(dplyr)
library(stringr)
library(magrittr)
library(tidyverse)
library(ggsplot2)
library(scales)

### Load data
hike_tt <- tidytuesdayR::tt_load('2020-11-24')
hike <- hike_tt$hike_data

### Clean hike
hike %<>%
  distinct(name, location, .keep_all = TRUE) %>%
  # convert character fields to numeric
  mutate(across(.cols = c(length, gain, highpoint, rating), parse_number),
         region = str_trim(word(location, 1, sep = "\\--")), "both") %>%
  # create highest point per region and order ids for nice plotting
  group_by(region) %>%
  mutate(highest_point = max(highpoint)) %>%
  ungroup() %>%
  mutate(starting_point = highpoint - gain) %>%
  arrange(highest_point, highpoint) %>% 
  mutate(trail_id = row_number())

### Plot - high points by region
hike %>%
  mutate(highest_id = case_when(highpoint == highest_point ~ trail_id)) %>%
  mutate(starting_point = highpoint - gain) %>%
  ggplot() +
  geom_linerange(aes(x = trail_id, ymin = starting_point, ymax = highpoint), color = '#82A3A1', size = 0.2) +
  geom_text(aes(highest_id, 0, label = region), color = '#82A3A1', vjust= 1, nudge_x = -50, nudge_y = -5000, size = 4, angle = 90, family = "Avenir") +
  scale_x_continuous(limits = c(0, 1975)) +
  scale_y_continuous(breaks = seq(-20000, 15000, 5000), label = comma) +
  theme_minimal() +
  theme(text = element_text(family = 'Avenir', color = "#011936"),
        plot.title = element_text(family = 'Montserrat', face = "bold", size = 20),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title = "ELEVATION OF WASHINGTON TRAILS",
       subtitle = "This visualization displays low points and high points of each trail in the Pacific Northwest by region.",
       caption = "Data from from Washington Trails Association courtesy of the TidyX crew, Ellis Hughes and Patrick Ward",
       y = 'Feet above sea level')

ggsave(filename = "plots/hike.png")
