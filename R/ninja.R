library(dplyr)
library(magrittr)
library(tidyverse)
library(ggsplot2)
library(tidytext)
library(widyr)

ninja_tt <- tidytuesdayR::tt_load('2020-12-15')
ninja <- ninja_tt$ninja_warrior

obstacles <- ninja %>%
  distinct(obstacle_name)

obstacles_pairwise <- obstacles %>%
  mutate(id = seq(1, n(), 1)) %>%
  unnest_tokens(word, obstacle_name) %>%
  anti_join(stop_words) %>%
  pairwise_count(word, id) %>%
  arrange(desc(n)) %>%
  filter(n > 1) %>%
  mutate(n = factor(n))

ggplot(obstacles_pairwise) +
  geom_point(aes(item1, item2, size = n, color = n)) +
  annotate("text",
           x = 'cargo',
           y = 'net',
           label = paste('Jumping Bars into Cargo Net\n', 'Rope Swing Into Cargo Net\n', 'Spikes Into Cargo Net'),
           color = '#d2d2d2',
           family = 'DM Mono',
           size = 2.5,
           hjust = 0) +
  annotate("text",
           x = 'salmon',
           y = 'ladder',
           label = paste('Salmon Ladder\n', 'Double Salmon Ladder\n', 'Down Up Salmon Ladder\n', 'Criss Cross Salmon Ladder'),
           color = '#d2d2d2',
           family = 'DM Mono',
           size = 2.5,
           hjust = 1) +
  scale_color_brewer(palette = "Reds") +
  theme(plot.caption = element_text(family = 'DM Mono', size = 8),
        title = element_text(family = 'Diamante', size = 20),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "#333336", linetype = 'dashed'),
        panel.grid.minor = element_line(color = "#333336", linetype = 'dashed'),
        text = element_text(color = "#d2d2d2", family = 'DM Mono'),
        axis.text = element_text(color = "#d2d2d2"),
        legend.background = element_rect(fill = "black"),
        legend.key = element_rect(fill = "black"),
        legend.text = element_text(family = 'DM Mono'),
        legend.title = element_text(family = 'DM Mono', size = 10)) +
  labs(title = 'AMERICAN NINJA WARRIOR OBSTACLE NAMES', 
       x = '',
       y = '',
       caption = '@lubovmck TidyTuesday 12-15-2020. Data from Sasukepedia.',
       color= 'correlation', 
       size= 'correlation')

ggsave(filename = 'plots/ninja.png')




