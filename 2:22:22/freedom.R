library(tidyverse)
library(viridis)
library(patchwork)
library(MetBrewer)
library(gridExtra)
library(ggtext)
library(ggrepel)

freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

rcodes <- c(2 = "Africa", 9 = "Oceania", 19 = "Americas", 142 = "Asia", 150 = "Europe")

bg_color <- met.brewer("VanGogh3")[1]

theme_border <- theme(plot.background = element_rect(fill = bg_color, colour = bg_color, size = 0))

cons_theme <- theme(
  plot.margin = margin(1,0,1,1, "cm"),
  panel.grid = element_line(color = "#D5DCC8"),
  plot.background = element_rect(fill = bg_color, color = bg_color, size = 1),
  text = element_text(family = "Source Serif Pro"),
  plot.title = element_text(size = 18),
  plot.subtitle = element_text(color = "#5F5F5F"),
  axis.title.x  = element_text(margin = margin(30,0,0,0), size = 12),
  axis.title.y = element_text(margin = margin(0,30,0,0), size = 12))

region_years <- freedom %>% 
  group_by(year, Region_Code) %>% 
  summarize(CL = mean(CL), PR = mean(PR)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = CL, color = as.factor(Region_Code)))

freedom %>% 
  group_by(year, is_ldc) %>% 
  summarize(PR = mean(PR)) %>% 
  ggplot() +
  geom_point(aes(x = year, y = PR, color = as.factor(is_ldc)))

freedom %>% 
  group_by(CL, PR) %>% 
  summarize(n = n()) %>% 
  ggplot() +
  geom_tile(aes(x = CL, y = PR, fill = n))+
  coord_fixed() +
  scale_fill_viridis() + theme_bw()

linrel <- freedom %>% 
  group_by(CL, PR) %>% 
  summarize(n = n()) %>% 
  ggplot() +
  geom_point(aes(x = CL, y = PR, size = n, alpha = n), color = "#95BC95", show.legend = FALSE) +
  xlim(c(0, 8)) +
  ylim(c(0, 8)) +
  scale_size_continuous(range = c(1, 10)) +
  scale_alpha_continuous(range = c(0.5, 1)) +
  labs(x = "Civil Liberties Score", y = "Political Rights Score", title = "Civil Liberties vs. Political Rights", subtitle = "Darker/Larger the circle = higher frequency") +
  theme_minimal() +
  cons_theme

freedom_means <- freedom %>% 
  group_by(year, is_ldc) %>% 
  summarize(across(everything(), mean)) %>% 
  mutate(ldc_full = ifelse(is_ldc == 1, "Least Developing Countries", "Non-Least Developing Countries"))

cl_years <- freedom_means %>% 
  ggplot(aes(x = year, y = CL)) +
  geom_line(aes(group = year), size = 1, alpha = 0.5, color = "#757671", show.legend = FALSE) +
  # geom_text_repel(data = freedom_means[freedom_means$year==1995, ], aes(label = ldc_full), nudge_y = ifelse(freedom_means$is_ldc[freedom_means$year==1995] == 1, 0.3, -0.3), family = "Source Serif Pro") +   
  geom_point(aes(color = as.factor(is_ldc)), size = 2, show.legend = FALSE) +
  scale_color_manual(values = c("0" = "#95BC95", "1" = "#A7B7D9")) +
  labs(x = "", y = "", subtitle = "Civil Liberties Score (1995-2020)", caption = "@swingmisstake | Data: Freedom House & United Nations") +
  ylim(c(1.5, 5.2)) +
  theme_minimal() +
  cons_theme +
  theme(plot.margin = margin(0, 2, 0, 0, "cm"))

pr_years <- freedom_means %>% 
  ggplot(aes(x = year, y = PR)) +
  geom_line(aes(group = year), size = 1, alpha = 0.5, color = "#757671", show.legend = FALSE) +
  geom_text_repel(data = freedom_means[freedom_means$year==1995, ], aes(label = ldc_full), nudge_y = ifelse(freedom_means$is_ldc[freedom_means$year==1995] == 1, 0.7, -0.7), family = "Source Serif Pro", color = "#5F5F5F") + 
  geom_point(aes(color = as.factor(is_ldc)), size = 2, show.legend = FALSE) +
  scale_color_manual(values = c("0" = "#95BC95", "1" = "#A7B7D9")) +
  labs(x = "", y = "", title = "Developing. vs. Non-Developing Countries", subtitle = "Political Rights Score (1995-2020)") +
  ylim(c(1.5, 5.2)) +
  theme_minimal() +
  cons_theme +
  theme(plot.margin = margin(0, 2, 0, 0, "cm"))

plts <-  linrel + (pr_years / cl_years)

ggsave("~/desktop/projects/tidytuesday/viz1.png", plts, width = 12, height = 8, units = "in")

freedom %>%
  ggplot() +
  geom_histogram(aes(x = CL)) +
  facet_wrap(~ year)
