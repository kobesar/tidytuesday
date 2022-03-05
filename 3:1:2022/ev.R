library(tidyverse)
library(viridis)
library(patchwork)
library(MetBrewer)
library(gridExtra)
library(ggtext)
library(ggrepel)
library(ggmap)
library(sf)
library(cowplot)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(ggarrange)

# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

# Load this file. (Note: I stored in a folder called DATA)
spdf <- geojson_read("ushexgrid.geojson",  what = "sp")

# Bit of reformating
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# Show it
plot(spdf)
# I need to 'fortify' the data to be able to show it with ggplot2 (we need a data frame format)
library(broom)
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

# Calculate the centroid of each hexagon to add the label:
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

states <- read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv")

stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv') %>%
  janitor::clean_names()

spdf_fortified <- tidy(spdf, region = "google_name")

evus <- stations %>% 
  filter(fuel_type_code == "ELEC" & country == "US")

non_main <- c("VI","AS","GU","MP","PR", "ON")

bg_color <- met.brewer("VanGogh3")[1]

gn <- met.brewer("Derain")[2]

cons_theme <- theme(
  text = element_text(family = "Source Serif Pro"),
  plot.background  = element_rect(fill = bg_color, color = bg_color, size = 1),
  panel.border = element_blank(),
  panel.grid = element_line(size = 0),
  plot.title = element_text(size = 16, margin = margin(30,0,0,0)),
  plot.subtitle = element_text(size = 12, face = "italic"),
  axis.text.x = element_text(size = 10, margin=margin(0,0,20,0)),
  axis.text.y = element_text(size = 10, margin=margin(0,0,20,0))
)

year_sum <- stations %>% 
  filter(fuel_type_code == "ELEC" & country == "US") %>% 
  summarize(open_year = as.numeric(str_sub(open_date, 1, 4))) %>% 
  group_by(open_year) %>% 
  summarize(n = n()) %>% 
  filter(open_year < 2022)

year_plot <- year_sum %>% 
  filter(open_year > 2009) %>% 
  ggplot() +
  geom_text(data = year_sum[year_sum$open_year > 2009, ], aes(x = open_year, y = n + 700, label = n), family = "Source Serif Pro", fontface = "italic") +
  geom_bar(aes(x = open_year, y = n), stat = "identity", fill = gn) +
  scale_x_discrete(limits = c(2010:2021)) +
  labs(x = "", y = "", title = "New EV Charging Stations Opened in the U.S.", subtitle = "2010-2021") +
  theme_minimal() +
  cons_theme +
  theme(plot.title = element_text(size = 16, margin = margin(60,0,0,0)),
        plot.margin = margin(0,2,0,0, "cm"))

ggplot() + 
  geom_sf(data = states, fill = NA, size = .1) +
  geom_hex(data = evus, aes(x = x, y = y))

pchange20_21 <- evus %>% 
  mutate(open_year = as.numeric(str_sub(open_date, 1, 4))) %>% 
  group_by(open_year, state) %>% 
  summarize(n = n()) %>% 
  group_by(state) %>% 
  mutate(n_cum = cumsum(n)) %>% 
  # filter(open_year == 2010 | open_year == 2021) %>% 
  group_by(state) %>% 
  arrange(open_year) %>% 
  summarize(open_year, pchange = n_cum/lag(n_cum) * 100 - 100, n_cum) %>%
  filter(open_year == 2021 & !state %in% non_main) %>% 
  select(state, pchange, n_cum)

spdf_fortified_test <- spdf_fortified %>%
  left_join(states, by = c("id" = "State")) %>% 
  left_join(. , pchange20_21, by=c("Abbreviation"="state")) 

ggplot() +
  geom_polygon(data = spdf_fortified_test, aes(fill =  pchange, x = long, y = lat, group = group)) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=3, alpha=0.6) +
  scale_fill_gradient(trans = "log", palette = met.brewer("Derain")) +
  theme_void() +
  coord_map()

spdf_fortified_test$bin <- cut( spdf_fortified_test$pchange , breaks=c(0, 40, 80, Inf), labels=c("0-40%", "40-80%", "80%+"), include.lowest = TRUE )

evmap <- ggplot() +
  geom_polygon(data = spdf_fortified_test, aes(fill =  bin, x = long, y = lat, group = group)) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=3, alpha=0.6) +
  labs(title = "The Future of America is Looking Electric",
       subtitle = "All states look to be expanding their EV charging network") +
  scale_fill_manual(
    name = "Percent Increase of EV Charging Stations From 2020 to 2021",
    values = rev(met.brewer("Derain")[c(2,3,4)]),
    guide = guide_legend(keyheight = unit(4, units = "mm"), keywidth = unit(20, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1, override.aes = list(size = 5), title.theme = element_text(size = 16, family = "Source Serif Pro")) 
  ) +
  coord_map() +
  theme_void() +
  cons_theme +
  theme(
    plot.title = element_text(size = 30, family = "Source Serif Pro", face = "bold", hjust = 0.2),
    plot.subtitle = element_text(size = 14, family = "Source Serif Pro", face = "italic", hjust = 0.1),
    panel.border = element_blank(), 
    legend.position = c(0.5, 0.9),
    plot.background = element_rect(fill = bg_color),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())

cumStations <- pchange20_21 %>% 
  arrange(n_cum) %>% 
  left_join(states, by = c("state" = "Abbreviation"))

cumStations$state <- factor(cumStations$state, levels=cumStations$state)

bot10 <- cumStations %>% 
  filter(!is.na(state) & !state %in% non_main) %>% 
  head(10) %>% 
  ggplot() +
  geom_bar(aes(x = n_cum, y = state), stat = "identity", fill = met.brewer("Derain")[4]) +
  labs(y = "", x = "", title = "Bottom 10 States by Total EV Charging Stations", subtitle = "1990-2022",
       caption = "@swingmisstake | Data: USDOT") +
  cons_theme +
  theme(
    panel.background = element_rect(fill = bg_color),
    plot.margin = margin(0,2,1,0, "cm"))

# (evmap | (year_plot / year_plot)) +
#   plot_layout(widths = c(20, 12, 12))

(year_plot / bot10) + evmap  + 
  plot_layout(widths = c(5, 5, 10), 
              design = "
              CCCAA
              CCCAA
              CCCBB
              CCCBB
              ")

  plot_annotation(title = "The Future of America is Looking Electric",
                  subtitle = "All states look to be expanding their EV charging network",
                  theme = theme(plot.title = element_text(size = 30, family = "Source Serif Pro", face = "bold", hjust = 0.1, vjust = -2),
                                plot.subtitle = element_text(size = 14, family = "Source Serif Pro", face = "italic", hjust = 0.1, vjust = -6),
                                plot.background = element_rect(fill = bg_color, color = bg_color, size = 1)))




ggsave("test.png", width = 18, height = 10, units = "in")
