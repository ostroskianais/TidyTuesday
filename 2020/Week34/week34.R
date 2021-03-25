library(tidyverse)
library(tidytuesdayR)
library(sf)
library(cowplot)
library(patchwork)
# Read data with the tidytuesdayR package
df <- tidytuesdayR::tt_load(2020, week = 34)

# Take the plants data
plants <- df$plants
threats <- df$threats
actions <- df$actions

# Sum threats
plants$threat_SUM <- rowSums(plants[,c("threat_AA","threat_BRU","threat_RCD","threat_ISGD",
                                       "threat_EPM", "threat_CC", "threat_HID", "threat_P",
                                       "threat_TS", "threat_NSM", "threat_GE", "threat_NA")], na.rm=T)

# Import world geometry
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# See if there are countries that do not match between the
# world geometry df and the plants df
plants_countries <- plants %>% select(country) %>% unique() %>% 
  mutate(country = as.character(country))
world_countries <- world %>% select(sovereignt) %>% unique()  %>% 
  mutate(sovereignt = as.character(sovereignt)) %>% rename(country = sovereignt)

missing_countries <- anti_join(plants_countries, world_countries)

# Match country names
# Probably a better way to do this, but I am not sure :)
world <- world  %>% 
  mutate(sovereignt = ifelse(sovereignt == "United States of America",
                             yes = "United States", no = sovereignt),
         sovereignt = ifelse(sovereignt == "United Republic of Tanzania",
                             yes = "Tanzania", no = sovereignt),
         sovereignt = ifelse(sovereignt == "Republic of Congo",
                             yes = "Congo", no = sovereignt),
         sovereignt = ifelse(sovereignt == "Ivory Coast",
                             yes = "Côte d'Ivoire", no = sovereignt),
         sovereignt = ifelse(admin == "Bermuda",
                             yes = "Bermuda", no = sovereignt),
         sovereignt = ifelse(admin == "Pitcairn Islands",
                             yes = "Pitcairn", no = sovereignt),
         sovereignt = ifelse(admin == "Cape Verde",
                             yes = "Cabo Verde", no = sovereignt),
         sovereignt = ifelse(admin == "Vietnam",
                             yes = "Viet Nam", no = sovereignt),
         sovereignt = ifelse(admin == "Saint Helena",
                             yes = "Saint Helena, Ascension and Tristan da Cunha", no = sovereignt))

# Merge geometry
plants_sf <- plants %>% group_by(country) %>% summarise(threat_SUM = mean(threat_SUM)) %>% 
  merge(world, by.x="country", by.y="sovereignt") %>% st_as_sf()

nplants_country <- plants %>% select(country) %>% 
  mutate(n = 1) %>% 
  group_by(country) %>%  
  summarise(n=sum(n)) %>% 
  merge(world, by.x="country", by.y="sovereignt") %>% st_as_sf()

# Map the average number of threats
pal <- PNWColors::pnw_palette("Cascades", 100, "continuous")
map <- ggplot(world) +
  geom_sf(color=NA, fill = "gray30") +
  geom_sf(data=nplants_country, aes(fill=n), color=NA) +
  scale_fill_gradientn(name = "Number of extinct plants \n per country", 
                       colours = rev(pal)) +
  theme_void() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill="gray15"),
        plot.background = element_rect(fill="gray15"),
        legend.position = c(0.1,0.4),
        legend.text = element_text(color = "white", 
                                   family = "sans",
                                   size = ),
        legend.title = element_text(color = "white", 
                                    family = "sans",
                                    size = 8),
        legend.key.size = unit(x = 0.3, units = "cm"),
        legend.background = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))
map



# Find which threats threaten the most!

nplants_continent <- plants %>% select(continent) %>% 
  mutate(n = 1) %>% 
  group_by(continent) %>%  
  summarise(n=sum(n))

threats_present <- threats %>% filter(threatened >0) %>% 
  group_by(continent, threat_type) %>%
  summarise(threatened = sum(threatened)) %>% 
  mutate(total_threat = sum(threatened),
         threatened_rel = threatened/total_threat)

pal2 <- PNWColors::pnw_palette("Bay", 12, "continuous")

bars <- threats_present %>% 
  ggplot() +
  geom_bar(aes(x=continent, fill=threat_type, y=threatened_rel), 
           stat="identity", position = "dodge", color=NA) +
  scale_fill_manual(values = pal2) +
  xlab("") +
  ylab("Relative threat\n") +
  theme_void() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill="gray15"),
        plot.background = element_rect(fill="gray15"),
        legend.position = "top",
        legend.text = element_text(color = "white", 
                                   family = "sans",
                                   size = ),
        legend.title = element_text(color = "white", 
                                    family = "sans",
                                    size = 8),
        legend.key.size = unit(x = 0.3, units = "cm"),
        legend.background = element_blank(),
        axis.title = element_text(color = "white", 
                                  family = "sans",
                                  size = 8),
        axis.title.y = element_text(angle = 90),
        axis.text = element_text(color = "white", 
                                 family = "sans",
                                 size = 8))
bars

# Arrange

bigplot <- plot_grid(map,bars, ncol=1,
                     align = "c",
                     rel_widths = c(1,0.8),
                     rel_heights = c(1,0.6))

ggdraw() +
  draw_plot(map, height=0.55, width = 1, x=0,y=0.45) +
  draw_plot(bars, height=0.44, width = 0.6,x=0.2,y=0.05) +
  theme(plot.background = element_rect(fill="gray15"),
        panel.grid = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))

