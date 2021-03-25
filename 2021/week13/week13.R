library(tidyverse)
library(ggtext)
library(systemfonts)


# Get titytuesday data ---------------------------------------------------------
df <- tidytuesdayR::tt_load(2021, week = 13)
unvotes <- df$unvotes
roll_calls <- df$roll_calls
roll_calls_description <- roll_calls %>% 
  dplyr::select(rcid, short) %>% 
  unique()

# Select fields and explore ----------------------------------------------------
# Environment-related topics
# Descriptions
selected <- c(5140, 5621, 6142, 6143, 6149, 9105)

rcid_order <- data.frame(rcid = selected,
                         x = c(1:6))

# 10-biggest economies as of 2021
countries <- c("US", "CN", "JP", "DE", "IN",
               "GB", "FR", "IT", "BR", "CA")

order <- data_frame(country_code = as.character(countries),
                    country_order = c(10:1)) # reverse to show #1 first

# See long descriptions                    
long_descriptions <- roll_calls %>% 
  filter(rcid %in% selected) %>% 
  select(descr)

# Short and descriptive names
issues <- data.frame(rcid = as.numeric(selected),
                     issues_descr = c("Agricultural technology",
                                      "Sustainable fisheries",
                                      "Entrepreneurship for sustainability",
                                      "Eradicate rural poverty",
                                      "Agenda 21",
                                      "Agenda 21")) %>% 
  as_tibble()

# Prepare data -----------------------------------------------------------------
data <- full_join(unvotes, roll_calls) %>% 
  filter(rcid %in% selected,
         country_code %in% countries) %>% 
  mutate(year = sapply(strsplit(as.character(date), "-"), "[", 1)) %>% 
  mutate(vote_number = case_when(vote == "yes" ~ 1,
                                 vote == "no" ~ 0,
                                 vote == "abstain" ~ -99)) %>% 
  # mutate(vote_number = ifelse(is.na(vote_number),
  #                             yes = -99,
  #                             no = vote_number)) %>% 
  select(country, country_code, year, rcid, vote_number, vote) %>% 
  left_join(issues, by = "rcid") %>% 
  left_join(order, by = "country_code") %>% # y axis
  left_join(rcid_order, by = "rcid")
  
# Count how many "yes" per year
year_order <- data %>% 
  group_by(rcid, year) %>% 
  filter(vote_number == 1) %>%  # so it will not sum the -99
  summarise(year_y = sum(vote_number))

data <- data %>% 
  left_join(year_order)

# Plot -------------------------------------------------------------------------
# Fonts
sysfonts::font_add_google("Cardo")
sysfonts::font_add_google("Bebas Neue")
showtext::showtext_auto()

data %>% 
  
  ggplot(aes(x = x)) +
  
  # Countries
  geom_text(aes(label = country, 
                y = country_order + year_y + 2, 
                color = vote),
            family = "Bebas Neue",
            show.legend = FALSE) +
  
  # dummy point for legend
  geom_point(aes(y = country_order + year_y + 2, 
                color = vote), size = 0) +
  
  # line between rcid description and year
  geom_segment(aes(x=x, xend=x, 
                   y = year_y + 25, yend=year_y+30), 
               linetype = "dotted", color = "#F9F6F0") +
  
  # rcid description
  geom_textbox(aes(label = issues_descr, 
                   y = year_y + 30), 
               width = unit(1.5, "inch"),
               halign = 0.5,
               family = "Cardo", 
               color = "#F9F6F0",
               fill = "#040910") +
  
  # year circle
  geom_point(aes(x = x, y = year_y + 20), 
             shape = 21, size = 50, 
             color = "#F9F6F0") +
  
  # year in circle
  geom_text(aes(label = year, y = year_y + 20), 
            size = 12,
            family = "Cardo", 
            color = "#F9F6F0") +
  
  # title
  geom_text(data = tibble(x = 3.5, y = 48,
                             label = "How the 10 largest economies have been voting on sustainability-related issues"),
                 aes(x = x, y = y, label = label),
                family = "Bebas Neue", fontface = "bold",
                color = "#F9F6F0",
                size = 10) +
  # subtitle
  geom_text(data = tibble(x = 2.48, y = 45,
                          label = "TidyTuesday week 13: Votes on selected UN sessions related to sustainability in the past decade"),
            aes(x = x, y = y, label = label),
            family = "Cardo", fontface = "bold",
            color = "#F9F6F0",
            size = 4) +
  
  # caption
  geom_text(data = tibble(x = 4, y = 1,
                          label = "Visualization by Anaís Ostroski | Data from Harvard Dataset by way of Mine Çetinkaya-Rundel, David Robinson, and Nicholas Goguen-Compagnoni."),
            aes(x = x, y = y, label = label),
            family = "Cardo",
            color = "#F9F6F0",
            size = 3.5) +
  
  # colors
  scale_color_manual(values = c("gray80", "tomato", "darkcyan")) +
  
  # limits
  scale_y_continuous(limits = c(0,50)) + scale_x_continuous(limits = c(0,7.5)) +
  
  # theme
  theme_void() +
  
  theme(legend.position = c(0.5,0.1),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        panel.background = element_rect(fill = "#040910"),
        plot.background = element_rect(fill = "#040910"),
        legend.text = element_text(family = "Bebas Neue", color="#F9F6F0", size = 12)) +
  
  guides(colour = guide_legend(override.aes = list(size=5)))
  
  
ggsave("week13_unvotes.png")
  
  
  
  
  
  
  
  







  
