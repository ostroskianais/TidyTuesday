library(tidyverse)
library(tidytuesdayR)
library(sf)
library(cowplot)

# DATA --------------------------------------------------------------------------------------------
# Read data with the tidytuesdayR package
df <- tidytuesdayR::tt_load(2020, week = 36)

key_crop_yields <- df$key_crop_yields

long_crops <- key_crop_yields %>% 
  pivot_longer(cols = 4:last_col(),
               names_to = "crop", 
               values_to = "crop_production") %>% 
  mutate(crop = str_remove_all(crop, " \\(tonnes per hectare\\)")) %>% 
  set_names(nm = names(.) %>% tolower())

# Get yield average for the countries/crops throughout the years
key_crop_yields_average <- long_crops %>% 
  group_by(Entity, Code) %>% 
  summarise(across(ends_with(")"),
                    ~mean(.x, na.rm=F)))

# Pivoted
long_crops_average <- long_crops %>% 
  group_by(entity, code, crop) %>% 
  summarise(crop_production = mean(crop_production)) %>% 
  drop_na() %>% 
  ungroup() %>% 
  group_by(crop) %>% 
  mutate(med = median(crop_production),
         min = min(crop_production),
         max = max(crop_production)) %>% 
  ungroup() %>% 
  mutate(
    crop = fct_reorder(crop, med),
    y = as.numeric(crop) - .3) ## last peace of code from Cedric Scherer 

# Crop Global average
crops_global <- long_crops_average %>% 
  select(crop, crop_production) %>% 
  group_by(crop) %>% summarise(crop_production = mean(crop_production)) %>% 
  arrange(desc(crop_production))

# Maximum value/country for each crop
maximum_crops <- long_crops_average %>% 
  filter(crop_production == max)

# Maximum value/country for each crop
minimum_crops <- long_crops_average %>% 
  filter(crop_production == min)

# Write about the data!
title <- tibble(x = 45, y = 6.7,
                label = "Differences in crop yield around the world")

text1 <- tibble(x = 45, y = 5,
               label = "The crop yield average varies greatly based on country and type of crop! 
               Some countries achieve impressive numbers of productivity. Belgium had an average yield of 
               44 tonnes potatoes/hectares when the global median is 13.8. Belgium also ranks the highest in
               average yields for barley and wheat. Mali reaches an average
               productivity of 55 tonnes banana/hectares. Italy has the highest average yield for Soybeans." )

text2 <- tibble(x = 45, y = 3.5,
               label = "Some countries prioritize some crops over others given climate, economy, available land, etc. 
               Guatemala, for example, has the lowest value for cassava yield, but performs well above average when 
               producing bananas (30 ton/ha). Other countries, like Brazil, have yield values very similar to the 
               median for all these crops!")

credit <- tibble(x = 46, y = 1,
                 label = "Tidy Tuesday, week 36 (2020). Data by Our World in Data.
                 Visualization by Anaís Ostroski. Inspiration and base code by Cédric Scherer.")
# Plot --------------------------------------------------------------------------------------------
ggplot(data = long_crops_average, aes(x = crop_production, y=y)) +
  
  # add title
  ggtext::geom_textbox(data = title, aes(x = x, y = y, label = label),
                       lineheight = 1.3,
                       width = unit(7, "inch"),
                       box.color = NA,
                       color = "gray70",
                       size = 8,
                       fontface = "bold") +
  
  # add text 1
  ggtext::geom_textbox(data = text1, aes(x = x, y = y, label = label),
               lineheight = 1.3,
               width = unit(7, "inch"),
               box.color = NA,
               family = "Nirmala UI") +
  
  # add text 2
  ggtext::geom_textbox(data = text2, aes(x = x, y = y, label = label),
                       lineheight = 1.3,
                       width = unit(7, "inch"),
                       box.color = NA,
                       family = "Nirmala UI") +
  
  # add credits
  ggtext::geom_textbox(data = credit, aes(x = x, y = y, label = label),
                       lineheight = 1.3,
                       width = unit(7.5, "inch"),
                       size = 3,
                       box.color = NA,
                       family = "Nirmala UI") +
  
  ## labels crops
  geom_text(data = long_crops_average, aes(x = max, y = y, label = crop),
    color = "gray70",
    fontface = "bold",
    vjust = 0.5,
    hjust = -0.2,
    size = 8) +
  
  ## indicator median
  geom_point(
    data = long_crops_average,
    aes(x = med, y = y - .18),
    shape = 17,
    color = "grey20",
    size = 2) +
  
  geom_text(
    data = long_crops_average,
    aes(x = med + 0.5, y = y - .11,
      label = glue::glue("{round(med, 1)}") ),
    color = "grey20",
    size = 4,
    vjust = 1,
    hjust = 0
  ) +
  
  ## stripe
  ggdist::stat_interval(
    aes(y = y - .05),
    orientation = "horizontal",
    .width = c(.25, .5, .95, 1),
    stroke = 0, size = 1.4, show.legend = FALSE
  ) +
  
  ## dots
  ggdist::stat_dots(
    quantiles = NA,
    orientation = "horizontal",
    normalize = "none",
    scale = .87,
    color = "#6f4e37",
    fill = "#6f4e37"
  ) +
  
  xlim(-10, 70) +
  
  theme_void()

ggsave()





