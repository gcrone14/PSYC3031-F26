##### Compile Data #####
# load libraries
library(janitor)
library(tidyverse)

# Import data sets
star_drinks <- read_csv("Misc/starbucks-menu-nutrition-drinks.csv",
                        na = "-")
star_food <- read_csv("Misc/starbucks-menu-nutrition-food.csv",
                      # Fix weird encoding error
                      locale = locale(encoding = "UTF-16"),
                      na = "-")

# Clean up drinks data
star_drinks_revised <- star_drinks |>
  # Clean up names to snake_case
  janitor::clean_names() |>
  rename("item" = "x1") |>
  # Identify item as a drink for later data merge
  mutate(type = "Drink")

star_food_revised <- star_food |>
  # Clean up names to snake_case
  janitor::clean_names() |>
  rename("item" = "x1") |>
  # Identify item as a drink for later data merge
  mutate(type = "Food")

# Merge data sets
star_merged <- bind_rows(star_drinks_revised, 
                         star_food_revised) |>
  # Move the "type" variable so it's adjacent 
  # to items
  select(item, type, everything())

# View data to see if everything makes sense
glimpse(star_merged)

# Export final merged data
write_csv(star_merged, "Data/starbucks_items.csv")

