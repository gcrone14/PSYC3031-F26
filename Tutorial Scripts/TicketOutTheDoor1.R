#### Ticket out-the-door (1) - Answer ####

#' **NOTE: The data to use is starbucks_items.csv.** 
#' This file cannot be found directly, but its 
#' constituent data files can be found on this Kaggle page:
#' https://www.kaggle.com/datasets/starbucks/starbucks-menu

#' What I did was import the data from online, and merge two data
#' sets from the base files:
#' starbucks-menu-nutrition-drinks.csv (drinks), and
#' starbucks-menu-nutrition-food.csv (food).

#' The columns are as follows:
#' * item = The name of the item
#' * type = Whether the item is a food (Food) or a drink (Drink)
#' * calories = # of Calroies in the item
#' The other variables are other nutritional info, including
#' fat (g), carb (g), fiber (g), protein (g), sodium (mg),
#' and protein (g).

#' We begin by loading our packages and importing the data.
#' Next, we clean the data so it's easier to analyse, before
#' generating descriptive statistics and a visualization.

# Loading packages
library(here)
library(psych)
library(tidyverse)
    
# Loading the data 
star_items <- read.csv(here("Data/starbucks_items.csv")) |>
  # For simplicity of viewing
  as_tibble()

# Checking data 
str(star_items)
glimpse(star_items)

#' Here we do a simple data cleaning/modification. Note that this
#' step is technically optional in this case. It is nice to clean the data
#' up a tad.
star_items_final <- star_items |>
  # You may not wish to select only these variables,
  # but it's up to you.
  select(type, fat_g, carb_g) |>
  # We filter out missing data because it doesn't
  # provide any useful information.
  filter(!is.na(fat_g) & !is.na(carb_g))

# Descriptive stats for `type`
star_items_final |>
  select(type) |>
  table()

#' The above shows that there are slightly more food items (113) 
#' than drink items (92).

#' (Note: You may show a bar chart of this result, but I find it superfluous
#' since we only have two values.)

# Descriptive stats for fat and carbs
star_items |>
  select(fat_g, carb_g) |>
  psych::describe()

#' On average, items at Starbucks tend to contain more grams of carbohydrates
#' (*M* = 33.97, *SD* = 17.61, *Mdn* = 35) than grams of fats 
#' (*M* = 10.06, *SD* = 9.96, *Mdn* = 7.0).
#' 
#' (Note: Reporting the median is usually optional; I did so because there was
#' a large discrepancy between the mean and median for fats).

#'  It's often helpful to visualize the distributions of continuous data
# For fats
star_items_final |>
  ggplot(aes(x = fat_g)) +
  geom_histogram(bins = 15, color = "black", fill = "royalblue") +
  # restrict x-axis for cleaner comparison
  xlim(0, 90) +
  ylim(0, 80)

# For carbs
star_items_final |>
  ggplot(aes(x = carb_g)) +
  geom_histogram(bins = 15, color = "black", fill = "royalblue") +
  xlim(0, 90) +
  ylim(0, 80)

#' (We'll discuss ways to polish plots later on. These work for now.)

#' Fats appear to be far more right-skewed (i.e., more values)
#' that are lower) with lower variability compared to carbs, which appears more
#' normally distributed and more varied.

#' It would also be worthwhile to compare based on the type of item
#' they are (drink vs. food).
#' 
#' We didn't discuss it yet, but using the psych::describeBy()
#' function would accomplish this. (I would encourage you to look at the
#' help documentation for this function.)
star_items_final |>
  select(-type) |>
  psych::describeBy(group = star_items_final$type)

#' Drinks appear to have fewer grams of fat (*M* = 2.34, *SD* = 3.89)
#' and carbohydrates (*M* = 27.74, *SD* = 15.21)
#' content relative to foods (fat: *M* = 16.35, *SD* = 8.30; 
#' carbs: *M* = 41.49, *SD* = 15.80).

# Main visualization
star_items_final |>
  ggplot(aes(x = carb_g, y = fat_g, color = type)) +
  geom_point() +
  # Good addition: Fits a line of best fit with margin of error
  geom_smooth(method = "lm", formula = "y ~ x") +
  # Good addition: Adds axis labels
  labs(x = "Carbohydrates (g)", y = "Fats (g)",
       title = "Figure 1") +
  # Good addition: Makes theme simpler
  theme_bw()

#' SAMPLE WRITEUP:
#' **DISCLAIMER---PLEASE READ!**

#' Writing a proper statistical report is not easy. Students
#' are usually tempted to copy-paste any template provided and
#' sub in words, tweak interpretations, and be done.
#' Doing that, while ok in the short term, does not actually help
#' your statistical skills because interpreting data becomes
#' a memorization game.
#' 
#' Rather than relying on a template, do your best to interpret
#' descriptive stats and plots on your own first. After giving it a try,
#' you may refer to a template just to demonstrate a good example to yourself.
#' Always think critically: what aspects of the template work well, and which
#' don't? 
#' 
#' If you really want to use a template, please do not simply copy-paste the 
#' one below for your own. Doing so is a bad idea because I have my own style of 
#' writing and explaining myself. If you wish to make use of it, 
#' you should always modify it to suit the data, descriptive stats,
#' and  visualizations that are produced. Most importantly, you must
#' write it in your own voice and style.
#' **END OF DISCLAIMER**

#' **START WRITEUP**
#' The data set at present represents nutritional information
#' for various items at *Starbucks* stores. I was interested
#' in determining if there was a linear relationship between
#' the grams of carbohydrates (sugar) and the grams of fat
#' in each item, and whether this relationship differed
#' as a function of the type of *Starbucks* item (food or drink).
#'  
#' To accomplish this goal, I generated and interpreted simple univariate
#' descriptive statistics and visualizations.
#' 
#' On average, *Starbucks* items tended to contain more grams of sugar
#' (*M* = 33.97, *SD* = 17.61, *Mdn* = 35) than grams of fats 
#' (*M* = 10.06, *SD* = 9.96, *Mdn* = 7.0).

#' As well, drinks appear to have fewer grams of fat (*M* = 2.34, *SD* = 3.89)
#' and carbohydrates (*M* = 27.74, *SD* = 15.21)
#' relative to foods (fat: *M* = 16.35, *SD* = 8.30; 
#' carbs: *M* = 41.49, *SD* = 15.80). (This makes sense because drinks often
#' have lower contents of sugars, fats, etc... than foods.)

#' Figure 1 (see above) illustrates the linear relationship between
#' fats and sugar as a function of drink type. In the plot, 
#' there is a positive relationship between carbohydrates (g) and fats (g)
#' for both drink and food items. Both relationships tended to have roughly
#' the same linear slope, but had different intercepts.
#' 
#' Overall, this suggests that foods tended to have a higher fat and carbojydrate
#' content relative to drinks, but for foods and drinks, items containing more
#' sugar also tended to contain more fat (and vice versa). Further
#' statistical models should be fit to confirm this relationship and explore
#' it further.
#' **END WRITEUP**