##### Mini-Assignment 1 Solutions #####
# Load libraries
library(psych)
library(here)
library(tidyverse)

#' **1) Import the TIPI_data.csv file into R and save it as a dataframe called personality**
#' NOTE: I use read_csv() from tidyverse, rather
#' than read.csv(), since read_csv() automatically
#' converts the imported data to a tibble,
#' which I prefer.

# Import data
# Note: I use here as a wrapper on the directory to the file
# because it's safer when the document is knitted.
personality <- read_csv(here("Data/TIPI_data.csv"))

#' Inspecting data is also a good idea, to ensure the data
#' was read in properly.

# Inspect data
str(personality)
glimpse(personality)

#' **2) Using the personality dataframe, create a new dataframe called personality_domains that contains only ID and the five personality domain variables.**
# Select vars of interest
personality_domains <- personality |>
  select(ID, Extraversion:Openness)

# Check if selection worked
colnames(personality_domains)

#' **3) Using the personality dataframe, create a new dataframe called hs_or_less that contains all variables for individuals whose education level is either less than high school or high school.**

#' It's wise to first view the unique levels of education

# View unique values of education
table(personality$education)

#' Based on this, education takes on four values:
#' <HS, Grad, HS, and Univ.
#' Thus, our filter should ensure that people are either in the "HS" or "<HS"
#' groups.

# Filter data so people are in HS or lower
# (Using %in% is Gabe's preference.)
hs_or_less <- personality |>
  filter(education %in% c("<HS", "HS"))
# (The following is equivalent to the above and would also be accepted.)
hs_or_less <- personality |>
  filter(education == "HS" | education == "<HS")

# Check that filter worked
table(hs_or_less$education)

#' **4) Using the personality dataframe, create a new dataframe called married_suburban that contains all variables for individuals who are currently married and live in a suburban neighbourhood.**

# Check levels of married and neighborhood
table(personality$married)
table(personality$urban)

# Filter data so only currently married people
# in suburban homes appear
married_suburban <- personality |>
  filter(married == "Currently married" & 
         urban == "Suburban")

# Check that filter worked
# Since only one level appeared,
# the filter worked appropriately
table(married_suburban$married,
      married_suburban$urban)

#' The transformation worked since only one
#' combination of both variables was counted.

#' **5) In the personality dataframe, use familysize to create a new categorical variable called familysize_cat with two categories: 2 or less and more than 2. Hint: You will need to use an ifelse() statement as part of your code.**

# Check levels of familysize
table(personality$familysize)

# Save into new data object because over-writing
# data is poor practice
personality_revised <- personality |>
  mutate(familysize_cat = ifelse(
    familysize <= 2, # Condition
    "Two or less", # Value if TRUE
    "More than two" # Value if FALSE
  ))

# Check if transformation worked
table(personality_revised$familysize_cat)

# OPTIONAL: More thorough check
personality_revised |>
  count(familysize, familysize_cat)

#' A more elagant solution, IMO, is to use
#' case_when() instead of ifelse(), as it's
#' more flexible in case missing values arise.

# OPTIONAL: case_when()
personality_revised <- personality |>
  # Must convert familysize to numeric first
  mutate(familysize = as.numeric(familysize)) |>
  mutate(familysize_cat = case_when(
    # Condition 1 ~ Value if Condition 1 is TRUE,
    # Condition 2 ~ Value if Condition 2 is TRUE,
    # etc...
    familysize <= 2 ~ "Two or less",
    familysize > 2 ~ "More than two",
    # TRUE denotes what to do if all prior logical
    # statements are FALSE (e.g., in case of
    # missing data)
    TRUE ~ NA_character_
  ))

#' **6) Using the personality dataframe, compute the mean Agreeableness score separately for individuals who voted and individuals who did not vote.**

# Check levels of voting
table(personality$voted)

# Create data frames for people who voted
# vs. didn't vote
voted <- personality |> 
  filter(voted == "Yes")
did_not_vote <- personality |> 
  filter(voted == "No")

# Check if filters worked
table(voted$voted)
table(did_not_vote$voted)

# Compute means for both data frames
mean(voted$Agreeableness)
mean(did_not_vote$Agreeableness)

#' The mean Agreeableness was higher among those who voted
#' (*M* = 4.45) than those who did not vote (*M* = 4.23),
#' though this mean difference was small.