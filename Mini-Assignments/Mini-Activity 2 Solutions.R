##### Mini-Activity 2 Solutions #####

#' We start by setting up our environment.

# Load libraries
library(psych)
library(here)
library(tidyverse)

#' **1) Import the data into R and select the following variables and create a new dataframe with the following variables: ID, Extraversion, Neuroticism, Conscientiousness, Agreeableness, Openness, education, urban, age, race, married. You will use these eleven variables only in subsequent questions.**

# Import data
personality <- read_csv(here::here("Data/TIPI_data.csv"))

#' To check if data import worked, I'll
#' just use glimpse(), but you can use whatever
#' you wish.

# Check if data imported properly
glimpse(personality)

# Select vars of interest
personality_select <- personality |>
  select(ID, Extraversion:Openness,
         education, urban, age,
         race, married)

#' **2) Based on the type of variables in your new dataframe created in (1), identify those that may need to be converted to factors and do this conversion in R.**

#' Let's take a look at what variables we're working with.
glimpse(personality_select)

#' Clearly, the categorical variables are
#' education, urban, race, and married.
#' 
#' We should convert these to factors, as
#' we do below.
#' 
#' I like to just remind myself in code
#' what each level is.
table(personality_select$education) |> names()
table(personality_select$urban) |> names()
table(personality_select$race) |> names()
table(personality_select$married) |> names()

#' To make things look a bit neater, I'll save
#' character vectors to capture the unique levels
#' of each categorical variable, then use those
#' into the eventual conversion.
#' 
#' We should also really convert the ID variable
#' to a character, so as not to get it confused with
#' a numeric variable.

# Save unique levels of each variable
education_levels <- table(personality_select$education)  |> names()
urban_levels <- table(personality_select$urban) |> names()
race_levels <- table(personality_select$race) |> names()
married_levels <- table(personality_select$married) |> names()

# Convert factor variables to factor type
personality_final <- personality_select |>
  mutate(ID = as.character(ID),
         education = factor(education, education_levels),
         urban = factor(urban, urban_levels),
         race = factor(race, race_levels),
         married = factor(married, married_levels))

# Check that modification worked
personality_final |>
  select(ID, education, urban, race, married) |>
  glimpse()

#' **3) Generate the appropriate descriptives (both numerical and graphical) for each individual variable.**

#' Let's start with the continuous variables.
personality_final |>
  select(Extraversion:Openness, age) |>
  psych::describe()

#' If we look at age, something is going
#' wrong: there's someone in the data
#' who is 5555 years old! Call Guinness World Records!
#' 
#' It's likely an error with how data were recorded,
#' so I'd suggest putting an NA in its place instead.
personality_final <- personality_final |>
  mutate(age = ifelse(age > 200, NA, age))

#' Now let's take a look again
personality_final |>
  select(Extraversion:Openness, age) |>
  psych::describe()

#' Age looks a lot better! Note that if
#' you didn't repair age here, the boxplot
#' for age would look extremely off and flattened.
#' That's why it's so important to always
#' explore your data!
#' 
#' If we want to plot each one, I'd simply
#' make boxplots for each variable.

#' The basic code is as follows:
# personality_final |>
#   ggplot(aes(x = VARIABLE)) +
#   geom_boxplot()

#' Now we just sub in VARIABLE with
#' each variable.
personality_final |>
  ggplot(aes(y = Extraversion)) +
  geom_boxplot()

personality_final |>
  ggplot(aes(y = Neuroticism)) +
  geom_boxplot()

personality_final |>
  ggplot(aes(y = Conscientiousness)) +
  geom_boxplot()

personality_final |>
  ggplot(aes(y = Agreeableness)) +
  geom_boxplot()

personality_final |>
  ggplot(aes(y = Openness)) +
  geom_boxplot()

personality_final |>
  ggplot(aes(y = age)) +
  geom_boxplot()

#' All of the plots give us a nice overview
#' of the data.
#' 
#' **TOTALLY OPTIONAL**: I'd find a density
#' plot useful to see distributional
#' differences on the personality
#' characteristics.
personality_final |>
  # Advanced technique: convert data
  # to long-form to make later
  # plotting easier
  pivot_longer(cols = Extraversion:Openness,
              names_to = "personality_dim",
              values_to = "score") |>
  select(personality_dim, score) |>
  ggplot(aes(x = score, fill = personality_dim)) +
  geom_density(alpha = 0.3, adjust = 1)

#' Each personality characteristic has a unique
#' density curve, each with a different central
#' tendancy, variability, and skew.
#' (These are reflected in the descriptive stats
#' from earlier.)
#' 
#' For the categorical variables, we simply generate frequency counts
#' for each one.

#' Quickest way to do this is using the summary() function.
personality_final |>
  select(education, urban, race, married) |>
  summary()

#' We immediately get a sense of the frequency of each group
#' in our data. The sample consists of mostly high school
#' graduates, those living in suburbia, white people, 
#' and those who have never been married.
#' 
#' (This information is useful when considering how generalizable
#' our results will be if using inferential tests.)
#' 
#' It's usually not worth it to make plots of this information,
#' but if you had to, you can make simple bar plots.
#' 
#' The generic formatting is as follows:
# personality_final |>
#   filter(!is.na(VARIABLE)) |>
#   ggplot(aes(x = VARIABLE)) +
#   geom_bar()

personality_final |>
  filter(!is.na(education)) |>
  ggplot(aes(x = education)) +
  geom_bar()

personality_final |>
  filter(!is.na(urban)) |>
  ggplot(aes(x = urban)) +
  geom_bar()

personality_final |>
  filter(!is.na(race)) |>
  ggplot(aes(x = race)) +
  geom_bar()

personality_final |>
  filter(!is.na(married)) |>
  ggplot(aes(x = married)) +
  geom_bar()

#' **4) Choose two variables only and generate the appropriate graph to display their relationship. **

#' This question will vary based on what you decide to check for.
#' 
#' I'm curious to see if neuroticism differs as
#' a function of one's marital status.
#' Let's take a look...
#' 
#' The plot we'd want to make is a boxplot,
#' using marital status to separate each
#' box.
personality_final |>
  # Filter NAs for clearer plot
  filter(!is.na(married)) |>
  ggplot(aes(x = married, y = Neuroticism)) +
  geom_boxplot()

#' There appear to be only slight differences
#' between groups.
#' 
#' It'd help to see the numeric descriptives
#' accompnying this graph.
psych::describeBy(personality_final$Neuroticism,
                  personality_final$married)

#' More simple display
personality_final |>
  group_by(married) |>
  summarise(mean_neuro = mean(Neuroticism),
            sd_neuro = sd(Neuroticism)) |>
  # Drop out the NAs for clarity
  drop_na() |>
  # Arrange values from highest to lowest
  arrange(desc(mean_neuro))

#' The mean neuroticism (*M* = 4.12, *SD* = 1.77) is highest
#' within those who are never married. The mean for those who are 
#' currently married (*M* = 3.47, *SD* = 1.65)
#' or previously married (*M* = 3.44, *SD* = 1.63) 
#' is similar.

#' (Perhaps neurotic people tend not to get married, or marriage
#' results in less neuroticism.)
#' 
#' **5) Provide a written summary of the information you generated in 3 and 4 above. When presenting your summary please note you will need to provide enough information about the variables so that someone not familiar with the data can understand.**
#' 
#' Researchers collected data from `r nrow(personality_final)` participants
#' regarding several variables, including their personality scores
#' (along the "Big Five" traits: Openness, Conscientiousness, Extroversion,
#' Agreeableness, and Neuroticism), their educational background,
#' community of residence (urban, suburban, or rural), age, race, and
#' marital status (currently married, never married, or previously married).
#'
#' The sample had an average age of 28.44 years (*Mdn* = 23.0,
#' *SD* = 13.76). Regarding education, most of the sample were high school
#' graduates (*n* = 757), followed by university graduates (*n* = 488),
#' those who did not graduate high school (*n* = 278), those who attended
#' and completed graduate school (*n* = 245), and those who did not provide
#' information on this item (*n* = 31).
#'
#' Most participants lived in suburban neighborhoods (*n* = 815),
#' followed by urban neighborhoods (*n* = 645), or rural neighborhoods
#' (*n* = 317). Twenty-two people did not specify their community of
#' residence.
#'
#' Most participants were white (*n* = 1440), followed by those who
#' specified "other" (*n* = 179), those who were Asian (*n* = 119),
#' Black (*n* = 28), or Arab (*n* = 14). Nineteen people did not specify
#' their race.
#'
#' Participants were largely never married (*n* = 1383); 314 people were
#' currently married, and 93 were previously married (divorced). Nine
#' people did not specify their marital status.
#'
#' Regarding personality characteristics, the sample had a mean
#' extraversion of 3.20 (*SD* = 1.68), mean neuroticism of 3.97
#' (*SD* = 1.76), mean conscientiousness of 4.39 (*SD* = 1.56),
#' mean agreeableness of 4.30 (*SD* = 1.41), and mean openness of 5.50
#' (*SD* = 1.21).
#'
#' The plots below summarise the descriptive statistical information
#' above.
#'
#' (NOTE: In APA, you should actually label and format each plot differently
#' so it is neater, and each time you refer to a plot, it's with a purpose.
#' Doing this kind of thing in actual papers is never a good idea, unless
#' you put it in the supplemental. We usually avoid plotting descriptive
#' stats beyond EDA, and opt instead for an informative table (or tables)
#' for numeric descriptives.)
personality_final |>
  filter(!is.na(education)) |>
  ggplot(aes(x = education)) +
  geom_bar()

personality_final |>
  filter(!is.na(urban)) |>
  ggplot(aes(x = urban)) +
  geom_bar()

personality_final |>
  filter(!is.na(race)) |>
  ggplot(aes(x = race)) +
  geom_bar()

personality_final |>
  filter(!is.na(married)) |>
  ggplot(aes(x = married)) +
  geom_bar()

personality_final |>
  ggplot(aes(y = Extraversion)) +
  geom_boxplot()

personality_final |>
  ggplot(aes(y = Neuroticism)) +
  geom_boxplot()

personality_final |>
  ggplot(aes(y = Conscientiousness)) +
  geom_boxplot()

personality_final |>
  ggplot(aes(y = Agreeableness)) +
  geom_boxplot()

personality_final |>
  ggplot(aes(y = Openness)) +
  geom_boxplot()

personality_final |>
  ggplot(aes(y = age)) +
  geom_boxplot()

#' I was interested in determining if there was a mean difference in 
#' neuroticism between those of different marital stauts.

#' The plot below summarises the mean difference between those who 
#' have never been married, those who were once married, and
#' those who are currently married.

personality_final |>
  # Filter NAs for clearer plot
  filter(!is.na(married)) |>
  ggplot(aes(x = married, y = Neuroticism)) +
  geom_boxplot()

#' As the above plot shows, the mean neuroticism (*M* = 4.12, *SD* = 1.77) is highest
#' within those who are never married. The mean for those who are 
#' currently married (*M* = 3.47, *SD* = 1.65)
#' or previously married (*M* = 3.44, *SD* = 1.63) 
#' is similar.





