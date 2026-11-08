##### Week 8 Exercise Solutions ######
# Load libraries
library(psych)
library(here)
library(ez)
library(janitor)
library(ggridges)
library(WRS2)
library(tidyverse)

#' As a reminder:
#' Research scenario: a researcher is 
#' interested in determining whether the effect of studying (high and low)
#' on test performance (ranging from 0 to 100) is impacted by the 
#' amount of sleep a student gets (1-3hrs, 4-6, 7+). 
#' Conduct a factorial ANOVA using PerformanceData2.csv.
#' 
#' Provide a summary of the res. For this practice scenario 
#' use an alpha of 0.05. 
#' 
#' We begin by importing the data
perf_dat <- read_csv(here::here("Data/PerformanceData2.csv"))
glimpse(perf_dat)

#' Let's clean this data up a little bit
perf_final <- perf_dat |>
  janitor::clean_names() |>
  rename("ID" = "x1") |>
  mutate(ID = as.factor(ID),
         study_hours = factor(study_hours,
                              levels = c("Low", "High")),
         sleep_hours = factor(sleep_hours,
                              levels = c("1-3", "4-6", "7+")))
glimpse(perf_final)

#' And we'll explore the data prior to running the ANOVA
#' to get a sense of what's going on
#' 
#' Start with numeric descriptives

# Describe overall scores
perf_final |> select(scores) |> psych::describe()

# Frequencies of study_hours, sleep_hours, and both
table(perf_final$study_hours)
table(perf_final$sleep_hours)
table(perf_final$study_hours, perf_final$sleep_hours)

#' This data is 100% manipulated or fabricated; 
#' you never see even groups in real life like this!

# Descriptive stats of scores across both factors
perf_final |>
  group_by(study_hours, sleep_hours) |>
  summarise(mean_scores = mean(scores),
            sd_scores = sd(scores),
            median_scores = median(scores))

#' There appears to be a higher mean score with more
#' sleep hours, and with more studying.
#' 
#' Let's visualize this with a grouped boxplot.
perf_final |>
  ggplot(aes(x = scores, y = sleep_hours, fill = study_hours)) +
  geom_boxplot() + 
  labs(title = "The Distribution of Scores by study and sleep hours",
       x = "Exam Scores",
       y = "# of Hours Slept per Night") + 
  scale_fill_manual(values = c("red", "green")) +
  theme_classic()

#' This re-iterates the finding from earlier,
#' but suggests an interaction going on:
#' The more hours slept per night, the stronger
#' the effect of study hours on scores.
#' 
#' Now let's test our assumptions and 
#' run the two-way ANOVA.
#' 
#' 1) Normality
perf_final |>
  ggplot(aes(sample = scores)) +
  geom_qq() +
  geom_qq_line() +
  theme_classic() +
  facet_grid(sleep_hours ~ study_hours)

#' Normality appears to be met.

#' 2) Homogeneity of variance
res <- ezANOVA(perf_final,
               wid = ID,
               # Same as before,
               # Now specify two factors
               # with fct1:fct2:
               between = sleep_hours:study_hours,
               dv = scores,
               return_aov = TRUE)

res$`Levene's Test for Homogeneity of Variance`

#' The assumption also appears to be met.
#' 
#' (I can conclude that these data were
#' simulated; it's too perfect!)
#' 
#' Now we run and summarise the ANOVA.
#' 
res$ANOVA

#' There is a statistically significant 
#' interaction.
#' 
#' Run post-hoc tests:
mod_1 <- aov(scores ~ sleep_hours*study_hours,
             data = perf_final)
TukeyHSD(mod_1)[["sleep_hours:study_hours"]]

#' The output is a lot more manageable than
#' it was prior.
#' 
#' The various mean differences suggest that
#' there is an interaction: within 
#' each sleep group, there was a mean difference
#' based on hours, and it was stronger
#' the more studying that occured
#' (and vice versa).
#' 
#' We summarise the ANOVA in APA format
#' as follows:
#' 
#' (For brevity, I don't include this, but
#' you should also summarise what you found
#' in the descriptive analysis in terms
#' of means and SDs across all of the
#' different level combinations, e.g.,
#' mean score for 7+ sleep hours and low study,
#' mean score for 4-6 sleep hours and low study,
#' etc...)
#' 
#' To determine whether the average daily sleep
#' hours and the amount of studying (high vs. low)
#' affected the acquired exam scores,
#' I ran a two-way ANOVA. 
#' 
#' The statistical assumptions of normality,
#' homogeneity of variance, independence, and
#' random sampling were met.
#' 
#' The ANOVA suggested that 
#' there was a statistically significant interaction
#' between the number of hours of sleep and the
#' extent to which students studied, 
#' *F*(2, 294) = 9.92, p < .001. Tukey's post-hoc
#' tests suggested that the more hours 
#' students spent studying, the better their
#' scores improved, and these improvements were
#' stronger when they also studied to a high
#' degree.
#' 
#' (You may now include any post-hoc test differences
#' of interest, or provide it as a table. Remember
#' to just highlight enough statistically significant
#' mean differences to get the point across regarding
#' which groups differ!)