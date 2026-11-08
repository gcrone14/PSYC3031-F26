##### Mini-Activity 4 Solutions #####
#' We begin by setting up our environment
#' for the assignment.

# Load libraries
library(rstatix)
library(psych)
library(ez)
library(here)
library(tidyverse)

# Import data
worry <- read_csv(here::here("Data/worry.csv"))
glimpse(worry)

# Clean data (convert from wide to long,
# convert IDs to factor)
worry_final <- worry |>
  pivot_longer(cols = time1:time4,
               names_to = "time",
               values_to = "DEW_score") |>
  mutate(ID = factor(ID),
         time = factor(time,
                       levels = paste0("time", 1:4),
                       labels = c("Just married", "5 years",
                                  "10 years", "15 years"))
  )
glimpse(worry_final)

#' 1)
#' 
#' Do men tend to express worries
#' more (or less) to their wives
#' the longer they are married?
#' 
#' 
#' 2) 
#' 
#' 
#' Let's explore the differences
#' as a function of time.
worry_final |>
  select(DEW_score) |>
  describeBy(worry_final$time)

#' There seems to be a decrease
#' in men's desire to express their
#' worries to their wives as the
#' marriage goes on.
#' 
#' Let's plot this.
worry_final |>
  ggplot(aes(x = time, y = DEW_score)) +
  geom_boxplot() +
  labs(x = "Time", 
       y = "Desire to Express Worries") +
  theme_classic()

#' There does appear to be a slight
#' decrease over time, but it's
#' not particularly strong, and
#' there seems to be more variability
#' in responses as the marriage
#' goes on.
#' 
#' Perhaps a spaghetti plot will
#' further elucidate this?
worry_final |>
  ggplot(aes(x = time, y = DEW_score)) +
  geom_point(alpha = 0.2) +
  geom_line(aes(group = ID), alpha = 0.2) +
  stat_summary(geom = "point", fun = mean,
               color = "red", size = 3) +
  stat_summary(aes(group = 1),
               geom = "line", fun = mean,
               color = "red") +
  labs(x = "Timepoint", y = "Desire to Express Worries") +
  theme_classic()

#' The spaghetti plot affirms this
#' small decrease on average over
#' time, with some men showing an
#' increased desire to express
#' worries, and some showing
#' the opposite.
#' 
#' In APA:
#' 
#' As the length of marriages went on,
#' men tended to, on average, express
#' fewer worries to their wives as measured
#' by the DEW scale
#' (Just Married: *M* = 65.9, *SD* = 9.23;
#' 5 years: *M* = 65.4, *SD* = 10.69;
#' 10 years: *M* = 63.1, *SD* = 10.68;
#' 15 years: *M* = 61.93, *SD* = 12.57).
#' 
#' 3)
#' 
#' We assess normality and sphericity.
#' 
#' Note that the assumption of independence
#' is inherently violated, because the data are nested
#' (people are nested within repeated-measures), but
#' we can assume that individuals in the study were
#' randomly selected to take part.
#' 
#' Normality can be assessed with a Q-Q plot.
worry_final |>
  ggplot(aes(sample = DEW_score)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~time)

#' The data align with the
#' Q-Q plots well, so the assumption
#' of normality seems met.
#' (I wonder if the data were
#' simulated...)
#' 
#' Sphericity can be assessed using
#' McCauchly's test for sphericity.
res <- ezANOVA(data = worry_final,
               wid = ID,
               # New argument: within
               # specifies the within-subjects
               # variable, indicating that the
               # observations are DEPENDENT
               within = time,
               dv = DEW_score)

res$`Mauchly's Test for Sphericity`

#' The assumption of sphericity is,
#' however, violated, since the
#' Mauchly's test is statistically
#' significant.
#' 
#' Thus, we'll need to use the
#' sphericity-adjusted estimates
#' when interpreting the eventual
#' ANOVA.
res$`Sphericity Corrections`

#' Given that both epsilon
#' estimates are in different ranges,
#' we can't use our recommendation blindly.
#' 
#' Whichever correction you use is fine, so long
#' as you justify it.
#' 
#' I'll use the Greenhouse-Gaiser correction because
#' it's more conservative (so less likely to make
#' a Type I error). The epsilon value is 0.70, 
#' which is statistically significant, *p* < .0001.
#' 
#' Thus, the average DEW scores differ across at least
#' two time points.
#' 
#' 4) 
#' We already interpreted the epsilon values and their
#' *p*-values to account for the sphericity violation. Since
#' the ANOVA was technically statistically significant,
#' we can conduct pairwise tests.
pairwise_res <- pairwise_t_test(data = worry_final,
                                formula = DEW_score ~ time,
                                paired = TRUE,
                                p.adjust.method = "bonferroni",
                                detailed = TRUE) |> 
  select(-p) |>
  # organize output from highest-to-lowest differences
  arrange(desc(estimate))
pairwise_res |>
  select(estimate, group1, group2, p.adj,
         p.adj.signif)

# Double-check range of DEW_score
range(worry_final$DEW_score)

#' All mean differences were statistically significant,
#' except for the difference between 10 and 15 years,
#' and the difference between just married and 5 years.
#' 
#' However, it is crucial to note that these mean
#' differences are *very small*: the largest
#' difference is only 3.87, which is tiny given
#' that the DEW scale ranges from 35 to 91.
#' 
#' So even though several mean differences are
#' statistically significant, it's not very 
#' meaningful in practice. Practically, a ~4-unit
#' difference in DEW may mean almost nothing.
#' 

#' APA results:
#' 
#' Janice was interesting in determining
#' if the average man tended to express their
#' worries more (or less) the longer they are married,
#' so I (kindly) ran a repeated-measures ANOVA
#' (for her).
#' 
#' Descriptively, as the length of marriages went on,
#' men tended to, on average, express
#' fewer worries to their wives as measured
#' by the DEW scale
#' (Just Married: *M* = 65.9, *SD* = 9.23;
#' 5 years: *M* = 65.4, *SD* = 10.69;
#' 10 years: *M* = 63.1, *SD* = 10.68;
#' 15 years: *M* = 61.93, *SD* = 12.57).
#' 
#' Prior to running the repeated-measures ANOVA,
#' we checked both the assumptions of normality
#' and sphericity, assuming that the assumption
#' of random sampling was met. While the assumption
#' of normality was met (given high alignment of 
#' observed data to the Q-Q line for data on the
#' outcome variable at each time point), the
#' assumption of sphericity was not,
#' Mauchly's Test: W = 0.48, p = .001.
#' 
#' Thus, instead of using the traditional
#' ANOVA F-statistic, we must instead refer to the
#' sphericity-adjusted $\epsilon$ values.
#' 
#' We use the Greenhouse-Gaiser-adjusted $\epsilon$
#' because it is more conservative, and less likely
#' to result in a type I error.
#' 
#' The GG-adjusted $\epsilon$ was 0.70, 
#' which was statistically significant, *p* < .0001,
#' indicating that at least two time point had
#' a different mean DEW score.
#' 
#' Post-hoc tests (pairwise *t*-tests adjusted
#' with Bonferroni corrections) indicated that
#' most mean differences were statistically
#' significant, except for the differences between
#' 10 years and 15 years of marriage, and
#' those who are just married and married for 5 years.
#' 
#' Most mean differences, although significant,
#' were rather small: given the wide range of WES
#' scores, the largest mean difference was only 
#' 3.87 units, which was very small, and practically
#' nearly meaningless.
#' 
#' Thus, men on average tend to very slightly open
#' up to their wives regarding their desires to
#' express worries over time, though such a difference
#' may not be large enough to matter.