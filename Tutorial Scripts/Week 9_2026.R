# Week 9 - Repeated Measures ANOVA



### Note: for this tutorial we will be using TreatmentData.csv (simulated data).  



#' **Research scenario: A researcher was interested in seeing if a new medication is effective**
#' **for treating depression (measured on a 1-20 scale). The researcher evaluated changes**
#' **in depression in a sample of individuals diagnosed with depression over three times (i.e., time 1,**
#' **time 2, and time 3).**



# TOPIC 1 - Review



#' **STEP 1: Load the packages that you will need in that script into your current R session.**



#Loading packages
library(rstatix) ### Post-hoc tests
library(psych)
library(ez) ### ANOVA package
library(here)
library(tidyverse)



#' **STEP 2: load TreatmentData.csv into R. Save the data in an object called TreatmentData**



#Load the data
treatment_dat <- read_csv(file = here("Data", "TreatmentData.csv")) |>
  janitor::clean_names() |>
  rename("ID" = "id")


#' **STEP 3: check treatment_dat to ensure that the data has correctly been loaded into R**
glimpse(treatment_dat)
str(treatment_dat)

#' Long format: Each row is a unique observation on the DV
#' (1 row per time point per person)
#' Wide format: Each row is an observation AT a specific time
#' (1 row per person; 2 or more columns for time points)


### QUESTION: Is the data in long or wide format?



#' **STEP 4: clean treatment_dat**


#Changing the data to long format
treatment_long <- treatment_dat |> 
  pivot_longer(cols = c(time1, time2, time3),
               names_to = "time",
               values_to = "depress")


#Selecting the important variables and changing categorical variables/ID to factor 

### Note: we need to have an ID variable to conduct the ANOVA. The ID variable 
### needs to be a factor. 

treatment_final <- treatment_long |> 
  select(-age, -sex) |> # Omit age and sex
  mutate(ID = factor(ID),
         time = factor(time)) #Changing the structure of the variables

#' **STEP 5: Descriptive statistics and Visualizations**


# Generating numeric descriptive statistics for time 
summary(treatment_final$time)

# Generating numeric descriptive statistics for depress by time
treatment_final |>
  select(depress) |>
  describeBy(treatment_final$time)

# Generating a theme 
vic_theme <- theme(plot.title = element_text(family = "times", size = 12, face = "bold", hjust = 0.5),
                axis.title =  element_text(family = "times", size = 12),
                axis.text = element_text(family = "times", size = 10),
                panel.background = element_blank(),
                panel.border = element_rect(colour = "black", fill = NA),
                axis.text.x = element_text(),
                legend.text = element_text(family = "times", size = 12),
                legend.title = element_text(family = "times", size = 12, hjust = 0.5)
)

#Generating histograms for depress by time
treatment_final |>
  ggplot(aes(x = depress)) +
  geom_histogram(colour = "black", fill = "#E66100", bins = 20) + 
  labs(title = "Distribution of Depression Scores by Time Point", 
       x = "Depression Score",
       y = "Count") + 
  vic_theme + 
  facet_grid(~time)

# Boxplot of depression over time
treatment_final |>
  ggplot(aes(x = time, y = depress)) +
  geom_boxplot() +
  labs(x = "Timepoint", y = "Depression") +
  theme_classic()

# New plot! 
# Spaghetti plot
# Purpose: Show individual changes
# over time within each individual
# in the sample
treatment_final |>
  ggplot(aes(x = time, y = depress)) +
  geom_point(alpha = 0.2, width = 0.1) +
  geom_line(aes(group = ID), alpha = 0.2) +
  stat_summary(geom = "point", fun = mean,
               color = "red", size = 3) +
  stat_summary(aes(group = 1),
               geom = "line", fun = mean,
               color = "red") +
  labs(x = "Timepoint", y = "Depression") +
  theme_classic()

# TOPIC 2 - Repeated Measures ANOVA 

#'    * An inferential procedure to evaluate whether there is a difference among 
#'      three or more means.
#'    
#'    * The ANOVA indicates whether at least one of the means are different; however,
#'      the test provides no information regarding which means are different. Follow-up
#'      analyses are required to determine which means are different. 
#'
#' Variables: 
#'    * One categorical IV with 3+ levels/groups. These groups are not independent of each other.   
#'    * One continuous DV
#'    
#' Hypotheses:
#'    * H0: µ1 = µ2 = ... = µk = 0 (for all k levels of the IV,
#'    which is usually time)
#'    * H1: there is at least one mean difference 
#'    over time.

#' Q: Does the above look familiar? Why or why not?



#### Steps for completing a repeated-measures ANOVA in R ####

#' Step 1: Check assumptions
#' Step 2: Conduct the ANOVA
#'         (Use robust versions if assumptions are violated)
#' Step 3: If ANOVA is significant, perform post-hoc tests

# TOPIC 3 - Assumptions



#' Assumptions (that are evaluated through stats): 

#' * Normality: The distribution of scores is normal for each condition
#' * Sphericity (NEW): 
#' The variance of the DIFFERENCE SCORES are equal across group comparisons

### Normality
# Numeric descriptives
treatment_final |>
  select(depress) |>
  psych::describeBy(treatment_final$time)


# Histogram
treatment_final |>
  ggplot(aes(x = depress)) +
  geom_histogram(colour = "black", fill = "#E66100", bins = 20) + 
  labs(title = "Distribution of Depression Scores by Time Point", 
       x = "Depression Score",
       y = "Count") + 
  vic_theme + 
  facet_grid(~time)

# QQ-plot
treatment_final |>
  ggplot(aes(sample = depress)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~time)

# Boxplot 
treatment_final |>
  ggplot(aes(x = time, y = depress)) +
  geom_boxplot(fill = "light blue") +
  labs(x = "Timepoint", y = "Depression") +
  theme_classic()


### QUESTION: Is the assumption of normality met? 



### Sphericity


# Mauchly’s test

# Conducting ANOVA test (to access the Mauchly’s test)
res <- ezANOVA(data = treatment_final,
                   wid = ID,
                   # New argument: within
                   # specifies the within-subjects
                   # variable, indicating that the
                   # observations are DEPENDENT
                   within = time,
                   dv = depress)

#' Arguments:
#' 
#' * data: the name of the object containing the data 
#' * wid: the name of the ID column 
#' * within: the IV
#' * dv: the DV

#Generating Mauchly's Test
res$"Mauchly's Test for Sphericity"
# OR
res[2]

#' This test is analogous to Levene's test in terms
#' of interpreting whether the assumption is met.
#' 
#' That is,
#' H0: The variance of all of the difference score pairs are equal
#' (i.e., Sphericity is met)
#' H1: At least one pair of difference score variances
#' are not equal
#' (i.e., Sphericity is violated).

#### QUESTION: What conclusions would we draw based on this output? 

# TOPIC 4 - ANOVA



#' Note: we have already conducted the ANOVA and saved the results in the res object

# Viewing the results
res$'ANOVA'


#' Note: 
#' 
#' * Effect: the IV 
#' * DFn: degrees of freedom for the numerator
#' * DFd: degrees of freedom for the denominator
#' * F: the statistic (which for an ANOVA is the F statistic)
#' * p: the p-value 
#' * p<.05: indicates whether the p-value is less than 0.05 with an asterisks
#' * ges: generalized eta-square (effect size)



#### QUESTION: What conclusions would we draw based on this output? 



# TOPIC 5 - Follow-up analyses



## Conducting pairwise tests
pairwise_res <- pairwise_t_test(data = treatment_final,
                          formula = depress ~ time,
                          paired = TRUE,
                          p.adjust.method = "bonferroni",
                          detailed = TRUE) |> 
  select(-p)
pairwise_res


#' Purpose: 
#' 
#' * Essentially, conducts a dependent samples t-test for each pair-wise comparison
#' * Provides the adjusted p-values (due to inflated type I error rates from multiple comparisons) 
#' 
#' Arguments:
#' 
#' * data: the data object
#' * fomula: outcome ~ IV
#' * paired: if the observations are not independent (TRUE)
#' * p.adjust.method: the multiplicity control method 
#' * detailed: if an expanded version of the output should be generated (TRUE)
#' 
#' Output: 
#' 
#' * .y.: the outcome variable 
#' * group1/group2: the levels of the IV being compared
#' * n1/n2: the sample size of group1/group2
#' * statistic: the t-statistic 
#' * df: the degrees of freedom
#' * conf.low/conf.high: the lower and upper bound of the CI
#' * method: the statistical test  
#' * alternative: the alternative hypothesis
#' * p.adj: the adjusted p-value using the Bonferroni method 
#' * p.adj.signif: an indicator of whether the adjusted p-value is statistically significant



#### QUESTION: What conclusions would we draw based on this output? 



# TOPIC 6 - Alternatives when assumptions are violated



## Normality violation - Friedman Rank Sum Test

#Friedman Rank Sum Test
friedman.test(formula = depress~time|ID,
              data = treatment_final)

#' Arguments:
#' 
#' * data: the data object
#' * fomula: outcome ~ IV|ID
#' 
#' Output: 
#' 
#' * Friedman chi-squared: the statistic  
#' * df: the degrees of freedom
#' * p-value: the p-value



#### QUESTION: What conclusions would we draw based on this output? 



## Sphericity violation - Friedman Rank Sum Test (again!)

#Sphericity corrections
res$'Sphericity Corrections'


#' Output:
#' 
#' * Effect: IV
#' * GGe: epsilon estimate based on Greenhouse-Geisser
#' (equivalent to the test-statistic)
#' * p[GGe]: adjusted p-value 
#' * p[GG]<.05: whether the adjusted p-value is statistically significant
#' * HFe: epsilon estimate based on Huynh-Feldt
#' (another test statistic based on a different adjustment)
#' * p[HF]: adjusted p-value
#' * p[HF]<.05: whether the adjusted p-value is statistically significant

#' NOTE: The GGe estimate tends to be more CONSERVATIVE
#' than the HF. That is, GGE results in rejecting H0 LESS often
#' than the HF. (HF --> more power, but more Type I error)

#### QUESTION: What p-value should you look at?

#' Recommendation: 
#' 
#' GGe - when epsilon estimate is ≤ .75 (because this correction is more conservative)
#' HFe - when epsilon estimate is .75 < epsilon < 1 (because this correction is more liberal)



#### QUESTION: What df should be reported in an APA summary? 




#ANOVA output
res$'ANOVA'

#DFn 
res$'ANOVA'[[2]]

#DFd
res$'ANOVA'[[3]]


# TOPIC 7 - Practice Questions 

#' Research Scenario: A researcher was interested in investigating how coffee intake throughout the day
#' impacts individuals energy levels. To test this, the researcher gathered a sample of 200 young adults.
#' Each adult was asked to record their energy levels (on a scale from 1-20) when they wake up (i.e., no
#' coffee) and after their first coffee, second coffee, and third coffee. Conduct a repeated measures ANOVA 
#' using the CoffeeData.csv. Provide a brief summary of the results. 
