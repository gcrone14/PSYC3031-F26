# Week 8 - Two-Way Independent Samples ANOVA
#' BRAINSTORM:
#' Which ideas/topics do you remember from last tutorial?

#' 1.
#' 2. 
#' 3. 
#' 4. 
#' ...


#' Which topics do you feel could use some additional coverage
#' or a quick refresher before diving in today?

#' 1. 
#' 2. 
#' 3. 
#' 4. 
#' ...




### Note: for this tutorial we will be using job_dat.csv.  



### Research question: Does the relationship between hourly wage (hrlyearn) and 
### education (EDUC) vary by biological sex?



# TOPIC 1 - Review
#' **STEP 1: Load the packages that you will need in that script into your current R session.**

# Loading packages
library(psych)
library(here)
library(ez) # New package to run ANOVAs
library(janitor) # New package to clean up column names
library(ggridges)
library(WRS2)
library(tidyverse)

#' **STEP 2: load JobData.csv into R. Save the data in an object called job_dat**

# Load the data
job_dat <- read.csv(file = here("Data", "JobData.csv"))

#' **STEP 3: check job_dat to ensure that the data has correctly been loaded into R**

# Checking job_dat
str(job_dat)
glimpse(job_dat)

#' This data set is massive, so we'll only select the columns
#' we care about.

#' **STEP 4: clean job_dat**

# Completing the data cleaning tasks in one chunk of code

### Note: we need to have an ID variable to conduct the ANOVA. The ID variable 
### needs to be a factor. 

job_dat_final <- job_dat |>
  janitor::clean_names() |> # Converts names to snake_case
  mutate(ID = factor(x),
         educ = factor(educ,
                       levels = 0:6,
                       labels = c("0 to 8 years","Some high school", "High school graduate",
                                  "Some postsecondary", "Postsecondary certificate or diploma",
                                  "Bachelor's degree", "Above bachelor's degree")),
         sex = factor(sex,
                      levels = 1:2,
                      labels = c("Male", "Female"))) |>
  # Remove participants with missing data on hrlyearn
  filter(!is.na(hrlyearn)) |>
  # Re-name sex to gender (see disclaimer)
  rename("gender" = "sex") |>
  # Re-organize column names (my preference!)
  select(ID, hrlyearn, educ, gender) # We add in gender here!

#' Disclaimer: For simplicity, we'll assume that
#' the sex reported corresponds with participants'
#' actual experienced gender identities, 
#' so we'll refer to this variable
#' as "gender". Note, though, that you should NEVER make this
#' assumption; sex and gender are very different constructs.
#' 
#' In your own surveys, you should use
#' more inclusive categories to specify one's identified
#' gender.

#' You may wish to re-inspect the data at this point.
glimpse(job_dat_final)

#' **STEP 5: Descriptive statistics and Visualizations**

# Generate numeric descriptive statistics for educ 
summary(job_dat_final$educ)

# Generate numeric descriptive stats for gender
summary(job_dat_final$gender)

# Are there a different number of men to women across
# levels of education?
job_dat_final |>
  count(educ, gender, sort = TRUE)

# Generating numeric descriptive statistics for hrlyearn 
job_dat_final |>
  select(hrlyearn) |>
  describe()

# Hourly earnings by education level
job_dat_final |>
  select(hrlyearn) |>
  describeBy(job_dat_final$educ)

# Hourly earnings by gender
job_dat_final |>
  select(hrlyearn) |>
  describeBy(job_dat_final$gender)

# Hourly earning means, medians,
# and SD for both factors
job_dat_final |>
  group_by(educ, gender) |>
  summarise(mean_earn = mean(hrlyearn),
            sd_earn = sd(hrlyearn),
            median_earn = median(hrlyearn)
            )

#' Q: What can we learn from the data based on
#' the above?
#' 
#' 
#' Q: What do you predict the ANOVA will show?

#' Let's make some more plots to concretize this
#' further...

#' I call the theme Vic_theme since Victoria made it.

# Save a theme
Vic_theme <- theme(plot.title = element_text(size = 12, 
                                             face = "bold", hjust = 0.5),
                   axis.title =  element_text(size = 12),
                   axis.text = element_text(size = 10),
                   panel.background = element_blank(),
                   panel.border = element_rect(colour = "black", fill = NA),
                   axis.text.x = element_text(),
                   legend.position = "none"
)

# Generating bar plot for frequencies of each group of gender
job_dat_final |>
  ggplot(aes(x = gender)) +
  geom_bar(fill = "royalblue") +
  Vic_theme +
  labs(x = "Gender", y = "Freq")

# Generate FREQUENCY plot for gender by level of education
# (This only shows frequency differences, NOT
# the outcome we care about!)
job_dat_final |>
  count(educ, gender) |>
  ggplot(aes(x = n, y = educ, fill = gender)) +
  geom_col(position = "dodge") +
  theme_classic() +
  labs(x = "Freq", y = "Education") +
  scale_fill_manual(values = c("royalblue", "pink"))


# Generating histogram
hrlyearn_hist <- job_dat_final |>
  ggplot(aes(x = hrlyearn)) +
  geom_histogram(colour = "black", fill = "royalblue", bins = 30) + 
  labs(title = "Distribution of the Hourly Wages of Employees", 
       x = "Hourly Wages of Employees ($)",
       y = "Count") + 
  Vic_theme

# Does the distribution differ by gender?
hrlyearn_hist + facet_wrap(~gender)

# By education level?
hrlyearn_hist + facet_wrap(~educ)

#' Remember this?
job_dat_final |>
  ggplot(aes(x = hrlyearn, y = educ, fill = educ)) +
  geom_boxplot() + 
  labs(title = "The Distribution of Hourly Wages by Education Level",
       x = "Hourly Wage ($)",
       y = "Education Level") + 
  scale_fill_manual(values = c("#803E75", "#0057E9", "#FFB300", 
                               "#8FBC8F", "#E66100", "#2E2E2E", 
                               "#DC267F")) +
  Vic_theme

#' We can break it up by gender by adding a 
#' fill aesthetic:
job_dat_final |>
  ggplot(aes(x = hrlyearn, y = educ, fill = gender)) +
  geom_boxplot() + 
  labs(title = "The Distribution of Hourly Wages by Education Level",
       x = "Hourly Wage ($)",
       y = "Education Level") + 
  scale_fill_manual(values = c("royalblue", "pink")) +
  Vic_theme

#' Q: What can we conclude from the above graphs?
#' regarding a gender difference in hourly earnings
#' by education level?
#' 
#' Q: Do you think the ANOVA will be significant?
#' Why or why not?

# TOPIC 2 - Two-Way Independent Samples ANOVA

#' Purpose:
#'    * To test if an INTERACTION exists b/w the two factors
#'    
#'    * Interaction: the magnitude/direction of the effect 
#'      of one IV on the DV is
#'      dependent on the levels of another IV.
#'    
#'    * Same as before: ANOVA tells you whether any groups are different,
#'      not WHICH groups are (need post-hoc tests).
#'
#' Variables: 
#'    * Two categorical IV with 2+ levels/groups. 
#'    These groups must be independent.
#'    * One continuous DV
#'    
#' Hypotheses:
#'    * H0-1 (main effect 1): 
#'      µ1 = µ2 = ... = µk (for all means 1, 2, .., k OF FACTOR 1)
#'    * H0-2 (main effect 2): 
#'      µ1 = µ2 = ... = µj (for all means 1, 2, .., j OF FACTOR 2)
#'    * H0-3 (interaction): All group means are equal.
#'      

#### Steps for completing an ANOVA in R ####

#' Step 1: Check assumptions.
#' Step 2: Conduct the ANOVA. (Use robust stats if assumptions are violated.)
#' Step 3: If ANOVA shows significance, run post-hoc tests.

# TOPIC 3 - Assumptions



#' 
#' *The scores on the DV are normally distributed in each population (normality)*
#' *The variance of the scores on the DV are equal across all the populations (homogeneity of variance)*
#' 
#' * The scores on the DV are independent of each other (independence)
#' * The scores represent a random sample from the population


### Normality ###


#Note: the other outputs for checking normality were generated in the descriptive statistics
#      and visualization section.

# QQ-plot (another useful plot!)

#' Start with basic version:
job_dat_final |>
  ggplot(aes(sample = hrlyearn)) +
  geom_qq_line(colour = "dark red") +
  geom_qq(colour = "dark blue") +
  labs(title = "QQ Plots", 
       x = "", y = "") + 
  Vic_theme

#' Then we make a new version for each 
#' gender and education group
job_dat_final |>
  ggplot(aes(sample = hrlyearn)) +
  geom_qq_line(colour = "dark red") +
  geom_qq(colour = "dark blue") +
  labs(title = "QQ Plots", 
       x = "", y = "") + 
  Vic_theme +
  facet_grid(gender ~ educ)

#' Code:
#' 
#' * sample: the variable you want to evaluate the normality assumption. 
#' * geom_qq_line: geom to draw the qq plot line
#' * geom_qq: geom to draw the qq plot points
#' * facet_grid: to create the grid layout of the graph



#### Question: is the assumption of normality met? 


### Homogeneity of variance ###



# Conducting ANOVA test (to access the Levene's Test)
res <- ezANOVA(job_dat_final,
                   wid = ID,
                   # Same as before,
                   # Now specify two factors
                   # with fct1:fct2:
                   between = educ:gender,
                   dv = hrlyearn,
                   return_aov = TRUE)

#Generating Levene's Test 
res$"Levene's Test for Homogeneity of Variance"
# OR
res[2]

#' Arguments:
#' 
#' * data: the name of the object containing the data 
#' * wid: the name of the ID column 
#' * between: the IV's
#' * dv: the DV
#' * return_aov: the output is an aov object


#### QUESTION: is the assumption of homogeneity of variance met? 



# TOPIC 4 - ANOVA



#' Note: we have already conducted the ANOVA and saved the res in the res object

#Viewing the ANOVA res
res$'ANOVA'
# OR
res[1]



#' Note: 
#' 
#' * Effect: main effect (IV1), main effect (IV2) and the interaction effect 
#' * DFn: degrees of freedom for the numerator
#' * DFd: degrees of freedom for the denominator
#' * F: the statistic (which for an ANOVA is the F statistic)
#' * p: the p-value 
#' * p<.05: indicates whether the p-value is less than 0.05 with an asterisks
#' * ges: generalized eta-square (effect size) 





#### QUESTION: What is your conclusion? 








#' The interaction is statistically significant. Therefore, the relationship between
#' hourly earnings and education level is influenced by gender.  



#### QUESTION: Why are these res unsurprising (hint: think about the sample size)?





# Creating a plot 
job_dat_final |>
  ggplot(aes(x = educ, y = hrlyearn, colour = gender, group = gender)) +
  stat_summary(fun = mean, geom = "point") + 
  stat_summary(fun = mean, geom = "line") +
  labs(title = "Mean Hourly Earnings by Education and Gender",
       x = "Education Level",
       y = "Mean Hourly Earnings ($)",
       colour = "Gender") + 
  scale_color_manual(values = c("royalblue", "pink")) +
  Vic_theme



# TOPIC 5 - Follow-up analyses




# Create data subsets
job_dat_male <- job_dat_final |> filter(gender == "Male")
job_dat_female <- job_dat_final |> filter(gender == "Female")

# Note: It does not matter which IV you use to create the data subsets. Use the variable 
# with the less levels. 

### Q: How can I check this code?




# Create ANOVA objects (compatible with the TukeyHSD function)

## Males
mod_1 <- aov(hrlyearn ~ educ*gender,
                data = job_dat_final)


# Conducting tests
# All tests
TukeyHSD(x = mod_1)

# Tests for main effect of education
TukeyHSD(x = mod_1)[["educ"]]

# Tests for main effect of gender
TukeyHSD(x = mod_1)[["gender"]]

# Test for interaction effect
TukeyHSD(x = mod_1)[["educ:gender"]]

#' Purpose: 
#' 
#' * Essentially, conducts an independent samples t-test for 
#' each pair-wise comparison
#' * Provides the adjusted p-values (due to inflated type I error rates from multiple comparisons) 
#' 
#' Output: 
#' 
#' * diff: the mean difference between the groups (the groups being compared are indicated on the right)
#' * lwr and upr: the lower and upper bounds of the 95% confidence interval 
#' * p adj: the adjusted p-value

# TOPIC 6 - Alternatives when assumptions are violated


### Violation of Normality and/or Homogeneity of Variance Assumptions



# Conducting a two way independent samples ANOVA with trimmed means 
t2way(formula = hrlyearn ~ educ * gender,
      data = job_dat_final,
      tr = .1)

#' Purpose: 
#' 
#' * Conducts a two way independent samples ANOVA with trimmed means 
#'   (based on the Welch test) 
#' * Trimmed mean: computing the mean without a certain proportion of 
#'   extreme observations (in this case, 0.1)
#' * Welch test: appropriate when the homogeneity of variance 
#'   assumption is not met
#' 
#' Code: 
#' 
#' formula: The ANOVA model (outcome variable ~ IV1 * IV2)
#' data: the data
#' tr: the proportion of extreme observations to exclude
#' 
#' Output: 
#' 
#' * value: the value of the statistic 
#' * p.value: the p-value




# TOPIC 7 - Practice Questions


#' Research scenario: a researcher is 
#' interested in determining whether the effect of studying (high and low)
#' on test performance (ranging from 0 to 100) is impacted by the 
#' amount of sleep a student gets (1-3hrs, 4-6, 7+). 
#' Conduct a factorial ANOVA using PerformanceData2.csv.
#' 
#' Provide a summary of the res. For this practice scenario 
#' use an alpha of 0.05. 
