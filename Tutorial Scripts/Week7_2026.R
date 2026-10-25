# Week 7 - One-way ANOVA

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



### Research question: We are interested in investigating the 
# relationship between 
### hourly wage (hrlyearn) and education (educ). 



# TOPIC 1 - Review



#' **QUESTION: What are the steps for performing analyses in R (so far)** 














#' **STEP 1: Load the packages that you will need in that script into your current R session.**



# Loading packages
library(psych)
library(here)
library(ez) # New package to run ANOVAs
library(janitor) # New package to clean up column names
library(ggridges)
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
  select(X, HRLYEARN, EDUC) |>
  janitor::clean_names() |> # Converts names to snake_case
  mutate(ID = factor(x),
         educ = factor(educ,
                         levels = 0:6,
                         labels = c("0 to 8 years","Some high school", "High school graduate",
                                    "Some postsecondary", "Postsecondary certificate or diploma",
                                    "Bachelor's degree", "Above bachelor's degree"))) |>
  # Remove participants with missing data on hrlyearn
  filter(!is.na(hrlyearn)) |>
  # Re-organize column names (my preference!)
  select(ID, hrlyearn, educ)

#' You may wish to re-inspect the data at this point.
glimpse(job_dat_final)

#' **STEP 5: Descriptive statistics and Visualizations**

# Generate numeric descriptive statistics for educ 
summary(job_dat_final$educ)

# Generating numeric descriptive statistics for hrlyearn 
job_dat_final |>
  select(hrlyearn) |>
  describe()

job_dat_final |>
  select(hrlyearn) |>
  describeBy(job_dat_final$educ)

# Generating a histogram for hrlyearn

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

# Generating histogram
job_dat_final |>
  ggplot(aes(x = hrlyearn)) +
  geom_histogram(colour = "black", fill = "royalblue", bins = 30) + 
  labs(title = "Distribution of the Hourly Wages of Employees", 
       x = "Hourly Wages of Employees ($)",
       y = "Count") + 
  Vic_theme

# Generate a boxplot for hrlyearn by educ
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



#' QUESTION: What do we think the results of the ANOVA 
#' will tell be given the descriptive statistics?
#' (Will the group means be statistically significantly
#' different?)
job_dat_final |>
  select(hrlyearn) |>
  describeBy(job_dat_final$educ)


# TOPIC 2 - One-Way ANOVA Introduction



#' Purpose:
#'    * An inferential procedure to evaluate whether there is a 
#'    difference among three or more means.
#'    
#'    * ANOVA only tells you that at least two means differ;
#'      cannot tell you WHICH means differ.
#'
#' Variables: 
#'    * One categorical IV with 3+ levels/groups. 
#'      These groups are independent of each other.
#'    * One continuous DV
#'    
#' Hypotheses:
#'    * H0: µ1 = µ2 = ... = µk (for all means 1, 2, .., k)
#'    * H1: At least one pair of means differs.


#### Steps for completing an ANOVA in R ####

#' *Step 1*: Check if the assumptions of the inferential procedure are 
#' reasonably met.
#' 
#' *Step 2*: Conduct the ANOVA.
#' Use robust versions of the ANOVA if assumptions are violated.
#' 
#' *Step 2a*: If the ANOVA returns a statistically significant result,
#' complete follow-up analyses (e.g., post-hoc tests, linear contrasts).

# TOPIC 3 - Assumptions

#' Assumptions: 
#' *1) Normality:*
#' The scores on the DV are normally distributed for each level of the IV.
#' 
#' *2) Homogeneity of variance:*
#' The variance of the scores on the DV are equal across the levels of the IV.
#' 
#' *3) Independence:*
#' The scores on the DV are independent of each other.
#' 
#' *4) Random Sampling:*
#' The scores represent a random sample from the population.


### Normality ###


# Numeric descriptives
job_dat_final |>
  select(hrlyearn) |>
  psych::describeBy(job_dat_final$educ)

# Visualizations

#' PLEASE only choose 1 visualization
#' to help you interpret normality.
#' 
#' I find the density plot the most
#' intuitive, but choose the one you
#' like most.
#' 
#' Do not use all three (unless all three
#' are equally as informative.)

# Boxplot (from before)
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

#' I prefer a nice facetted density plot
# Faceted Density plot
job_dat_final |>
  ggplot(aes(x = hrlyearn, fill = educ)) +
  geom_density(alpha = 0.3, adjust = 3) + 
  labs(title = "The Distribution of Hourly Wages by Education Level",
       x = "Hourly Wage ($)",
       y = "Education Level") + 
  scale_fill_manual(values = c("#803E75", "#0057E9", "#FFB300", 
                               "#8FBC8F", "#E66100", "#2E2E2E", 
                               "#DC267F")) +
  facet_wrap(~educ) +
  Vic_theme


#### QUESTION: What conclusions would we draw based on this output? 



### Homogeneity of variance ###

# Conducting ANOVA test (to access the Levene's Test)
res <- ezANOVA(data = job_dat_final,
                   wid = ID,
                   between = educ,
                   dv = hrlyearn,
                   return_aov = TRUE)

#' Arguments:
#' 
#' * data: the name of the object containing the data 
#' * wid: the name of the ID column 
#' * between: the IV
#' * dv: the DV
#' * return_aov: the output is an aov object

# Generating Levene's Test 
res$"Levene's Test for Homogeneity of Variance"

#### QUESTION: What conclusions would we draw based on this output? 



#' Note: 
#' H0: All population variances are equal.
#' H1: At least one population variance differs.
#' 
#' Thus:
#' * p <= alpha --> reject H0 --> Variances are NOT equal.
#' * p > alpha: --> fail to reject H0 
#'   --> variances ARE equal

# TOPIC 4 - ANOVA

#' Note: we have already conducted the ANOVA 
#' and saved the results in the res object.

#Viewing the ANOVA results
res$'ANOVA'

#' Breakdown of output:
#' 
#' * Effect: the IV 
#' * DFn: degrees of freedom for the numerator 
#'   (num_groups - 1)
#' * DFd: degrees of freedom for the denominator 
#'   (N - num_groups)
#' * F: F-statistic of the ANOVA
#' * p: the p-value 
#' * p<.05: indicates whether the p-value is less than 0.05 with an asterisk
#' * ges: generalized eta-square (effect size) 

#' In the above, p < .00001 (extremely small, so R
#' approximates it to be 0.)

#' QUESTION: What conclusions would we draw 
#' based on this output?

#' QUESTION: Why are these results unsurprising 
#' (hint: think about the sample size)?

# TOPIC 5 - Follow-up analyses

# Conducting Tukey's post-hoc tests 
TukeyHSD(x = res$aov)

#' Purpose: 
#' 
#' * Essentially, conducts an independent 
#'   samples t-test for each pair-wise comparison
#' * Provides the adjusted p-values 
#'   (due to inflated type I error rates from multiple comparisons) 
#' 
#' Output: 
#' 
#' * diff: the mean difference between the groups 
#'   (the groups being compared are indicated on the right)
#' * lwr and upr: the lower and upper bounds of the 
#'   95% confidence interval 
#' * p adj: the adjusted p-value

# TOPIC 6 - Alternatives when assumptions are violated

### Violation of Homogeneity of Variance

# Conducting a Welch Test
oneway.test(formula = hrlyearn ~ educ,
            data = job_dat_final,
            var.equal = FALSE)

#' The Welch test is appropriate when the assumption of homogeneity of variance 
#' is violated because it does not use a pooled variance.
#' 
#' Arguments:
#' 
#' * formula: DV ~ IV 
#' * data: the name of the object containing the data
#' * var.equal: an argument asking you to specify 
#'   whether the homogeneity of variance assumption is met.
#'   If the assumption is not met, the Welch Test is used.
#' 
#' Output: 
#' 
#' * F: The statistic
#' * num df: The degrees of freedom for the numerator
#' * denom df: The degrees of freedom for the denominator
#' * p-value: the p-value

### Violation of Normality Assumption

#Conducting a Kruskal-Wallis Rank Sum Test
kruskal.test(formula = hrlyearn ~ educ,
             data = job_dat_final)

#' The Kruskal-Wallis Rank Sum Test is appropriate 
#' when the assumption of normality is violated because
#' it is based on ranked data. 
#' 
#' * Note: only appropriate when the type of non-normality is the same across groups AND
#'   the assumption of normality is violated.
#'   
#' Arguments:
#' 
#' * formula: DV ~ IV 
#' * data: the name of the object containing the data
#' 
#' Output: 
#' 
#' * Kruskal-Wallis chi-squared: The statistic
#' * df: The degrees of freedom 
#' * p-value: the p-value


### Violation of the homogeneity of variance and normality assumptions

# Welch test on the ranks
pairwise.t.test(rank(job_dat_final$hrlyearn), 
                job_dat_final$educ,
                pool.sd=FALSE, 
                p.adjust="fdr")

#' Conducting the Welch test on the ranks
#'   
#' Arguments:
#' 
#' * rank(): computing the ranks for the DV
#' * pool.sd: an argument asking you to indicate whether 
#' the pooled SD should be computed. (the pooled SD should 
#' only be computed when the assumption of homogeneity of variance is met.)
#' * p.adjust: the multiplicity control method that will be used to adjust the p-values
#' 
#' Output: 
#' 
#' * Provides you a matrix of p-values for the different pairwise comparisons


# TOPIC 3 - Practice Questions (please do this LIVE so we may
# take it up and discuss.)


#' 1. Install the cereal 3.csv data into a new R script. You may
#' call this script whatever you wish (e.g., Week7_practice.R). Then,
#' save the cereal data as cereal_dat.
#' 
#' 2. Perform the following data cleaning steps and save the result into 
#' a new data frame called cereal_final. The steps are as follows:
#' 
#' 2a. Select the name, shelf and 
#' rating variables from revised_cereal_dat.
#' 
#' 2b. Change the structure of the shelf variable to a factor 
#' and relabel the levels so that: 
#'   * 1 = Bottom Shelf 
#'   * 2 = Middle Shelf 
#'   * 3 = Top Shelf
#' 
#' 2c. Change the structure of the name variable to a factor.
#' 
#' 3. Go through the different steps of a One Way ANOVA to determine whether there 
#'    is a difference in the average rating of cereals placed on the bottom, middle, 
#'    and top shelf. Write a brief APA summary of the results.
