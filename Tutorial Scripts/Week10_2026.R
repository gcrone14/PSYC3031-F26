# Week 11 - Correlation and Simple Linear Regression 

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


### Note: for this tutorial we will be using JobData.csv.  



#' Research question (correlation): 
#' Is their a relationship between hourly wage and 
#' the number of weeks an individual is absent from work?

#' Phrased differently:
#' Is hourly wage a PREDICTOR of the number of weeks 
#' an individual is absent from work?


# TOPIC 1 - Review
#' Quiz time!
#' 1) 
#' 2)
#' 3)
#' 4)
#' 5)
#' 6)


#' **STEP 1: Load the packages that you will need in that script into your **
#' **current R session.**



# Loading packages
library(psych)
library(here)
library(QuantPsyc) # Helps get standardized regression coefficients
library(broom) # Helps to find CIs
library(ggiraphExtra) # Visualizes model
library(tidyverse)

#' **STEP 2: load JobData.csv into R. Save the data in an object called JobData**

# Load the data
job_dat <- read_csv(file = here("Data/JobData.csv"))

#' **STEP 3: check job_dat to ensure that the data has correctly been loaded into R**
glimpse(job_dat)
str(job_dat)

#' How many people have missing data on wksaway?
is.na(job_dat$WKSAWAY) |> mean()
#' A whopping 95% of people have data missing
#' on the # of weeks they're away. I wonder
#' why... :)
#' 
#' What about hourly earnings
is.na(job_dat$HRLYEARN) |> mean()
#' Around 50%

#' **STEP 4: clean job_dat**
# Clean data
job_dat_final <- job_dat |>
  dplyr::select(HRLYEARN, WKSAWAY) |>
  janitor::clean_names() |> # Converts names to snake_case
  # Remove participants with missing data on hrlyearn
  # AND those with missing data on number of week missing
  filter(!is.na(hrlyearn) & !is.na(wksaway))

#' **STEP 5: descriptive statistics and visuaializations**

# Descriptive statistics
describe(job_dat_final)

# Plots
# Generating a theme 
Vic_theme <- theme(plot.title = element_text(family = "Times", size = 12, face = "bold", hjust = 0.5),
                axis.title =  element_text(family = "Times", size = 12),
                axis.text = element_text(family = "Times", size = 10),
                panel.background = element_blank(),
                panel.border = element_rect(colour = "black", fill = NA),
                axis.text.x = element_text(),
                legend.position = "none"
)

# Histogram of hourly earnings
job_dat_final |>
  ggplot(aes(x = hrlyearn)) + 
  geom_histogram(colour = "black", fill = "#8FBC8F", bins = 20) + 
  labs(title = "Distribution of Hourly Wage", x = "Hourly Earnings ($/Hour)", y = "Count") + 
  Vic_theme

# Histogram of weeks away
job_dat_final |>
  ggplot(aes(x = wksaway)) + 
  geom_histogram(colour = "black", fill = "dark red",
                 bins = 30) + 
  labs(title = "Distribution of Absences from Work", x = "Absenses from Work (Weeks)", y = "Count") + 
  Vic_theme

#' Scatterplot with regression line
#' (we've seen this before!)
job_dat_final |>
  ggplot(aes(x = wksaway, y = hrlyearn)) + 
  geom_point() + 
  # Makes regression line of best fit
  geom_smooth(method = lm, formula = y~x) +
  labs(title = "# of Weeks away vs. Hourly Earnings",
       x = "# Weeks Away", y = "Hourly Earnings") + 
  Vic_theme

#' Q: What can we conclude from the above plots?

# TOPIC 2 - Correlation

#' Purpose: 
#' 
#' * Evaluate the LINEAR relationship b/w two continuous variables.
#' 
#' * Two continuous variables: the product moment correlation.
#'  (Other correlations exist, but we'll focus on this one.)
#' 
#'   
#' * The correlation is technically an effect size:
#'    * Ranges from 0-1 in magnitude
#'    * Can be positive or negative, with sign
#'    indicating direction of relationship.
#'
#' * There is an inferential test associated with the correlation estimate: 
#'    * H0: rho = 0 
#'    * H1: rho ≠ 0 
#'    
#' * Do we care about this inferential test? Probably not!
#' 
#' 
#' * Everything is correlated with everything 
#' (Link: https://www.tylervigen.com/spurious-correlations)
#' 
#' Say it with me, kids:
#' "Correlation. Does. Not. Imply. causation!"

# Correlation test
cor.test(job_dat_final$hrlyearn, job_dat_final$wksaway)

#' Arguments: 
#' 
#' * cor.test(Data$Variable1, Data$Variable2)
#' 
#' Output: 
#' 
#' * t: t-statistic
#' * df: degrees of freedom 
#' * p-value: the p-value associated with the t-statistic
#' * 95 percent confidence interval: the 95% CI
#' * Simple estimates: the correlation estimate



### QUESTION: How would you interpret the output? 




### QUESTION: Are you surprised about this inferential result? Why or why not?

# Correlation estimate
cor(job_dat_final$hrlyearn, job_dat_final$wksaway, use = "complete.obs")

# Correlation matrix
cor(job_dat_final, use = "complete.obs")



# TOPIC 3 - Simple Linear Regression (SLR)

#' Purpose: 
#' 
#' * Determine extent to which you can
#'   predict a continuous DV based on a single IV.
#'   
#' * Regression allows researchers to make predictions.
#' 
#' * The IV and DV will be continuous in this course.
#' 
#' * A SLR is of the form:
#' Y = a + bX, where 
#' a = y-intercept,
#' b = slope.
#' 
#' Essentially: we're trying to find the values
#' a and b that minimizes the discrepancy between the
#' observed and predicted values.
#' 
#' Q: What is a y-intercept? What is a slope?
#' 
#' 
#' 
#' 
#' Hypotheses:
#' Relate to many things:
#' 1) The intercept term
#' H0: a = 0
#' H1: a ≠ 0 
#' 
#' (We don't really care much about the intercept...)
#' 
#' 2) The slope terms
#' H0: b = 0
#' H1: b ≠ 0 
#' 
#' 3) The overall model
#' H0: a = b = 0
#' H1: Above is false.
#' 
#' Steps: 
#' 
#' * Fit the regression model
#'
#' * Check the assumptions/diagnostics (next week!)
#' 
#' * Generate the model results



# TOPIC 4 - Fitting the Regression Model

#' We use extremely similar model syntax as before,
#' now with the `lm` function (lm = "linear model").

# Fitting the model
mod <- lm(wksaway ~ hrlyearn, job_dat_final)

#' Arguments:
#' 
#' * lm(Outcome ~ IV, Data) 



# TOPIC 5 - Generating the Model Results



# Display results 
summary(mod)

#' Output (there's a lot; we'll break it down slowly).
#' 
#' All output summarised for future reference: 
#' 
#' * Residuals: a 5 number summary of the distribution of the residuals
#' * Coefficients: model estimates of our y-intercept
#' (e.g., (Intercept)) and the slope (e.g., hrlyearn).
#' * Estimate: The model's estimates of the intercept and regression 
#' coefficients.
#' * Std.Error: the SE associated with the estimates.
#' * t-value: the t-statistic
#' * Pr(>|t|): the p-value
#' * Residual standard error: the SE associated with the model 
#' (i.e., on average how much are your predictions of observations off by?)
#' * Degrees of freedom: the DF of the denominator
#' * Multiple R-squared: the R2, or the % of the DV explained by
#' the IV.
#' 
#' * Adjusted R-squared: an R2 that is adjusted for the incorporation 
#' of multiple predictors in a model (not relevant to SLR).
#' * F-statistic: the R statistic associated with the R2 estimate 
#' * p-value: the p-value associated with the F-statistic
#' 
#' * Note: do not interpret the number of stars as an indication of 
#' how statistically significant a statistic is!!
#' * Note: the function removes all observations 
#' containing missing values by default! 


# QUESTION: How would you interpret this output?


# Standardized regression coefficients 
lm.beta(mod)

# QUESTION: How would you interpret this output?


# Tidy version of estimates (along with 95% CIs)
tidy(mod, conf.int = TRUE, conf.level = .95)

#' Output: 
#' 
#' * Estimate: the unstandardized intercept and regression coefficient estimates
#' * Std.Error: the SE associated with the estimates (i.e., the variability of the sampling distribution associated with estimate)
#' * t-value: the t-statistic
#' * p-value: the p-value associated with the t-statistic
#' * conf.low/conf.high: the lower and upper bound of the confidence intervals

#' Can also use `confint`:
confint(mod)

# Generating a scatterplot with a line of best fit (method 1)
# (We did this earlier!)
job_dat_final |>
  ggplot(aes(x = wksaway, y = hrlyearn)) + 
  geom_point() + 
  # Makes regression line of best fit
  geom_smooth(method = lm, formula = y~x) +
  labs(title = "# of Weeks away vs. Hourly Earnings",
       x = "# Weeks Away", y = "Hourly Earnings") + 
  Vic_theme

# Generating a scatterplot with a line of best fit (method 2)
ggPredict(mod)
ggPredict(mod, interactive = TRUE) # Interact with plot

# TOPIC 4 - Extra Practice 

#' Research Scenario: A researcher is interested in seeing if 
#' studying in a significant predictor 
#' of exam performance. 
#' 
#' To test this the researcher collected 
#' a sample of 100 undergraduate students
#' enrolled in an introductory statistics course. 
#' 
#' Participants were asked to report the number of hours 
#' they studied and their final grade in their 
#' statistics course (0-100%). 
#' 
#' Conduct a correlation test and 
#' SLR using GradeData.csv. 
#' Provide a brief summary of the results. 