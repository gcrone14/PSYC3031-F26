# Week 11 - Multiple Linear Regression (MLR)

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


### Note: for this tutorial we will be using job_dat.csv
### (for the last time!)

# TOPIC 1 - Review
#' Quiz time!
#' 1) 
#' 2)
#' 3)
#' 4)
#' 5)
#' 6)


### Research question (MLR): Is paid overtime hours 
### in reference week (PAIDOT) 
### a predictor of the number of extra 
### hours worked (overtime; XTRAHRS), controlling for 
### Usual hourly wages (HRLYEARN)?



# TOPIC 1 - Review



#' **STEP 1: Load the packages that you will need in that script into your current R session.**



#Loading packages
library(psych)
library(here)
library(QuantPsyc)
library(broom)
library(ggiraphExtra)
library(GGally) # New: Summarise bivariate stats quickly!
library(car) # Help conduct the Durbin-Watson Test
library(tidyverse)



#' **STEP 2: load job_dat.csv into R. Save the data in an object called job_dat**



#Load the data
job_dat <- read_csv(file = here("Data", "JobData.csv"))



#' **STEP 3: check job_dat to ensure that the data has correctly been loaded into R**
glimpse(job_dat)
str(job_dat)

#' **STEP 4: clean job_dat**

#Completing the data cleaning tasks in one chunk of code
job_dat_final <- job_dat |>
  dplyr::select(XTRAHRS, PAIDOT, HRLYEARN) |>
  janitor::clean_names() |>
  filter(!is.na(hrlyearn) & !is.na(paidot) & !is.na(hrlyearn))

glimpse(job_dat_final)

### Note: add dplyr:: if you are having difficulties using the select() function.


#' **STEP 5: descriptive statistics and visuaializations**



### Univariate



#Descriptive statistics
describe(job_dat_final)

#Generating a theme 
vic_theme <- theme(plot.title = element_text(family = "Times", size = 12, face = "bold", hjust = 0.5),
                axis.title =  element_text(family = "Times", size = 12),
                axis.text = element_text(family = "Times", size = 10),
                panel.background = element_blank(),
                panel.border = element_rect(colour = "black", fill = NA),
                axis.text.x = element_text(),
                legend.position = "none"
)

##Graph (hrlyearn)
job_dat_final |>
  ggplot(aes(x = hrlyearn)) + 
  geom_histogram(colour = "black", fill = "#8FBC8F", bins = 20) + 
  labs(title = "Distribution of Hourly Wage", x = "Hourly Earnings ($/Hour)", y = "Count") + 
  vic_theme

# Graph (xtrahrs)
job_dat_final |>
  ggplot(aes(x = xtrahrs)) + 
  geom_histogram(colour = "black", fill = "dark red", bins = 20) + 
  labs(title = "Distribution of number of overtime or extra hours worked", x = "Number of overtime or extra hours worked", y = "Count") + 
  vic_theme

# Graph (paidot)
job_dat_final |>
  ggplot(aes(x = paidot)) + 
  geom_histogram(colour = "black", fill = "#8FBC8F", bins = 20) + 
  labs(title = "Distribution of number of paid overtime hours in reference week", x = "Number of paid overtime hours in reference week", y = "Count") + 
  vic_theme



### Bivariate



#Correlations
cor(job_dat_final, use = "complete.obs")

# Graphs

# Below is a nice shortcut from the `GGally`
# package to make bivariate plots.

# Generating a scatter plot matrix
GGally::ggpairs(job_dat_final)

#' Q: What can we interpret from this
#' output?



# TOPIC 2 - Multiple linear regression (MLR)



#' Purpose: 
#' 
#' * Evaluate if two+ IV's are a predictor of a DV 
#'  (while controlling for, or holding constant, 
#'  other predictors).
#'   
#' * Regression allows researchers to make predictions 
#'  (within the range of the data)
#' 
#' * The IV and DV will be continuous in this course.
#' 
#' Hypotheses (R2): 
#' 
#' H0: R^2 = 0
#' H1: R^2 ≠ 0
#' 
#' Hypotheses (B):
#' 
#' Note: there will be an inferential test for EACH regression coefficent
#' 
#' H0: B = 0 
#' H1: B ≠ 0 
#' 
#' Note: there is also a hypothesis for the intercept. 
#' Usually the intercept is not of interest.
#' 
#' Steps: 
#' 
#' * Fit the regression model
#'    * MLR model: a mathematical representation 
#'    (in this case, a line of best fit) of the variation 
#'    of Y as a function of X1, X2, ..., Xn (with n 
#'    continuous predictors).
#'
#' * Check the assumptions/diagnostics
#' 
#' * Generate the model results



# TOPIC 3 - Fitting the Regression Model

#' We fit the model identically to how we did so
#' last week, but literally add another predictor
#' on top:
#' formula = Y ~ IV1 + IV2

# Fitting the model
mod <- lm(xtrahrs ~ paidot + hrlyearn, job_dat_final)


#' Arguments:
#' 
#' * lm(Outcome ~ IV1 + IV2, Data) 
#' 
#' Note: you can add predictors to the regression model via "+"
#' 
#' Note: you need to fit the model to get the information required to test the assumptions 
#' and evaluate the diagnostics.


# TOPIC 4 - Assumptions and Diagnostics

#' Purpose: 
#' 
#' * The assumptions and diagnostics PRIMAIRLY impact the accuracy
#'   of the inferential conclusions.
#'   
#' * Remember: Garbage-in-garbage-out? Same applies to
#'   multiple linear regression!
#' 
#' * Before we checked assumptions on the observed data.
#'   DO NOT do that for regression! In regression, all
#'   assumptions are on the level of the RESIDUALS
#'   (differences between the observed and model-predicted
#'   values.) 
#' 
#' Assumptions: 
#' 
#' * Independence: the residuals are uncorrelated (independent)
#' * Linearity: The relationship between Y (or the errors) 
#'   and each predictor is linear.
#' * Normality: The errors are normally distributed 
#'   at each predicted value of Y.
#' * Homogeneity of variance: The variability of the errors is 
#'   constant across all predicted values of Y.
#' 
#' Diagnostics: 
#' 
#' * Multicollinearity: the predictors are not explaining a 
#'   substantial shared amount of information
#'   in the outcome variable.
#' * Outliers/influential cases: there are no extreme observations 
#'   that have a substantial impact
#'   one the model estimates (not all outliers are bad!!)

#### Independence: the residuals are uncorrelated (independent)



#' Note: this assumption is typically evaluated based on your study design (e.g., 
#' are the observations related); however, 
#' the Durbin Watson (DW) Test can be used to test this assumption.

durbinWatsonTest(mod)

#' Argument:
#' 
#' * The regression model 

### Threshold: values less than 1 or greater than 3 are cause for concern; 
### values closer to 2 are better

#' Null hypothesis implies that the assumption is met. We DON'T
#' want to reject it!

#### Linearity: The relationship between Y (or the errors) and each predictor is linear.


# Residuals vs fitted values
plot(mod, 1)


#' Argument:
#' 
#' * The regression model 
#' * Only generate the first plot
#' 
#' Output: 
#' 
#' * A residuals vs fitted plot. 
#'   The residuals plotted against all the fitted
#'   values of Y (e.g., the values of Y predicted 
#'   from the regression equation).
#'   
#' * Goal:
#'    
#'    * The red (loess) line should be relatively straight; 
#'      however, if the sample is
#'      very small or large, the line may not be an 
#'      accurate reflection of reality.
#'      
#'    * The points should not form any weird 
#'     patterns around the dotted line. 
#'   
#' * Purpose: see whether Y is linearly related to all X's simultaneously. 
#'
#'   * Importance: the relationship between X1 and Y may differ when considering X2.
#'     Therefore, we want to look at this relationship simultaneously. 

#' Q: Is the assumption met or violated?


#### Normality: The errors are normally distributed at each predicted value of Y.



# QQ-plot (you should be familiar!)
plot(mod, 2)


#' Argument:
#' 
#' * The regression model 
#' * Only generate the second plot
#' 
#' #' Output: 
#' 
#' * The standardized residuals plotted against the 
#'   theoretical quantiles of a 
#'   normal distribution 
#'   (i.e., reality (standardized residuals) 
#'   versus expectation (theoretical quantiles)).
#'   
#' * Goal:
#'    
#'    * The residuals should be relatively close to the dotted line. 
#'      Pay extra attention to the tails (where deviations are 
#'      likely to occur). Look for extreme deviations from
#'      the dotted line.

#' Q: Is the assumption met or violated?




#### Homogeneity of variance: The variability of the 
#### errors is constant across all predicted values of Y.



#Residuals vs fitted values
plot(mod, 1)



#' Argument:
#' 
#' * The regression model 
#' * Only generate the first plot
#' 
#' Output: 
#' 
#' * A residuals vs fitted plot. The residuals plotted against all the fitted
#'   values of Y (e.g., the values of Y predicted from the regression equation).
#'   
#' * Goal:
#'    
#'    * The distribution of the residuals at each fitted value should be relatively 
#'      equal across all the fitted values. For example, a funnel shape would indicate
#'      the assumption of homogeneity of variance is violated. 
#'   

#' Q: Is the assumption met or violated?


#### Multicollinearity: the predictors are not explaining a substantial shared amount of information in the outcome variable.


# Generating vif estimates 
vif(mod)

#' Arguments 
#' 
#' * The model 
#' 
#' Output: 
#' 
#' * The VIF estimate for each predictor 
#' * Threshold: values less than 4 are acceptable.

#' Is the assumoption met or violated?



#### Outliers/influential cases: their are no 
#### extreme observations that have a substantial impact 
#### one the model estimates (not all outliers are bad!!)


#' Thresholds: 
#'  * Leverage: See if there are cases with leverage values close to 1.
#'  * Residual: See if there are studentized residual values beyond 2.5. 
#'  * Cook's distance: See if there are cases with a cook's distance value >= 1.



# Method 1 

# Generating important info
important_info <- broom::augment(mod) |> #Generating important information about model
  mutate(.stud.resid = rstudent(mod)) #Adding studentized residuals to output

# Generating descriptive statistics 
important_info |> 
  dplyr::select(.stud.resid,.hat, .cooksd) |> # Select important info
  describe() |> # Descriptive stats
  round(digits = 2) # Round values to 2 decimals

#' Output: 
#'  * You are given descriptive statistics for the studentized residuals, 
#'  cook's distance and leverage values
#'  
#' Interpretation: 
#'  * Leverage: What is the maximum leverage value? Is this value close to 1?
#'  * Residual: What is the maximum studentiized residual value? 
#'  Is this value equal to or greater than 2.5?  
#'  * Cook's distance: What is the maximum Cook's distance value? 
#'  Is this value equal to or greater than 1?


# Visualization 

#Adding an index column
important_info <- important_info |> 
  mutate(ID = 1:nrow(important_info))

# Leverage 
important_info |>
  ggplot(aes(y =.hat, x = ID)) + 
  geom_point() + 
  labs(y = "Hat Values", x = "Index") + 
  theme(axis.title = element_text(family = "times", size = 15),
        axis.text = element_text(family = "times", size = 13),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) + 
  ylim(0,1)

#' Interpretation: 
#'  * Are there any values close to 1?

# Residuals 
important_info |>
  ggplot(aes(y =.stud.resid, x = ID)) + 
  geom_point() + 
  labs(y = "Studentized Residuals", x = "Index") + 
  theme(axis.title = element_text(family = "times", size = 15),
        axis.text = element_text(family = "times", size = 13),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

#' Interpretation: 
#'  * Are there any values >= 2.5?
#'  
#'  We can check with code!
important_info |>
  filter(.stud.resid >= 2.5) |>
  nrow()

#' In fact, there are 1596 values like this!
#' 
#' Goes to show: using thresolds might not
#' be best idea, especially with a lot
#' of data!


# Cook's distance
important_info |>
  ggplot(aes(y =.cooksd, x = ID)) + 
  geom_point() + 
  labs(y = "Cook's Distance", x = "Index") + 
  theme(axis.title = element_text(family = "times", size = 15),
        axis.text = element_text(family = "times", size = 13),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

#' Interpretation: 
#'  * Are there any values >= 1?
#'  
#'  Let's check with code!
important_info |>
  filter(.cooksd >= 1) |>
  nrow()

#' No rows!


# Method 2 - Determining influencial cases
plot(mod, 5)

#' Interpretation: 
#'  * Are there any values >= 1?

#' Q: Are there any outliers?


# TOPIC 5 - Generating the Model Results

# Displaying results 
summary(mod)

#' Output: 
#' 
#' * Residuals: a 5 number summary of the distribution of the residuals
#' * Coefficients: first row = intercept; second row = regression coefficient
#' * Estimate: the unstandardized intercept and regression coefficient estimates
#' * Std.Error: the SE associated with the estimates (i.e., the variability of the sampling distribution associated with estimate)
#' * t-value: the t-statistic
#' * Pr(>|t|): the p-value
#' * Residual standard error: the SE associated with the model (i.e., on average how much are your predictions of observations off by)
#' * Degrees of freedom: the DF of the denominator
#' * Multiple R-squared: the R2
#' * Adjusted R-squared: an R2 that is adjusted for the incorporation of multiple predictors in a model
#' * F-statistic: the R statistic associated with the R2 estimate 
#' * p-value: the p-value associated with the F-statistic
#' 
#' * Note: do not interpret the number of stars as an indication 
#'   of how statistically significant a statistic is!
#' * Note: the function removes all observations containing missing 
#'   values by default! 


#' Q: How would you interpret this output?


# Standardized regression coefficients 
lm.beta(mod)
confint(mod) #alternative


#' Q: How would you interpret this output?


# Generating CI's (and other information)
tidy(mod, conf.int = TRUE, conf.level = .95)

#' Output: 
#' 
#' * Estimate: the unstandardized intercept and regression coefficient estimates
#' * Std.Error: the SE associated with the estimates (i.e., the variability of the sampling distribution associated with estimate)
#' * t-value: the t-statistic
#' * p-value: the p-value associated with the t-statistic
#' * conf.low/conf.high: the lower and upper bound of the confidence intervals

#Generating a scatterplot with a line of best fit
ggPredict(mod)



# TOPIC 6 - Extra Practice 


#' Research Scenario: A researcher is interested in seeing if studying is a 
#' significant predictor of exam performance, controlling for sleep. To test this, 
#' the researcher collected a sample of 100 undergraduate students enrolled in 
#' an introductory statistics course. Participants were asked to report the 
#' number of hours they studied, the amount of sleep they got the night before 
#' the exam, and their final grade in their statistics course (0-100%). Conduct 
#' the appropriate test using GradeData.csv. Provide a summary of the results. 
