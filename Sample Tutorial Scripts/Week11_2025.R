# Week 11 - Correlation and Simple Linear Regression 



### Note: for this tutorial we will be using JobData.csv.  



### Research question (correlation): Is their a relationship between hourly wage and 
### the number of weeks an individual is absent from work?

### Research question (SLR): Is hourly wage a predictor of the number of weeks an individual is absent from work?



# TOPIC 1 - Review



#' **STEP 1: Load the packages that you will need in that script into your current R session.**



#Loading packages
library(psych)
library(here)
library(QuantPsyc) ## Contains a function that generates standardized regression coefficients
library(broom) ## Contains a function that generates confidence intervals associated with your model estimates
library(ggiraphExtra) ### Contains a function that generates visualizations of a model
library(tidyverse)



#' **STEP 2: load JobData.csv into R. Save the data in an object called JobData**



#Load the data
JobData<- read.csv(file = here("Data", "JobData.csv"))



#' **STEP 3: check JobData to ensure that the data has correctly been loaded into R**



#Checking JobData
view(JobData) ### Note: do NOT include this code in your script for assignments

#Checking the structure of the variables in JobData
str(JobData) ### Note: do NOT include this code in your script for assignments



#' **STEP 4: clean JobData**



#Completing the data cleaning tasks in one chunk of code
JobData<- JobData %>% 
  dplyr::select(HRLYEARN, WKSAWAY) #Extracting the variables 


### Note: add dplyr:: if you are having difficulties using the select() function.


#' **STEP 5: descriptive statistics and visuaializations**


#Descriptive statistics
describe(JobData)

#Generating a theme 
MyTheme<- theme(plot.title = element_text(family = "Times", size = 12, face = "bold", hjust = 0.5),
                axis.title =  element_text(family = "Times", size = 12),
                axis.text = element_text(family = "Times", size = 10),
                panel.background = element_blank(),
                panel.border = element_rect(colour = "black", fill = NA),
                axis.text.x = element_text(),
                legend.position = "none"
)

##Graph (HRLYEARN)
ggplot(JobData, aes(x = HRLYEARN)) + 
  geom_histogram(colour = "black", fill = "#8FBC8F", bins = 20) + 
  labs(title = "Distribution of Hourly Wage", x = "Hourly Earnings ($/Hour)", y = "Count") + 
  MyTheme

##Graph (WKSAWAY)
ggplot(JobData, aes(x = WKSAWAY)) + 
  geom_histogram(colour = "black", fill = "dark red", bins = 30) + 
  labs(title = "Distribution of Absences from Work", x = "Absenses from Work (Weeks)", y = "Count") + 
  MyTheme



# TOPIC 2 - Correlation



#' Purpose: 
#' 
#' * Evaluate whether their is a relationship between to variables.
#' 
#' * Two continuous variables: the product moment correlation 
#' 
#'    * Note: there are different types of correlations for variables of different data types.
#'    
#'    * Note: the product moment correlation is only appropriate when the relationship between two variables is linear. 
#'   
#' * The correlation estimate is an effect size:
#'  
#'    * The magnitude of the correlation can range from 0-1 with higher values indicating a stronger relationship.
#'    
#'    * The sign indicates the direction of the relationship.
#'
#' * There is an inferential test associated with the correlation estimate: 
#' 
#'    * H0: rho = 0 
#'    * H1: rho ≠ 0 
#'    
#' * Do we care about this inferential test? Probably not! But, good to know that you can conduct an inferential test :) 
#' 
#' * Everything is correlated with everything (Link: https://www.tylervigen.com/spurious-correlations)




#Correlation test
cor.test(JobData$HRLYEARN, JobData$WKSAWAY)

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




#Correlation estimate
cor(JobData$HRLYEARN, JobData$WKSAWAY, use = "complete.obs")

#Correlation matrix
cor(JobData, use = "complete.obs")



# TOPIC 3 - Simple Linear Regression (SLR)



#' Purpose: 
#' 
#' * Evaluate if one IV is a predictor of a DV.
#'   
#' * Regression allows researchers to make predictions (within the range of the data)
#' 
#' * The IV and DV will be continuous in this course.
#' 
#' Hypotheses (R2): 
#' 
#' H0: R2 = 0
#' H1: R2 ≠ 0
#' 
#' Hypotheses (B):
#' 
#' H0: B = 0 
#' H1: B ≠ 0 
#' 
#' Note: there is also a hypothesis for the intercept. Usually the intercept is not of interest.
#' 
#' Steps: 
#' 
#' * Fit the regression model
#'    * SLR model: a mathematical representation (in this case, a line of best fit) of the variation of Y as a function of X using population parameters.
#'
#' * Check the assumptions/diagnostics (next week!)
#' 
#' * Generate the model results



# TOPIC 4 - Fitting the Regression Model



#Fitting the model
mod<- lm(WKSAWAY ~ HRLYEARN, JobData)

#' Arguments:
#' 
#' * lm(Outcome ~ IV, Data) 



# TOPIC 5 - Generating the Model Results



#Displaying results 
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
#' * Note: do not interpret the number of stars as an indication of how statistically significant a statistic is!!
#' * Note: the function removes all observations containing missing values by default! 


### QUESTION: How would you interpret this output?


#Standardized regression coefficients 
lm.beta(mod)


### QUESTION: How would you interpret this output?


#Generating CI's (and other information)
tidy(mod, conf.int = TRUE, conf.level = .95)

#' Output: 
#' 
#' * Estimate: the unstandardized intercept and regression coefficient estimates
#' * Std.Error: the SE associated with the estimates (i.e., the variability of the sampling distribution associated with estimate)
#' * t-value: the t-statistic
#' * p-value: the p-value associated with the t-statistic
#' * conf.low/conf.high: the lower and upper bound of the confidence intervals


#Generating a scatterplot with a line of best fit (method 1)
ggplot(JobData, aes(y = WKSAWAY, x = HRLYEARN)) + 
  geom_point(colour = "dark red") + 
  geom_smooth(method=lm, se=FALSE, colour = "dark green") +
  labs(title = "Relationship Between Hourly Wage and Work Absences", x = "Hourly Wage ($/Hour)", y = "Work Absences (Weeks)") + 
  MyTheme

#Generating a scatterplot with a line of best fit (method 2)
ggPredict(mod, interactive = TRUE)



# TOPIC 4 - Extra Practice 



#' Research Scenario: A researcher is interested in seeing if studying in a significant predictor 
#' of exam performance. To test this the researcher collected a sample of 100 undergraduate students'
#' enrolled in an introductory statistics course. Participants' were asked to report the number of hours 
#' they studied and their final grade in their statistics course (0-100%). Conduct a correlation test and 
#' SLR using GradeData.csv. Provide a brief summary of the results. 