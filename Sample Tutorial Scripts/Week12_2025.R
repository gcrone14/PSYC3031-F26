# Week 12 - Multiple Linear Regression (MLR)



### Note: for this tutorial we will be using JobData.csv.  



### Research question (MLR): Is paid overtime hours in reference week (PAIDOT) a predictor of the number of overtime or extra 
### hours worked (XTRAHRS), controlling for Usual hourly wages (HRLYEARN).



# TOPIC 1 - Review



#' **STEP 1: Load the packages that you will need in that script into your current R session.**



#Loading packages
library(psych)
library(here)
library(QuantPsyc) ## Contains a function that generates standardized regression coefficients
library(broom) ## Contains a function that generates confidence intervals associated with your model estimates and 
library(ggiraphExtra) ### Contains a function that generates visualizations of a model
library(GGally) ## Contains a function that generates a scatter plot matrix 
library(car) ## Contains a function that conducts the Durbin Watson Test
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
  dplyr::select(XTRAHRS, PAIDOT, HRLYEARN) #Extracting the variables 


### Note: add dplyr:: if you are having difficulties using the select() function.


#' **STEP 5: descriptive statistics and visuaializations**



### Univariate



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

##Graph (XTRAHRS)
ggplot(JobData, aes(x = XTRAHRS)) + 
  geom_histogram(colour = "black", fill = "dark red", bins = 20) + 
  labs(title = "Distribution of number of overtime or extra hours worked", x = "Number of overtime or extra hours worked", y = "Count") + 
  MyTheme

##Graph (PAIDOT)
ggplot(JobData, aes(x = PAIDOT)) + 
  geom_histogram(colour = "black", fill = "#8FBC8F", bins = 20) + 
  labs(title = "Distribution of number of paid overtime hours in reference week", x = "Number of paid overtime hours in reference week", y = "Count") + 
  MyTheme



### Bivariate



#Correlations
cor(JobData, use = "complete.obs")

##Graphs

### Note: you can generate the scatter plots individually for each graph using ggplot.
### OR you can use this function:

#Generating a scatter plot matrix
ggpairs(JobData)



# TOPIC 2 - Multiple linear regression (MLR)



#' Purpose: 
#' 
#' * Evaluate if two+ IV's are a predictor of a DV (while controlling or holding constant other predictors).
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
#' Note: there will be an inferential test for EACH regression coefficent
#' 
#' H0: B = 0 
#' H1: B ≠ 0 
#' 
#' Note: there is also a hypothesis for the intercept. Usually the intercept is not of interest.
#' 
#' Steps: 
#' 
#' * Fit the regression model
#'    * MLR model: a mathematical representation (in this case, a line of best fit) of the variation of Y as a function of X using population parameters.
#'
#' * Check the assumptions/diagnostics (next week!)
#' 
#' * Generate the model results



# TOPIC 3 - Fitting the Regression Model



#Fitting the model
mod<- lm(XTRAHRS ~ PAIDOT + HRLYEARN, JobData)


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
#' * The assumptions and diagnostics PRIMAIRLY impact the accuracy of the inferential conclusions.
#' 
#' * Essentially, we are seeing whether our model is a good description of reality given the data. 
#'   If the model is NOT a good description of reality, why would we trust the model results? 
#'   
#' *  For example, let's say the homogeneity of variance assumption is violated. This information could 
#'    suggest that some values of Y are better predicted by the model then other predicted values of Y.
#'    In other words, our precision of prediction may vary depending on the value of Y. Why would we trust
#'    this model?
#'    
#' *  Another example, let's say the relationship between Y and the X's is non-linear. 
#'    Why would I use a standard linear regression to model this relationship?   
#' 
#' * These assumptions are based on the distribution of the errors in the population.
#'   You evaluate whether these assumptions are sufficiently met via the model residuals.
#' 
#' Assumptions: 
#' 
#' * Independence: the residuals are uncorrelated (independent)
#' * Linearity: The relationship between Y (or the errors) and each predictor is linear.
#' * Normality: The errors are normally distributed at each predicted value of Y.
#' * Homogeneity of variance: The variability of the errors is constant across all predicted values of Y.
#' 
#' Diagnostics: 
#' 
#' * Multicollinearity: the predictors are not explaining a substantial shared amount of information
#'   in the outcome variable.
#' * Outliers/influential cases: their are no extreme observations that have a substantial impact
#'   one the model estimates (not all outliers are bad!!)



#### Independence: the residuals are uncorrelated (independent)



#' Note: this assumption is typically evaluated based on your study design (e.g., 
#' are the observations related); however, the Durbin Watson Test can be used to test this assumption.

durbinWatsonTest(mod)

#' Argument:
#' 
#' * The regression model 

### Threshold: values less than 1 or greater than 3 are cause for concern; values closer to 2 are better



#### Linearity: The relationship between Y (or the errors) and each predictor is linear.


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
#'    * The red (loess) line should be relatively straight; however, if the sample is
#'      very small or large, the line may not be an accurate reflection of reality.
#'      
#'    * The points should not form any weird patterns around the dotted line. 
#'   
#' * Purpose: see whether Y is linearly related to all X's simultaneously. 
#'
#'   * Importance: the relationship between X1 and Y may differ when considering X2.
#'     Therefore, we want to look at this relationship simultaneously. 



#### Normality: The errors are normally distributed at each predicted value of Y.



#QQ-plot
plot(mod, 2)


#' Argument:
#' 
#' * The regression model 
#' * Only generate the second plot
#' 
#' #' Output: 
#' 
#' * The standardized residuals plotted against the theoretical quantiles of a 
#'   normal distribution (i.e., reality (standardized residuals) versus expectation (theoretical quantiles)).
#'   
#' * Goal:
#'    
#'    * The residuals should be relatively close to the dotted line. Pay extra attention
#'      to the tails (where deviations are likely to occur). Look for extreme deviations from
#'      the dotted line.
 

#### Homogeneity of variance: The variability of the errors is constant across all predicted values of Y.



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



#### Multicollinearity: the predictors are not explaining a substantial shared amount of information in the outcome variable.


#Generating vif estimates 
vif(mod)

#' Arguments 
#' 
#' * The model 
#' 
#' Output: 
#' 
#' * The VIF estimate for each predictor 
#' * Threshold: values less than 4 are acceptable.



#### Outliers/influential cases: their are no extreme observations that have a substantial impact one the model estimates (not all outliers are bad!!)



## Link: https://www.econometrics-with-r.org/4.4-tlsa.html



#' Thresholds: 
#'  * Leverage: See if there are cases with leverage values close to 1.
#'  * Residual: See if there are studentized residual values beyond 2.5. 
#'  * Cook's distance: See if there are cases with a cook's distance value >= 1.



#Method 1 

#Generating important info
ImportantInfo <- broom::augment(mod) %>% #Generating important information about model
  add_column(.stud.resid = rstudent(mod)) #Adding studentized residuals to output

#Generating descriptive statistics 
ImportantInfo %>% 
  dplyr::select(.stud.resid,.hat, .cooksd) %>% #Selecting the important information
  describe() %>% #Generating descriptive statistics
  round(digits = 2) #Rounding values to two decimal places

#' Output: 
#'  * You are given descriptive statistics for the studentized residuals, cook's distance and leverage values
#'  
#' Interpretation: 
#'  * Leverage: What is the maximum leverage value? Is this value close to 1?
#'  * Residual: What is the maximum studentiized residual value? Is this value equal to or greater than 2.5?  
#'  * Cook's distance: What is the maximum Cook's distance value? Is this value equal to or greater than 1?


#Visualization 

#Adding an index column
ImportantInfo<- add_column(ImportantInfo, ID = 1:nrow(ImportantInfo))

## Leverage 
ggplot(ImportantInfo, aes(y =.hat, x = ID)) + 
  geom_point() + 
  labs(y = "Hat Values", x = "Index") + 
  theme(axis.title = element_text(family = "times", size = 15),
        axis.text = element_text(family = "times", size = 13),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) + 
  ylim(0,1) #Sets the maximum and minimum on the y-axis based on the range of possible leverage values

#' Interpretation: 
#'  * Are there any values close to 1?

## Residuals 
ggplot(ImportantInfo, aes(y =.stud.resid, x = ID)) + 
  geom_point() + 
  labs(y = "Studentized Residuals", x = "Index") + 
  theme(axis.title = element_text(family = "times", size = 15),
        axis.text = element_text(family = "times", size = 13),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

#' Interpretation: 
#'  * Are there any values >= 2.5?

## Cook's distance
ggplot(ImportantInfo, aes(y =.cooksd, x = ID)) + 
  geom_point() + 
  labs(y = "Cook's Distance", x = "Index") + 
  theme(axis.title = element_text(family = "times", size = 15),
        axis.text = element_text(family = "times", size = 13),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

#' Interpretation: 
#'  * Are there any values >= 1?

#Method 2 - Determining influencial cases
plot(mod, 5)

#' Interpretation: 
#'  * Are there any values >= 1?



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
