# Week 6 - One-way ANOVA



### Note: for this tutorial we will be using JobData.csv.  



### Research question: We are interested in investigating the relationship between 
### hourly wage (HRLYEARN) and education (EDUC). 



# TOPIC 1 - Review



#' **QUESTION: What are the steps for performing analyses in R (so far)** 









#' **STEP 1: Load the packages that you will need in that script into your current R session.**



#Loading packages
library(psych)
library(here)
library(ez) ### ANOVA package
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

### Note: we need to have an ID variable to conduct the ANOVA. The ID variable 
### needs to be a factor. 

JobData<- JobData %>% 
  
  select(HRLYEARN, EDUC, X) %>% #Extracting the variables 
  
  mutate(ID = factor(X),
         EDUC_F = factor(EDUC,
                         levels = c(0, 1,2,3,4,5,6),
                         labels = c("0 to 8 years","Some high school", "High school graduate",
                                    "Some postsecondary", "Postsecondary certificate or diploma",
                                    "Bachelor's degree", "Above bachelor's degree"))) #Changing the structure of the variables and relabeling the levels

# Removing participants with missing data on HRLYEARN
JobData <- JobData[!is.na(JobData$HRLYEARN), ]


#' **STEP 5: Descriptive statistics and Visualizations**


# Generating numeric descriptive statistics for EDUC_F 

JobData %>% #Data 
  select(EDUC_F) %>% #Selecting EDUC_F
  summary(maxsum = 50) #Generating count information

# Generating numeric descriptive statistics for HRLYEARN 

JobData %>% #Data
  select(HRLYEARN) %>% #Selecting HRLYEARN
  describe() #Generating descriptive statistics

# Generating numeric descriptive statistics for HRLYEARN by EDUC_F 

describeBy(JobData$HRLYEARN, JobData$EDUC_F)

# Generating a histogram for HRLYEARN

#Generating a theme 
MyTheme<- theme(plot.title = element_text(family = "Times", size = 12, face = "bold", hjust = 0.5),
                axis.title =  element_text(family = "Times", size = 12),
                axis.text = element_text(family = "Times", size = 10),
                panel.background = element_blank(),
                panel.border = element_rect(colour = "black", fill = NA),
                axis.text.x = element_text(),
                legend.position = "none"
)

#Generating histogram
ggplot(JobData, 
       aes(x = HRLYEARN)) +
  geom_histogram(colour = "black", fill = "#E66100", bins = 30) + 
  labs(title = "Distribution of the Hourly Wages of Employees", 
       x = "Hourly Wages of Employees ($)",
       y = "Count") + 
  MyTheme

# Generating a boxplot for HRLYEARN by EDUC_F

ggplot(JobData, aes(x = HRLYEARN, y = EDUC_F, fill = EDUC_F)) +
  geom_boxplot() + 
  labs(title = "The Distribution of Hourly Wages by Education Level",
       x = "Hourly Wage ($)",
       y = "Education Level") + 
  scale_fill_manual(values = c("#803E75","#0057E9", "#FFB300", "#8FBC8F", "#E66100", "#2E2E2E", "#DC267F")) +
  MyTheme



#### QUESTION: What do we think the results of the ANOVA will tell us given the descriptive statistics?



# TOPIC 2 - One-Way ANOVA Introduction



#' Purpose:
#'    * An inferential procedure to evaluate whether there is a difference among 
#'      three or more means.
#'    
#'    * The ANOVA indicates whether at least one of the means are different; however,
#'      the test provides no information regarding which means are different. Follow-up
#'      analyses are required to determine which means are different. 
#'
#' Variables: 
#'    * One categorical IV with 3+ levels/groups. These groups are independent of each other.   
#'    * One continious DV
#'    
#' Hypotheses:
#'    * H0: µ1 - µ2 - µk = 0
#'    * H1: µ1 - µ2 - µk ≠ 0  


#### Steps for completing an ANOVA in R ####

#' Step 1: Check if the assumptions of the inferential procedure are reasonably met
#' Step 2: Conduct the ANOVA. Use robust versions of the ANOVA if assumptions are violated.
#' Step 3: Complete follow-up analyses if the main ANOVA results are statistically significant. 



# TOPIC 3 - Assumptions



#' Assumptions: 
#' 
#' *The scores on the DV are normally distributed for each level of the IV (normality)*
#' *The variance of the scores on the DV are equal across the levels of the IV (homogeneity of variance)*
#' 
#' * The scores on the DV are independent of each other (independence)
#' * The scores represent a random sample from the population


### Normality ###


# Numeric descriptives

### Note: we have already generated this output in the descriptive statistics section ###

# Visualizations

### Note: we have already generated this output in the visualization section ###



#### QUESTION: What conclusions would we draw based on this output? 



### Homogeneity of variance ###



#Conducting ANOVA test (to access the Levene's Test)
Results <- ezANOVA(data = JobData,
                   wid = ID,
                   between = EDUC_F,
                   dv = HRLYEARN,
                   return_aov = TRUE)

#' Arguments:
#' 
#' * data: the name of the object containing the data 
#' * wid: the name of the ID column 
#' * between: the IV
#' * dv: the DV
#' * return_aov: the output is an aov object

#Generating Levene's Test 
Results$"Levene's Test for Homogeneity of Variance"



#### QUESTION: What conclusions would we draw based on this output? 

















#' Note: 
#' 
#' * p<= alpha: reject the null hypothesis that the variance is the same across the groups
#' * p>alpha: fail to reject the null hypothesis that the variance is the same across the groups



# TOPIC 4 - ANOVA



#' Note: we have already conducted the ANOVA and saved the results in the Results object

#Viewing the ANOVA results
Results$'ANOVA'

#' Note: 
#' 
#' * Effect: the IV 
#' * DFn: degrees of freedom for the numerator (the number of groups - 1)
#' * DFd: degrees of freedom for the denominator (the sample size - the number of groups)
#' * F: the statistic (which for an ANOVA is the F statistic)
#' * p: the p-value 
#' * p<.05: indicates whether the p-value is less than 0.05 with an asterisks
#' * ges: generalized eta-square (effect size) 




#### QUESTION: What conclusions would we draw based on this output? 

#### QUESTION: Why are these results unsurprising (hint: think about the sample size)?



# TOPIC 5 - Follow-up analyses



#Conducting Tukey's post-hoc tests 
TukeyHSD(x = Results$aov)

#' Purpose: 
#' 
#' * Essentially, conducts an independent samples t-test for each pair-wise comparison
#' * Provides the adjusted p-values (due to inflated type I error rates from multiple comparisons) 
#' 
#' Output: 
#' 
#' * diff: the mean difference between the groups (the groups being compared are indicated on the right)
#' * lwr and upr: the lower and upper bounds of the 95% confidence interval 
#' * p adj: the adjusted p-value



# TOPIC 6 - Alternatives when assumptions are violated



### Violation of Homogeneity of Variance

#Conducting a Welch Test
oneway.test(formula = HRLYEARN~EDUC_F,
            data = JobData,
            var.equal = FALSE)

#' The Welch test is appropriate when the assumption of homogeneity of variance 
#' is violated because it does not use a pooled variance.
#' 
#' Arguments:
#' 
#' * formula: DV ~ IV 
#' * data: the name of the object containing the data
#' * var.equal: an argument asking you to specify whether the homogeneity of variance assumption is met.
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
kruskal.test(formula = HRLYEARN~EDUC_F,
             data = JobData)

#' The Kruskal-Wallis Rank Sum Test is appropriate when the assumption of normality 
#' is violated because it is based on the ranked data. 
#' 
#' * Note: only appropriate when the type of non-normality is the same across groups AND
#'   the assumption of 
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
pairwise.t.test(rank(JobData$HRLYEARN), JobData$EDUC_F,
                pool.sd=FALSE, p.adjust="fdr")

#' Conducting the Welch test on the ranks
#'   
#' Arguments:
#' 
#' * rank(): computing the ranks for the DV
#' * pool.sd: an argument asking you to indicate whether the the pooled SD should be computed 
#'   (the pooled SD should only be computed when the assumption of homogeneity of variance is met)
#' * p.adjust: the multiplicity control method that will be used to adjust the p-values
#' 
#' Output: 
#' 
#' * Provides you a matrix of p-values for the different pairwise comparisons



# TOPIC 3 - Practice Questions


# 1. Install the cereal 3.csv data into a new R script. Save the data in an object called RevisedCerealData.
# 
# 2. Extract the name, shelf and rating variables from RevisedCerealData.
# 
# 3. Change the structure of the shelf variable to a factor and relabel the levels so that: 
#   * 1 = Bottom Shelf 
#   * 2 = Middle Shelf 
#   * 3 = Top Shelf
# 
# 4. Change the structure of the name variable to a factor. 
# 
# 5. Go through the different steps of a One Way ANOVA to determine whether there 
#    is a difference in the average rating of cereals placed on the bottom, middle, 
#    and top shelf. Write a brief APA summary of the results. 
