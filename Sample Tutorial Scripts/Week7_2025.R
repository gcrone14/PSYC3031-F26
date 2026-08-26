# Week 7 - Two-Way Independent Samples ANOVA



### Note: for this tutorial we will be using JobData.csv.  



### Research question: Does the relationship between hourly wage (HRLYEARN) and 
### education (EDUC) vary by gender?



# TOPIC 1 - Review



#' **STEP 1: Load the packages that you will need in that script into your current R session.**



#Loading packages
library(psych)
library(here)
library(ez) ### ANOVA package
library(WRS2) ### When assumptions are violated
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
  
  select(HRLYEARN, EDUC, X, SEX) %>% #Extracting the variables 
  
  mutate(ID = factor(X),
         EDUC_F = factor(EDUC,
                         levels = c(0, 1,2,3,4,5,6),
                         labels = c("0 to 8 years","Some high school", "High school graduate",
                                    "Some postsecondary", "Postsecondary certificate or diploma",
                                    "Bachelor's degree", "Above bachelor's degree")),
         SEX_F = factor(SEX, 
                        levels = c(1,2),
                        labels = c("Male", "Female"))) #Changing the structure of the variables and relabeling the levels

# Removing participants with missing data on HRLYEARN
JobData <- JobData[!is.na(JobData$HRLYEARN), ]


#' **STEP 5: Descriptive statistics and Visualizations**


# Generating numeric descriptive statistics for EDUC_F 

JobData %>% #Data 
  select(EDUC_F, SEX_F) %>% #Selecting EDUC_F
  summary() #Generating count information

# Generating numeric descriptive statistics for HRLYEARN 

JobData %>% #Data
  select(HRLYEARN) %>% #Selecting HRLYEARN
  describe() #Generating descriptive statistics

# Generating numeric descriptive statistics for HRLYEARN by EDUC_F and SEX_F

## Method 1
describeBy(x = JobData$HRLYEARN,
           group = list(JobData$EDUC_F,
                        JobData$SEX_F))

## Method 2
JobData %>% 
  group_by(EDUC_F, SEX_F) %>%
  summarize(describe(HRLYEARN))

# Generating a histogram for HRLYEARN

#Generating a theme 
MyTheme<- theme(plot.title = element_text(family = "Times", size = 12, face = "bold", hjust = 0.5),
                axis.title =  element_text(family = "Times", size = 12),
                axis.text = element_text(family = "Times", size = 10),
                panel.background = element_blank(),
                panel.border = element_rect(colour = "black", fill = NA),
                axis.text.x = element_text(),
                legend.text = element_text(family = "Times", size = 12),
                legend.title = element_text(family = "Times", size = 12, hjust = 0.5)
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

ggplot(JobData, aes(x = HRLYEARN, y = EDUC_F, fill = SEX_F)) +
  geom_boxplot() + 
  labs(title = "The Distribution of Hourly Wages by Education Level",
       x = "Hourly Wage ($)",
       y = "Education Level",
       fill = "Sex") + 
  scale_fill_manual(values = c("#803E75", "#E66100")) +
  MyTheme




# TOPIC 2 - Two-Way Independent Samples ANOVA




#' Purpose:
#'    * An inferential procedure to evaluate whether there is an interaction.
#'    
#'    * Interaction: the magnitude/direction of the effect of one IV on the DV is
#'      dependent on the levels of another IV.
#'    
#'    * The ANOVA indicates whether at least one of the means are different; however,
#'      the test provides no information regarding which means are different. Follow-up
#'      analyses are required to determine which means are different. 
#'
#' Variables: 
#'    * Two categorical IV with 2+ levels/groups. These groups are independent of each other.   
#'    * One continuous DV
#'    
#' Hypotheses:
#'    * Main effect 1: The population means on the dependent variable are the same across levels of IV1 
#'    * Main effect 2: The population means on the dependent variable are the same across levels of IV2
#'    * Interaction: The population means on the dependent variable for IV1 are the same across levels of IV2


#### Steps for completing an ANOVA in R ####

#' Step 1: Check if the assumptions of the inferential procedure are reasonably met
#' Step 2: Conduct the ANOVA. Use robust versions of the ANOVA if assumptions are violated.
#' Step 3: Complete follow-up analyses if the main ANOVA results are statistically significant. 






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

#QQ-plot 
ggplot(data = JobData,
       aes(sample = HRLYEARN)) +
  geom_qq_line(colour = "dark red") +
  geom_qq(colour = "dark blue") +
  facet_grid(EDUC_F~SEX_F) + 
  labs(title = "QQ Plots", 
       x = "", y = "") + 
  MyTheme




#' Code:
#' 
#' * sample: the variable you want to evaluate the normality assumption. 
#' * geom_qq_line: geom to draw the qq plot line
#' * geom_qq: geom to draw the qq plot points
#' * facet_grid: to create the grid layout of the graph




#### QUESTION: is the assumption of normality met? 



#### In the pre-recorded lecture it was mentioned as an example that skew = -0.26 
#### is a slight deviation from normal distribution. What values would indicate 
#### us to conduct a non-parametric test ?



### Homogeneity of variance ###



#Conducting ANOVA test (to access the Levene's Test)
Results <- ezANOVA(data = JobData,
                   wid = ID,
                   between = EDUC_F:SEX_F,
                   dv = HRLYEARN,
                   return_aov = TRUE)

#Generating Levene's Test 
Results$"Levene's Test for Homogeneity of Variance"


#' Arguments:
#' 
#' * data: the name of the object containing the data 
#' * wid: the name of the ID column 
#' * between: the IV's
#' * dv: the DV
#' * return_aov: the output is an aov object


#### QUESTION: is the assumption of homogeneity of variance met? 



# TOPIC 4 - ANOVA



#' Note: we have already conducted the ANOVA and saved the results in the Results object

#Viewing the ANOVA results
Results$'ANOVA'



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



#### QUESTION: Why are these results unsurprising (hint: think about the sample size)?





# Creating a plot 
ggplot(JobData, aes(x = EDUC_F, y = HRLYEARN, colour = SEX_F, group = SEX_F)) +
  stat_summary(fun = mean, geom = "point") + 
  stat_summary(fun = mean, geom = "line") +
  labs(title = "Mean Hourly Earnings by Education and Sex",
       x = "Education Level",
       y = "Mean Hourly Earnings ($)",
       colour = "Sex") + 
  MyTheme



# TOPIC 5 - Follow-up analyses




#Create data subsets

## Males
JobDataMale <- filter(JobData,
                      SEX_F == "Male")

## Females
JobDataFemale <- filter(JobData,
                        SEX_F == "Female")

#Note: it does not matter which IV you use to create the data subsets. Use the variable 
#with the less levels. 




### QUESTION: How can I check this code?




#Create ANOVA objects (compatible with the TukeyHSD function)

## Males
MaleModel<- aov(HRLYEARN ~ EDUC_F,
                  data = JobDataMale)

## Females
FemaleModel<- aov(HRLYEARN ~ EDUC_F,
                    data = JobDataFemale)


#Conducting tests

## Males
TukeyHSD(x = MaleModel)

## Females
TukeyHSD(x = FemaleModel)



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




### Violation of Normality and/or Homogeneity of Variance Assumptions



#Conducting a two way independent samples ANOVA with trimmed means 
t2way(formula = HRLYEARN ~ EDUC_F * SEX_F,
      data = JobData,
      tr = .1)

#' Purpose: 
#' 
#' * Conducts a two way independent samples ANOVA with trimmed means (based on the Welch test) 
#' * Trimmed mean: computing the mean without a certain proportion of extreme observations (in this case, 0.1)
#' * Welch test: appropriate when the homogeneity of variance assumption is not met
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


#' Research scenario: a researcher is interested in determining whether the effect of studying (high and low)
#' on test performance (ranging from 0 to 100) is impacted by the amount of sleep a student gets (1-3hrs, 4-6, 7+). 
#' Conduct a factorial ANOVA using PerformanceData2.csv. Provide a summary of the results. For this practice scenario 
#' use an alpha of 0.05. 
