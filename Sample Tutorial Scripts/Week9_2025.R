# Week 9 - Repeated Measures ANOVA



### Note: for this tutorial we will be using TreatmentData.csv (simulated data).  



#' **Research scenario: A researcher was interested in seeing if a new medication is effective**
#' **for treating depression (measured on a 1-20 scale). The researcher evaluated changes**
#' **in depression in a sample of individuals diagnosed with depression over three timepoints (i.e., Time 1,**
#' **Time 2, and Time 3).**



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
TreatmentData<- read.csv(file = here("Data", "TreatmentData.csv"))



#' **STEP 3: check TreatmentData to ensure that the data has correctly been loaded into R**



#Checking TreatmentData
view(TreatmentData) ### Note: do NOT include this code in your script for assignments

#Checking the structure of the variables in TreatmentData
str(TreatmentData) ### Note: do NOT include this code in your script for assignments



### QUESTION: Is the data in long or wide format?












#' **STEP 4: clean TreatmentData**


#Changing the data to long format
TreatmentData<- TreatmentData %>% 
  pivot_longer(cols = c(Time1, Time2, Time3),
               names_to = "TimePoint",
               values_to = "DepressionLevel")


#Selecting the important variables and changing categorical variables/ID to factor 

### Note: we need to have an ID variable to conduct the ANOVA. The ID variable 
### needs to be a factor. 

TreatmentData<- TreatmentData %>% 
  
  select(-Age, -Sex) %>% #Extracting the variables 
  
  mutate(ID = factor(ID),
         TimePoint = factor(TimePoint)) #Changing the structure of the variables


#' **STEP 5: Descriptive statistics and Visualizations**


# Generating numeric descriptive statistics for TimePoint 

summary(TreatmentData$TimePoint)

# Generating numeric descriptive statistics for DepressionLevel by TimePoint

describeBy(TreatmentData$DepressionLevel, TreatmentData$TimePoint)

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

#Generating histograms for DepressionLevel by TimePoint
ggplot(TreatmentData, 
       aes(x = DepressionLevel)) +
  geom_histogram(colour = "black", fill = "#E66100", bins = 20) + 
  labs(title = "Distribution of Depression Scores by Time Point", 
       x = "Depression Score",
       y = "Count") + 
  MyTheme + 
  facet_grid(~TimePoint)



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
#'    * H0: µ1 - µ2 - µk = 0
#'    * H1: there is at least one mean difference between groups  


#### Steps for completing an ANOVA in R ####

#' Step 1: Check if the assumptions of the inferential procedure are reasonably met
#' Step 2: Conduct the ANOVA. Use robust versions of the ANOVA if assumptions are violated.
#' Step 3: Complete follow-up analyses if the main ANOVA results are statistically significant. 




# TOPIC 3 - Assumptions



#' Assumptions (that are evaluated through stats): 

#' * Normality: The distribution of scores is normal for each condition
#' * Sphericity: The variance of the difference scores are equal across group comparisons  



### Normality

#Graphically 

#Histogram

### Note: we have already generated this output in the visualization section ###

#Boxplot 
ggplot(TreatmentData, aes(y = DepressionLevel, x = TimePoint)) + 
  geom_boxplot(fill = "light blue", colour = "black") + 
  labs(title = "Distribution of Depression Levels by Time Points", x = "Time Points", y = "Depression Levels") +
  MyTheme

#QQplot 
ggplot(TreatmentData, aes(sample = DepressionLevel)) + 
  geom_qq(fill = "dark blue", colour = "black") + 
  geom_qq_line() +
  labs(title = "QQ Plot", x = "", y = "") +
  MyTheme + 
  facet_wrap(~TimePoint)


#Numeric

### Note: we have already generated this output in the descriptive statistics section ###



### QUESTION: Is the assumption of normality met? 



### Sphericity


#Mauchly’s test

#Conducting ANOVA test (to access the Mauchly’s test)
Results <- ezANOVA(data = TreatmentData,
                   wid = ID,
                   within = TimePoint,
                   dv = DepressionLevel)

#' Arguments:
#' 
#' * data: the name of the object containing the data 
#' * wid: the name of the ID column 
#' * within: the IV
#' * dv: the DV

#Generating Mauchly's Test
Results$"Mauchly's Test for Sphericity"



#### QUESTION: What conclusions would we draw based on this output? 




















#' Note: 
#' 
#' * p<= alpha: reject the null hypothesis that the assumption of sphericity is met
#' * p>alpha: fail to reject the null hypothesis that the assumption of sphericity is met



# TOPIC 4 - ANOVA



#' Note: we have already conducted the ANOVA and saved the results in the Results object

#Viewing the ANOVA results
Results$'ANOVA'


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
results<- pairwise_t_test(data = TreatmentData,
                formula = DepressionLevel ~ TimePoint,
                paired = TRUE,
                p.adjust.method = "bonferroni",
                detailed = TRUE) %>% 
  select(-p)


#' Purpose: 
#' 
#' * Essentially, conducts an dependent samples t-test for each pair-wise comparison
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
friedman.test(formula = DepressionLevel~TimePoint|ID,
              data = TreatmentData)

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



## Sphericity violation - Friedman Rank Sum Test

#Sphericity corrections
Results$'Sphericity Corrections'


#' Output:
#' 
#' * Effect: IV
#' * GGe: epsilon estimate based on Greenhouse-Geisser
#' * p[GGe]: adjusted p-value 
#' * p[GG]<.05: whether the adjusted p-value is statistically significant
#' * HFe: epsilon estimate based on Huynh-Feldt
#' * p[HF]: adjusted p-value
#' * p[HF]<.05: whether the adjusted p-value is statistically significant



#### QUESTION: What p-value should you look at?














#' Recommendation: 
#' 
#' GGe - when epsilon estimate is ≤ .75 (because this correction is more conservative)
#' HFe - when epsilon estimate is .75 < epsilon < 1 (because this correction is more liberal)



#### QUESTION: What df should be reported in an APA summary? 

















#ANOVA output
Results$'ANOVA'

#DFn 
2*0.9339071

#DFd
198*0.9339071


# TOPIC 7 - Practice Questions 

#' Research Scenario: A researcher was interested in investigating how coffee intake throughout the day
#' impacts individuals energy levels. To test this, the researcher gathered a sample of 200 young adults.
#' Each adult was asked to record their energy levels (on a scale from 1-20) when they wake up (i.e., no
#' coffee) and after their first coffee, second coffee, and third coffee. Conduct a repeated measures ANOVA 
#' using the CoffeeData.csv. Provide a brief summary of the results. 
