#### Ticket out-the-door (2) - Answer ####


#' This data was collected by Statistics Canada with the aim of getting a better 
#' understanding of the Canadian labor force. We are interested in investigating 
#' the relationship between hourly wage (HRLYEARN) and education (EDUC). 
#' 
#' This week you are responsible for answering the research question through descriptive
#' statistics and visualizations. Use the steps of data analysis to guide your code. 
#' Your response should be written in paragraph format following APA guidelines. Use 
#' your desciption from last week as a starting point for your write-up.
#' 
#' Codebook: 
#' 
#' https://borealisdata.ca/api/datasets/export?exporter=html&persistentId=doi:10.5683/SP3/GFEQ3K#AGE_12
#' 
#' Reference: 
#' 
#' Statistics Canada, 2024, "Labour Force Survey, October 2024 [Canada]", https://doi-org.ezproxy.library.yorku.ca/10.5683/SP3/GFEQ3K, Borealis, V1, UNF:6:GAQpXRir5bZmoNo0l1d0cw== [fileUNF]



# Loading packages
library(here)
library(psych)
library(tidyverse)

# Loading the data 
JobData<- read.csv(file = here("Data", "JobData.csv"))

# Checking data 
view(JobData)
str(JobData)

# Extracting the variables of interest
JobData<- JobData %>%
  select(SEX, AGE_12, EDUC, HRLYEARN, PROV)

# Changing the structure of variables

JobData<- JobData %>% #Data
  mutate(SEX_F = factor(SEX,
                        levels = c(1, 2),
                        labels = c("Male", "Female")),
         AGE_12_F = factor(AGE_12, 
                           levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                           labels = c("15 to 19 years", "20 to 24 years", "25 to 29 years",
                                      "30 to 34 years", "35 to 39 years", "40 to 44 years",
                                      "45 to 49 years", "50 to 54 years",  "55 to 59 years", 
                                      "60 to 64 years", "65 to 69 years", "70 and over")),
         EDUC_F = factor(EDUC,
                         levels = c(0, 1,2,3,4,5,6),
                         labels = c("0 to 8 years","Some high school", "High school graduate",
                                    "Some postsecondary", "Postsecondary certificate or diploma",
                                    "Bachelor's degree", "Above bachelor's degree")), 
         PROV_F = factor(PROV,
                         levels = c(35, 24,59,48, 46,  47, 13, 12, 10, 11),
                         labels = c("Ontario", "Quebec", "British Columbia", "Alberta", "Manitoba",
                                    "Saskatchewan", "New Brunswick", "Nova Scotia", "Newfoundland and Labrador",
                                    "Prince Edward Island"))) 


#### TIP ####

#' * How you order the levels with the levels argument will arrange how the levels appear in the output 
#' * For nominal variables (e.g., province), order the levels by frequency 
#' * For ordinal variables (e.g., level of education), order the levels by their order 

# Generating numeric descriptive statistics for the categorical variables 

JobData %>% #Data 
  select(SEX_F, AGE_12_F, EDUC_F, PROV_F) %>% #Selecting categorical variables
  summary(maxsum = 50) #Generating count information

#' Note: the maxsum argument in summary() allows you to adjust the number of levels
#' that will be visible in the output. 

#Generating visualizations

## SEX_F
ggplot(JobData, 
       aes(x = SEX_F)) +
  geom_bar(colour = "black", fill = "light blue") + 
  labs(title = "Distribution of Sex", 
       x = "Sex",
       y = "Count")

## AGE_12_F  
ggplot(JobData, 
       aes(x = AGE_12_F)) +
  geom_bar(colour = "black", fill = "light blue") + 
  labs(title = "Distribution of Age", 
       x = "Age Group",
       y = "Count")

## EDUC_F  
ggplot(JobData, 
       aes(x = EDUC_F)) +
  geom_bar(colour = "black", fill = "light blue") + 
  labs(title = "Distribution of Level of Education", 
       x = "Level of Education",
       y = "Count")

## PROV_F  
ggplot(JobData, 
       aes(x = PROV_F)) +
  geom_bar(colour = "black", fill = "light blue") + 
  labs(title = "Distribution of Province", 
       x = "Province",
       y = "Count")


#' Note: in practice, you do NOT need to generate bar charts and count statistics 
#' because they tell you the same information. However, we are asking you to 
#' generate both in this activity for practice. 


# Generating numeric descriptive statistics for the continuous variable 

JobData %>% #Data
  select(HRLYEARN) %>% #Selecting continuous variable
  describe() #Generating descriptive statistics

#Generating visualization

## Academic stress 
ggplot(JobData, 
       aes(x = HRLYEARN)) +
  geom_histogram(colour = "black", fill = "light blue", bins = 30) + 
  labs(title = "Distribution of the Hourly Wages of Employees", 
       x = "Hourly Wages of Employees ($)",
       y = "Count")

#Generating bivariate descriptive statistics 
describeBy(JobData$HRLYEARN, JobData$EDUC_F)

#Adjusting labels for plotting 
JobData<- JobData %>% #Data
  mutate(
         EDUC_F = factor(EDUC,
                         levels = c(0, 1,2,3,4,5,6),
                         labels = c("Grade school","Some high school", "High school graduate",
                                    "Some postsecondary", "Postsecondary certificate or diploma",
                                    "Bachelor's degree", "Above bachelor's degree")))

#Generating a theme 
MyTheme<- theme(plot.title = element_text(family = "Times", size = 12, face = "bold", hjust = 0.5),
                axis.title =  element_text(family = "Times", size = 12),
                axis.text = element_text(family = "Times", size = 10),
                panel.background = element_blank(),
                panel.border = element_rect(colour = "black", fill = NA),
                axis.text.x = element_text(),
                legend.position = "none"
                )

#Generating boxplots
ggplot(JobData, aes(x = HRLYEARN, y = EDUC_F, fill = EDUC_F)) +
  geom_boxplot() + 
  labs(title = "The Distribution of Hourly Wages by Education Level",
       x = "Hourly Wage ($)",
       y = "Education Level") + 
  scale_fill_manual(values = c("#803E75","#0057E9", "#FFB300", "#8FBC8F", "#E66100", "light green", "#DC267F")) +
  MyTheme
  


#Write-up 

#' A researcher was interested in investigating whether the hourly wages of 
#' employees in Canada are related to their level of education. This data consists 
#' of 54,712 males and 58,007 females participants from Ontario (n = 36,324), 
#' Quebec (n = 20,443), British Columbia (n = 14,043),  Alberta (n = 8,602), 
#' Manitoba (n = 8,064), Saskatchewan (n = 6,632), New Brunswick (n = 5,704),  
#' Nova Scotia (n = 5,404), Newfoundland and Labrador (n = 5,114) and 
#' Prince Edward Island (n = 2,389). There was a relatively even distribution of 
#' participants between the ages of 15 and 19 (n = 7,656), 20 and 24 (n = 7,168), 
#' 25 and 29 (n = 7,649), 30 and 34 (n = 8,732), 35 and 39 (n = 8,996), 40 and 44 
#' (n = 8,908), 45 and 49 (n = 8272), 50 and 54 (n = 8,201), 55 and 59 (n = 8,233), 
#' 60 and 64 (n = 9,793) and 65 and 69 (n = 9,182); however, their is a larger 
#' number of participants 70 and over (n = 19,929) compared to the other age groups. 
#' 
#' The highest level of education attained by participants was diverse, ranging 
#' from grade school (n = 4,213), some high school (n = 11,801), high school 
#' graduate (n = 21,513), some postsecondary (n = 6,261), postsecondary certificate 
#' or diploma (n = 37,456), bachelor's degree (n = 20,849) and above bachelor's degree 
#' (n = 10,626). There was considerable variability in the reported hourly ages of 
#' participants (SD = 19.16), with the median wage being $30 an hour. The 
#' distribution of hourly wages was highly positively skewed, which is typical 
#' for wage data (considering that some individuals have abnormally large wages 
#' compared to the remainder of the participants).
#' 
#' The average wage of individuals in Canada with a highest level of education of 
#' grade school (Med = 21.56, SD = 9.79), some high school (Med = 20.0, SD = 11.25), 
#' high school graduate (Med = 24.0, SD = 13.33) and some postsecndary (Med = 22.35, SD = 13.78) 
#' appears relatively similar. However, individuals with a postsecondary certificate or 
#' diploma (Med = 30.0, SD = 15.19), bachelor's degree (Med = 38.46, SD = 21.3) and 
#' above bachelor's degree (Med = 45.0, SD = 25.9) appear to have higher hourly wages 
#' than the remaining groups, with individuals with above a bachelor's degree earning the most. 
#' Therefore, it appears that hourly wages is related to level of education. 

#' Note: I reported the median because the data was highly skewed. Therefore, the 
#' median provides a better estimate of the center of the distribution. 
