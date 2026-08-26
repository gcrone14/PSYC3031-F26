# Week 4 - Descriptives with R (Part 1)



# TOPIC 1 - Review



#' The steps for performing analyses in R (so far)
#' 
#' 1. Load the packages that you will need in that script into your current R session.
#'  
#'  * Note: only install packages that you have never installed before (that are 
#'    not pre-loaded into R; e.g., here, tidyverse and psych). If you have already 
#'    installed a package or it's automatically loaded in R, do NOT install the package.
#'    
#'  * Note: always load the tidyverse package last. 
#'    
#' 2. Load the data into R. 
#' 
#' 3. Check that the data has been correctly loaded into your R session. 
#' 
#' 4. Data cleaning.
#' 
#' 5. **Generate data visualizations and numeric descriptive statistics.**



#' **STEP 1: Load the packages that you will need in that script into your current R session.**



#Loading packages
library(psych)
library(here)
library(janitor)
library(tidyverse)



#' **STEP 2: load hsb10.csv into R. Save the data in an object called hsb10Data**



#Load the data
hsb10Data<- read.csv(file = here("Data", "hsb10.csv"))



#' **STEP 3: check hsb10Data to ensure that the data has correctly been loaded into R**



#Checking hsb10Data
view(hsb10Data)

#Checking the structure of the variables in hsb10Data
str(hsb10Data)



#' **STEP 4: clean hsb10Data**



#' **NEW DATA CLEANING TOOL** 

#' Pipe operator (%>%): a function in the tidyverse package that allows you to use 
#'                      the output produced from the object/function before the pipe 
#'                      operator in the subsequent function after the pipe operator. 
#'                      
#'                      (useful when you stack functions)


#' **Demonstration:**

#' * Your data cleaning tasks are: 
#'    
#'    * Extract the prog, female, write and math variables from hsb10Data,
#'    * Change the structure of the female and prog variables to factor in hsb10Data,
#'    * Relabel the levels of the female and prog variables in hsb10Data so that: 
#'        
#'        * 1 = female, 0 = male 
#'        * 1 = general, 2 = academic and 3 = vocational
#'        
#'    * Extract the participants with writing scores in hsb10Data (i.e., no missing data on the writing variable).



#' **No pipe operator:**



#Extracting the variables
hsb10DataDemo<- select(hsb10Data, prog, female, write, math)

#Changing the structure of the variables and relabeling the levels
hsb10DataDemo<- mutate(hsb10DataDemo,
                          f_female = factor(female, levels = c(1, 0), labels = c("female", "male")),
                          f_prog = factor(prog, levels = c(1, 2, 3), labels = c("general", "academic", "vocational"))
)

#Extracting participants that have writing scores
hsb10DataDemo<- filter(hsb10DataDemo, !is.na(write))


#' Note:
#' 
#' * Completing these tasks without the pipe operator required three chunks of code 
#'   containing some repetitive code (e.g., repeating hsb10DataDemo for each chunk of code).



#' **With pipe operator:**


#Completing the data cleaning tasks in one chunk of code
hsb10DataRevised<- hsb10Data %>% 
  
  select(prog, female, write, math) %>% #Extracting the variables 
  
  mutate(f_female = factor(female, levels = c(1, 0), labels = c("female", "male")),
         f_prog = factor(prog, levels = c(1, 2, 3), labels = c("general", "academic", "vocational"))) %>% #Changing the structure of the variables and relabeling the levels
  
  filter(!is.na(write)) #Extracting participants that have writing scores
  


#' Note:
#'    
#' * We were able to produce the same output using one chunk of code, without repeating 
#'   hsb10Data.
#' * We will be used the hsb10DataRevised object for the remainder of this tutorial.



# TOPIC 2 - Numeric Descriptives (Univariate)



#' Descriptive statistics provide researchers with a preliminary understanding of their data. 
#'
#' Purpose:
#'    * Getting an understanding of your data (e.g., central tendency, variability and shape)
#'    * Shed light on the research questions/hypotheses 
#'    * See issues with the data (e.g., data entry error)
#'    
#' Numeric descriptives (univariate): 
#' 
#' Functions: 
#' * base::summary() - generate count information for categorical data and basic estimates (e.g., mean) for continuous data
#' * psych::describe() - generate various estimates for continuous data (e.g., mean, 
#'                       median, sd, variance, range, etc.)



# Categorical variables



# Univariate (summary statistics for one variable): 

#' Function: 
#' 
#' * base::summary()
#' 
#' General format:
#' 
#' * summary(DataName$VariableName)
#'
#' *Note: the variable must be a factor or character to generate count information* 



#' **TASK: generate count information for the prog and female variables in hsb10DataRevised**

#Generating count information 
summary(hsb10DataRevised$f_prog)

#' *Interpretation: there are 4 participants in the applied general program, 4 participants* 
#' *in the academic program and 2 participants in the vocational program*

#Generating count information 
summary(hsb10DataRevised$f_female)

#' *Interpretation: there are 9 male participants and 1 female participant*


#' *Note: if you specify the data in the function, descriptive information will be generated for all variables*


#Generating descriptive information for all variables
summary(hsb10DataRevised)




## QUESTION: how could I generate count information for both the female and prog 
##           variable at the same time (taking advantage of the select function
##           and pipe operator)?














#Code to generate count information for f_female and f_prog
hsb10DataRevised %>%  #Data
  select(f_female, f_prog) %>% #Extracting columns
  summary() #Generating count information


#' Note:
#' 
#' * You do not need to input anything in summary() because the output 
#'   from select() is automatically being used in summary() via the pipe operator.




#Continuous variables



# Univariate: 



#' Functions:
#' 
#' * psych::describe() (recommended)
#' * base::summary() 
#' 
#' General format:
#' 
#' * describe(DataName$VariableName)
#' * summary(DataName$VariableName)
#' 
#' *Note: the variable must be numeric or integer to generate estimates* 

#' **TASK: generate estimates for the writing variable using the describe and summary function.**
#'      ** Provide a brief interpretation of the output.**

#Generate descriptive statistics using the describe function 
describe(hsb10DataRevised$write)

#' Output: 
#' 
#' * vars = variable number 
#' * n = number of participants 
#' * mean = average 
#' * sd = standard deviation 
#' * median = middle value 
#' * trimmed = trimmed mean (i.e., mean without the the lower and upper 5%)
#' * mad = median average deviation (variability measure)
#' * min = minimum score
#' * max = maximum score 
#' * range = max - min  
#' * skew = skewness 
#' * kurtosis = kurtosis 
#' * se = standard error (SD/sqrt(n))

#Generate descriptive statistics using the summary function 
summary(hsb10DataRevised$write)

#' Output: 
#' 
#' * Min. = minimum score
#' * 1st Qu. = first quartile (i.e., 25th percentile)
#' * Median = middle value 
#' * Mean = average 
#' * 3rd Qu. = third quartile (i.e., 75th percentile)
#' * Max. = maximum score

#' Interpretation: the mean writing score in the current sample is 50.9 (SD = 8.03).
#' The distribution appears relatively symmetrical based on the mean and median. 
#' 
#' *Note: your interpretation should be based on what information you think is relevant to your study/research hypothesis.*


#' *Note: if you specify the data in the function, descriptive information will be generated for all variables*

#Generating descriptive information for all variables
describe(hsb10DataRevised)

#Generating descriptive information for all variables
summary(hsb10DataRevised)



## QUESTION: how could I generate descriptive statistics for both the write and math 
##           variables at the same time (taking advantage of the select function
##           and pipe operator)?














#Code to generate count information for f_female and f_prog
hsb10DataRevised %>%  #Data
  select(write, math) %>% #Extracting columns
  describe() #Generating the descriptive statistics


#' Note:
#' 
#' * You do not need to input anything in describe() because the output 
#'   from select() is automatically being used in describe() via the pipe operator.

hsb10DataRevised %>% summarize(sd(write))
summary(hsb10DataRevised)

##' Final Note: do NOT generate descriptive statistics on an identification variable (e.g., ID).
##' An identification variable is when each participant in the data is given a unique 
##' value. This variable is not included in data for the purpose of analysis. 



# TOPIC 3 - Introduction to Data Visualization



#' Data visualizations are visual displays of data, including: 
#' 
#' * Histograms 
#' * Bar charts
#' * Info graphics 
#' * etc. 
#' 
#' Data visualizations can be used to: 
#' 
#' * Getting an understanding of your data (e.g., central tendency, variability and shape)
#' * Shed light on the research questions/hypotheses (e.g., comparisons between groups)
#' * Issues with data (e.g., data entry errors)
#' * For presentation (e.g., creating a graph to communicate a message to a particular audience)
#' * For exploration (e.g., creating a graph to check the assumption of normality)
#' 
#' Example 1:

#' **Note: in this example, anxiety scores can only range from 1-7**

#Creating a hypothetical data
AnxietyData<- data.frame(ID = 1:100,
                         AnxietyScores = sample(1:7, 100, replace = TRUE))

#Viewing the hypothetical data
view(AnxietyData)

#Creating a data entry issue (i.e., including an anxiety score of 30)
AnxietyData[100,2]<- 20

## This code is for demonstration purposes and will be broken down next week ##

#Generating a histogram to display the distribution of AnxietyScores
ggplot(AnxietyData, aes(x = AnxietyScores)) + 
  geom_histogram(bins = 14, colour = "black", fill = "light blue") + 
  labs(title = "Distribution of Anxiety Scores", x = "Anxiety Scores", y = "Count") + 
  theme(plot.title = element_text(face = "bold", family = "Times", hjust = 0.5),
        axis.title.x = element_text(face = "bold", family = "Times"),
        axis.title.y = element_text(face = "bold", family = "Times"),
        axis.text.x = element_text(face = "bold", family = "Times"),
        axis.text.y = element_text(face = "bold", family = "Times")) +
  annotate("point", x = c(20.25), y = 1.6, size = 19, shape = 1, color = "red")




#' When creating a graph it is important to think about:
#' 
#' * The purpose of your graph 
#' * The intended audience 
#' * The best way to achieve your purpose given your audience
#' 
#' Example 2: 

## This code is for demonstration purposes and will be broken down next week ##

#Generating histogram
ggplot(hsb10DataRevised, 
       aes(x = write)) + 
  geom_histogram(bins = 3,
                 colour = "black") + facet_grid(~f_female)

#' This code creates a histogram to display the distribution of writing scores for males 
#' and females. This graph would be appropriate if the purpose of this graph was to check 
#' the assumption of normality for the write variable across males and females. 
#' 
#' However, this graph would be horrible if it was intended to be published in a journal 
#' to show that there may be a difference in the writing scores of males and females! 
#' 
#' 
#' 
#' **QUESTION: Why would this graph not be good for this purpose?**
#' 
#' 
#' 
#' Therefore, when creating a graph (or completing your data visualization assignment),
#' thinking about these conceptual components of the graph is equally as important 
#' as determining the code to create the graph itself. 

#' Different graphs: 
#' 
#' * Histograms
#' * Boxplots
#' * Bar charts
#' * Density plots
#' * Violin plots
#' * Much more!!
#' 
#' Note: do NOT feel limited to using the graphs I am showing you today for the 
#' data visualization assignment.
#' 
#' Important package: 
#' 
#' * ggplot2 package (installed when you load the tidyverse package)
#' 
#' General format: 
#' 
#' * ggplot(Data, aes(x = , y = )) + geom_GraphType()
#' 
#' * data: the data 
#' * aes(): how the data will be plotted on your graph (i.e., on the x and/or y-axis) 
#' * geom_GraphType(): the type of graph
#' 
#' Note: these are the elements to create a basic graph. However, there are other function
#' you can add to this code to customize the graph further. Today you will be 
#' introduced to the basic elements of data visualization with ggplot2. Next week, 
#' we will look at how we can enhance basic data visualziations.
#'
#'
#' **QUESTION: what are the basic components of a graph?**




# Categorical variables




#' Note: The easiest way to plot a single categorical variable is with a bar chart;
#' however, there are other ways to plot categorical variables that we will not review
#' today. 

#Breaking down the code for creating a bar chart

#' Note: the variable should be factor or character

###
ggplot(hsb10DataRevised) ## Creates the plotting area 

###
ggplot(hsb10DataRevised, 
       aes(x = f_prog)) ## Creates the y and x-axis  

###
ggplot(hsb10DataRevised, 
       aes(x = f_prog)) +
  geom_bar() ## Plots the data using a bar chart


###
ggplot(hsb10DataRevised, 
       aes(x = f_prog)) +
  geom_bar() + 
  labs(title = "Distribution of Program Type", ## Changes the axis labels and plot title
       x = "Program Type",
       y = "Count")

### Final code ###
ggplot(hsb10DataRevised, 
       aes(x = f_prog)) +
  geom_bar(colour = "black", fill = "light blue") + ## Changes the fill and outline colour of the bars
  labs(title = "Distribution of Program Type", 
       x = "Program Type",
       y = "Count")



# Continuous variables



#' Note: There are MANY ways you can plot a continuous variable. I will only be 
#' demonstrating one method today. 


#Breaking down the code for creating a histogram

#' Note: the variable should be numeric or integer

###
ggplot(hsb10DataRevised) ## Creates the plotting area 

###
ggplot(hsb10DataRevised, aes(x = write)) +
  geom_histogram(colour = "purple", fill = "pink") +
  labs(title = "Distribution of Writing Scores",
       x = "Writing scores",
       y = "Count")
  ## ??

###
ggplot(hsb10DataRevised) ## ??


###
ggplot(hsb10DataRevised) ## ??

### Final code ###
ggplot(hsb10DataRevised) ## ??



# TOPIC 4 - Ticket Out-the-Door



#' This data was collected by Statistics Canada with the aim of getting a better 
#' understanding of the Canadian labor force. We are interested in investigating 
#' the relationship between hourly wage (HRLYEARN) and education (EDUC). 
#' 
#' This week you are responsible for providing a description of the sample and the 
#' important variables using descriptive statistics and visualizations. Use the 
#' steps of data analysis to guide your code. Your response should be written in 
#' paragraph format following APA guidelines.
#' 
#' Codebook: 
#' 
#' https://borealisdata.ca/api/datasets/export?exporter=html&persistentId=doi:10.5683/SP3/GFEQ3K#AGE_12
#' 
#' Reference: 
#' 
#' Statistics Canada, 2024, "Labour Force Survey, October 2024 [Canada]", https://doi-org.ezproxy.library.yorku.ca/10.5683/SP3/GFEQ3K, Borealis, V1, UNF:6:GAQpXRir5bZmoNo0l1d0cw== [fileUNF]
#' 




# EXTRA PRACTICE 




#' *Note: continue from the Week 2 Extra Practice R script*

#' 1. Extract the cereals where the manufacture is either General Mills or Kelloggs. Save this data as a new object called CerealsDataFinal.
#' 2. Generate the count information for the number of General Mills and Kelloggs cereals. Generate a graph to supplement this information.
#' 3. Generate important univariate descriptive statistics for the rating variable. Generate a graph to supplement this information.
#' 4. Provide a summary of the variables following APA formatting.

#' *Note: the answers will be reviewed next class*