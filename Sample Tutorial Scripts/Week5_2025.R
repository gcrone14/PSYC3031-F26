# Week 5 - Descriptives with R (Part 2)



# TOPIC 1 - Review



#' **QUESTION: What are the steps for performing analyses in R (so far)** 









#' **STEP 1: Load the packages that you will need in that script into your current R session.**



#Loading packages
library(psych)
library(here)
library(tidyverse)



#' **STEP 2: load hsb10.csv into R. Save the data in an object called hsb10Data**



#Load the data
hsb10Data<- read.csv(file = here("Data", "hsb10.csv"))



#' **STEP 3: check hsb10Data to ensure that the data has correctly been loaded into R**



#' Note: view() and str() can be used to check that your data has been correctly 
#' loaded into R. PLEASE DO NOT INCLUDE THESE FUNCTIONS IN YOUR FINAL R SCRIPT 
#' UNLESS EXPLICTLY ASKED IN THE INSTRUCTIONS



#Checking hsb10Data
view(hsb10Data)

#Checking the structure of the variables in hsb10Data
str(hsb10Data)



#' **STEP 4: clean hsb10Data**



#Completing the data cleaning tasks in one chunk of code
hsb10DataRevised<- hsb10Data %>% 
  
  select(prog, female, write, math) %>% #Extracting the variables 
  
  mutate(f_female = factor(female, levels = c(1, 0), labels = c("female", "male")),
         f_prog = factor(prog, levels = c(1, 2, 3), labels = c("general", "academic", "vocational"))) %>% #Changing the structure of the variables and relabeling the levels
  filter(!is.na(write)) #Extracting participants that have writing scores

#Checking data 

#' Note: colnames(), str() and summary() can be used to check that your code is 
#' correct. PLEASE DO NOT INCLUDE THESE FUNCTIONS IN YOUR FINAL R SCRIPT UNLESS 
#' EXPLICTLY ASKED IN THE INSTRUCTIONS

colnames(hsb10DataRevised) ### Check to see the columns in the data
str(hsb10DataRevised$f_female) ### Check to see the data type of the f_female variable
str(hsb10DataRevised$f_prog) ### Check to see the data type of the f_prog variable
summary(hsb10DataRevised$write) ### Check to see if there is missing data on the write variable



# TOPIC 2 - Numeric Descriptives (Bivariate)



#' Descriptive statistics provide researchers with a preliminary understanding of their data. 
#'
#' Purpose:
#'    * Getting an understanding of your data (e.g., central tendency, variability and shape)
#'    * Shed light on the research questions/hypotheses 
#'    * See issues with the data (e.g., data entry error)
#'    
#' Numeric descriptives (bivariate): 
#' 
#' Functions: 
#' * dplyr::group_by() - creates a grouping variable 
#' * dplyr::summarise() - allows you to generate summary statistics of a variable
#' * psych::describeBy() - generates various estimates for continuous data BY a grouping 
#'                         variable (that is categorical)


# Categorical variables



# Bivariate (summary statistics for one variable by another variable): 

#' Function: 
#' 
#' * dplyr::group_by()
#' * dplyr::summarise()
#' 
#' General format:
#' 
#' * Data %>% 
#' group_by(ColName = GroupingVariable1, ColName = GroupingVariable2) %>% 
#' summarise(ColName = SummaizingMethod)
#'
#' *Note: The variable must be a factor or character to generate count information* 



#' **TASK: Generate count information for the prog variable by sex**


#Generating count information for the prog variable by sex
hsb10DataRevised %>% #Data
  group_by(Program = f_prog, Sex = f_female) %>% #Specifying the categorical grouping variables
  summarise(Count = n()) #Generating the count information


#' **QUESTION: How would I interpret this output?** 



#Continuous variables



# Bivariate: 



#' Functions:
#' 
#' * psych::describeBy() (recommended)
#' * base::describe() 
#' * dplyr::group_by()
#' * dplyr::summarise()
#' 
#' General format:
#' 
#' * describeBy(DataName$ContiniousVariable, DataName$GroupingVariable)
#' 
#' * Data %>%
#' group_by(ColName = DataName$GroupingVariable) %>%
#' summarise(ColName = describe(ContiniousVariable))
#' 
#' *Note: the grouping variable must be a factor or character. The variable that* 
#' *you want to generate descriptive statistics for must be numeric or integer* 

#' **TASK: Generate descriptive statistics for the writing variable by the program variable.**

#METHOD 1: Generating descriptive statistics for the writing variable by the program variable
describeBy(hsb10DataRevised$write, hsb10DataRevised$f_prog)

#METHOD 2: Generating descriptive statistics for the writing variable by the program variable
hsb10DataRevised %>% #Data
  group_by(f_prog) %>% #Grouping variable
  summarise(describe(write)) #Generating descriptive statistics for the write variable


#' **QUESTION: How would I interpret this output?** 


# TOPIC 3 - Data Visualization



# Two categorical variables




#' Note: The easiest way to plot multiple categorical variables is with a bar chart;
#' however, there are other ways to plot categorical variables that we will not review
#' today. 

#Breaking down the code for creating a bar chart

#' Note: the variable should be factor or character

###
ggplot(hsb10DataRevised) ## Creates the plotting area 

###
ggplot(hsb10DataRevised, 
       aes(x = f_prog, fill = f_female)) ## Creates the y and x-axis  

###
ggplot(hsb10DataRevised, 
       aes(x = f_prog, fill = f_female)) +
  geom_bar() ## Plots the data using a bar chart


###
ggplot(hsb10DataRevised, 
       aes(x = f_prog, fill = f_female)) +
  geom_bar() + 
  labs(title = "Distribution of Program Type by Sex", ## Changes the axis labels and plot title
       x = "Program Type",
       y = "Count")

###

#### Stacked
ggplot(hsb10DataRevised, 
       aes(x = f_prog, fill = f_female)) +
  geom_bar(position = "stack") + ## Changes the type of bar chart
  labs(title = "Distribution of Program Type by Sex", 
       x = "Program Type",
       y = "Count")


#' **QUESTION: How would I interpret this output?** 


#### Fill
ggplot(hsb10DataRevised, 
       aes(x = f_prog, fill = f_female)) +
  geom_bar(position = "fill", colour = "black") + ## Changes the type of bar chart
  labs(title = "Ratio of Program Type by Sex", 
       x = "Program Type",
       y = "Proportion")


#' **QUESTION: How would I interpret this output?** 


#### Dodge
ggplot(hsb10DataRevised, 
       aes(x = f_prog, fill = f_female)) +
  geom_bar(position = "dodge") + ## Changes the type of bar chart
  labs(title = "Distribution of Program Type by Sex", 
       x = "Program Type",
       y = "Count")


#' **QUESTION: How would I interpret this output?** 


#### facet_grid()
ggplot(hsb10DataRevised, 
       aes(x = f_prog)) +
  geom_bar() + 
  labs(title = "Distribution of Program Type by Sex", 
       x = "Program Type",
       y = "Count") + 
  facet_grid(~f_female) ## Creates a grid layout by the grouping variable


#' **QUESTION: How would I interpret this output?** 


# One categorical, one continuous variable



#' Note: There are MANY ways you can plot a continuous variable. I will only be 
#' demonstrating some methods today. 


#Histogram


#' Note: the plotting variable should be numeric or integer. The grouping variable 
#' should be a factor or character. 

###
ggplot(hsb10DataRevised) ## Creates the plotting area 

###
ggplot(hsb10DataRevised,
       aes(x = write)) ## Creates the y and x-axis  

### 
ggplot(hsb10DataRevised,
       aes(x = write)) + 
  geom_histogram(bins = 4, colour = "black", fill = "light blue") ## Plots the data using a histogram

### 
ggplot(hsb10DataRevised,
       aes(x = write)) + 
  geom_histogram(bins = 4, colour = "black", fill = "light blue") +
  facet_grid(~f_prog) ## Plots the data by the grouping variable

### 
ggplot(hsb10DataRevised,
       aes(x = write)) + 
  geom_histogram(bins = 4, colour = "black", fill = "light blue") +
  facet_grid(~f_prog) +
labs(title = "Distribution of Writing Scores by Program", ## Changes the axis labels and plot title
     x = "Writing Scores",
     y = "Count")


#Boxplot 


###
ggplot(hsb10DataRevised) ## Creates the plotting area 

###
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write)) ## Creates the y and x-axis  

### 
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write)) + 
  geom_boxplot() ## Plots the data using a boxplot

### 
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Writing Scores by Program", ## Changes the axis labels and plot title
       x = "Program Type",
       y = "Writing Scores")


#' **Components** 
#' 
#' * Middle line: Median 
#' * Top and bottom of box: 25th and 75th percentile
#' * Box: the interquartile range (the range, not including scores beyond the 25th and 75th precentile)
#' * Points: outliers (any data points beyond the whiskers)
#' * Whiskers: the minimum and maximum values (not including outliers)


#Violin plot 


###
ggplot(hsb10DataRevised) ## Creates the plotting area 

###
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write)) ## Creates the y and x-axis  

### 
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write)) + 
  geom_violin() ## Plots the data using a violin plot

### 
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write)) + 
  geom_violin() + 
  labs(title = "Distribution of Writing Scores by Program", ## Changes the axis labels and plot title
       x = "Program Type",
       y = "Writing Scores")


# Two continuous


#' Note: The easiest way to plot two continious variables is with a scatterplot;
#' however, there are other ways to plot two continious variables that we will not review
#' today. 

###
ggplot(hsb10DataRevised) ## Creates the plotting area 

###
ggplot(hsb10DataRevised,
       aes(x = math, y = write)) ## Creates the y and x-axis  

### 
ggplot(hsb10DataRevised,
       aes(x = math, y = write)) + 
  geom_point() ## Plots the data using a scatterplot

### 
ggplot(hsb10DataRevised,
       aes(x = math, y = write)) + 
  geom_point() + 
  labs(title = "Relationship Between of Writing and Math Scores", ## Changes the axis labels and plot title
       x = "Math Scores",
       y = "Writing Scores")

###
ggplot(hsb10DataRevised,
       aes(x = math, y = write)) + 
  geom_point() + 
  labs(title = "Relationship Between of Writing and Math Scores", 
       x = "Math Scores",
       y = "Writing Scores") + 
  geom_smooth(method = "lm", se = FALSE) ## Plots the line of best fit


#' **QUESTION: How would I interpret this output?** 


# TOPIC 4 - Advanced Data Visualization

#' You may want to enhance your data visualization depending on its purpose. For example,
#' you would want to make the data visualization presentable if it was going to be 
#' presented at a conference.


### Multiple geoms

#' You can layer geoms. Whatever geom appears last in your code will be layered on top
#' of the previous.

### Before 
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")

### After 
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write)) + 
  geom_violin() + ### Adding a violin plot
  geom_boxplot() + 
  geom_jitter() + ### Adding points (that are scattered)
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")

### Adjusting the plot further 
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write)) + 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")


### Elements 

#' alpha = adjusts the opacity 
#' width = adjusts the width of the bars, boxes, etc.

### Adjusting the colours (default)
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")

### Adjusting the colours (manual)
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  scale_fill_manual(values = c("lightblue", "#6879D0", "#75909C")) + ## Setting the colours manually
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")

### Adjusting the colours (palette)


## Package (Example): RColorBrewer
library(RColorBrewer)

#' Examples:
#' 
#' * Set1
#' * Set2
#' * Pastel1
#' * Paired
#' * Dark2

ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  scale_fill_brewer(palette = "Pastel1") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")

## Colours and Accessibility  

### Colour oracle (https://colororacle.org/) - Demonstration

### Bad graph
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  scale_fill_brewer(palette = "Set1") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")

### Good graph 
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  scale_fill_manual(values = c("#803E75","#0057E9", "#FFB300")) + 
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")

### Package (Example): RColorBrewer

#See all the colour friendly pallets from the RColorBrewer package
display.brewer.all(colorblindFriendly = TRUE)

#Changing colour pallet
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  scale_fill_brewer(palette = "Blues") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")




### Themes

#' You can adjust the non-data details of your data visualization through a theme. There are a variety
#' of pre-created themes, including: 
#' 
#' * theme_bw()
#' * theme_light()
#' * theme_minimal()
#' * theme_classic()
#' * theme_void()

## Pre-created theme 

### Before 
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  scale_fill_brewer(palette = "Blues") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")

### After
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  scale_fill_brewer(palette = "Blues") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")+
  theme_light() ## Adding theme

## Customize your own theme 

#' You can also customize your graph by creating your own theme.
#' 
#' General format: 
#' 
#' * theme(
#' GraphComponent = ComponentElement(arguments)
#' )
#' 
#' GraphComponent: the component of the graph you are interested in changing (e.g., plot.title, axis.title.y, axis.text.x) 
#' ComponentElement: the aspect of the graph component that you are interested in changing (e.g., element_text, legend_position)
#' 
#' Note: there are a variety of ways to customize your graph beyond what will be shown
#' today. 


### Plot title 
MyTheme<- theme(
  plot.title = element_text(family = "times", size = 12, face = "bold", hjust = .5) #Adjusting the plot title font
)

### Before
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  scale_fill_brewer(palette = "Blues") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")

### After
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  scale_fill_brewer(palette = "Blues") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores") +
  MyTheme

### Axis titles  
MyTheme<- theme(
  plot.title = element_text(family = "times", size = 12, face = "bold", hjust = .5),
  axis.title.y = element_text(family = "times", size = 11, face = "italic"), #Adjusting the y-axis title font
  axis.title.x = element_text(family = "times", size = 11, face = "italic") #Adjusting the x-axis title font
)

### Graph
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  scale_fill_brewer(palette = "Blues") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores") + 
  MyTheme

### Axis elements  
MyTheme<- theme(
  plot.title = element_text(family = "times", size = 12, face = "bold", hjust = .5),
  axis.title.y = element_text(family = "times", size = 11, face = "italic"),
  axis.title.x = element_text(family = "times", size = 11, face = "italic"),
  axis.text.x = element_text(family = "times", size = 10), #Adjusting the x-axis text
  axis.text.y = element_text(family = "times", size = 10) #Adjusting the y-axis text
)

### Graph
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  scale_fill_brewer(palette = "Blues") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores") + 
  MyTheme

### Panel background and border  
MyTheme<- theme(
  plot.title = element_text(family = "times", size = 12, face = "bold", hjust = .5),
  axis.title.y = element_text(family = "times", size = 11, face = "italic"),
  axis.title.x = element_text(family = "times", size = 11, face = "italic"),
  axis.text.x = element_text(family = "times", size = 10),
  axis.text.y = element_text(family = "times", size = 10),
  panel.background = element_blank(), ## Removing the panel background 
  panel.border = element_rect(colour = "black", fill = NA) ## Adding a black border
)

### Graph
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  scale_fill_brewer(palette = "Blues") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores") + 
  MyTheme

### Removing the legend  
MyTheme<- theme(
  plot.title = element_text(family = "times", size = 12, face = "bold", hjust = .5),
  axis.title.y = element_text(family = "times", size = 11, face = "italic"),
  axis.title.x = element_text(family = "times", size = 11, face = "italic"),
  axis.text.x = element_text(family = "times", size = 10),
  axis.text.y = element_text(family = "times", size = 10),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "black", fill = NA),
  legend.position = "none" ## Removing legend
)

### Graph
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  scale_fill_brewer(palette = "Blues") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores") + 
  MyTheme

### Excluding missing data from a graph (continuous variable)

### Graph (with missing data)
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = math, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  scale_fill_brewer(palette = "Blues") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores") + 
  MyTheme

### Excluding missing data from a graph (grouping variable)

### Creating missing data for demonstration purposes
hsb10DataRevised[5, 6] <- NA
hsb10DataRevised[7, 6] <- NA
hsb10DataRevised[10, 6] <- NA

### Graph (with missing data)
ggplot(hsb10DataRevised,
       aes(x = f_prog, y = write, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1.5) +
  geom_boxplot(alpha = 0.9, width=0.1) + 
  geom_jitter(width = 0.2) + 
  scale_fill_brewer(palette = "Blues") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores") + 
  MyTheme

### Graph (without missing data)

#Creating the graph without the missing data on the f_prog variable 
remove_missing(hsb10DataRevised, vars = "f_prog") %>% ## Removing the missing data from f_prog 
  
### NOTE: you do NOT need to include the data object in the ggplot function because of the pipe operator (from the previous line of code)
  
  ggplot(aes(x = f_prog, y = write, fill = f_prog)) + ## Indicating that we want to adjust the fill by the elements of f_prog 
  geom_violin(alpha = 0.6, width=1) +
  geom_boxplot(alpha = 0.9, width=0.09) + 
  geom_jitter(width = 0.1) + 
  scale_fill_brewer(palette = "Blues") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores") + 
  MyTheme



## Beyond ggplot2

#' * R markdown
#' * Apps
#' * Posters
#' * Slides 
#' * And more!!!

personality_mod<- personality_mod[!personality_mod$age == 5555,]

# TOPIC 5 - Ticket Out-the-Door



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



# EXTRA PRACTICE 



#' *Note: continue from the same script from last weeks practice*

#' Generate the appropriate descriptive statistics and graphs to answer the research question. 
#' Provide an summary of the results using APA formatting. You can draw on information from last
#' weeks answer to supplement your response. 
#'   
#' Research question: does there appear to be a difference in the rating of General Mills and Kelloggs cereal?

#' *Note: the answers will be reviewed next class*