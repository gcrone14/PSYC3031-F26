# Week 4 - Descriptives with R (Part 1)

# TOPIC 1 - Review

#' The steps for performing analyses in R (so far)
#' 
#' 1. Load the packages that you will need in that script into your current R session.
#'  
#'  * Recall: Installation only needs to happen ONCE, but loading packages
#'  happens every time you load a new R session.
#'  Also, always load tidyverse last.
#'    
#' 2. Load the data into R. 
#' 
#' 3. Check that the data has been correctly loaded into your R session. 
#' 
#' 4. Data cleaning.
#' 
#' 5. **Generate data visualizations and numeric descriptive statistics.**
#' (today's disucssion!)

#' **STEP 1: Load the packages that you will need in that script into your current R session.**

# Loading packages
library(psych)
library(here)
library(tidyverse)

#' **STEP 2: load hsb10.csv into R. Save the data in an object called hsb_dat**

# Load the data
hsb_dat <- read.csv(file = here("Data/hsb10.csv"))

#' **STEP 3: check hsb_dat to ensure that the data has correctly been loaded into R**
# Check structure
str(hsb_dat)
glimpse(hsb_dat)
as_tibble(hsb_dat)


#' **STEP 4: clean hsb_dat**

#' **TASK: Complete the following cleaning tasks. You may (and are highly**
#' **encouraged!) to use pipes (|>), as discussed in Week 2.**
#'    
#'    * 1) Select the prog, female, write and math variables from hsb_dat,
#'    * 2) Change the structure of the female and prog variables to factor in hsb_dat.
#'    * Relabel the levels of the female and prog variables in hsb_dat so that: 
#'        
#'        * 1 = female, 0 = male 
#'        * 1 = general, 2 = academic and 3 = vocational
#'        
#'    * 3) Extract the participants with writing scores in hsb_dat 
#'    (i.e., no missing data on the writing variable).
#'    * 4) Save the final result to an object called hsb_dat_revised.
#'    (It may be a data.frame or tibble; the choice is yours.)



#' Please attempt before seeing the answer (below)

# ANS ----
hsb_dat_revised <- hsb_dat |>
  select(prog, female, write, math) |>
  mutate(female = factor(female, 
                         levels = c(1, 0), 
                         labels = c("female", "male")),
         prog = factor(prog,
                       levels = 1:3,
                       labels = c("general", "academic", "vocational"))) |>
  filter(!is.na(write))

#-----





# TOPIC 2 - Numeric Descriptive Statistics (Univariate)

#' Descriptive stats are useful to get a sense of one's data,
#' and relationships within it.
#'
#' Purpose:
#'    * Getting an understanding of your data (e.g., central tendency, variability and shape)
#'    * Shed light on the research questions/hypotheses 
#'    * See issues with the data (e.g., data entry error)
#'    
#' Numeric descriptives (univariate): 
#' 
#' Functions: 
#' * base::summary() - generate count information for categorical data and 
#' basic estimates (e.g., mean) for continuous data
#' * psych::describe() - generate various estimates for continuous data (e.g., mean, 
#'                       median, sd, variance, range, etc.)


# Categorical variables



# Univariate (summary statistics for one variable): 

#' Function: 
#' 
#' * base::table()
#' 
#' General format:
#' 
#' * table(DataName$VariableName)
#'
#' **Note: the variable must be a factor to generate count information.**
#' **If it is a character, the function WILL NOT WORK WELL**

#' **Best


#' **TASK: generate count information for the prog variable in hsb_dat_revised.**
#' **Then, describe what the output shows in 1-2 sentences.**


#' **TASK: generate count information for the female variable in hsb_dat_revised.**
#' **Then, describe what the output shows in 1-2 sentences.**


#' *Note: if you specify the data in the function, descriptive information will be generated for all variables*



## QUESTION: how could I generate count information for both the female and prog 
##           variable at the same time?



# Continuous variables


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

#' **TASK: generate estimates for the write variable using both the **
#' **describe() and summary functions.**
#' ** Provide a brief interpretation of the output.**






#' **Breakdown of describe()'s output:**
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

#' Breakdown of summary()'s output: 
#' 
#' * Min. = minimum score
#' * 1st Qu. = first quartile (i.e., 25th percentile)
#' * Median = middle value 
#' * Mean = average 
#' * 3rd Qu. = third quartile (i.e., 75th percentile)
#' * Max. = maximum score

#' *Note: if you specify the data in the function, descriptive information will be generated for all variables*


## QUESTION: how could I generate descriptive statistics for both the write and math 
##           variables at the same time?



##' Final Note: do NOT generate descriptive statistics on an identification variable (e.g., id).
##' An identification variable is when each participant in the data is given a unique 
##' value. This variable is not included in data for the purpose of analysis. 



# TOPIC 3 - Introduction to Data Visualization

#' Data visualizations are visual displays of data, including: 
#' 
#' * Histograms 
#' * Bar charts
#' * Infographics 
#' * etc. 
#' 
#' Data visualizations can be used to: 
#' 
#' * Get an understanding of your data (e.g., central tendency, variability and shape)
#' * Shed light on the research questions/hypotheses (e.g., comparisons between groups)
#' * Issues with data (e.g., data entry errors)
#' * For presentation (e.g., creating a graph to communicate a message to a particular audience)
#' * For exploration (e.g., creating a graph to check the assumption of normality)
#' 
#' Example 1:

#' **Note: in this example, anxiety scores can only range from 1-7**

#Creating a hypothetical data
anxiety_dat <- data.frame(ID = 1:100,
                         anxiety = sample(1:7, 100, replace = TRUE)
                         )

# Creating a data entry issue (i.e., including an anxiety score that is
# impossible, since anxiety here only ranges from 1-7)
anxiety_dat[100,2] <- 20 # Imputes 20 into existing datum (NEVER do this IRL!)

# Generating a histogram to display the distribution of anxiety
# (How to generate code will be discussed in a bit...)
anxiety_dat |>
  ggplot(aes(x = anxiety)) + 
  geom_histogram(bins = 14, colour = "black", fill = "light blue") + 
  labs(title = "Distribution of Anxiety Scores", x = "Anxiety Scores", y = "Count") + 
  annotate("point", x = c(20.25), y = 1.6, size = 14, shape = 1, color = "red")


#' When creating a graph it is important to think about:
#' 
#' * Purpose
#' * Intended audience
#' * The best way to achieve purpose given the audience
#' 
#' Example 2: 

# Histogram
# (How to generate code will be discussed LATER)
hsb_dat_revised |>
  ggplot(aes(x = write)) + 
  geom_histogram(bins = 3,
                 colour = "black") + 
  facet_grid(~female)

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
#' Data |>
#'   ggplot(aes(x = X_VARIABLE, y = Y_VARIABLE)) + 
#'   geom_GraphType()
#' 
#' * data: the data 
#' * aes(): how the data will be plotted on your graph,
#' i.e., where you specify your x (and sometimes also y) variable.
#' 
#' * geom_GraphType(): the type of geom (i.e., GRAPH) you're using.
#' 
#' Note: There are other functions you can add to this code to customize 
#' the graph further. Today you will be introduced to the basic elements of 
#' data visualization with ggplot2. Next week, 
#' we will look at how we can enhance basic data visualziations.
#'
#'
#' **QUESTION: what are the basic components of ANY graph?**


# Categorical variables

#' Note: The easiest way to plot a single categorical variable is with a bar chart;
#' however, there are other ways to plot categorical variables that we will not review
#' today. 

# Breaking down the code for creating a bar chart

#' Note: the variable should be factor or character

###
hsb_dat_revised |> 
  ggplot() ## Creates the plotting area 

###
hsb_dat_revised |>
  ## Specifies what the variable on the (eventual) x-axis will be
  ggplot(aes(x = prog))

###
hsb_dat_revised |>
  ggplot(aes(x = prog)) +
  geom_bar() ## Plots the data from x variable into a bar chart.

#' **KEY NOTE (PLEASE STOP AND READ THIS):**
#' **As soon as you call ggplot(), you MUST follow up every**
#' **new plotting command with +, NOT A PIPE (|>).**
#' 
#' **This is an extremely common mistake.**

#' For example, do NOT do this:
# hsb_dat_revised |>
#   ggplot(aes(x = prog)) |> # PIPE = Incorrect
#   geom_bar()

#' But instead this:
hsb_dat_revised |>
  ggplot(aes(x = prog)) +  # + sign = Correct
  geom_bar()

###
hsb_dat_revised |>
  ggplot(aes(x = prog)) +
  geom_bar() + 
  # labs()lets you add axis labels
  labs(
    title = "Distribution of Program Type",
    x = "Program Type",
    y = "Count")

### Final code ###
hsb_dat_revised |>
  ggplot(aes(x = prog)) +
  geom_bar(colour = "black", fill = "light blue") + ## Changes the fill and outline colour of the bars
  labs(title = "Distribution of Program Type", 
       x = "Program Type",
       y = "Count")

# Continuous variables



#' Note: There are MANY ways you can plot a continuous variable. I will only be 
#' demonstrating one method today. 


# Breaking down the code for creating a histogram

#' Note: the variable should be numeric or integer

#' **KEYNOTE: Whenever you plot a histogram, please please please**
#' **specify the bins argument (# of bins to plot), or the binwidth**
#' **argument (binwidth) Otherwise, it'll**
#' **default to a value that's often un-ideal.**

# FINAL CODE
hsb_dat_revised |>
  ggplot(aes(x = write)) +
  # Specifies a histogram
  geom_histogram(colour = "purple", fill = "pink",
                 binwidth = 5) +
  # Label axes
  labs(title = "Distribution of Writing Scores",
       x = "Writing scores",
       y = "Count")

# TOPIC 4 - Ticket Out-the-Door
#' The ticket-out-the-door is a live activity to practice content from
#' today's lecture, and figure out where students need additional help.
#' It also serves as excellent preparation for the class assignments
#' (especially data viz and the final assignment.)
#' 

#' **NOTE: The data to use is starbucks_items.csv.** 
#' This file cannot be found directly, but its 
#' constituent data files can be found on this Kaggle page:
#' https://www.kaggle.com/datasets/starbucks/starbucks-menu

#' What I did was import the data from online, and merge two data
#' sets from the base files:
#' starbucks-menu-nutrition-drinks.csv (drinks), and
#' starbucks-menu-nutrition-food.csv (food).

#' The columns are as follows:
#' * item = The name of the item
#' * type = Whether the item is a food (Food) or a drink (Drink)
#' * calories = # of Calroies in the item
#' The other variables are other nutritional info, including
#' fat (g), carb (g), fiber (g), protein (g), sodium (mg),
#' and protein (g).

#' Your goal for the ticket-out-the-door is to apply the skills
#' you've learned so far to DESCRIPTIVELY address the research
#' question: Do items with more sugar (carbs) also tend to
#' contain more fat? Is there a difference in this relationship
#' based upon whether an item is a food or drink?

#' Generate relevant descriptive statistics and at least one
#' informative plot to answer these questions. You might find
#' it useful to follow the steps we've outlined thus far to
#' assist with this task.

# EXTRA PRACTICE 

#' *Note: continue from the Week 2 Extra Practice R script*

#' 1. Extract the cereals where the manufacture is either General Mills or 
#' Kelloggs. Save this data as a new object called cereal_dat_final.
#' 2. Generate the count information for the number of General Mills and 
#' Kelloggs cereals. Generate a graph to supplement this information.
#' 3. Generate important univariate descriptive statistics for the rating 
#' variable. Generate a graph to supplement this information.
#' 4. Provide a summary of the variables following APA formatting.

#' *Note: the answers will be reviewed next class*