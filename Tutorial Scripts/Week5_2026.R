# Week 5 - Descriptives with R (Part 2)

#' BRAINSTORM:
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


# TOPIC 1 - Review

#' **QUESTION: What are the steps for performing analyses in R (so far)?**
#' 1. 
#' 2. 
#' 3. 
#' 4. 



 








#' **STEP 1: Load the packages that you will need in that script into your current R session.**

#Loading packages
library(psych)
library(here)
library(tidyverse)

#' **STEP 2: load hsb10.csv into R. Save the data in an object called hsb_dat**

#' **PAUSE:**
#' The hsb_dat we've been using feels too small and artifical,
#' so I think it's time to dip into the full data!
#' For simplicity, we'll still focus on 4 variables:
#' gender, prog, write, and math.
#' 
#' On eClass, please find the hsb10_full.csv file,
#' and import it into R.
hsb_dat <- read.csv(file = here("Data/hsb10_full.csv"))

#' **STEP 3: check hsb_dat to ensure that the data has correctly been loaded into R**
#' Do whichever method(s) you prefer!
str(hsb_dat)
glimpse(hsb_dat)
head(hsb_dat)

#' Note: view() is OK for personal use, 
#' but it should NEVER BE LEFT IN YOUR R SCRIPTS!

#' **STEP 4: clean hsb_dat**

#' Remember piping (|>)?

#' With pipes, you can perform all data cleaning operations 
#' in one go, so it's usually worth it to do!

#' (This code should look familiar; it's from last week!)
hsb_dat_revised <- hsb_dat |> 
  select(prog, female, write, math) |>
  mutate(female = 
           factor(female, 
                  levels = c(1, 0), 
                  labels = c("female", "male")),
         prog = factor(prog, 
                         levels = c(1, 2, 3), 
                         labels = c("general", "academic", "vocational")))

#' (I took out the filter cause the real data has no missingness)

#' Note: The various functions we used last week (e.g., table(), summary(),
#' dplyr::count(), psych::describe()) can be used to check if all the 
#' data cleaning operations work as expected.

#' You're encouraged to always check if a step you wrote in is working;
#' if not, you need to fix it on its own, otherwise the "pipe"-line breaks.

#' Here are some functions to give an idea of checking things with code:
colnames(hsb_dat_revised)      # Check the column (col) names
str(hsb_dat_revised$female)  # Check if female is a factor and has
                               # appropriately coded levels
str(hsb_dat_revised$prog)    # Do same as above with progress

#' **STEP 5: Explore your Data!**

#' We addressed some useful tools last week; today we will
#' simply expand your toolkit slightly for both descriptive
#' stats and plots.

# TOPIC 2 - Numeric Descriptives (Bivariate)

#' Descriptive statistics provide researchers with a preliminary 
#' understanding of their data. 
#' Purpose:
#'    * Getting an understanding of your data (e.g., central tendency, variability and shape)
#'    * Shed light on the research questions/hypotheses 
#'    * See issues with the data (e.g., data entry error)
#'    

#' We didn't discuss last week how to break up descriptive stats by 
#' a grouping variable, which is important to consider!

#' Functions: 
#' * dplyr::group_by() - creates a grouping variable 
#' 
#' General format:
#' Data |>
#'   select(variables_to_summarise) |>
#'   describeBy(Data$grouping_var_1)

#' * dplyr::summarise() - allows you to generate summary statistics of a variable
#' * psych::describeBy() - generates various estimates for continuous data 
#'                         by a grouping variable
#'                         
#' General format:
#' Data |>
#'   group_by(GroupingVariable1, GroupingVariable2) |>
#'   summarise(ColName = SummaizingMethod(summarized_variable))

#' NOTE: describeBy() is a LOT more powerful than either group_by() or 
#' summarise(), because it produces descriptive stats of many variables
#' at once, rather than for only the descriptive stats you explicitly
#' specify.

#' For example: 

#' Suppose you wanted to answer: how does one's mean writing score
#' differ based on one's high school program ("prog")?

# The describeBy() way:
describeBy(hsb_dat_revised$write, hsb_dat_revised$prog)

# the group_by() and summarise() way:
hsb_dat_revised |>
  group_by(prog) |>
  summarise(mean_prog = mean(write, na.rm = TRUE))

#' Notice something: the first method gives tons of info but can
#' appear a tad messy, whereas the second method gives specific
#' info, but is limited and becomes tedious to write with
#' more and more descriptive stats...

#' **TASK: Find the mean and SD of the math variable for different levels of**
#' **prog using describeBy().**

#' **TASK: Find the mean and SD of the math variable for different levels of**
#' **prog using group_by() followed by summarise().**

#' **TASK: Interpret the output in APA style.**
#' **Hint: Focus on means and SDs!**

#' Last thing: the count() function is very useful
#' for counting # of categorical variables present:
# dat |>
#   count(var1)

#' For example, if you want to know how many various programs people took,
#' you can do it with count:
hsb_dat_revised |>
  count(prog)

#' Say you want to sort from most-to-least?
#' Simply specify sort = TRUE
hsb_dat_revised |>
  count(prog, sort = TRUE)

#' If you want to see two variables at once, just tack on the second
#' variable inside of count:
# dat |>
#   count(var1, var2)

#' e.g., Show frequencies of gender and prog together
hsb_dat_revised |>
  count(prog, female)

#' **TASK: With the full data (hsb_dat), find the frequencies of ses AND prog** 


# TOPIC 3 - More Data Viz!

#' Today, we'll be going over different kinds of plots for
#' different types of data. Again, there are far more plots available
#' than we have time to cover, but don't feel restricted to only
#' use plots from class; the world is your oyster!

#' CAUTION: This is a whirlwind tour! Please ask q's as we go along,
#' and the main exercise/practice comes at the end of today's chat.
#' I'll put in breaks to space out content, but most of it just
#' re-iterates/elaborates on last week's material.

#' A brief review before getting started:
#' ANY ggplot2 graph is made with this basic template:
#' Data |>
#'   ggplot(aes(x = X_VARIABLE, y = Y_VARIABLE)) + 
#'   geom_GraphType() +
#'   OTHERSTUFF!!! +
#'   ... +
#'   ...

#' Most new content has to do with the OTHERSTUF!!!,
#' and changing up the geom_GraphType(). 
#' But if you have the basic code down, 
#' the sky is the limit!

### Categorical data plots with 2+ variables
#' The easiest way to plot multiple categorical variables is with a bar chart;
#' however, there are other ways to plot categorical variables that we will not review
#' today. 

###
hsb_dat_revised |>
  ggplot() # Creates the plotting area 

###
hsb_dat_revised |>
  ggplot(aes(x = prog, fill = female)) # Creates the y and x-axis  

###
hsb_dat_revised |>
  ggplot(aes(x = prog, fill = female)) +
  geom_bar() # Actually makes the bar plot!

###
hsb_dat_revised |>
  ggplot(aes(x = prog, fill = female)) +
  geom_bar() + 
  # Add label names
  labs(title = "Distribution of Program Type by Sex",
       x = "Program Type",
       y = "Count")

#### Stacked (default is already stacked)
hsb_dat_revised |>
  ggplot(aes(x = prog, fill = female)) +
  geom_bar(position = "stack") + # Changes the type of bar chart
  labs(title = "Distribution of Program Type by Sex",
       x = "Program Type",
       y = "Count")

# *Dodged bar chart (MY FAVORITE!)*
hsb_dat_revised |>
  ggplot(aes(x = prog, fill = female)) +
  geom_bar(position = "dodge") + # Changes the type of bar chart
  labs(title = "Distribution of Program Type by Sex",
       x = "Program Type",
       y = "Count")

# Add a fill
hsb_dat_revised |>
  ggplot(aes(x = prog, fill = female)) +
  geom_bar(position = "dodge", colour = "black") + # Changes the type of bar chart
  labs(title = "Ratio of Program Type by Sex", 
       x = "Program Type",
       y = "Proportion")


#' **QUESTION: How would you interpret this plot?** 



#' facet_wrap() allows you to make separate plots 
#' based on a grouping variable, for example...
hsb_dat_revised |>
  ggplot(aes(x = prog)) +
  geom_bar() + 
  labs(title = "Distribution of Program Type by Sex", 
       x = "Program Type",
       y = "Count") + 
  facet_wrap(~female) # Creates a grid layout by the grouping variable


#' **QUESTION: How would I interpret this output?** 


# One categorical, one continuous variable

#' Note: There are MANY ways you can plot a continuous variable. I will only be 
#' demonstrating some methods today. 


# Histogram


#' Note: the plotting variable should be numeric or integer. The grouping variable 
#' should be a factor or character. 

###
hsb_dat_revised |>
  ggplot() ## Creates the plotting area 

###
hsb_dat_revised |>
  ggplot(aes(x = write)) ## Creates the x-axis  

### 
hsb_dat_revised |>
  ggplot(aes(x = write)) + 
  geom_histogram(bins = 4, colour = "black", fill = "light blue") # Plots the data using a histogram

#' NOTE: You really should always specify the # of bins!

#' We can also break things up by prog
### 
hsb_dat_revised |>
  ggplot(aes(x = write)) + 
  geom_histogram(bins = 4, colour = "black", fill = "light blue") +
  # Breaks up plot by a grouping variable
  # (should be a factor)
  facet_wrap(~prog)

### 
hsb_dat_revised |>
  ggplot(aes(x = write)) + 
  geom_histogram(bins = 4, colour = "black", fill = "light blue") +
  facet_wrap(~prog) +
  # Add labels
  labs(title = "Distribution of Writing Scores by Program",
     x = "Writing Scores",
     y = "Count")

# Boxplots


###
hsb_dat_revised |> 
  ggplot() ## Creates the plotting area 

###
hsb_dat_revised |>
  # Creates the x and y-axes  
  ggplot(aes(x = prog, y = write))

### 
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write)) +
  # Plots the data using a boxplot
  geom_boxplot()

### 
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write)) + 
  geom_boxplot() +
  # Add labels
  labs(title = "Distribution of Writing Scores by Program", 
       x = "Program Type",
       y = "Writing Scores")


#' **Components** 
#' 
#' * Middle line: Median 
#' * Top and bottom of box: 25th and 75th percentile
#' * Box: the interquartile range (the range, not including scores beyond the 25th and 75th precentile)
#' * Points: outliers (any data points beyond the whiskers)
#' * Whiskers: the minimum and maximum values (not including outliers)

# Violin plots 

#' To quote The Office:
#' "NO GOD! NO GOD PLEASE NO. NO. NO.
#' NOOOOOOOOOOOO."

#' For an amusing video on why some people (including Gabe)
#' dislike boxplots, see here:
#' https://www.youtube.com/watch?v=_0QMKFzW9fw

#' In this course, I won't judge you for using them (you may),
#' but note that they are a somewhat controversial graphic:
#' some people love them, others don't (I'm in that group).

#' To make them, follow these steps:
#' 1) Make a regular geom_boxplot() graph (see above).
#' 2) Replace the geom_boxplot() call with
#' geom_violin()
#' 3) Add in after a geom_boxplot() call, making sure
#' to specify the width to be small (e.g., 0.2).

#' (I adjust the smoother with adjust = ...)
#' below.)

#' For example:
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write)) + 
  geom_violin(adjust = 0.7) + 
  geom_boxplot(width = 0.2) +
  labs(title = "Distribution of Writing Scores by Program", 
       x = "Program Type",
       y = "Writing Scores")

#' A MUCH BETTER alternative:
#' use density plots! 
#' They are designed to show distributions
#' nicely with clear shading!
hsb_dat_revised |>
  ggplot(aes(x = write, fill = prog)) + 
  # NEW GEOM: geom_density()
  # alpha = 0.4 makes the color fill
  # transluscent
  geom_density(alpha = 0.4) +
  labs(title = "Distribution of Writing Scores by Program",
       x = "Writing Scores",
       y = "Density")

#' You can (and SHOULD) adjust the
#' smoother for more (or less) 
#' resolution with the adjust = ... 
#' argument inside of geom_density:
hsb_dat_revised |>
  ggplot(aes(x = write, fill = prog)) + 
  geom_density(alpha = 0.4,
               # Adjust smoothing
               adjust = 0.8) +
  labs(title = "Distribution of Writing Scores by Program",
       x = "Writing Scores",
       y = "Density")

#' Another nice alternative: 
#' geom_ridgeline()!

#' You need to load in a new package:
#' ggridges:

# Install by running line:
# install.packages("ggridges")
# Load package
library(ggridges)

#' Same process as before, but YOU MUST
#' ensure that the y-variable is the grouping
#' one; it will error out otherwise.
hsb_dat_revised |>
  ggplot(aes(x = write, y = prog, fill = prog)) +
  geom_density_ridges(alpha = 0.7) + # Replace geom with geom_density_ridges()
  labs(
    title = "Distribution of Writing Scores by Program",
    x = "Writing Scores",
    y = "Program Type"
  )

# Two continuous variables

#' Note: The easiest way to plot two continuous variables is with a scatterplot.
#' (There are more ways which we won't discuss today, but I
#' encourage people to check them out.)

###
hsb_dat_revised |>
  ggplot() ## Creates the plotting area 

###
hsb_dat_revised |> 
  ggplot(aes(x = math, y = write)) # Creates the y and x-axis  

### 
hsb_dat_revised |>
  ggplot(aes(x = math, y = write)) + 
  geom_point() # Plots the data using a scatterplot

### 
hsb_dat_revised |>
  ggplot(aes(x = math, y = write)) + 
  geom_point() + 
  labs(title = "Relationship Between of Writing and Math Scores", 
       x = "Math Scores",
       y = "Writing Scores")

###
hsb_dat_revised |>
  ggplot(aes(x = math, y = write)) + 
  geom_point() + 
  labs(title = "Relationship Between of Writing and Math Scores", 
       x = "Math Scores",
       y = "Writing Scores") + 
  geom_smooth(method = "lm") # Plots the line of best fit
                             # with confidence bounds



#' **QUESTION: How would I interpret this output?** 


# TOPIC 4 - Advanced Data Visualization

#' You may want to enhance your data visualization depending on its purpose. 
#' For example, you would want to make the data visualization presentable 
#' if it was going to bepresented at a conference.


### Multiple geoms

#' You can layer geoms. Whatever geom appears last in your code 
#' will be layered on top of the previous.

### Before 
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")

### After
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write)) + 
  geom_boxplot() + 
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores") +
  geom_jitter(alpha = 0.1, width = 0.2)

#' geom_jitter() looks too busy, so I'll 
#' remove it from subsequent plots. It's good when data sets
#' aren't as large.

### Elements 

#' alpha adjusts the opacity 
#' width adjusts the width of the bars, boxes, etc.

# Adjusting the colours (default)
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write, fill = prog)) + ## Indicating that we want to adjust the fill by the elements of prog
  geom_boxplot(alpha = 0.9, width=0.3) + 
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")

# Adjusting the colours (manual)
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write, fill = prog)) + # Indicating that we want to 
                                                  # adjust the fill by the 
                                                  # elements of prog
  geom_boxplot(alpha = 0.9, width=0.3) + 
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

hsb_dat_revised |>
  ggplot(aes(x = prog, y = write, fill = prog)) + ## Indicating that we want to adjust the fill by the elements of prog 
  geom_boxplot(alpha = 0.9, width=0.3) + 
  scale_fill_brewer(palette = "Pastel1") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")

## Colours and Accessibility  

### Colour oracle (https://colororacle.org/) - Demonstration

### Graph that isn't color-blind friendly
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write, fill = prog)) + ## Indicating that we want to adjust the fill by the elements of prog 
  geom_boxplot(alpha = 0.9, width=0.3) + 
  scale_fill_brewer(palette = "Set1") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")

### Improved
hsb_dat_revised |>
       ggplot(aes(x = prog, y = write, fill = prog)) + ## Indicating that we want to adjust the fill by the elements of prog
  geom_boxplot(alpha = 0.9, width=0.3) + 
  scale_fill_manual(values = c("#803E75","#0057E9", "#FFB300")) + 
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")

### Package (Example): RColorBrewer

# See all the colour friendly pallets from the RColorBrewer package
display.brewer.all(colorblindFriendly = TRUE)

#Changing colour pallet
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write, fill = prog)) + ## Indicating that we want to adjust the fill by the elements of prog 
  geom_boxplot(alpha = 0.9, width=0.3) +
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
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write, fill = prog)) + ## Indicating that we want to adjust the fill by the elements of prog
  geom_boxplot(alpha = 0.9, width=0.3) + 
  scale_fill_brewer(palette = "Blues") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")

### After
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write, fill = prog)) + ## Indicating that we want to adjust the fill by the elements of prog 
  geom_boxplot(alpha = 0.9, width=0.3) + 
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
my_theme <- theme(
  plot.title = element_text(size = 12, face = "bold", hjust = .5) # Adjusting the plot title font
)

### Before
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write, fill = prog)) +
  geom_boxplot(alpha = 0.9, width=0.3) + 
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores")

### After
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write, fill = prog)) +
  geom_boxplot(alpha = 0.9, width=0.3) + 
  scale_fill_brewer(palette = "Blues") + 
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores") +
  my_theme

### Axis titles  
my_theme<- theme(
  plot.title = element_text(size = 12, face = "bold", hjust = .5),
  axis.title.y = element_text(size = 11, face = "italic"), #Adjusting the y-axis title font
  axis.title.x = element_text(size = 11, face = "italic") #Adjusting the x-axis title font
)

### Graph
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write, fill = prog)) + ## Indicating that we want to adjust the fill by the elements of prog 
  geom_boxplot(alpha = 0.9, width=0.3) + 
  scale_fill_brewer(palette = "Blues") + ## Setting the colours using a palette from the RColorBrewer package
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores") + 
  my_theme

### Axis elements  
my_theme<- theme(
  plot.title = element_text(size = 12, face = "bold", hjust = .5),
  axis.title.y = element_text(size = 11, face = "italic"),
  axis.title.x = element_text(size = 11, face = "italic"),
  axis.text.x = element_text(size = 10), #Adjusting the x-axis text
  axis.text.y = element_text(size = 10) #Adjusting the y-axis text
)

### Graph
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write, fill = prog)) +
  geom_boxplot(alpha = 0.9, width=0.3) + 
  scale_fill_brewer(palette = "Blues") + 
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores") + 
  my_theme

### Panel background and border  
my_theme <- theme(
  plot.title = element_text(size = 12, face = "bold", hjust = .5),
  axis.title.y = element_text(size = 11, face = "italic"),
  axis.title.x = element_text(size = 11, face = "italic"),
  axis.text.x = element_text(size = 10),
  axis.text.y = element_text(size = 10),
  panel.background = element_blank(), ## Removing the panel background 
  panel.border = element_rect(colour = "black", fill = NA) ## Adding a black border
)

### Graph
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write, fill = prog)) + 
  geom_boxplot(alpha = 0.9, width=0.3) + 
  scale_fill_brewer(palette = "Blues") + 
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores") + 
  my_theme

### Removing the legend  
my_theme <- theme(
  plot.title = element_text(size = 12, face = "bold", hjust = .5),
  axis.title.y = element_text(size = 11, face = "italic"),
  axis.title.x = element_text(size = 11, face = "italic"),
  axis.text.x = element_text(size = 10),
  axis.text.y = element_text(size = 10),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "black", fill = NA),
  legend.position = "none" ## Removing legend
)

### Graph
hsb_dat_revised |>
  ggplot(aes(x = prog, y = write, fill = prog)) + 
  geom_boxplot(alpha = 0.9, width=0.3) + 
  scale_fill_brewer(palette = "Blues") + 
  labs(title = "Distribution of Writing Scores by Program",
       x = "Program Type",
       y = "Writing Scores") + 
  my_theme

# TOPIC 5 - Ticket Out-the-Door

#' **NOTE: The data to use is starbucks_items.csv.** 
#' This file cannot be found directly, but its 
#' constituent data files can be found on this Kaggle page:
#' https://www.kaggle.com/datasets/starbucks/starbucks-menu

#' The columns are as follows:
#' * item = The name of the item
#' * type = Whether the item is a food (Food) or a drink (Drink)
#' * calories = # of Calories in the item
#' The other variables are other nutritional info, including
#' fat (g), carb (g), fiber (g), protein (g), sodium (mg),
#' and protein (g).

#' **THE TASK:**
#' Import the starbucks data and clean it up by removing any items
#' for which the grams of fat or carbs are missing. Feel free
#' to also select which variables are of most interest.
#' 
#' Once you do so, pipe in the following lines
#' of code to create a new variable, called category, which
#' gives more detailed info regarding what an item includes.
#' (Comment it out and modify as needed before using.)

# YOUR_STARBUCKS_DATA |>
# mutate(category = case_when(
#   str_detect(item, "Frappuccino") ~ "Frappuccino",
#   str_detect(item, "Latte|Cappuccino|Macchiato|Mocha|Americano") ~ "Espresso drink",
#   str_detect(item, "Coffee|Cold Brew|Roast") ~ "Coffee",
#   str_detect(item, "Tea|Tazo|Teavana") ~ "Tea",
#   str_detect(item, "Refresher|Pink Drink|Violet Drink") ~ "Refreshers",
#   str_detect(item, "Sandwich|Panini|Wrap") ~ "Sandwich/wrap",
#   str_detect(item, "Salad") ~ "Salad",
#   str_detect(item, "Protein Box|Protein Bowl") ~ "Protein meal",
#   str_detect(item, "Muffin|Cookie|Croissant|Scone|Cake|Brownie|Doughnut|Loaf|Tart") ~ "Bakery",
#   TRUE ~ NA
# ))

#' After making this modification, remove any data that's missing
#' a category (i.e., where category is NA). Now, save that result
#' into a new object, and create a plot (or plots) to answer
#' the following research question:
#' Does the relationship between carbs and fat differ based on
#' the CATEGORY of each item?

#' Make sure that the plot(s) is (are) not too "busy" 
#' with regard to the category variable
#' (facet_wrap() is your friend!).

# EXTRA PRACTICE 

#' *Note: continue from the same script from last weeks practice*

#' Generate the appropriate descriptive statistics and graphs to 
#' answer the research question below. 
#' Then, provide an summary of the results using APA formatting.
#' You can draw on information from last
#' week's answer to supplement your response. 
#'   
#' Research question: "Does there appear to be a difference in the 
#' rating of General Mills and Kelloggs cereals?"

#' *Note: the answers will be reviewed next class*

