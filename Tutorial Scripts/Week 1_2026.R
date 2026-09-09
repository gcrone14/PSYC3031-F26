# Week 1 - Introduction to R/RStudio (part 1)

#' ACKNOWLEDGEMENT: Thanks go to Victoria Celio
#' for providing the template for the present RScript.
#' Gabe has modified it to better suit this year's class.

#' On AI: When learning to code, AI can be your best friend or your
#' worst enemy.
#'
#' It can be your best friend because it can act as a personalized tutor:
#' answering questions, explaining concepts, and challenging you to practice
#' your skills.
#'
#' But over-relying on AI can also get in the way of learning. The most
#' effective learning comes from actively trying things yourself, making
#' mistakes, and working through problems. Don't ask AI to solve a problem
#' the moment you get stuck; give yourself a chance to figure it out.
#'
#' TL;DR: Use AI as a tutor, not a substitute for learning. Let it help you
#' think and practice rather than doing the hard work for you.

# TOPIC 1 - R and RStudio 

#' What is R and RStudio?
#' 
#'  * R is a coding language for stats.
#'  * RStudio provides users with a more user-friendly interface to interact 
#'  with R.
#'  
#'  * Analogy: R is the "car engine" and RStudio is the actual car.

#' * (Note: There are other "cars" to choose from, such as VSCode,
#'  but we'll be using R/Studio during tutorial.
#'  If you prefer another, by all means use it.)


#' RStudio layout

#' * Source editor: where you type out your RScripts, which are documents 
#' containing key R code. (This very document is an RScript!)
#' * Console: Where the code you run appears.
#' Also useful for "quick-and-dirty" coding.
#' * Environment/Files: where your saved objects appear
#' * Plots, packages, help: where your graphs, help documentation and 
#' graphs will appear. There are other tabs, too, you might find useful

#' *Note: the layout of the panels may appear differently on your computer*

#' Editing your user interface

#'  * Tools Ribbon -> Global Options
#'  
#' Changing the display color and font type/size:
#'  * Click "Appearance"
#'  
#' Changing the pane layout:
#'  * Click "Pane Layout"
#'  
#' Changing settings:
#'  * Click "General" 
#'  
#'  * Save workspace to .RData on exit: Never
#'  * Restore previously open-source documents at startup: deselect 
#'  * Restore .RData into workspace at startup:  deselect 
#'  * Save workspace to .RData on exit: select never
#'  
#' *Note: you only have to change these settings once*


#' Working Directories & Projects
#' * Working directory: Where on your current computer is drawing files from.
#' Think of it as a physical location. e.g., Imagine our class as a location 
#' on a computer. The working directory
#' might be: "/Canada/Ontario/York University/RS/Room 174"
#' Similarly, a working directory on your computer may be:
#' /Desktop/PSYC 3031/Tutorial
#' To get the working directory R is drawing files from:
# getwd()

#' * Project: a starting point for your working directory
#' 
#' 
#'    * Currently, you have no project created/opened
#'    * To check if a project is opened, look at the top right 
#'      corner of the screen (the little blue cube).

#' Creating a project:
#'  
#' 1. Create a file on your desktop (name this file PSYC3031)
#' 
#' 2. Create three files in the PSYC3031 file: 
#' 
#'  * Data
#'  * Scripts 
#'  * Output
#' 
#' 3. Cube with plus sign -> Existing Directory 
#' 
#' 4. Select the PSYC3031 folder located on your desktop
#' 
#' 5. Select "create project"
#'    
#' *Note: you do NOT have to save projects on your desktop.*
#' *They can be saved in another area of your computer.*

#' Creating an R script
#' 
#' * White box with a + (top right corner) -> R Script
#' 
#' * Result: an untitled R script is created and appears in the source pane. 
#' 
#' * Note: when the R script says Untitled, this means you have NOT saved your R script*
#'   
#' Saving an R script 
#'   
#'    * Mac: Command + S 
#'    * Windows: Control + S 
#'    * Press the save icon on the top left of your screen
#'
#' * Save your R scripts in PSYC3031 -> Script

#' **TASK: create an R Script and save it in the script file** 

# TOPIC 2 - Commenting

#' Comments: any text followed by a hashtag (i.e., # or #').
#' Think of it as an "off-switch" for a line of code.
#' Example of commented line vs. un-commented:
#' The next line will run, but the line after that will not run unless un-commented.
comment <- "I like stats!"
# commented_out_comment <- "I really like stats!"
 
#' 
#' * Purpose: to organize code
#' 
#' *Note: for assignments/activities, you will be expected to organize your code with comments* 

#' Different methods of commenting: 

# Put a hashtag before a single line to comment it out

#' Shortcut is extremely nice:
#'    * Mac: Command + Shift + C
#'    * Windows: Ctrl + Shift + C
#' 
#'  Useful because it enables you to un-comment many lines of code all
#'  in one shot! To do so, highlight many lines and hit the shortcut!

#' Hashtag with apostrophe - multiple lines
#' 
#' Hitting Enter after it will automatically put a new comment in the line
#' below.

#' (If curious: it's written in a markdown language, and is
#' useful if you want to render your RScripts as-is, to show
#' others what the code does on your own computer.)

# TOPIC 3 - Using R as a Calculator

#' Running code:
#' 
#' There are multiple ways to run a line of code: 
#' 
#' * Mac: command + enter 
#' * Windows: control + enter
#' * Hit the run button (please never do this.)


#' *Note: Your cursor should to be on the line of code you want to run OR you can* 
#'        *highlight the code you want to run.*
#'       
#' *Note: the output for your code will appear in your console.*

#' Operators: allows users to perform basic tasks in R
#' 
#' #' Examples: 
#' 
#' + Addition 
#' - Subtraction 
#' / Division 
#' * Multiplication 
#' ^ Exponent
#' < less than 
#' > greater than 
#' == equal to 
#' <= less than or equal to 
#' >= greater than or equal to 
#' != not equal to 
#' & and 
#' | or 
#' is.na() is missing data 

#' **TASK: run the code in your R script**

#Sum of 2+2 
2+2

#' **TASK: run the code in your R script**

# Determine if 4 is less than five
4<5

#' **TASK: run the code in your R script**

# Determine if four and six are less than five
4 + 6 < 5

# TOPIC 4 - Objects 
#' Object: a container to save output/values (e.g., numbers, words, etc.)
#' 
#' General Formatting:
#' 
#' object_name <- CODE DEFINING OBJECT
#' 
#' Displaying the output/values saved in the object: 
#' 
#' * Highlight the object name and run the code
#' * Type the object name on a different line in your script and run that new 
#'   line of code

#' Example:
Output <- 2 + 4 + 6 + 7
Output
Output2 <- Output + 4
Output2

#' Naming objects 
#' 
#' * Use a meaningful, specific name 
#' * Don't make the name too long/complicated 
#' * Recommended formats: 
#' 1) CamelCase: ObjectName OR 
#' 2) snake_case: object_name
#' 
#' Gabe likes snake_case more than CamelCase, but to each their own :)

#' **TASK: find the sum of 2 + 2, save the output in an object called sum_val, and print the output**


#' **TASK: save the value 6 into an object named Six and display the output**

#' *Notes:*




#' * Do NOT use the same name for different objects!
#' If you do so, you'll override the previous object
#' with the most recently saved one.
#' Example:
cool_quote <- "I love stats!"
cool_quote
cool_quote <- "Stats is cool!"
cool_quote

#' cool_quote gets overridden by the last object
#' it's saved as.

#' * R is case sensitive, e.g.,
#' val ≠ Val
val <- 5
Val <- 9
val == Val

# TOPIC 5 - Packages and Functions

#' Functions: pre-made code to complete operations in R
#' 
#'  * E.g., rather than computing the mean of a set of values by hand, there is a 
#'          function that can compute the mean for you (conveniently called mean()).
#'          
#'          Any function denoted by: function_name()
#'          e.g., sum(), mean(), min(), max(), etc...
#'          
#'          Inside the bracket, you'll specify the function's arguments:
#'          Basically, assigning values to the function to be evaluated.
#' e.g., sum() takes in a vector of numbers and provides the total,
#' while mean() provides the mean
# Sum of 1, 2, 3, 4, and 5
sum(1:5)

# Mean of 1, 2, 3, 4, and 5
mean(1:5)

#' Packages: Bundles of code that include functions.
#' 
#'  * Some packages are automatically installed and loaded in R  
#'  
#'    * E.g., the `base` package is automatically installed and loaded in R 
#'  
#'  * Some packages need to be installed (once) and loaded into R at the start of
#'    every session 
#'    
#'    * psych - generating important descriptive statistics
#'             
#'    * here - easily load data/files into R
#'    
#'    * tidyverse - a package containing a group of packages for data manipulation
#'    and cleaning.
#' 
#' Installing packages: 
#' Run line: install.packages("PACKAGENAME")
#' For example:
# install.packages("tidyverse")
#' 
#' 
#' Loading packages: 
#' 
#' * Use the library function 
#' * library(NameOfPackage)

library(psych)

#' *Notes:*
#' 
#' * Only load packages that are relevant to your analyses
#' 
#' * The tidyverse package will take some time to install. Whenever R is loading 
#'   large packages/completing computationally heavy tasks, a stop sign will appear 
#'   in the top right of your console. This stop sign means that R is currently completing 
#'   a job (e.g., installing a package). If you push it, it'll stop and you
#'   need to re-start the process.
#' 
#' * When you load the tidyverse package, it will list all the packages housed
#'   within the package. Useful because it shows objects/functions that get
#'   overriden by the tidyverse packages.
#'   
#' * If the package name (in the "Packages" tab) has a checkmark, this means that
#'   the package has successfully loaded into R. Notice that the base package already
#'   has a checkmark. 

#' **TASK: install the psych, here, and tidyverse packages** 

#' **TASK: after installing the psych, here, and tidyverse packages, load them into your R session**




#' More on Functions:
#' 
#' * Arguments: inputs that are required for the function to work
#'   
#'   * Some arguments have defaults
#'   * Some arguments are required 
#'   * If you follow the order of the arguments, labels are not required
#' 
#' Determining the arguments required for a function: 
#' 
#' * Use the args() function
#' 
#' Format for args function: 
#' 
#' args(PackageName::FunctionName) 
args(base::mean)
#' **TASK: see what arguments are required for the sum function**

#Seeing the arguments for sum() 
args(base::sum)

#' (This is a weird case: its arguments are un-defined because it's a function
#' that works based on the inputted object. sum() accepts numeric vectors.)

#' Help documentation
#' 
#' * The help documentation will provide you with important information regarding 
#'   functions (e.g., description, arguments, package, etc.)
#' * Put a ? in front of the function (with no input) to generate the help documentation
#' * The help documentation will appear in the "Plots, packages, help" panel

#' **TASK: generate the help documentation for the sum() function**


#' **TASK: Find the sum of 5, 6, 4 and 3 using the sum() function**


# TOPIC 6 - Importing Data 

#' Viewing files: 
#' 
#' * You can view/navigate through files on your computer through the "Files" 
#'   tab in the "Environment/Files" panel. 
#' 
#' We will be import data using the read.csv() and here() functions
#' 
#' * The read.csv() function will load the data into R 
#' * The here() function will help the read.csv() function locate files on your 
#'   computer by using your project as the "starting point" for your working directory
#'   
#' Format:
#' 
#' read.csv(file = here("FolderName", "FileName.csv"))
#' 
#' *Notes:* 
#' 
#' * FolderName: you are providing the read.csv() function a map to find the data 
#'               (starting from your working directory.) 
#' 
#'  * E.g., our data will be stored in the "Data" folder located in PSYC3031
#'    
#' * Make sure to correctly input the name of your file (including .csv)

#' **TASK: download the hsb10.csv (which can be found on eClass) onto your computer** 
#' **and save it in the Data folder (in PSYC3031)**

#' **TASK: load the hsb10.csv into your R session** 


#' EXTRA PRACTICE 


#' 1. Load the psych, here and tidyverse packages into your R session.
#' 2. Compute the mean of 1, 2, 3, 4, and 2 without using any functions. 
#' Save the output into an object called MeanOfValues. 
#' 3. Display the output stored in the MeanOfValues object

#' *Note: the answers will be reviewed next class*
