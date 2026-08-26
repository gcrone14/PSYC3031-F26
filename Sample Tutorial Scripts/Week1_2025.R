# Week 1 - Introduction to R/RStudio (part 1)



# TOPIC 1 - R and RStudio 



#' What is R and RStudio?
#' 
#'  * R is a coding language/environment
#'  * RStudio provides users with a more user-friendly interface to interact with R
#'  * Analogy: R is the "car engine" and RStudio is the actual car 

# RStudio layout

#' * Source editor: where you will type and run your code
#' * Console: where the output for your code will appear
#' * Environment/Files: where your saved objects will appear
#' * Plots, packages, help: where your graphs, help documentation and graphs will appear 
#' 
#' *Note: the layout of the panels may appear differently on your computer*

#' Editing your user interface
#'  
#'  * Edit > Preferences...
#'  * Edit > Settings...
#'  * Tools > Global Options...
#'  
#' Changing the display colour and font type/size:
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


#' Projects
#' 
#' * Project: a starting point for your working directory 
#' 
#' * Working directory: a map used to locate/open files on your computer
#' 
#'    * Currently, you have no project created/opened
#'    * To check if a project is opened, look at the top right corner of the screen 
#'      (the little blue cube)
#' 
#'    
#' Creating a project:
#'  
#' 1. Create a file on your desktop (name this file PSYC3031)
#' 
#' 2. Create three files in the PSYC3031 file: 
#' 
#'  * Data
#'  * Script 
#'  * Output
#' 
#' 3. Cube with plus sign > Existing Directory 
#' 
#' 4. Select the PSYC3031 folder located on your desktop
#' 
#' 5. Select "create project" 
#'    
#' *Note: you do NOT have to save projects on your desktop, they can be saved in another area of your computer.*

#' Creating an R script
#' 
#' * White box with a + (top right corner) > R Script
#' 
#' * Result: an untitled R script is created and appears in your console. 
#' 
#' * Note: when the R script says Untitled, this means you have NOT saved your R script*
#'   
#' Saving an R script 
#'   
#'    * Mac: Command + S 
#'    * Windows: Control + S 
#'    * Press the save icon on the top left of your screen
#'
#' * Save your R scripts in PSYC3031 > Script

#' **TASK: create an R Script and save it in the script file** 



# TOPIC 2 - Commenting



#' Comments: any text followed by a hashtag (i.e., # or #')
#' 
#' * Purpose: to organize code
#' 
#' *Note: for assignments/activities, you will be expected to organize your code with comments* 

#' Different methods of commenting: 

# Hashtag - single lines

#' Hashtag with apostrophe - multiple lines

# Example - investigating different methods of commenting

# Put your cursor at the end of this comment and press enter
#' Put your cursor at the end of this comment and press enter



# TOPIC 3 - Using R as a Calculator



#' Running code:
#' 
#' There are multiple ways to run a line of code: 
#' 
#' * Mac: command+enter 
#' * Windows: control+enter
#' * Hit the run button

2+2


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

#Determining if 4 is less than five
4<5

#' **TASK: run the code in your R script**

#Determining if four and six are less than five
4&6<5



# TOPIC 4 - Objects 

Output <- 2+4+6+7
Output2<- Output + 4

#' Object: a container to save output/values (e.g., numbers, words, etc.)
#' 
#' General Formatting:
#' 
#' ObjectName <- CODE
#' 
#' Displaying the output/values saved in the object: 
#' 
#' * Highlight the object name and run the code
#' * Type the object name on a different line in your script and run that new 
#'   line of code
#' 
#' Naming objects 
#' 
#' * Use a meaningful, specific name 
#' * Don't make the name too long/complicated 
#' * Recommended formats: ObjectName OR object_name

#' **TASK: find the sum of 2 + 2, save the output in an object called SumValues, and display the output**

#Sum of 2+2 saved in an object called SumValues
SumValues<- 2+2 
SumValues

#' **TASK: save the value 6 into an object named Six and display the output**

#Saving 6 in an object called Six
Six<- 6
Six

#' *Notes:*


# * Do not use the same name for multiple objects

#Example - Giving objects the same name


#Displaying the output saved in SumValues
SumValues

#Sum of 4+5 and saving the output in an object named SumValues
SumValues<- 4+5 

#Displaying the output of SumValues
SumValues


#' * R is case sensitive (see example 2)

#Example -  R is case sensitive


#Saving the value 1 in an object named RandomNumbers
RandomNumbers<- 1 

#Saving the value 2 in an object named randomnumbers
randomnumbers<- 2

#Displaying output for both objects 
RandomNumbers
randomnumbers



# TOPIC 5 - Packages and Functions



#' Functions: pre-made code to complete operations in R
#' 
#'  * E.g., rather than computing the mean of a set of values by hand, there is a 
#'          function that can compute the mean for you. 
#'      
#' Packages: where functions are stored 
#' 
#'  * Some packages are automatically installed and loaded in R  
#'  
#'    * E.g., the base package is automatically installed and loaded in R 
#'  
#'  * Some packages need to be installed (once) and loaded into R at the start of
#'    every session 
#'    
#'    * psych - generating important descriptive statistics
#'             
#'    * here - easily load data/files into R
#'    
#'    * tidyverse - a package containing a group of packages for data manipulation
#' 
#' Installing packages: 
#' 
#' 1. Go to the "Plots, packages, help" panel
#' 
#' 2. Click the "Packages" tab 
#' 
#' 3. Click the "Install" button 
#' 
#' 4. Type the name of the package
#' 
#' 5. Click install
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
#'   large packages/completing computationally heavy tasks a stop sign will appear 
#'   in the top right of your console. This stop sign means that R is currently completing 
#'   a job (e.g., installing a package)
#' 
#' * When you load the tidyverse package, it will list all the packages housed
#'   within the package
#'   
#' * If the package name (in the "Packages" tab) has a checkmark, this means that
#'   the package has successfully loaded into R. Notice that the base package already
#'   has a checkmark. 

#' **TASK: install the psych, here and tidyverse packages** 

#' **TASK: after installing the psych, here and tidyverse packages, load them into your R session**

#Loading packages 
library(psych) 
library(here)
library(tidyverse) #Note: this package may take some time to install

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

#' Help documentation
#' 
#' * The help documentation will provide you with important information regarding 
#'   functions (e.g., description, arguments, package, etc.)
#' * Put a ? in front of the function (with no input) to generate the help documentation
#' * The help documentation will appear in the "Plots, packages, help" panel

#' **TASK: generate the help documentation for the sum() function**

#Generating the help documentation for sum() 
?sum

#' **TASK: Find the sum of 5, 6, 4 and 3 using the sum() function**

#Sum of 5, 6, 4 and 3
SumVal<- sum(5, 6, 4, 3)
SumVal


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
#'               (starting from your package) 
#' 
#'  * E.g., our data will be stored in the "Data" folder located in PSYC3031
#'    
#' * Make sure to correctly input the name of your file (including .csv)

#' **TASK: download the hsb10.csv (which can be found on eClass) onto your computer** 
#' **and save it in the Data folder (in PSYC3031)**

#' **TASK: load the hsb10.csv into your R session** 

#Loading the hsb10.csv into R
read.csv(file = here("Data", "hsb10.csv"))

HsbData<- read.csv(file = here("Data", "hsb10.csv"))
view(HsbData)

# EXTRA PRACTICE 



#' 1. Load the psych, here and tidyverse packages into your R session.
#' 2. Compute the mean of 1, 2, 3, 4, and 2 without using any functions. Save the output into an object called MeanOfValues. 
#' 3. Display the output stored in the MeanOfValues object

#' *Note: the answers will be reviewed next class*
