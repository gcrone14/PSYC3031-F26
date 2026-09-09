# Week 2 - Introduction to R/RStudio (part 2)

# setwd()

#### WEEKLY QUESTIONS ####

# Any questions that don’t appear on the screen will be reviewed during today’s tutorial :) 

#' **Should I be creating a new project for each mini assignment?**

#' * Depends on how you want to organize your files.

#' **Do we need to install/load packages for each new project?**

# * No. You will only need to re-install packages if you update R/RStudio. 

#' **When should we include/not include printing in our assignments?**

#' * You do not need to print objects (unless explicitly asked in the assignment 
#'   instructions or you are expected to show the output of an object to answer a question).
#' * You can print an object for your own interest (e.g., to see if data loaded properly into R);
#'   however, make sure the object does not appear in the final script. 

#' **Package spreadsheet!!**

# * The package spreadsheet can be found under the week 2 materials. 

#' **Note: if you are having difficulties installing packages or with RStudio please email me, set up an appointment, come to be before/after class, etc.**


# TOPIC 1 - Review

#' **Note: this R tutorial requires hsb10.csv. Make sure you download hsb10.csv (which can be found on eClass) onto your computer** 
#' **and save it in the Data folder (in the PSYC3031 folder)**
#' 
#' Steps for performing analyses in R (so far)
#' 
#' 1. Load the packages that you will need in that script into your current R session.
#'  
#'  * Note: only install packages that you have never installed before (that are 
#'    not pre-loaded into R). If you have already installed a package or it's 
#'    automatically loaded in R, do NOT install the package.
#'    
#' 2. Load the data into R. 
#' 
#' 3. Check that the data has been correctly loaded into your R session. 
#' 
#' 4. **Data cleaning.**

#' **TASK: Load the required packages into your current R session. For this tutorial, you will need the psych, here and tidyverse packages.**

#' Notes:
#' 
#' * You will need to install the here, tidyverse and psych packages if you haven't installed them already. 
#' * Always load the tidyverse package last. 

#Loading packages
library(here)
library(psych)
library(tidyverse)

#' **TASK: load hsb10.csv data into your R session. Save the data in an object called HsbData.** 

#' Note:
#' 
#' * The code in here() may differ depending on where you saved the data. 

#Loading hsb10.csv and saving the data in an object called HsbData
HsbData<- read.csv(file = here("Data", "hsb10.csv"))

#' **TASK: check that the data has been correctly loaded into your R session.** 

#' Note:
#' 
#' * There are other ways to check your data has been correctly loaded into your R session.

#' Functions: 
#' 
#' view() - shows you the entire dataset 
#' str() - shows the data type of all the variables 

#Viewing the entire dataset 
view(HsbData)



# TOPIC 2 - Data type



#' Main Data types: 

#' Numeric: Numbers with decimals and can include negative values 
#' E.g., 1.1, 2.2, 3, 4, 5.2

#' Integer: Whole numbers 
#' E.g., 1, 2, 3, 4, 5

#' Logical: TRUE and FALSE values 
#' E.g., TRUE, FALSE, FALSE, TRUE

#' Character: Values with quotation marks ("")
#' E.g., "Dog", "Cat", "1", "5" "Blue" 


#' Checking the data type of objects: 
#' 
#' * When you create an object or load data into R, R will try to automatically
#'   assign the appropriate data type to your object/variables
#' * To check the data type (or structure) of an object/variable, use the str() function

#' **TASK: check the structure of the variables in HsbData**

#Checking the structure of the variables in HsbData
str(HsbData)



# TOPIC 3 - Data Structures 



#' Data structures: different ways R can store and organize data


# Different Data Structures:

# 1. Vectors: a string of values (numbers, words, etc.) of the same data type 

#' Creating vectors: 
#' 
#' * c(): allows you to combine multiple values (numbers, words, etc.) into a single vector
#'    
#'    * Separate each element of the vector with a comma

#' **TASK: create a vector containing the values 1, 2, 3, 4, 5, and 6. Save this**
#' **vector in an object named Numbers1**

#Creating a vector called Numbers1
Numbers1<- c(1, 2, 3, 4, 5, 6)

#' Importance of vectors:
#' 
#' * Many functions accept vectors 
#' * Coding efficiency (e.g., if you want to apply one function to multiple values)

#' Different types of vectors: 
#'  
#' Matrices: a 2D set of values of the same data type
#'
#' Arrays : matrices with more than 2 dimensions
#'
#' Lists: vectors that can store different data types

#' Different Data Structures (continued)
#' 
#' 2. Dataframe: a list of vectors. Each column of the dataframe is a vector. 
#'               Each cell within a column/vector must be of the same data type. 
#'    
#'    * The data you loaded into R is a dataframe (each column of the dataset 
#'      is a vector, each with a specific data type)

# Demonstration - HsbData is a dataframe 

#Investigating the structure of HsbData 
str(HsbData)

# Working with dataframes (examples): 

#' 
#' * Extracting columns/rows 
#' 
#' 
#' *Note: ObjectName[row#, column#]* 

#' **TASK: extract the third column of HsbData**

#Extracting the third column (method 1) 
Object<- HsbData[,3]



#Extracting the third column (method 2) 
Race<-HsbData$race

#' **TASK: extract the fourth participants data from HsbData**

#Extracting the fourth row/participant
Par4<- HsbData[4,]
view(Par4)

#' * Extracting rows with a specific value 


#' **TASK: extract the rows in HsbData where the writing score is less than or equal to 50**

#Extracting rows where the writing score is less than or equal to 50
Write50<- HsbData[HsbData$write<=50,]

Write50New<- Write50[,8]

#' *  Extracting a specific cell 


#' **TASK: extract the program information for the fifth participant in HsbData**

#Extracting the program information of the fifth participant
Par5Prog<- HsbData[5,6]

#' * Removing columns/rows 


#' **TASK: remove the ID column in HsbData**

#Removing the ID column
HsbData[,-1]


#' * Complete operations on columns/a row of the dataframe


#' **TASK: find the mean writing score for the sample** 

#Mean writing score   
mean(HsbData$write)

#' Note: To find the mean of values that are not saved as an object, 
#' you need to create a vector inside the function. 
#' For example, to find the mean of 3, 4, 5, and 6 using mean():

#Computing the mean (using mean())
mean(c(3, 4, 5, 6))

#Computing the mean (manually)
(3 + 4 + 5 + 6)/4


#' Different Data Structures (continued)
#' 
#' 3. Tibbles: used in tidyverse (similar to dataframes)



# TOPIC 4 - Data manipulation



#' What are data manipulations?
#' 
#' * Filtering certain participants 
#' * Selecting certain columns 
#' * Changing the structure of variables
#' * Creating new variables (from existing variables)
#' * Important when cleaning/preparing data for analyses

#' How can data manipulations be performed?
#' 
#' * We will be using a variety of different functions from the tidyverse package
#' 
#' *Note: check your data after performing a manipulation*


#Filtering rows/participants 

#' Filtering: extracting rows that meet a certain criteria 
#'            
#' Function: filter() from the tidyverse package
#' 
#' General formats: 
#' 
#' * filter(data, criteria)
#'    
#' *Note: to create filters (i.e., the criteria) you can use the operators taught last week!*
#'         
#'         * < less than 
#'         * > greater than
#'         *  == equal to
#'         * <= less than or equal to
#'         * >= greater than or equal to
#'         * != not equal to
#'         * & and
#'         * | or
#'         * is.na() is missing data
#'         * ! Not

#' **TASK: extract the participants in HsbData who have a writing score of 50 or lower.**
#' **Save this data into a new object called WritingScore50**

#Filtering participants with a writing score of 50 or lower
WritingScore50<- filter(HsbData, write <= 50)

#Displaying output
WritingScore50

#' **TASK: extract the participants in HsbData who are in program 1.**
#' **Save this data into a new object called ProgramOne**

#Filtering participants in program 1
ProgramOne<- filter(HsbData, prog == 1)

#Displaying output
ProgramOne

#' **TASK: extract the participants in HsbData who are NOT in program 1.**
#' **Save this data into a new object called NotProgramOne**

#Filtering participants NOT in program 1
NotProgramOne<- filter(HsbData, !prog == 1)

#Displaying output
NotProgramOne

#' **TASK: extract the participants in HsbData that are not missing their math AND writing scores.**
#' **Save this data into a new object called CompleteMathWrite** 

#Filtering participants that are NOT missing math and writing scores
CompleteMathWrite<- filter(HsbData, !is.na(math) & !is.na(write))

#Displaying output
CompleteMathWrite

#' **TASK: extract the participants in HsbData that have a read score greater than 50 OR a write score greater than 50.**
#' **Save this data into a new object called ReadWrite50** 

#Filtering participants that are NOT missing math and writing scores
ReadWrite50<- filter(HsbData, read > 50 | write > 50)

#Displaying output
ReadWrite50


#' **QUESTION: if someone has no response (such as how some people have no response**
#' **on education level) is there a way to retain those participants?**
#' 
#' **E.g., how can retain the participants with missing data on the math variable?**




# Selecting Columns: 


#' Selecting: extracting specific columns from your dataset 
#'            
#' Function: select() from the tidyverse package
#' 
#' General formats: 
#' 
#' * select(data, columns)
#'    
#' * Note: you can either input the names of columns or the number of 
#'         the column

#' **TASK: remove the ID column from HsbData. Call this a new object called HsbDataRevised**

#Removing the ID column (method 1)
HsbDataRevised<- select(HsbData, -id)

#Removing the ID column (method 2)
HsbDataRevised<- select(HsbData, -1)

#Removing the ID column (method 3)
HsbDataRevised<- select(HsbData, female:socst)

#Removing the ID column (method 4)
HsbDataRevised<- select(HsbData, 2:11)

#Displaying output
HsbDataRevised


#' **TASK: select the prog, ses and write columns from HsbData.** 
#' **Create a new object called WritingData**

#Selecting the prog, ses and write columns (method 1)
WritingData<- select(HsbData, prog, ses, write)

#Selecting the prog, ses and write columns (method 2)
WritingData<- select(HsbData, 4, 6, 8)

#Displaying output
WritingData


#' Creating New Variables: 
#' 
#' Creating a new variable: manipulating existing variables in the data to create a new variable. 
#'            
#' Function: mutate() from the tidyverse package
#' 
#' General formats:
#' 
#' * mutate(data, NewVariableName = manipulation)

#' **TASK: create a new variable (called AverageScore) that is the average of participants wiring and math score**
#' **using the CompleteMathWrite data**

#Creating a new variable that is the average of each participants writing and math score
CompleteMathWrite<- mutate(CompleteMathWrite, AverageScore = (write + math)/2)

#Displaying output
CompleteMathWrite


#' Changing the Data Type of Variables: 
#' 
#' R may assign the wrong data type to a variable. It is your job to change the 
#' structure of the variable to the correct data type.   
#'            
#' Function: mutate() from the tidyverse package
#' 
#' General formats:
#' 
#' * mutate(data, NewVariableName = manipulation)
#'    
#' *Notes:* 
#'
#' * Beginners should always save the manipulated variable as a new variable
#'   
#' * Useful functions: 
#'      * factor() - change the structure to factor 
#'      * as.numeric() - change the structure to numeric 
#'      * as.character() - change the structure to character
#'      * as.integer() - change the structure to integer 
#'      * as.logical() - change the structure to logical 
#'    
#' * When changing the structure of a variable to numeric, first change the structure 
#'   to character. 

#' **TASK: change the structure of the write variable in HsbData to numeric.**
#' **Check the structure of the new variable.**

#Changing the structure of the write variable to numeric (method 1)

HsbData<- mutate(HsbData, 
                 n_write = as.character(write))

HsbData<- mutate(HsbData, 
                 n_write = as.numeric(n_write))

#Changing the structure of the write variable to numeric (method 2)

HsbData<- mutate(HsbData, 
                 n_write = as.numeric(as.character(write)))

#Checking the structure of the variable
str(HsbData)


#' **TASK: change the race variable in HsbData to factor and check the structure of the new variable**

#Changing the structure of the race variable (method 1)
HsbData<- mutate(HsbData, 
                 f_race = factor(race))

#Checking the structure of the variables
str(HsbData)

#' Additional arguments for the factor() function: 

#' **TASK: change the female variable in HsbData to factor.**
#' **Relabel the levels so that 1 = female and 0 = male.**
#' **Check the structure of the new variable.**

#Changing the structure and labels of the female variable
HsbData<- mutate(HsbData, f_female = factor(female, levels = c(0, 1),labels = c("Male", "Female")))

#Checking the structure of the variable
str(HsbData)


#' Re-coding variables: 
#' 
#' Purpose: to make a continuous variable categorical   
#'            
#' Function: 
#' * mutate() - create the new variable
#' * base::cut() - re-code the continuous variable according to user specified intervals
#' 
#' General formats:
#' 
#' * mutate(data, NewVariableName = manipulation)
#' * cut(variable, 
#' breaks = c(-Inf, #, #, Inf),
#' labels = c("Label", "Label", "Label")
#' )
#'    
#' *Note about cut():* 
#'
#' * The "breaks" specify the range of values for each level of the new categorical 
#'   variable you are creating. For example, if breaks = c(-Inf, 10, 20, Inf), you are 
#'   creating a 3 level categorical variable. Level 1 = values of the continuous variable
#'   <= (less than or equal to) 10. Level 2 = values of the continuous variable between 11-20. Level 3 = values 
#'   of the continuous variable >= (greater than or equal to) 21. 
#'      

#' **TASK: Create a categorical variable (called WriteCat) for write where participants**
#' **with a writing score less than or equal to 46 have a "Low" score. Otherwise,** 
#' **participants have a "High" score.**

#Changing the structure and labels of the female variable
HsbData<- mutate(HsbData, 
                 WriteCat = cut(n_write,
                                breaks = c(33, 46.5, 59),
                                labels = c("Low", "High")))


str(HsbData)
min(HsbData$n_write)
max(HsbData$n_write)
# TOPIC 5 - Compile Report 



#' Purpose: 
#' 
#' * Allows you to send the code and resulting output in a document format (e.g., HTML, PDF, Word, etc.)
#' 
#' Different methods to compile a report: 
#' 
#' 1. Mac: Command + K
#' 2. Windows: Ctrl+Shift+K
#' 3. File > Compile Report...> Choose format > Compile
#' 
#' 
#' **TASK: compile the current R script into an HTML file.**


# EXTRA PRACTICE 



#' 1. Create and save a new R script for these practice questions
#'    This R script will also be used for next weeks practice questions. 
#' 2. Load the psych, here and tidyverse packages into your R session.
#' 3. Load the cereal 3.csv data into R. Save the dataset into an object called CerealData
#' 4. Check that the data was loaded correctly into R. 
#' 5. Extract the name, type, mfr, rating, calories, sugars and carbo columns from the dataset. Call
#'    this new dataframe RevisedCerealData
#' 6. Extract only the cold cereals (i.e., type = C) from the RevisedCerealData dataframe. Save 
#'    this revised dataset in the RevisedCerealData object
#' 7. Remove the type variable from the RevisedCerealData dataframe. Save 
#'    this revised dataset in the RevisedCerealData object
#' 8. Change the structure of the mfr variable from RevisedCerealData to a factor. Relabel the levels so that:
#'        * A = American Home Food Products
#'        * G = General Mills
#'        * K = Kelloggs
#'        * N = Nabisco
#'        * P = Post
#'        * Q = Quaker Oats
#'        * R = Ralston Purina
#' 9. Create a new categorical variable (called RatingCat) from rating where cereals 
#'    with a rating between:
#'        * 18-43 = "Bad"
#'        * 44-69 = "Neutral"
#'        * 70-95 = "Good"

#' *Note: the answers will be reviewed next class*

