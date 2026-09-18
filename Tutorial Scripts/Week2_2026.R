# Week 2 - Introduction to R/RStudio (part 2)

# TOPIC 1 - Review

#' BRAINSTORM:
#' Which ideas/topics do you remember from last class? (Try to avoid
#' peeking at your notes)

#' 1.
#' 2. 
#' 3. 
#' 4. 
#' ...

#' Which topics do you feel you need additional practice or would like
#' additional resources on?

#' 1.
#' 2. 
#' 3. 
#' 4. 


#' **Note: this R tutorial requires hsb10.csv. Make sure you download hsb10.csv (which can be found on eClass) onto your computer** 
#' **and save it in the Data folder (in the PSYC3031 folder)**

#' Today's goal: Go over process of importing and cleaning data so it's
#' ready to be anlayzed.

#' Steps for performing analysis in R (so far):
#' 
#' 1. Load the packages into current R session (must do once per new session)
#'  
#'  * Installation is required if using packages you have never installed before.
#'  * We load packages with the _________ function.
#'    
#' 2. Load the data into R. 
#'  * We load data into R with one of several _________ functions, such as
#'  ___________.
#' 
#' 3. Check that the data has been correctly loaded into your R session.
#'   * We can print our data by ___________.
#' 
#' 4. **Data cleaning.** (today's focus!)

#' **TASK: Load the required packages into your current R session. For this tutorial, you will need the psych, here and tidyverse packages.**

#' Notes:
#' 
#' * You will need to install the here, tidyverse and psych packages if you haven't installed them already. 
#' * Always load the tidyverse package last. 

# Loading packages
library(here)
library(psych)
library(tidyverse)

#' **TASK: load hsb10.csv data into your R session. Save the data in an object called hsb_dat.** 

#Loading hsb10.csv and saving the data in an object called hsb_dat
hsb_dat <- read.csv("Data/hsb10.csv")

#' **TASK: check that the data has been correctly loaded into your R session.** 


#' Note:
#' 
#' * There are other ways to check your data has been correctly loaded into your R session.

#' Functions: 
#' 
#' view() - shows you the entire dataset 
#' str() - shows the data type of all the variables 

# Viewing the entire dataset (as a spreadsheet) 
# view(hsb_dat)


# TOPIC 2 - Data types

#' Main Data types: 

#' Numeric (dbl or num): Numbers with decimals and can include negative values 
#' E.g., 1.1, 2.2, 3, 4, 5.2

#' Integer (int): Whole numbers 
#' E.g., 1, 2, 3, 4, 5

#' Logical (lgl): TRUE and FALSE values 
#' E.g., TRUE, FALSE, FALSE, TRUE

#' Character (chr): Values with quotation marks (""); text strings.
#' E.g., "Dog", "Cat", "1", "5" "Blue" 


#' Checking the data type of objects: 
#' 
#' * When you create an object or load data into R, R will try to automatically
#'   assign the appropriate data type to your object/variables.
#' * Sometimes it gets it right; other times, it's very wrong. It's your
#' job to understand if the data are imported correctly.
#' * To check the data type (or structure) of an object/variable, use the str() function

#' **TASK: check the structure of the variables in hsb_dat**



# TOPIC 3 - Data Structures

#' Data structures: different ways R can store and organize data

# Different Data Structures:

# 1. Vectors: a string of values (numbers, words, etc.) of the SAME data type

#' Creating vectors: 
#' 
#' * c(): concatenate (or combine)- allows you to combine multiple values (numbers, words, etc.) 
#' into a single vector
#'    
#'    * Separate each element of the vector with a comma

#' **TASK: create a vector containing the values 1, 2, 3, 4, 5, and 6. Save this**
#' **vector in an object named nums_1**


#' **TASK: create a vector containing the following values: 0, "zero", FALSE.**
#' **Save your vector as an object called co_vec.**
#' **Guess what the structure of the vector will be, then check using str()**
#' **HINT: Concept this demonstrates is known as COERCION.**



#' Importance of vectors:
#' 
#' * Many functions accept vectors as arguments
#' * Coding efficiency (e.g., if you want to apply one function to multiple values)

#' Example: repeated addition
#' Suppose 4 students answer 7 questions. Their data are found in `scores`.
scores <- c(3, 1, 7, 4)
scores + 1 # Adds 1 to all scores in the vector
scores * 2 # Doubles all scores
scores / 7 * 100 # Find the percent score for each student


#' 2. Dataframe: Common data storage object (think: Excel spreadsheet), with
#' some rules: 
#'   a) Each column is a vector, and each row is an observation.
#'   b) Since each column is a vector, all data in a single column must be
#'   of the same type.
#'    
#'    * The data you loaded into R is a data.frame (where each row is a type of
#'    vector.)

# Demonstration - hsb_dat is a dataframe 

#Investigating the structure of hsb_dat 
str(hsb_dat)

# Working with dataframes (examples): 

#' * Extracting columns/rows 
#' 
#' 
#' To extract a single value from the data frame: ObjectName[row#, column#]

#' If only want a row, leave column# blank: ObjectName[row#, ]
#' If only want a column, leave row# blank: ObjectName[, column#]
#' 
#' The row# or column# can either be an exact value (index) or a logical condition

#' EXAMPLES:
#' Extracting first row
hsb_dat[1,]
#' First column
hsb_dat[,1]
#' First cell (row 1, column 1)
hsb_dat[1,1]

#' **Simpler way to extract columns: ObjectName$column_name**

#' Example: Extract first column
# 1. Check names with names() function
names(hsb_dat)
# 2. Extract column
hsb_dat$id

#' **TASK: extract the third column of hsb_dat**


#' **TASK: extract the fourth participant's data from hsb_dat**

#' **TASK: extract the rows in hsb_dat where the writing score is less than or equal to 50**
#' Hint: Use this format: ObjectName[ObjectName$col_name <= NUM,]
#' (Fill in the correct values; col_name = name of the column)

#' **TASK: extract the program information for the fifth participant in hsb_dat**

#' * Removing columns/rows 

#' Can remove columns/rows by adding a minus sign
#' before the column# or row#, 
#' e.g., Deleting 2nd row
hsb_dat[-2,]
#' e.g., Deleting 3rd column
hsb_dat[,-3]

#' **TASK: remove the ID column in hsb_dat**


#' * Complete operations on columns/a row of the dataframe

#Computing the mean (using mean()) can be done as so:
mean(c(3, 4, 5, 6))

# As opposed to doing it manually (NEVER do this!)
(3 + 4 + 5 + 6)/4

#' **TASK: find the mean of the writing score variable within hsb_dat**
#' **HINT: You'll need to index the column using Object$col_name** 


#' Different Data Structures (continued)
#' 
#' 3. Tibbles: used in tidyverse (similar to dataframes)

#' Can convert any data.frame to a tibble with as_tibble()
#' (Make sure to load tidyverse first!)
hsb_dat_tibble <- as_tibble(hsb_dat)

#' Can convert back with as.data.frame()
as.data.frame(hsb_dat_tibble)

#' Why use tibbles?
#' They look much nicer when printed (give much better sense of data)
#' compared to data frames, but both behave (nearly) identically.
hsb_dat # data.frame
hsb_dat_tibble # tibble

#' Use whichever you like, but be consistent

# TOPIC 4 - Data manipulation

#' What are data manipulations?
#' 
#' * Filtering certain participants out
#' * Selecting certain columns 
#' * Changing the structure of variables
#' * Creating new variables (from existing variables)
#' * Important when cleaning/preparing data for analyses

#' How can data manipulations be performed?
#' 
#' * We will be using a variety of different functions from the dplyr package
#' (One of MANY tidyverse packages.)
#' 
#' *Note: good habit- check your data after performing a manipulation*


# Filtering rows/participants 

#' Filtering: extracting rows that meet a certain criteria 
#'            
#' Function: filter() from dplyr
#' 
#' General formats: 
#' 
#' * filter(data, criteria)
#' * The criteria is the logical check, which uses the variables in the data
#'    
#' *Note: to create filters (i.e., the criteria) you can use the logical checks taught last week!*
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

#' An example: Filter data so only female participants are present
#' (i.e., female is equal to 0)
filter(hsb_dat, female == 0)

#' BEWARE: Common mistake is writing "=" instead of "==":
#' = means "assign objects or arguments"
#' == means "check if two things are equal"

#' **TASK: extract the participants in hsb_dat who have a writing score of 50 or lower.**
#' **Save this data into a new object called writing_score_50**
#' **HINT: Recall filter(data, criteria) as a general format.**



#' **TASK: extract the participants in hsb_dat who are in program 1.**
#' **Save this data into a new object called program_one and print it**



#' **TASK: extract the participants in hsb_dat who are NOT in program 1.**
#' **Save this data into a new object called not_program_one and print it**



#' **TASK: extract the participants in hsb_dat that are not missing their math AND writing scores.**
#' **Save this data into a new object called complete_math_write and print it** 
#' **HINT: Consider using the following symbols: !, is.na, and &**


#' **TASK: extract the participants in hsb_dat that have a read score greater than 50 OR a write score greater than 50.**
#' **Save this data into a new object called read_write_50 and print it**


#' **QUESTION: if someone has no response (such as how some people have no response**
#' **on education level) is there a way to retain those participants?**
#' 
#' **E.g., how can retain the participants with missing data on the math variable?**


# Selecting Columns: 


#' Selecting: extracting specific columns from your dataset 
#'            
#' Function: select() function
#' 
#' General format: 
#' 
#' * select(data, columns)
#'    
#' * Note: you can either input the names of columns or the number of 
#'         the column (best practice is to use names)

#' To select out columns, put a - before the column you wish to remove:
# select(data, -columns)
#' If selecting multiple columns, should save it as a vector (using c()) or
#' by using the ":" shorcut. For example, if a data set had 10 columns, named
paste0("col", 1:10)

#' and you wanted to only select the first four columns, you'd say:
#' select(dat, col1:col4).

#' **TASK: remove the ID column from hsb_dat. Call this a new object called hsb_dat_revised**
#' **and print it.**


#' **TASK: select the prog, ses and write columns from hsb_dat.** 
#' **Create a new object called writing_dat, and print it**


#' Creating New Variables: 
#' 
#' Creating a new variable: manipulating existing variables in the data to create a new variable. 
#'            
#' Function: mutate() from dplyr
#' 
#' General format:
#' 
#' * mutate(data, NewVariableName = manipulation)


#' **TASK: create a new variable (called average_score) that is the average of participants' wiring and math scores**
#' **using the complete_math_write data from earlier. You may overwrite the data**


#' Changing the Data Type of Variables: 
#' 
#' R may assign the wrong data type to a variable. It is your job to change the 
#' structure of the variable to the correct data type.   
#'            
#' Function: mutate() from the dplyr package
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

#' Sidenote: There are far more efficient ways to convert many variables' data types
#' using more advanced formats and functions. Gabe can share them if they would
#' be useful.

#' **TASK: change the structure of the write variable in hsb_dat to numeric.**
#' **Check the structure of the new variable.**



#' **TASK: change the race variable in hsb_dat to factor and check the structure of the new variable**



#' Additional arguments for the factor() function: 
#' 1. x: the column
#' 2. levels: vector of unique values in the column
#' 3. labels: new labels to input in order of existing levels

#' Basic format:
#' mutate(dat, column = factor(column, levels = c(), labels = c()))
#' both c() must be filled in with correct values

#' **TASK: change the female variable in hsb_dat to factor.**
#' **Relabel the levels so that 1 = female and 0 = male.**
#' **Check the structure of the new variable.**
mutate(hsb_dat, female = factor(female, levels = c(0, 1), labels = c("male", "female")))

# TOPIC 5 - Piping

#' Pipes are an extremely useful (and very common) shorthand for most
#' of the syntax we've been using, and permit you to more flexibly do
#' commands in R.

#' The pipe symbol, |>, basically just says, 
#' "take the object made from previous line(s), and then do this..."

#' For example: If you want to take your data, then select the id
#' column, pipe notation would be:
hsb_dat |> select(id)

#' Compare this with the alternative:
select(hsb_dat, id)

#' This might seem inconsequential, but it's not:
#' it's a lot more readable.

#' Pipes become even more useful when "chaining" multiple commands together
#' For example: suppose you wanted to select math scores and filter them
#' so none were missing

#' The regular way would be to run two separate commands:
math_scores <- select(hsb_dat, math)
filter(math_scores, !is.na(math))

#' Notice that you must save each intermediate step, which becomes
#' annoying/tedious quickly.

#' But with pipes, it becomes a lot easier:
hsb_dat |>             # Take hsb_dat
  select(math) |>      # select math scores
  filter(!is.na(math)) # filter it so only non-missing math scores appear

#' Pipes "know" when to stop because it runs all lines up to those without the
#' pipe (the final line.) If you put a pipe on the final line above, it'll try to keep
#' running subsequent lines and error out.

#' **EXERCISE: Convert the following lines of code to a more readable version**
#' **with pipes**

# ORIGINAL:
# Steps: Filter so race == 4, both read and write are greater than 50,
# then select the read and write variables.
race_4 <- filter(hsb_dat, race == 4)
read_gr_50 <- filter(race_4, read > 50)
write_gr_50 <- filter(read_gr_50, write > 50)
select_vars <- select(write_gr_50, read:write)

# WITH PIPES:
# ...


#' **KEY NOTE: It's a great idea to get used to using pipes now; they become extremely useful for the**
#' **course and make using R a lot more pleasant!**

# TOPIC 6 - Compile Report 

#' Purpose: 
#' 
#' * Allows you to send the code and resulting output in a document format (e.g., HTML, PDF, Word, etc.)
#' 
#' Different methods to compile a report: 
#' 
#' 1. Mac: Command + Shift + P -> Type "Compile Report" and click "Compile"
#' 2. Windows: Ctrl + Shift + P -> Type "Compile Report" and click "Compile"
#' 3. File > Compile Report...> Choose format > Compile
#' 
#' **TASK: compile the current R script into an HTML file.**


# EXTRA PRACTICE 

#' 1. Create and save a new R script for these practice questions
#'    This R script will also be used for next weeks practice questions. 
#' 2. Load the psych, here and tidyverse packages into your R session.
#' 3. Load the cereal 3.csv data into R. Save the dataset into an object called cereal_dat
#' 4. Check that the data was loaded correctly into R. 
#' 5. Extract the name, type, mfr, rating, calories, sugars and carbo columns from the dataset. Call
#'    this new dataframe revised_cereal_dat
#' 6. Extract only the cold cereals (i.e., type = C) from the revised_cereal_dat dataframe. Save 
#'    this revised dataset in the revised_cereal_dat object
#' 7. Remove the type variable from the revised_cereal_dat data frame. Save 
#'    this revised dataset in the revised_cereal_dat object
#' 8. Change the structure of the mfr variable from revised_cereal_dat to a factor. Relabel the levels so that:
#'        * A = American Home Food Products
#'        * G = General Mills
#'        * K = Kelloggs
#'        * N = Nabisco
#'        * P = Post
#'        * Q = Quaker Oats
#'        * R = Ralston Purina
#' 9. Do steps 1-8 with pipes.

#' *Note: the answers will be reviewed next class*

