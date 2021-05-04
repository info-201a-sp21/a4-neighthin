# A4 Data Wrangling
This rubric is intended to help guide expectations and increase transparency. However, it **is not** necessarily fully exhaustive. Make sure to read the detailed explanation on Canvas, and follow the best practices described in the course book and in class.

For this assignment, you must use the appropriate `dplyr` functions to complete each task, returning the specified data type. Partial credit will be given where appropriate. 

## Loading and Exploring Data (**29 points**)
- Load the `dplyr` package (**1 point**)
- Include link to dplyr cheat sheet (**1 point**)
- Download appropriate data into a `data/` directory (**2 points**)
    - -1 if not in the `data/` directory
- Load your data (**2 points**)

- Get summary information about the dataset (**3 points**)
    - 1 point for column names
    - 1 point for rows
    - 1 point for columns

- Write the `get_col_info()` function (**10 points**)
    - 1 point for extracting the values from the dataframe using `dplyr`
    - 1 point for an appropriate check for the variable type
    - 2 points for returning proper information for numeric variables
    - 1 point for dealing with NA values appropriately for numeric variables
    - 2 points for returning proper values for non-numeric columns with < 10 unique values
    - 3 points for returning proper values for non-numeric columns with > 10 unique values (including a method to *randomly sample* the values)

- Demonstrate that your function works by passing a column name of your choice and the kickstarter data to your function (**2 points**) 
    - 1 point for function working
    - 1 point for storing in a variable with a meaningful name

- Write the `get_summary_info()` function (**3 points**)
    - 1 point for using the `get_col_info()` function
    - 2 points for returning a *list* where the keys are the column names and the values are the output from `get_col_info()`
    
- Demonstrate that your `get_summary_info()` function works (**2 points**) 
    - 1 point for function working
    - 1 point for storing in a variable with a meaningful name

- Take note of 3 observations that you find interesting from this summary (**3 points**)
    - 1 point for each observation, must be driven by the results of the `get_summary_info()` function

## Asking question of the data ------------------- (**23 points**)
- What was the name of the project(s) with the highest goal? (**3 points**)
    - -1 if result not in a vector of names
    - -1 if logic is incorrect
    - -1 if variable name isn't meaningful

- How many projects had a deadline in 2018? (**4 points**)
    - -1 if result isn't a single numeric value
    - -1 if variable name isn't meaningful
    - -1 if date values aren't handled appropriately
    - -1 if logic is incorrect

- What was the category of the project(s) with the lowest goal? (**3 points**)
    - -1 if result not in a vector of category(s)
    - -1 if logic is incorrect
    - -1 if variable name isn't meaningful
    
- What proportion or projects weren't successful? Your result can be a decimal (**4 points**)
    - -1 if result isn't a single numeric value (decimal between 0 and 1)
    - -1 if variable name isn't meaningful
    - -1 if filtering logic is incorrect
    - -1 if percentage calculation is incorrect

- What was the amount pledged for the project with the most backers? (**3 points**)
    - -1 if result not in a vector of amount(s)
    - -1 if logic is incorrect
    - -1 if variable name isn't meaningful

- Of all of the projects that *failed*, what was the name of the project with the highest amount of money pledged? (**3 points**)
    - -1 if result not in a vector of name(s)
    - -1 if logic is incorrect
    - -1 if variable name isn't meaningful

- How much total money was pledged to projects that weren't successful? (**3 points**)
    - -1 if result not a single value
    - -1 if logic is incorrect
    - -1 if variable name isn't meaningful

## Performing analysis by *grouped* observations ----------------- (31 Points)
- Which category had the most money pledged (total)? (**5 points**)
    - 1 point for grouping
    - 1 point for computing summary information
    - 1 point for filtering appropriately
    - 1 point for returning the value of interest (category)
    - 1 point for a meaningful variable name

- Which country had the most backers?  (**5 points**)
    - 1 point for grouping
    - 1 point for computing summary information
    - 1 point for filtering appropriately
    - 1 point for returning the value of interest (country)
    - 1 point for a meaningful variable name

- Which year had the most money pledged (**6 points**)
    - 1 point for grouping
    - 1 point for computing summary information
    - 1 point for filtering appropriately
    - 1 point for returning the value of interest (year)
    - 1 point for a meaningful variable name
    - 1 point for sentence on why they chose deadline or launched date

- What were the top 3 main categories in 2018 (as ranked by number of backers)? (**5 points**)
    - 1 point for grouping
    - 1 point for computing summary information
    - 1 point for filtering appropriately
    - 1 point for returning the value of interest (year)
    - 1 point for a meaningful variable name

- What was the most common day of the week on which to launch a project? (**5 points**)
    - 1 point for grouping
    - 1 point for computing summary information
    - 1 point for filtering appropriately
    - 1 point for returning the value of interest (day)
    - 1 point for a meaningful variable name

- What was the least successful day on which to launch a project? (**5 points**)
    - 1 point for grouping
    - 1 point for computing summary information
    - 1 point for filtering appropriately
    - 1 point for returning the value of interest (day)
    - 1 point for a meaningful variable name


## Code clarity (**5 points**)
To earn full points, you must not have any `lintr()` errors. 
- -1 point for 1 - 2 `lintr` errors
- -2 points for 3 - 4 `lintr` errors
- -3 points for 5 - 6 `lintr` errors
- -4 points for 7 - 8 `lintr` errors
- -5 points for 9+ `lintr` errors
