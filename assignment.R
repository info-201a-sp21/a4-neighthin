# A4 Data Wrangling

# We provide this line to delete all variables in your workspace.
# This will make it easier to test your script.
rm(list = ls())

 # Loading and Exploring Data -------------------------------- (**29 points**)

# First, search online for a dplyr cheatsheet and put the link to one you
# like in the comments here (it's ok if the link makes the line too long):

# - https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf


# To begin, you'll need to download the Kickstarter Projects data from the
# Kaggle website: https://www.kaggle.com/kemical/kickstarter-projects
# Download the `ks-projects-201801.csv` file into a new folder called `data/`

# Load the `dplyr` package

library(dplyr)

# If your computer isn't in English, you made need to use this line of code
# to get the csv to load correctly (if the data gets messed up a few rows in):
# Sys.setlocale("LC_ALL", "English")

# Load your data

ks_project <- read.csv("data/ks-projects-201801.csv", stringsAsFactors = FALSE)

# To start, write the code to get some basic information about the dataframe:
# - What are the column names?
# - How many rows is the data frame?
# - How many columns are in the data frame?

colnames(ks_project)
nrow(ks_project)
ncol(ks_project)

# Use the `summary` function to get some summary information

summary(ks_project)

# Unfortunately, this doesn't give us a great set of insights. Let's write a
# few functions to try and do this better.
# First, let's write a function `get_col_info()` that takes as parameters a
# column name and a dataframe. If the values in the column are *numeric*,
# the function should return a list with the keys:
# - `min`: the minimum value of the column
# - `max`: the maximum value of the column
# - `mean`: the mean value of the column
# If the column is *not* numeric and there are fewer than 10 unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `unique_values`: a vector of each unique value in the column
# If the column is *not* numeric and there are 10 or *more* unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `sample_values`: a vector containing a random sample of 10 column values
# Hint: use `typeof()` to determine the column type

get_col_info <- function(colname, df) {
  values <- df %>% pull(colname)
  if (is.numeric(values)) {
    returned_info <- list(min = min(values, na.rm = TRUE),
                        max = max(values, na.rm = TRUE),
                        mean = mean(values, na.rm = TRUE))
  } else if (length(unique(values)) < 10) {
    returned_info <- list(n_values = length(unique(values)),
                        unique_values = unique(values))
  } else {
    returned_info <- list(n_values = length(unique(values)),
                        sample_values = sample(unique(values), 10))
  }
  return(returned_info)
}

# Demonstrate that your function works by passing a column name of your choice
# and the kickstarter data to your function. Store the result in a variable
# with a meaningful name

function_test_1 <- get_col_info("category", ks_project)

# To take this one step further, write a function `get_summary_info()`,
# that takes in a data frame  and returns a *list* of information for each
# column (where the *keys* of the returned list are the column names, and the
# _values_ are the summary information returned by the `get_col_info()` function
# The suggested approach is to use the appropriate `*apply` method to
# do this, though you can write a loop

get_summary_info <- function(df) {
  sapply(colnames(df), get_col_info, df)
}

# Demonstrate that your function works by passing the kickstarter data
# into it and saving the result in a variable

function_test_2 <- get_summary_info(ks_project)

# Take note of 3 observations that you find interesting from this summary
# information (and/or questions that arise that want to investigate further)

# 1) For each column it tells u the type stored in it, such as character,
# double or integer. 2) The lists returned are in an organized fashion,
# making reading it much easier. 3) each list produced is kind of like a
# dataframe. It has it's 3 columns when you click to see into the list, and
# you read the info of whats inside it across the rows.


# Asking questions of the data ----------------------------- (**29 points**)

# Write the appropriate dplyr code to answer each one of the following questions
# Make sure to return (only) the desired value of interest (e.g., use `pull()`)
# Store the result of each question in a variable with a clear + expressive name
# If there are multiple observations that meet each condition, the results
# can be in a vector. Make sure to *handle NA values* throughout!
# You should answer each question using a single statement with multiple pipe
# operations!
# Note: For questions about goals and pledged, use the usd_pledged_real
# and the usd_goal_real columns, since they standardize the currancy.

# What was the name of the project(s) with the highest goal?

highest_goal <- ks_project %>%
  filter(usd_goal_real == max(usd_goal_real), na.rm = TRUE) %>%
  select(name) %>%
  pull(name)

# What was the category of the project(s) with the lowest goal?

lowest_goal <- ks_project %>%
  filter(usd_goal_real == min(usd_goal_real), na.rm = TRUE) %>%
  select(category) %>%
  pull(category)

# How many projects had a deadline in 2018?
# Hint: start by googling "r get year from date" and then look up more about
# different functions you find

deadline_2018 <- format(as.Date(ks_project$launched), format = "%Y")
ks_project$year <- deadline_2018
ks_project %>%
  filter(year == "2018") %>%
  summarise(year, n()) %>%
  select("n()") %>%
  slice(1L)

# What proportion of projects weren't marked successful (e.g., failed or live)?
# Your result can be a decimal

not_successful <- ks_project %>%
  filter(state == "failed" | state == "live")
answer <- nrow(not_successful) / nrow(ks_project)

# What was the amount pledged for the project with the most backers?

most_backers <- ks_project %>%
  filter(backers == max(backers)) %>%
  group_by(usd_pledged_real) %>%
  summarise(usd_pledged_real) %>%
  pull(usd_pledged_real)

# Of all of the projects that *failed*, what was the name of the project with
# the highest amount of money pledged?

highest_amount_failed_projects <- ks_project %>%
  filter(state == "failed") %>%
  filter(usd_pledged_real == max(usd_pledged_real)) %>%
  pull(usd_pledged_real)

# How much total money was pledged to projects that weren't marked successful?

total_not_successful <- not_successful %>%
  select(usd_pledged_real) %>%
  sum()

# Performing analysis by *grouped* observations ----------------- (31 Points)

# Which category had the most money pledged (total)?

most_pledged <- ks_project %>%
  group_by(category) %>%
  summarize(total_pledged = sum(usd_pledged_real)) %>%
  filter(total_pledged == max(total_pledged)) %>%
  pull(category)

# Which country had the most backers?

most_backers <- ks_project %>%
  group_by(country) %>%
  summarise(total_backers = sum(backers)) %>%
  filter(total_backers == max(total_backers)) %>%
  pull(country)

# Which year had the most money pledged (hint: you may have to create a new
# column)?
# Note: To answer this question you can choose to get the year from either
# deadline or launched dates.

most_year_pledged <- ks_project %>%
  mutate(year = str_sub(launched, 1, 4)) %>%
  group_by(year) %>%
  summarise(total_pledged = sum(usd_pledged_real)) %>%
  filter(total_pledged == max(total_pledged)) %>%
  pull(year) %>%
  as.numeric()

# Write one sentence below on why you chose deadline or launched dates to
# get the year from:

# I chose to use launched dates, because I assume that the projects received
# their pledged money before hand to fund their projects, as I don't think they
# would have received their pledges after completing the projects.

# What were the top 3 main categories in 2018 (as ranked by number of backers)?

top_three_2018 <- ks_project %>%
  filter(year == 2018) %>%
  group_by(main_category) %>%
  summarize(total_backers = sum(backers)) %>%
  arrange(desc(total_backers)) %>%
  top_n(3) %>%
  pull(main_category)

# What was the most common day of the week on which to launch a project?
# (return the name of the day, e.g. "Sunday", "Monday"....)

most_common_day <- ks_project %>%
  mutate(day_of_week = weekdays(as.Date(launched))) %>%
  group_by(day_of_week) %>%
  summarise(count = n()) %>%
  filter(count == max(count)) %>%
  pull(day_of_week)

# What was the least successful day on which to launch a project? In other
# words, which day had the lowest success rate (lowest proportion of projects
# that were marked successful )?
# Hint: Try googling "r summarize with condition in dplyr"

launches_by_day <- ks_project %>%
  mutate(day_of_week = weekdays(as.Date(launched))) %>%
  group_by(day_of_week) %>%
  summarize(count = n())

successes_by_day <- ks_project %>%
  filter(state == "successful") %>%
  mutate(day_of_week = weekdays(as.Date(launched))) %>%
  group_by(day_of_week) %>%
  summarize(success = n())

lowest_successful_day <- left_join(launches_by_day, successes_by_day) %>%
  mutate(success_rate = success / count) %>%
  filter(success_rate == min(success_rate)) %>%
  pull(day_of_week)