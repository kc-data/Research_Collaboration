# Author: Rory Pulvino
# Date:   July 20, 2017
# Re:     Cleaning up data 
# Info:   Functions to help clean up and add variables to data

# Function to use a list to create aggregate from one column in creating a new column
create_aggr_column <- function(list, data = df, columnToUse, columnToChange, varInput){
  require(stringr)
  # Setting up column names to use
  col1 <- deparse(substitute(columnToUse))
  col2 <- deparse(substitute(columnToChange))
  
  # Changing the row entry based on the list of strings
  for(x in list){
    data[[col2]] <- ifelse(str_detect(data[[col1]], x), varInput, as.character(data[[col2]]))
  }
  
  return(data)
}

# Function creates two new columns. NewCol1 groups a dataframe based on an ID
# and then NewCol1 is created based on the dummy column, columnToUse, and then is filled
# based on missing values. NewCol2 groups by ID then sums a dummy column, columnToUse,
# and distributes this sum to all rows of ID
AggregatedColumns <- function(df, columnToUse, NewCol1, NewCol2) {
  # Needed libraries
  library(lazyeval)
  library(tidyr)
  library(dplyr)
  
  # Setting up column names to use
  columnToUse <- deparse(substitute(columnToUse))
  NewCol1 <- deparse(substitute(NewCol1))
  NewCol2 <- deparse(substitute(NewCol2))
  
  # Creating new columns 
  df[[NewCol1]] <- ifelse(df[[columnToUse]] == 1, 1, NA)
  df <- df %>% group_by_("UID") %>% fill_(NewCol1) %>% fill_(NewCol1, .direction = 'up')
  df[[NewCol1]] <- ifelse(is.na(df[[NewCol1]]), 0, df[[NewCol1]])
  
  #df <- df %>% group_by(UID) %>% sort(df[[columnToUse]], decreasing = TRUE) %>% fill_(NewCol1) 
  #df <- df %>% group_by(UID) %>% sort(df[[columnToUse]], decreasing = TRUE) %>% fill_(NewCol1, .direction = 'up')
  
  # Counting up total offenses
  mutate_call = lazyeval::interp(~sum(a), a = as.name(columnToUse))
  df <- df %>% group_by_("UID") %>% mutate_(.dots = setNames(list(mutate_call), NewCol2))
  
  df
}

# Create new columns of dummies based on a list of strings to match in a different column
create_dummies <- function(list, data = df, columnToUse, columnToCreate){
  # Needed library
  require(stringr)
  
  # Setting up column names to use
  col1 <- deparse(substitute(columnToUse))
  col2 <- deparse(substitute(columnToCreate))
  
  # initiaize new column
  data[, col2] <- 0
  
  # Changing the row entry based on the list of strings
  for(x in list){
    data[[col2]] <- ifelse(str_detect(data[[col1]], x), 1, data[[col2]])
  }
  
  return(data)
}