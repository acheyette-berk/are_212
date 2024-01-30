# Problem Set 1 - ARE 212 - Spring 2024 
# Anna Cheyette
# January 23, 2024

library(haven)
library(tidyverse)

# get directory of current file
current_directory <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))

# read in data
carsdf <- read_dta(file.path(current_directory, "Data",
                             "pset1_2024.dta"))

# work with data
summary(carsdf)
