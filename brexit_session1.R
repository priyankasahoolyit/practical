# Read the dataset first
brexit_data <- read.csv ("data_brexit_referendum.csv", na = "")

#Examine the structure
str(brexit_data)
class(brexit_data)
head(brexit_data, 5)

# Count the number of -1 in the leave column
leave_issue <- brexit_data(brexit_data['Leave'] == "-1")

nrow(brexit_data[brexit_data$Leave == -1,])
sum(brexit_data$Leave[brexit_data$Leave == -1])

# Replace these with NA

brexit_data$Leave[brexit_data$Leave == -1] <- NA

# Verify that the "-1" values have been replaced

sum(brexit_data$Leave[brexit_data$Leave == -1])

#view the no of records with NA

na_records <- brexit_data[!complete.cases(brexit_data),]
  
# count the no of NA records
nrow(na_records)

# visualise the patterns of NA data
# using mice or VIM
library(mice)
md.pattern(brexit_data)

#VIM
library(VIM)
missing_values <- aggr(brexit_data, prop = FALSE, numbers = TRUE)
summary(missing_values)

# Looking at the proportions of voters who
# are in favour of leaving the EU
# create a new variable to decide whether each area (ward) decided
# to remain or leave the EU
# Examine the proportion of leave and NVotes
brexit_data$Proportion <- brexit_data$Leave/brexit_data$NVotes


# craete a new variable and figure out
# whether each ward decided to leave or remain
# we'll  use the  rproportion variable  to make  this decision
# If proportion < 0.5 = remain
# IF proprotion > 0.5 = leave
brexit_data$Vote[brexit_data$Proportion <= 0.5] <- "Remain"
brexit_data$Vote[brexit_data$Proportion > 0.5] <- "Leave"

str(brexit_data)
# var is a character so no need to convert first
attach(brexit_data)

brexit_data$RegionName[RegionName == "London"] <- "L"
brexit_data$RegionName[RegionName == "North West"] <- "NW"
brexit_data$RegionName[RegionName == "North East"] <- "NE"
brexit_data$RegionName[RegionName == "South West"] <- "SW"
brexit_data$RegionName[RegionName == "South East"] <- "SE"
brexit_data$RegionName[RegionName == "East Midlands"] <- "EM"
brexit_data$RegionName[RegionName == "West Midlands"] <- "WM"
brexit_data$RegionName[RegionName == "East of England"] <- "EE"
brexit_data$RegionName[RegionName == "Yorkshire and the Humber"] <- "Y"

summary(brexit_data)

is.numeric(Proportion)
is.null(RegionName)

# use sapply90 function  to examine the structure
# ofeach variable
numeric_variable_list <- sapply(brexit_data, is.numeric)
class(numeric_variable_list)

# we can use this logic  to create a subset  of the data 
mumerical_data <- brexit_data[numeric_variable_list]
head(numeric_variable_list, 5)

# Remove the Id field as it is no benefit
# Remove it from the numerical variable list 
numeric_variable_list["ID"] <- FALSE

# we can use this logic  to create a subset  of the data 
# now the ID field is removed
numerical_data <- brexit_data[numeric_variable_list]
head(numeric_variable_list, 5)
numerical_data

# We can use the apply() function to return  a list
# where  each list  member has a corresponding name
lapply(numerical_data, summary)

names(brexit_data)
