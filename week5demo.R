# Read in the dataset first
# Data needs to be in the workign directory
getwd()
diabetes_data <- read.csv("diabetes.csv" , na = "")
str(diabetes_data)

# Check for missing variables. Delete if there are any 
# Examine missing data
incomplete_data <- diabetes_data[!complete.cases(diabetes_data),]
incomplete_data

missing_values <- aggr(diabetes_data, prop = FALSE, numbers = TRUE)

# We need to check whether there’s any missing data. 
# Display the missing values in the dataset. 
# Decide what to do with missing values. What should you do with missing data? 
# What effect will it have on remaining data? Choose what to do based on your analysis of missing values.

install.packages("mice")
library(mice)
md.pattern(diabetes_data)


# Installed VIM package and displayed the missing values
install.packages("VIM")
library(VIM)
missing_values <- aggr(diabetes_data, prop = FALSE, numbers = TRUE)

# show summary of the content of missing_values 
summary(missing_values)

missing_values <- aggr(diabetes_data, prop = FALSE, numbers = TRUE)
missing_values


# create a new variable  called date that contains month and year data

#diabetes_data$Date <- as.Date(paste(diabetes_data$Month, diabetes_data$Year, sep='-'), "%m-%Y")
#str(diabetes_data)


diabetes_data$Date <- paste(diabetes_data$Month, diabetes_data$Year, sep='/')
str(diabetes_data)

# Change the date variable to a date
# Date has a particular date requirement as it should contain
# day, month, Year

# Add element 
converted_date$Date <- paste("01", diabetes_data$Month, diabetes_data$Year, sep='/')
converted_date

diabetes_data$Date <- as.Date(converted_date, "%d/%m/%Y")
str(diabetes_data$Date)

# Plot the status variable using the plot() function
# Convert the factor list
# you could plot the summary() of the status variable data

diabetes_data$Status <- factor(diabetes_data$Status)
str(diabetes_data)
plot(diabetes_data$Status)
summary(diabetes_data$Status)

# Add titles to the chart that are relevant
attach(diabetes_data)
display_settings <- par(no.readonly = TRUE)
plot(Status, main = " Number of patients recoveries", xlab = "Daibetes Status", ylab = "No of Patients")

# Save the modified diabetes data frame
write.csv(diabetes_data, file = "diabetes-data-modified.csv")


