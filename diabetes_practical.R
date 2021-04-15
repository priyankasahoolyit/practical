# Read in the dataset first
# Data needs to be in the workign directory
getwd()
# Move data from blackboard to WD in files window
diabetes_data <- read.csv("Diabetes-md.csv" , na = "")
str(diabetes_data)
class(diabetes_data)
dim(diabetes_data)
row(diabetes_data)
names(diabetes_data) # Show headers of the data

# Examine missing data
diabetes_data[!complete.cases(diabetes_data),]

# diabetes_data$Status[diabetes_data$Status == ""] <- NA

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

# dealing with missing data
# I've decided to keep only data with missing type and health status
# And the addresses are not important

keep_this_data <- diabetes_data[!complete.cases(diabetes_data$Daibetes.type, diabetes_data$Status),]
keep_this_data
dim(keep_this_data)

# 15rows with missing data I'd like to keep
# Reverse the logic to contain relevant data

keep_this_data <- diabetes_data[complete.cases(diabetes_data$Daibetes.type, diabetes_data$Status),]
keep_this_data

dim(keep_this_data)
dim(diabetes_data)

# replace the original dataframe with the content of the processed data
diabetes_data <- keep_this_data
head(diabetes_data, 15)

#5- Define new column names
col_names <- c("Paitent Name", "NI address", "Type", "Age", "Health status")
colnames(diabetes_data) <- col_names

#4 - Configure Type to an un ordered factor with 2 levels

diabetes_data$Type <- factor(diabetes_data$Type, order=FALSE, levels = c("Type-I", "Type-II"))

str(diabetes_data)

# lets look at the class of each element in the dataframe
class_list <- lapply(diabetes_data, class)
class_list

#build the data frame first

dose <- c(20, 30, 40, 45, 60)
drug_a <- c(16, 20, 27, 40, 60)
drug_b <- c(15, 18, 25, 31, 40)

# create a new dataframe
drugs <- data.frame(dose, drug_a, drug_b)
str(drugs) 
class(drugs)

# General plot of the data
plot(drugs)

attach(drugs)
plot(dose, type="o", col="blue")
?plot

#option type = "b" shows both lines should be plotted
plot(dose, drug_a, type="b")

# parameter customise any setting such as fonts, colours etc.
# par = parameters

opar <- par(no.readonly = TRUE)

# lty = line type
# lwd = line width
# lty = 2 dashed line
# pch = 17 solid triangle

par(lty = 2, pch = 17)
plot(dose, drug_a, type="b")
par = opar

plot(dose, drug_a, type="b", lty = 2, pch = 17)

graph_range <- range(0, drug_a, drug_b)
graph_range
plot(drug_a, type="o", col = "blue", ylim = graph_range, axes = FALSE, ann = FALSE)
lines(drug_b, type="o", pch = 22, lty = 2, col= "red")

# make the axes label
axis(1, at = 1:5, labels = c("20ml","40ml","60ml", "80ml","100ml"))

# make the y axis that displays ticks at
# every 5 marks
# marks = 5 *

axis(2, las = 1, at = 5*0:graph_range[2] )









include_list <- new_managers_data[c("Date", "Country", "Gender", "Age", "Q1", "Q2", "Q3", "Q4", "Q5")]
include_list
# OR
include_list <- new_managers_data[c(6,3,7,4,8:12)]
include_list

# We can't use rbind() function yet
rbind(managers_data, include_list)

# We can create blank columns first and then import into our data frame
blank_vectors <- c("AgeCat", "Answer total", "mean value")
include_list [ , blank_vectors] <- NA
include_list

# OR we can calculate values
attach(include_list)
include_list$AgeCat[Age >= 45] <- "Elder"
include_list$AgeCat[Age >= 26 & Age <= 44] <- "Middle Aged"
include_list$AgeCat[Age <= 25] <- "Young"
include_list$AgeCat[is.na(Age)] <- "Elder"
detach(include_list)

include_list

# Then calculate answer total

attach(include_list)
include_list$`Answer total` <- Q1 + Q2 + Q3 + Q4 + Q5

# Calculate mean value for each row
include_list$`mean value` <- rowMeans(include_list[5:9])

# Now we need to convert the date field to date
str(include_list)

# Examine the date field
str(include_list$Date)

# Default date format in R is yyyy-mm-dd

# Define position of date contents first 
# in the imported data frame eg using %d, %m etc
# R then puts the date field into default yyyy-mm-dd
# format when using Date data type
restructured_date <- as.Date(include_list$Date, "%m/%d/%Y")
restructured_date
# Check date structure
str(restructured_date)

# We can convert the date structure to improve readability
# to display a diffent date output
# But this is not used when storing the date values in a data frame
date_format <- "%m/%d/%Y"
new_date_format <- "%d/%m/%Y"
formatted_date <- format(as.Date(restructured_date, format = date_format), new_date_format)
formatted_date
# Date is now a character as it is not in the
# default date structure
str(formatted_date)

include_list$Date = restructured_date
include_list
str(include_list)

# Check date field of managers data frame
managers$Date
# Needs converting - currently in US format
converted_date <- as.Date(managers$Date, format = "%Y-%d-%m")
converted_date
class(converted_date)

# Now assign this to the managers data frame
managers$Date <- converted_date
str(managers)

# Now we can combine both datasets with
# rbind function
managers <- rbind(managers, include_list)
managers
str(managers)

# Set AgeCat with ordered factors
managers$AgeCat <- factor(managers$AgeCat, levels = c("Young", "Middle Aged", "Elder"), ordered = TRUE)
