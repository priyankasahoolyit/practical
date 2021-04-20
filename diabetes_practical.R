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
