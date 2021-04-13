getwd()

managers_data <- read.csv("managers.csv")  
new_managers_data <- read.csv("MoreData.csv")  
str(new_managers_data)

#show headers from both dataframes
names(managers_data)
names(new_managers_data)

include_list <- new_managers_data[c("Date","Country","Gender","Age","Q1","Q2","Q3","Q4","Q5")]
include_list



# This is how we combine the data frames
# Now this will not work
rbind(managers_data, include_list)

str(managers_data)
str(include_list)

# create new AgeCat variable in include_list
# and calculate containing variables
attach(include_list)
include_list$AgeCat[Age >=45] <- "Elder"
include_list$AgeCat[Age >=26 & Age <=44] <- "Middle Aged"
include_list$AgeCat[Age <25] <- "Young"

# if NA is found, categorise as Elder
include_list$AgeCat[is.na(Age)] <- "Elder"
detach(include_list)

# Dropping the X variable 
names(managers_data)

modified_managers <- managers_data[2:11]
modified_managers


#update the date fields in both dataframes
# so that they are in correct format

# The datetime field is converted to a date variable in mm/dd/yyyy format from chr variable.

modified_managers$Date <- as.Date(modified_managers$Date, "%m/%d/%y")
str(modified_managers)

include_list$Date <- as.Date(include_list$Date, "%m/%d/%Y")
str(include_list)







#combine the dataframes
combined_managers <- rbind(modified_managers, include_list)

#set AgeCat with ordered factor
# so that young < middle_age < elder

combined_managers$AgeCat <- factor(combined_managers$AgeCat, 
                                   levels = c("Young", "Middle Aged", "Elder", ordered = TRUE))

str(combined_managers)


