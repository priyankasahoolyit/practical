# use statistical methods to examine 
# the relationship between our variables of interest

# beavers dataset
# contains the body temp of 4 beavers captured every 10 mins
# over a day
# We want to examne difference in average body temp
# to evaluate whether body temp effected by activity

#H0: mean beaver temperature is not affected by activity
#H1: mean beaver temperature is affected by activity

?beavers
str(beaver2)

# vars we will need
# we will evaluate beaver temp and activity

# temp = continuous variable
# activity = categorical dichotomos variable

# Copy the data into dataframe
beavers_data <- beaver2
str(beavers_data)
head(beavers_data)

# convert the activ variable to a categorical dichotomous variable
beavers_data$activ <- factor(beavers_data$activ, labels = c("No", "Yes")) 
str(beavers_data)

install.packages("psych")
library(psych)

pairs.panels(beavers_data,
             smooth = TRUE, # If TRUE, draws loess smooths
             scale = FALSE, # If TRUE, scales the correlation text font    
             density = TRUE, # If TRUE, adds density plots and histograms    
             ellipses = TRUE, # If TRUE, draws ellipses    
             method = "spearman",# Correlation method (also "pearson" or "kendall")    
             pch = 21, # pch symbol    
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit    
             cor = TRUE, # If TRUE, reports correlations    
             jiggle = FALSE, # If TRUE, data points are jittered    
             factor = 2, # Jittering factor    
             hist.col = 4, # Histograms color    
             stars = TRUE, # If TRUE, adds significance level with stars    
             ci = TRUE) # If TRUE, adds confidence intervals   


#Q2

attach(beavers_data)
plot(activ, temp, pch = 9, col= "lightblue")
# We can split the dictomous var into 2
#and then examine the data

library("lattice")
#
histogram(~temp | activ,
          data = beavers_data,
          main = "Distributation of beavers activity data",
          xlab = "Temperature (degree)" ,ylab = "Activity")
#Visual analysis seems to indicate the data normally distributed
#Summarize the
tapply(temp, activ, median)

#Quantile-quantile plot (Q-Q plot) allows us to check
#if the data is normally distributed or not 

#Is temp normally distributed?
qqnorm(temp)
# Add line that represents normal distribution
qqline(temp, col = "red")

# Temp appears not to be normally distributed

with(beavers_data, 
     qqplot(temp[activ == "Yes"], 
            temp[activ == "No"], 
            main = "Comapring 2 samples of cativity data", 
            xlab = "Acitive temp = Yes", 
            ylab = "Active temp = No"))
#We can add normality line to the plot
# to help evaluate normality
with(beavers_data, {
  qqnorm(temp[activ == "No"],
         main = "Inactive data")
  qqline(temp[activ == "No"])
})

#Formal test of normality
#Shapiro-Wilks test
#p-Value tells us the cahnes that the sample
#comes form a normal distribution
#if p>0.05 = normally distributed
normality_test <- shapiro.test(beavers_data$temp)
normality_test$p.value
# p-value = 7.763623e-05

# This test doesnt work on dicotomous variable
with(beavers_data, tapply(temp, activ, shapiro.test))

#Result show
# No = p-value = 0.1231 >0.05 normally distributed
# Yes = p-value = 0.5583  0.05 normally distributed
# temp = not normally distributed

# After consulting  the chart, I am amining
# a dependent variable (temp)
# with an independent categorical var (activ)
# format wilcox.test (dependent ~ independent)
wilcox.test(temp~activ)
# cut off = 0.05
#p_value < = 2.2e-16 (2.2 power)

#p-value < 0.05 so this indicates the NULL (H0) hypothesis rejected.
# Therefore this indicates that beaver body temperature
# is affected by activity (p = 2.2e-16)
