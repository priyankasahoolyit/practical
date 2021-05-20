#Multiple Linear regression (MLR)

# check model assumptions
# Linearity: There is a linear relatonship among the variables
# Normality: Residuals are normally distributed
# Homoscedasticity: Residuals have a constant variance
# No Collinearity: Variables not linear combinations of each other
# Independence: Residuals are independent and not correlated

# use the state.x77 dataset -base package
help("state.x77")
head(state.x77, 15)
class(state.x77)
# Convert to a dataframe
states <- as.data.frame(state.x77)
class(states)
head(states, 15)
str(states)

colnames(states)[colnames(states) == "Life Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_Grad"
# Check for NA's
# using VIM 0r mice

# Check for linearity
# variables chosen will be used for the model
names(states)
# could remove a subset of the data first
#choose the vars and then show them in the pairs function ()
variables_of_interest <- c("Murder",
                           "Population",
                           "HS_Grad",
                           "Illiteracy",
                           "Income",
                           "Life_exp",
                           "Area",
                           "Frost")
variables_of_interest

library(psych)

pairs(states[variables_of_interest])

pairs.panels(states,
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


# Independent var = x-axis
# Dependent variabl = y-axis

attach(states)
scatter.smooth(x = Murder, y = Population,
               main = "Murder ~ Population",
               xlab = "Murder (per 100,000)",
               ylab = "Populaton (estimate)")

# check numerically the correlation of these variables
# value of -0.2 < x < 0.2
cor(Murder, Population) # Medium correlaton. Value = 0.3436428
# The correlation test shows that the correlaton between the murder and 
#population variable = 0.3436428 Indicating a Medium correlaton 


scatter.smooth(x = Murder, y = Frost,
               main = "Murder ~ Frost",
               xlab = "Murder (per 100,000)",
               ylab = "Frost (mean min temp below freezing )")

cor(Murder, Frost) # Medium correlaton. Value = -0.5388834
# The correlation test shows that the correlaton between the murder and 
#population variable = -0.5388834 Indicating a Medium correlaton 

paste("correlation for Murder and Frost:" , cor(Murder, Frost))
paste("correlation for Murder and Illiteracy:" , cor(Murder, Illiteracy))
paste("correlation for Murder and Population:" , cor(Murder, Population))
paste("correlation for Murder and HS_Grad:" , cor(Murder, HS_Grad))
paste("correlation for Murder and Income:" , cor(Murder, Income))
paste("correlation for Murder and HS_Grad:" , cor(Murder, HS_Grad))
paste("correlation for Murder and Life_Exp:" , cor(Murder, Life_Exp))
paste("correlation for Murder and Area:" , cor(Murder, Area))


#The correlation between murder and area is very low. 
#Therefore the area variable is removed from the liat of variables

# Decided to remove the "area" variable
states <- subset(states, select = -c(Area)) 
head(states)

# Chec for outliers

opar <- par(no.readonly = TRUE)
par(mfrow = c(2,4)) # charts shown in 4 rows x 2 cols

boxplot(Murder, 
        main = "Murder", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Murder)$out))

boxplot(Population, 
        main = "Population", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Population)$out))

boxplot(HS_Grad, 
        main = "Graduation", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(HS_Grad)$out))

boxplot(Illiteracy , 
        main = "Illiteracy", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Illiteracy)$out))

boxplot(Income , 
        main = "Income", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Income)$out))


boxplot(Frost , 
        main = "Frost", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Frost)$out))



boxplot(Life_Exp , 
        main = "Life_Exp", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Life_Exp)$out))

par(opar)

#use boxplot.stats(0 function to extract the outliers
outlier_values <- boxplot.stats(Population)$out
paste ("Population outliers:", paste(outlier_values, collapse = ", "))

# repeat for the Income var

outlier_values <- boxplot.stats(Income)$out
paste ("Income outliers:", paste(outlier_values, collapse = ", "))

# Remove outliers
state_ <- subset(states, Population != 21198 & 
                   Population != 11197
                 & Population != 18076
                 & Population != 11860
                 & Population != 12237)


# Remove income outliers
states <- subset(states, Income != 6315 )


boxplot(Population, 
        main = "Population", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Population)$out))


boxplot(Income , 
        main = "Income", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Income)$out))

# Decide whether to delete a few more outliers and what this results in 
# Cecknormality
libraray(e1071)

opar <- par(no.readonly = TRUE)
par(mfrow = c(2,4)) # charts shown in 4 rows x 2 cols





# check normality using QQnorm





attach(states)
mlr_model <- lm(Murder ~ Illiteracy + Population + HS_Grad + Income + Frost, data = states  )
summary(mlr_model)
