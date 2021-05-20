# use dataset called "women" containing
# height and weight for  15 women between 30 -39 
# dependent variable = weight
# independent variable = height

attach(women)
simple_linear_model <- lm(weight ~ height, data = women)
simple_linear_model

plot(height, 
     weight, 
        main = "Scatter plot showing regression linefor weight predicted from height",
        xlab = "Height (inches)",
        ylab = "Weight (lbs)" )
abline(simple_linear_model)
summary(simple_linear_model)

#corelation coefficients of 2 variables
confint(simple_linear_model)
cor(height,weight)

summary(simple_linear_model)

#mtcars dataset - predict car stopping distance from speed

# First step - check  model assumptions
# For Linear Models

# Linearity - Linear Relatonship
# Normality - residuals are normally distributed
# Homoscedasticity - residuals have a constant variance
# No Collinearity - not linear combinations of each other
# Independence -residuals are independent and not correlated


# Check Linearity
# speed = independent var
# Distance = dependent var

detach(cars)
attach(cars)
str(cars)
scatter.smooth(x = speed, 
               y = dist,
               main = "Distance ~ speed",
               xlab = "car speed (mph)",
               ylab = "stopping distance (feet)")

# check correlation
cor(speed, dist)

# 0.8068949 = high correlation

# check for outliers 
# outliers = 1.5 *  IQR

opar <- par(no.readonly = TRUE)
par((mfrow = c(1,2) ) # 1 row x cols
    
boxplot(speed, 
        main = "Speed",
        sub = paste("outliers rows: ",
                    boxplot.stats(speed)$out))   

boxplot(dist, 
        main = "Distance",
        sub = paste("outliers rows: ",
                    boxplot.stats(speed)$out)) 

par <- opar

# remove 1 outlier where dist = 120
cars <- subset(cars, cars$dist != 120)

# check for normality
install.pacakges("e1071")
library(e1071)

#Skewness < -1 or > 1 = Highly skewed
# moderately skewed = -1 to -0.5 and 0.5 to 1
# Highly skewed =< -1  or > 1
# -0.5  to 0.5 = approx Symmetrical

opar <- par(no.readonly = TRUE)
plot(density(speed), 
     main = "Density plot: Speed", 
     ylab = "Frequency",
     sub= paste("Skewness: ", round(e1071::skewness(speed), 2)))

# Fill in  the area  under the plot with red
polygon(density(speed), col = "red")
# skewness = -0.11 = approx Symmetrical


plot(density(dist), 
     main = "Density plot: Speed", 
     ylab = "Frequency",
     sub= paste("Skewness: ", round(e1071::skewness(dist), 2)))

# Fill in  the area  under the plot with red
polygon(density(dist), col = "red")
# skewness = -0.11 = approx Symmetrical

par <- opar

# using qqnorm()
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2))
hist(dist)
qqline(dist)
par = opar


# create training and testing dataset
set.seed(1)
no_rows_data <- nrow(cars)
data_sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- cars[data_sample, ]
testing_data <- cars[-data_sample, ]

# 34 + 15 = 49

linear_mod <- lm(dist ~ speed, data = training_data)
linear_mod

summary(linear_mod)

# significace of Model
# t value
# p Value

# prediction
predictive_distance <- predict(linear_mod, testing_data )

actual_predictions <- data.frame(cbind(actuals = testing_data$dist, predicted = Predictive_distance ))

head(actual_predictions, 15)

correlation_accuracy <- cor(actual_predictions)
correlation_accuracy
