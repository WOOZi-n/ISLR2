# Install packages
install.packages("MASS")
install.packages("ISLR2")
# load packages
library (MASS)
library (ISLR2)
# Print the head of the data
head(Boston)
#########################################
## Let's try fitting a simple linear model
#########################################
# first trial
lm.fit <- lm(medv~lstat)
# second trial 
lm.fit <- lm(medv~lstat , data = Boston)
# print the result
lm.fit
# print the summaries of the result
summary(lm.fit)
# see what "lm.fit" includes
names(lm.fit)
# Get confidence intervals
confint(lm.fit)
# Find the fitted values
predict(lm.fit , data.frame(lstat = (c(5, 10, 15))),
         interval = "confidence")
predict (lm.fit , data.frame(lstat = (c(5, 10, 15))),
         interval = "prediction")
#########################################
## Play with visualization tools
#########################################
# Plot lstat vs. medv
plot(Boston$lstat , Boston$medv)
# Add the regression line
abline(lm.fit)
# Draw a thicker line
abline (lm.fit , lwd = 3)
# Change the color of the line
abline (lm.fit , lwd = 3, col = "red")
# Change the color of the points
plot (Boston$lstat , Boston$medv , col = "red")
# Change the shape of the points
attach(Boston)
plot (lstat , medv , pch = 20)
plot (lstat , medv , pch = "+")
plot (1:20, 1:20, pch = 1:20)

#####################################
## Let's try fitting a multiple linear model
#####################################
lm.fit.multi <- lm(medv~lstat + age , data = Boston)
summary (lm.fit.multi)
lm.fit.multi2 <- lm(medv~. - age , data = Boston)
summary (lm.fit2)
#####################################
## Let's try fitting a linear model with interaction terms
#####################################
summary (lm(medv~lstat*age , data = Boston))

#####################################
## Let's try fitting a linear model with qualitative predictor variables
#####################################
head(Carseats)
lm.fit.cat <- lm(Sales~. + Income:Advertising + Price:Age ,
               data = Carseats)
summary (lm.fit.cat)
attach(Carseats)
contrasts (ShelveLoc)

#####################################
## Let's write a function
#####################################
add_two_values = function(x,y){
  z = x+y
  return(z)
}

add_two_values(2,3)
