library (ISLR2)
###########################################
##########################################
# Split a dataset randomly into two parts and set the training and the validation sets
#########################################
#########################################

## set the seed for random number generator
set.seed (1)
# make a training set
train <- sample (392, 196)
# fit a linear model with the training set
lm.fit <- lm(mpg ~ horsepower , data = Auto , subset = train)
attach (Auto)
# Evaluate the mse value on the validation set
mean ((mpg - predict (lm.fit , Auto))[-train ]^2)

# fit second order polynomial model on the training set and evaluate mse on the validation set
lm.fit2 <- lm(mpg ~ poly (horsepower , 2), data = Auto ,
                subset = train)
mean ((mpg - predict (lm.fit2 , Auto))[-train]^2)

# fit third order polynomial model on the training set and evaluate mse on the validation set
lm.fit3 <- lm(mpg ~ poly (horsepower , 3), data = Auto ,
                subset = train)
mean ((mpg - predict (lm.fit3 , Auto))[-train]^2)

## set a different seed and get different training/validation sets
set.seed (2)
train <- sample (392, 196)
lm.fit <- lm(mpg ~ horsepower , subset = train)
mean ((mpg - predict (lm.fit , Auto))[-train ]^2)

lm.fit2 <- lm(mpg ~ poly (horsepower , 2), data = Auto ,
                subset = train)
mean ((mpg - predict (lm.fit2 , Auto))[-train]^2)

lm.fit3 <- lm(mpg ~ poly (horsepower , 3), data = Auto ,
                subset = train)
mean ((mpg - predict (lm.fit3 , Auto))[-train]^2)


##########################################
########################################
# Play with cross-validation
########################################
#########################################

# compare glm without family argument and lm

glm.fit <- glm (mpg ~ horsepower , data = Auto)
coef (glm.fit)

lm.fit <- lm(mpg ~ horsepower , data = Auto)
coef (lm.fit)

#### Leave-one-out cross-validation
# perform leave-one-out cross-validation
library (boot)
glm.fit <- glm (mpg ~ horsepower , data = Auto)
cv.err <- cv.glm (Auto , glm.fit)
cv.err$delta

# perform LOOCV varying the polynomial model
cv.error <- rep (0, 10)
for (i in 1:10) {
   glm.fit <- glm (mpg ~ poly (horsepower , i), data = Auto)
   cv.error[i] <- cv.glm (Auto , glm.fit)$delta[1]
   }
cv.error

### k-fold cross-validation
set.seed (17)
cv.error.10 <- rep (0, 10)
for (i in 1:10) {
   glm.fit <- glm (mpg ~ poly (horsepower , i), data = Auto)
   cv.error.10[i] <- cv.glm (Auto , glm.fit , K = 10)$delta[1]
   }
cv.error.10

##########################################
########################################
# The bootstrap
########################################
#########################################

# write a function that evaluates a statistic with a given subset
alpha.fn <- function (data , index) {
   X <- data$X[index]
   Y <- data$Y[index]
   ( var (Y) - cov (X, Y)) / ( var (X) + var (Y) - 2 * cov (X, Y))
}
# one realization using the first 100 data points
alpha.fn(Portfolio , 1:100)

# another realization with randomly selected 100 data points with replacement
set.seed (7)
alpha.fn(Portfolio , sample (100, 100, replace = T))

# use the "boot" function
boot (Portfolio , alpha.fn, R = 1000)

### now perform bootstrap on linear regression
boot.fn <- function (data , index){
   coef (lm(mpg ~ horsepower , data = data , subset = index))
}
### compare the results with original lm result
boot.fn(Auto , 1:392)

set.seed (1)
boot.fn(Auto , sample (392, 392, replace = T))
boot.fn(Auto , sample (392, 392, replace = T))

summary (lm(mpg ~ horsepower , data = Auto))$coef

### bootstrap on the quadratic model
boot.fn <- function (data , index){
   coef (
    lm(mpg ~ horsepower + I(horsepower^2),
       data = data , subset = index)
    )
  }
set.seed (1)
boot (Auto , boot.fn, 1000)