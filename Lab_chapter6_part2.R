######################################################
####### Choose variagbles using three methods
#####################################################
library (ISLR2)
View (Hitters)
names (Hitters)

### preliminary step
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters <- na.omit(Hitters)
dim(Hitters)

x <- model.matrix (Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary
##########################
######## Ridge regression
############################
library (glmnet)
# set a vector of lambda values
grid <- 10^seq (10, -2, length = 100)
# evaluate the ridge regression with the lambda values above
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
dim ( coef (ridge.mod))

## check the 50th lambda value and its corresponding estimation results. 
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
# check the evaluated penalty term
sqrt ( sum ( coef (ridge.mod)[-1, 50]^2))
## check the 60th lambda value and its corresponding estimation results. 
ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
# check the evaluated penalty term
sqrt ( sum ( coef (ridge.mod)[-1, 60]^2))
# compare two estimation results
coef(ridge.mod)[, c(50, 60)] 
# fit the data with lambda=50
predict (ridge.mod , s = 50, type = "coefficients")[1:20, ]

#### split the test and the training sets
set.seed (1)
train <- sample (1: nrow (x), nrow (x) / 2)
test <- (-train)
y.test <- y[test]
## fit the model on a training set
ridge.mod <- glmnet(x[train , ], y[train], alpha = 0,
                       lambda = grid, thresh = 1e-12)
## find the fitted value on a test set
## evaluate validation errors with various lambda values
ridge.pred <- predict(ridge.mod , s = 4, newx = x[test , ])
mean ((ridge.pred - y.test)^2)
ridge.pred <- predict(ridge.mod , s =  1e10, newx = x[test , ])
mean ((ridge.pred - y.test)^2)

ridge.pred <- predict (ridge.mod , s = 0, newx = x[test , ],
                       exact = T, x = x[train , ], y = y[train])
mean ((ridge.pred - y.test)^2)

## Choosing the appropriate value of lambda via CV
set.seed (1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 0)
plot (cv.out)
## the lambda value that minimizes cv mse.
bestlam <- cv.out$lambda.min
bestlam
## check out the validation error with the chosen lamda value
ridge.pred <- predict (ridge.mod , s = bestlam ,
                       newx = x[test , ])
mean ((ridge.pred - y.test)^2)
## Now, fit the whole data with the chosen tuning paramter labmda.
out <- glmnet (x, y, alpha = 0)
predict (out , type = "coefficients", s = bestlam)[1:20, ]

##########################
######## Lasso regression
############################
## Fit a lasso model
lasso.mod <- glmnet (x[train , ], y[train], alpha = 1,
                     lambda = grid)
# draw the coefficients over the change of tuning parameters
plot(lasso.mod)
## perform CV to choose the value of lambda
set.seed (1)
cv.out <- cv.glmnet (x[train , ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
# check out the validation error with the chosen lambda value
lasso.pred <- predict (lasso.mod , s = bestlam ,
                         newx = x[test , ])
mean ((lasso.pred - y.test)^2)
# fit the wholde data with the chosen lambda value
out <- glmnet (x, y, alpha = 1, lambda = grid)
# check out the coefficient estimation results
lasso.coef <- predict (out , type = "coefficients",
                         s = bestlam)[1:20, ]
lasso.coef

########################
## Dimension reduction method
########################
library(pls)
set.seed (2)

## fit principal component regression on the whole data
pcr.fit <- pcr(Salary ~ ., data = Hitters , scale = TRUE ,
                  validation = "CV")
summary (pcr.fit)

validationplot (pcr.fit , val.type = "MSEP")

## fit pcr on the training set
set.seed (1)
pcr.fit <- pcr (Salary ~ ., data = Hitters , subset = train ,
                  scale = TRUE , validation = "CV")
validationplot (pcr.fit , val.type = "MSEP")

## fit the fitted model on the test set with ncomp=5
pcr.pred <- predict (pcr.fit , x[test , ], ncomp = 5)
mean ((pcr.pred - y.test)^2)

## now fit pcr on the whole data with the chosen ncomp=5
pcr.fit <- pcr (y ~ x, scale = TRUE , ncomp = 5)
summary (pcr.fit)

#### Partial Least Regression
set.seed (1)
pls.fit <- plsr (Salary ~ ., data = Hitters , subset = train ,
                   scale = TRUE , validation = "CV")
summary (pls.fit)

validationplot(pls.fit , val.type = "MSEP")

pls.pred <- predict (pls.fit , x[test , ], ncomp = 1)
mean ((pls.pred - y.test)^2)

pls.fit <- plsr (Salary ~ ., data = Hitters , scale = TRUE ,
                 ncomp = 1)
summary (pls.fit)
