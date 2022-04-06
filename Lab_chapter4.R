# load the library
library (ISLR2)
# browse the dataset
names (Smarket)
dim (Smarket)
summary (Smarket)
cor (Smarket)
cor (Smarket[, -9])
###################
# Logistic regression
#####################
# fit the logistic regression
glm.fits <- glm (
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume ,
  data = Smarket , family = binomial
)
coef (glm.fits)
summary (glm.fits)$coef
# fitteded probability
glm.probs <- predict (glm.fits , type = "response")
glm.probs[1:10]
# classify based on the fitted probability
glm.pred <- rep ("Down", 1250)
glm.pred[glm.probs > .5] = "Up"
# Separate year 2005's data
attach(Smarket)
train <- (Year < 2005)
Smarket.2005 <- Smarket[!train , ]
dim (Smarket)
dim (Smarket.2005)
Direction.2005 <- Direction[!train]
# fit the logistic model from the training set (the data before 2005)
glm.fits <- glm (
  Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume ,
  data = Smarket , family = binomial , subset = train
)
# fit the 2005's data
glm.probs <- predict (glm.fits , Smarket.2005,
                        type = "response")

glm.probs
glm.pred <- rep ("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table (glm.pred , Direction.2005)
# test correct rate
mean(glm.pred == Direction.2005)
# test error rate
mean(glm.pred != Direction.2005)

glm.fits <- glm (Direction ~ Lag1 + Lag2 , data = Smarket ,
                   family = binomial , subset = train)
glm.probs <- predict (glm.fits , Smarket.2005,
                        type = "response")
glm.pred <- rep ("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table (glm.pred , Direction.2005)
mean (glm.pred == Direction.2005)
summary (glm.fits)
# make predictions
predict (glm.fits,
         newdata =
           data.frame (Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)),
         type = "response")
#######################
# LDA
##########################
library (MASS)
# fit LDA
lda.fit <- lda (Direction ~ Lag1 + Lag2 , data = Smarket ,
                  subset = train)
lda.fit
plot(lda.fit)

lda.pred <- predict (lda.fit , Smarket.2005)
names (lda.pred)

lda.class <- lda.pred$class
table (lda.class, Direction.2005)
# test correct rate
mean (lda.class == Direction.2005)
# check the fitted posterior probabilities of being "down"
sum (lda.pred$posterior[, 1] >= .5)
sum (lda.pred$posterior[, 1] < .5)
# compare the posterior probabilities with the fitted class
lda.pred
$posterior[1:20, 1]
lda.class[1:20]
# What if we change the threshold of the classification?
sum (lda.pred$posterior[, 1] > .9)
#######################
# QDA
##########################
# fit qda
qda.fit <- qda (Direction ~ Lag1 + Lag2 , data = Smarket ,
                  subset = train)
qda.fit

qda.class <- predict (qda.fit , Smarket.2005)$class
table (qda.class , Direction.2005)

mean (qda.class == Direction.2005)
#######################
# Naive Bayes
##########################
library (e1071)
nb.fit <- naiveBayes (Direction ~ Lag1 + Lag2 , data = Smarket ,
                        subset = train)
nb.fit
mean (Lag1[train][Direction[train] == "Down"])
nb.class <- predict (nb.fit , Smarket.2005)
table (nb.class , Direction.2005)
mean (nb.class == Direction.2005)

nb.preds <- predict (nb.fit , Smarket.2005, type = "raw")
nb.preds[1:5, ]
