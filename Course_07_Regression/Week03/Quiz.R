
# QUESTION 01 -------------------------------------------------------------

head(mtcars)

cars <- lm(mpg ~ as.factor(cyl) + wt, mtcars)

summary(cars)


# QUESTIO 02 --------------------------------------------------------------

cars_nowt <- lm(mpg ~ as.factor(cyl), mtcars)

summary(cars_nowt)$coef


# QUESTION 3 --------------------------------------------------------------

cars_wtinter <- lm(mpg ~ as.factor(cyl) + wt + wt*as.factor(cyl), mtcars)

anova(cars, cars_wtinter)


# QUESTION 04 -------------------------------------------------------------

cars04 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
       
summary(cars04) 


# QUESTION 5 --------------------------------------------------------------

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

fit1 <- lm(y ~ x)

hat(fit1)
hatvalues(fit1)


# QUESTION 6 --------------------------------------------------------------

dfbetas(fit1)[which.max(hatvalues(fit1)), 2]
