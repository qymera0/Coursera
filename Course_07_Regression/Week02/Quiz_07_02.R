
# QUESTION 01, 02 -------------------------------------------------------------

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)

y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

summary(lm(y~x))


# QUESTION 03 -------------------------------------------------------------

mpg <- lm(mpg ~ wt, mtcars)

newdata <- as.data.frame(mean(mtcars$wt))

names(newdata) <- c("wt")

predict(mpg, 
        newdata = newdata, 
        se.fit = TRUE, 
        interval = c("confidence"))


# QUESTION 05 -------------------------------------------------------------

newdata <- as.data.frame(3)

names(newdata) <- c("wt")

predict(mpg, 
        newdata = newdata, 
        se.fit = TRUE, 
        interval = c("prediction"))


# QUESTION 06  ------------------------------------------------------------

wt2 <- 0.5*mtcars$wt

mpg2 <- lm(mtcars$mpg ~ wt2)

mpg2$terms

confint(mpg2)


# QUESTION 09 -------------------------------------------------------------

