
# QUESTION 01 -------------------------------------------------------------

library(MASS)

View(shuttle)

with(shuttle, summary(use))

shuttle$usefac <- factor(shuttle$use, levels = c("auto", "noauto"),
                            labels = c("1", "0"))

useMdl <- glm(usefac ~ wind, family = binomial, data = shuttle)        

summary(useMdl)

exp(coef(useMdl))


# QUESTION 02 -------------------------------------------------------------

useMdl2 <-  glm(usefac ~ wind + magn, family = binomial, data = shuttle) 

exp(coef(useMdl2))


# QUESTION 03 -------------------------------------------------------------

shuttle$usenum <- factor(1 - as.numeric(shuttle$use))

useMdl3 <- glm(usenum ~ wind, family = binomial, data = shuttle)   

summary(useMdl3)


# QUESTION 04 -------------------------------------------------------------

View(InsectSprays)

countMdl <- glm(count ~ spray -1, family = poisson, data = InsectSprays)

summary(countMdl)

exp(coef(countMdl))[1] / exp(coef(countMdl))[2] # estimated relative rate


# QUESTION 6 --------------------------------------------------------------

library(lspline)

x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

mdl <- lm(y ~ lspline(x, c(0)))        

summary(mdl)
