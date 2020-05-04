ggpairs(mtcars)

scale(mtcars[ ,-1], center = TRUE, scale = TRUE)

names(mtcars2$`mtcars$mpg`) <- "mpg"

mtcars2 <- cbind(mtcars["mpg"], as.data.frame(scale(mtcars[ ,2:7], center = T, scale = T)), mtcars[8:11])

m1 <- lm(mpg ~., data = mtcars2)

residualPlot(m2)
par(mfrow = c(2,2))

plot(m2)

std <- rstudent(m2)

hats <- hatvalues(m2)

cooks <- cooks.distance(m2)
