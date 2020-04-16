# QUESTION 01 -------------------------------------------------------------

library(reshape2)

df <- as.data.frame(matrix(0, ncol = 3, nrow = 5))

names(df) <- c("subject", "baseline", "week02")

data.entry(df)

df <- as.data.frame(t(do.call(rbind, df)))

dfStack <- melt(df[ ,2:3])

t <- t.test(df$week02, df$baseline, alternative = ("two.side"), mu = 0, paired = TRUE)

t

# QUESTION 02 -------------------------------------------------------------

n <- 9
mu <- 1100
sd <- 30
alpha <- 0.05

mu + c(-1, 1)*qt((1-alpha/2), (n-1))*sd/sqrt(n)

# QUESTION 03 -------------------------------------------------------------

binom.test(c(3, 1), p = 0.5, alternative = c("greater"))

# QUESTION 04 -------------------------------------------------------------

binom.test(c(10, 1787), p = (1/100), alternative = c("less"))


# QUESTION 05 -------------------------------------------------------------

n1 <- 9
n2 <- 9
mu1 <- -3
sd1 <- 1.5
mu2 <- 1
sd2 <-1.8

pool <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2)/((n1 - 1) + (n2 - 1)))

t <- (mu1 - mu2)/(pool * sqrt((1 / n1) + (1 / n2)))

pt(t, df = (n1 + n2 - 2))


# QUESTION 06 -------------------------------------------------------------

n <- 9
mu <- ((1123 + 1077)/2)
alpha <- 0.1

sd <- ((1123 - mu)*sqrt(n))/(qt((1-alpha/2), (n-1)))

t <- (mu - 1078)/(sd*sqrt(n))

pt(t, df = (n-1))


# QUESTION 07 --------------------------------------------------------------

n <- 100
delta <- 0.01
sd <- 0.04
sg.level <- 0.05

p <- power.t.test(n = n, delta = delta, sd = sd, sig.level = sg.level, type = c("paired"),
                  alternative = c("one.sided"))


# QUESTION 08 -------------------------------------------------------------

p <- power.t.test(power = 0.9, delta = delta, sd = sd, sig.level = sg.level, type = c("paired"),
                  alternative = c("one.sided"))        


