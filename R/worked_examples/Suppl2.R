library(tidyverse)
library(metafor)

# creating data
set.seed(77777)

# setting parameters
n.effect <- 100
sigma2.s <- 0.05
beta1 <- 0.2
# using negative binomial we get good spread of sample size
n.sample <- rnbinom(n.effect, mu = 30, size = 0.7) + 4
# variance for Zr
vi <- 1/(n.sample - 3)
# moderator x 1
xi <- rnorm(n.effect)

# there is underling overall effect to r = 0.2 or Zr = 0.203
Zr <- atanh(0.2) + beta1*xi + rnorm(n.effect, 0, sqrt(sigma2.s)) + rnorm(n.effect, 0, sqrt(vi))
qplot(Zr, 1/sqrt(vi))

dat <- data.frame(yi = Zr, vi = vi, sei = sqrt(vi), xi = xi, ni = n.sample, prec = 1 / sqrt(vi), wi = 1/vi, zi = Zr/sqrt(vi))

rows <- 1:nrow(dat)
expected <- which(1/dat$sei < 5 & dat$yi < 0.25)
unexpected <- which(1/dat$sei > 4.7 & dat$yi > 0.25)

col_point <- ifelse(rows %in% expected, "red", ifelse(rows %in% unexpected, "blue", "black"))

dat$col_point <- col_point

# data with "publication bias"
dat2 <- dat[dat$col_point != "red", ]

mod_ra2 <- rma(yi, vi, data = dat2)


regtest(mod_ra2, model = "lm")

# HERE HERE

# they are the same
egger1 <- lm(zi ~ prec, data = dat2)
egger2 <- lm(yi ~ sei, weight = wi, data = dat2)

summary(egger1)
summary(egger2)