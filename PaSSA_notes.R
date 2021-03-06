# Power and sample size analysis notes

# Three topics
# 1. sample size for estimating a parameter
# 2. sample size and power for hypothesis testing
# 3. sample size and power via simulation

# asbio package has see.power.tck() to interactively depict power

# effect size - kind of like a z-score
# d = mean_a - mean_b / sd
# small effect size: 0.2
# medium effect size: 0.5
# large effect size: 0.8

# packages
library(pwr)


# visualize power
library(asbio)
see.power.tck()


# comparing pwr.t.test and power.t.test
pwr.t.test(d=0.2, n=60, sig.level=0.10, 
           type = "one.sample", alternative = "two.sided")

# the above "manually"
qu <- qt(0.10/2, 59, lower = FALSE)
# power
pt(qu, 59, ncp = sqrt(60/1) * 0.2, lower = FALSE) + 
  pt(-qu, 59, ncp = sqrt(60/1) * 0.2, lower = TRUE)


power.t.test(n = 60, delta = 0.2, sd = 1, sig.level = 0.10, 
             type = "one.sample", alternative = "two.sided")

# the above "manually"
pt(qt(0.10/2, 59, lower.tail = FALSE), 59, ncp = sqrt(60/1) * 
     0.2/1, lower.tail = FALSE)


# Why are these different?
# strict = FALSE argument in power.t.test
# change to strict = TRUE t0 match power.t.test
power.t.test(n = 60, delta = 0.2, sd = 1, sig.level = 0.10, 
             type = "one.sample", alternative = "two.sided",
             strict = TRUE)


# p. 40 example
pwr.t.test(n = 30, 0.5, 0.05)
power.t.test(n = 30, delta = 0.5, sd = 1, sig.level = 0.05, strict = TRUE)

# can use multiple n
pwr.t.test(n = c(30,40,50), 0.5, 0.05)


# if we though diff in means was 2 and estimated sigma as 2.8
pwr.t.test(n = 30, d = 2/2.8, 0.05)
power.t.test(n = 30, delta = 2, sd = 2.8, sig.level = 0.05, strict = TRUE)

# one direction
pwr.t.test(n = 30, d = 0.5, sig.level = 0.05, alternative = "greater")
power.t.test(n = 30, delta = 0.5, sd = 1, sig.level = 0.05, 
             alternative = "one.sided", strict = TRUE)

# p. 42 example
pwr.t.test(n = 500, d = 0.2, sig.level = 0.01)
pwr.t.test(n = 500, d = 0.2, sig.level = 0.05)



# pwr.p.test: test for one proportion (ES=h) 
# pwr.2p.test: test for two proportions (ES=h) 

pwr.2p.test(h=ES.h(0.67,0.5),n=80,sig.level=0.05,alternative="greater")
pwr.2p.test(h=0.3,n=80,sig.level=0.05,alternative="greater")


# pwr.2p2n.test: test for two proportions (ES=h, unequal sample sizes) 
# pwr.t.test: one sample and two samples (equal sizes) t tests for means (ES=d) 
# pwr.t2n.test: two samples (different sizes) t test for means (ES=d) 
# pwr.anova.test: test for one-way balanced anova (ES=f) 
# pwr.r.test: correlation test (ES=r) 
# pwr.chisq.test: chi-squared test (ES=w) 
# pwr.f2.test: test for the general linear model (ES=f2) 


# Simulation --------------------------------------------------------------

# generate logit model
x1 <- runif(500)
x2 <- runif(500)
xb <- 2 + 0.75*x1 - 1.25*x2 # linear model
exb <- 1/(1 + exp(-xb)) # logit transformation of linear model (ie, range = (0,1))
by <- rbinom(500, size = 1, prob = exb) # generate response
dat <- data.frame(by, x1, x2)
glm1 <- glm(by ~ x1 + x2, data=dat, family = binomial)
summary(glm1)



qs <- 0:20
plot(qs, dbinom(x = qs, size = 20, prob = 0.5), type="h", xlab="X", ylab="P(X)",
     main="Binomial distribution with n=20 and p=0.5")
# sum(dbinom(x = 15:20, size = 20, prob = 0.5))
segments(x0 = 15:20, y0 = 0, x1 = 15:20, y1 = dbinom(x = 15:20, size = 20, prob = 0.5),col = "red", lwd = 3)
text(x = 17, y = .05, labels = expression(P(X) > 14 %~~% 0.02))


qs <- 0:100
plot(qs, dbinom(x = qs, size = 100, prob = 0.6), type="h", xlab="X", ylab="P(X)")

qbinom(p = 0.05, size = 100, prob = 0.5, lower.tail = F)

