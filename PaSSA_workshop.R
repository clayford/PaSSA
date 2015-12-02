# Clay Ford
# Power and Sample Size Analysis
# Spring 2016
# UVa StatLab

# install.packages("pwr")
library(pwr)



# pwr.p.test --------------------------------------------------------------

# test for one proportion (ES=h) 

# When we want to test if a proportion is equal to some value. Notice the effect
# size is h. We can use ES.h to calculate effect size.

# This applies to our coin example. Say we think our coin is biased to show 
# heads 65% percent of the time instead of 50%. What sample size do we need to 
# show this assuming a significance level (Type I error) of 0.05 and a desired
# power of 0.80?

pwr.p.test(h = ES.h(p1 = 0.65, p2 = 0.50), sig.level = 0.05, power = 0.80)

# Our effect size is 65 - 50 = 15. But 85 - 70 = 15 as does 16 - 1. These are 
# equal differences but they result in different power and sample size results.
# Hence the need to calculate an effect size.

# Notice the different effect sizes:
ES.h(p1 = 0.65, p2 = 0.50)
ES.h(p1 = 0.85, p2 = 0.70)
ES.h(p1 = 0.16, p2 = 0.01)

# The effect size is an arcsine transformation:
ES.h(p1 = 0.65, p2 = 0.50)
2*asin(sqrt(0.65))-2*asin(sqrt(0.50))

# Say we think our coin is biased to show heads 60% percent of the time instead 
# of 50%. What is the power of our test if we flip the coin 125 times provided
# we accept a significance level (Type I error) of 0.05?

pwr.p.test(h = ES.h(p1 = 0.60, p2 = 0.50), n = 125, sig.level = 0.05)

# Notice the default is a "two.sided" test. Our test is more powerful if we're 
# willing to believe the alternative is "greater" than .50 rather than "not
# equal" to .50

pwr.p.test(h = ES.h(p1 = 0.60, p2 = 0.50), n = 125, sig.level = 0.05, 
           alternative = "greater")

# Say we think our coin is biased to show heads 55% percent of the time instead
# of 50%. What sample size do we need to show this assuming a significance level
# (Type I error) of 0.05 and a desired power of 0.80?
pwr.p.test(h = ES.h(p1 = 0.55, p2 = 0.50), sig.level = 0.05, power = 0.80)



# pwr.2p.test -------------------------------------------------------------

# test for two proportions (ES=h) 

# We want to test if two proportions are equal. To use this function, we once
# again need to calculate effect size using ES.h().

# Let's say I randomly sample male and female UVa undergrad students and ask 
# them if they consume alcohol at least once a week. My null hypothesis is no 
# difference in the proportion that answer yes. My alternative hypothesis is 
# that there is a difference. I'd like to detect a difference as small as 5%. 
# How many students do I need to sample in each group if we want 80% power and
# 5% Type 1 error?

# 55% vs. 50%
pwr.2p.test(h = ES.h(p1 = 0.55, 0.50), sig.level = 0.05, power = .80)
# 35% vs. 30%
pwr.2p.test(h = ES.h(p1 = 0.35, 0.30), sig.level = 0.05, power = .80)

# Sample size is per group. Always round sample size up.

# Base R has a function called power.prop.test that allows you to use the raw
# proportions in the function.
power.prop.test(p1 = 0.55, p2 = 0.50, sig.level = 0.05, power = .80)
power.prop.test(p1 = 0.35, p2 = 0.30, sig.level = 0.05, power = .80)



# pwr.2p2n.test -----------------------------------------------------------

# test for two proportions (ES=h, unequal sample sizes) 

# This function allows us to calculate power when we have unequal sample sizes. 
# We can also use it to find a sample size for one group when we already have
# data for another.

# Let's return to our undergraduate survey of alcohol consumption. It turns out 
# we were able to survey 543 males and 675 females. What's the power of our
# test?

# Before we answer, let's revisit effect size. I honestly have no idea if there 
# would be a difference, or if there was, how big that difference would be.
# Let's say we're interested in being able to detect a "small" effect size.

cohen.ES(test = "p", size = "small") # 0.2
pwr.2p2n.test(h = 0.2, n1 = 543, n2 = 675, sig.level = 0.05)

# Let's say I previously surveyed 763 females and found that p% said they 
# consumed alcohol once a week. I'd like to survey some males and see if a 
# significantly different proportion respond yes. How many do I need to sample 
# to detect a small effect size in either direction with 80% power and a
# significance level of 0.05?

pwr.2p2n.test(h = 0.2, n1 = 763, power = 0.8, sig.level = 0.05)



# pwr.t.test --------------------------------------------------------------

# one sample and two samples (equal sizes) t tests for means (ES=d) 

# There is no function for effect size d. We have to calculate this ourselves. 
# It is simply the difference in population means divided by the standard 
# deviation of either population (since they are assumed equal). This is
# sometimes called "Cohen's d".

# d = mean_1 - mean_2 / sd

# The cohen.ES function can return small, medium and large d values, which are
# 0.2, 0.5 and 0.8:
cohen.ES(test = "t", size = "small")
cohen.ES(test = "t", size = "medium")
cohen.ES(test = "t", size = "large")

# I'm interested to know if there is a difference in the mean price of what male
# and female students pay at the library coffee shop. Let's say I randomly 
# observe 30 male and 30 female students check out from the coffee shop and note
# their total purchase price. How powerful is this experiment if I want to
# detect a "small" effect?

pwr.t.test(n = 30, d = 0.2, sig.level = 0.05)

# Not very powerful. How many do I need to observe for a test with 80% power?

pwr.t.test(d = 0.2, power = 0.80, sig.level = 0.05)

# Let's say we want to be able to detect a difference of at least 75 cents in 
# the mean purchase price. How can we convert that to an effect size? First we 
# need to make a guess at the population standard deviation. If we have 
# absolutely no idea, one rule of thumb is to take the difference between the 
# maximum and minimum values and divide by 4. Let's max is 10 and min is 1. So
# our guess at a standard deviation is 9/4 = 2.25. Therefore d is 

d <- 0.75/2.25
pwr.t.test(d = d, power = 0.80, sig.level = 0.05)

# If you don't like working with d, you can use the power.t.test function that 
# comes with base R. It allows you to specify "delta" (true difference in means)
# and "sd" (standard deviation):

power.t.test(delta = 0.75, sd = 2.25, sig.level = 0.05, power = 0.8)

# For any of the pwr functions we can provide multiple sample size values to get
# multiple power estimates. For example, let's see how power changes as we let n
# go from 100 to 300 by 25.

pwr.t.test(n = seq(100,300,25), d = 0.2, sig.level = 0.05)

# We can save this output and graph:
n <- seq(100,900,10)
pout <- pwr.t.test(n = n, d = 0.2, sig.level = 0.05)
plot(x = n, y = pout$power, type = "l")
abline(h = 0.8, col="red")



# pwr.t2n.test ------------------------------------------------------------


# two samples (different sizes) t test for means (ES=d) 



# pwr.anova.test ----------------------------------------------------------


# test for one-way balanced anova (ES=f) 


# pwr.r.test --------------------------------------------------------------


# correlation test (ES=r) 


# pwr.f2.test -------------------------------------------------------------


# test for the general linear model (ES=f2) 

