# Clay Ford
# Power and Sample Size Analysis
# Spring 2016
# UVa StatLab

# Power and sample size calculations for various statistical tests using the
# "pwr" package and a few built-in R functions


# install.packages("pwr")
library(pwr)


# pwr.p.test --------------------------------------------------------------

# one-sample test for proportions  (ES=h) 

# When we want to test if a proportion is equal to some hypothesized value 
# versus a null value, such as random chance, or 0.5. Notice it requires an
# effect size expressed as "h". Use the ES.h() function for this.

# This applies to our name tag example. Say we think people place name tags on 
# the left side of their chest 65% percent of the time versus random chance
# (50%). What sample size do we need to show this assuming a significance level
# (Type I error) of 0.05 and a desired power of 0.80?

# The effect size is .65 - .50 = .15. But notice that .16 - .01 = .15 as well. 
# The absolute differences are equal but notice their ratios:

0.65/0.50
0.16/0.01

# A difference of 0.16 and 0.01 is more dramatic than 0.65 and 0.50. Because of 
# this the difference 0.65 vs. 0.50 requires a larger sample size. Hence the
# need to calculate an effect size that reflects this. Using ES.h()

ES.h(p1 = 0.65, p2 = 0.50)
ES.h(p1 = 0.16, p2 = 0.01) # larger effect size

# NOTE: the effect size is calculated using an arcsine transformation. Also, the
# "h" effect size ranges (practically) from about 0.05 to 2.7 in absolute value.

# In this particular statistical test, we want the p1 = alternative and p2 =
# Null: ES.h(p1 = 0.65, p2 = 0.50)

# How many people do I need to sample to reject the null of random chance (0.50)
# if the true proportion is 0.65, with 80% power and 0.05 significance?
h <- ES.h(p1 = 0.65, p2 = 0.50)
pwr.p.test(h = h, sig.level = 0.05, power = 0.80)

# About 85 people; always round up.

# The previous example assumed a two-sided test: Null = 0.5, Alt != 0.5. However
# our hypothesis is that the proportion is GREATER than 0.5, so we should modify
# our calculations to be one-sided. Our test is more powerful if we're willing
# to believe the alternative is "greater" than .50. Set alternative = "greater"
pwr.p.test(h = h, sig.level = 0.05, power = 0.80, alternative = "greater")

# Notice we need fewer people. 

# Say we think people place name tags on the left 75% percent of the time
# instead of 50%. What is the power of our test if we survey 30 people provided 
# we accept a significance level of 0.05? (Notice we can use the ES.h() function
# in pwr.p.test)
pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50), n = 30, sig.level = 0.05)

# Again, notice the default is a "two.sided" test. This means we're simply
# testing that the alternative proportion is not 0.5. It could be lower or
# higher. Assuming "greater" increases the power:
pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50), n = 30, sig.level = 0.05, 
           alternative = "greater")

# Equivalently we can think of one proportion being "less" than some value, say
# 0.25 versus random chance. Notice the effect is negative and resulting power is
# the same:
pwr.p.test(h = ES.h(p1 = 0.25, p2 = 0.50), n = 30, sig.level = 0.05, 
           alternative = "less")

# NOTE: Usually recommended to stick with the two-sided alternative.


# YOUR TURN!
# Say we think people place name tags on the left 70% percent of the time instead
# of 50%. What sample size do we need to show this assuming a significance level
# of 0.01, a desired power of 0.90, and a one-sided "greater" alternative?
pwr.p.test(h = ES.h(p1 = 0.70, p2 = 0.50), sig.level = 0.01, power = 0.90, 
           alternative = "greater")


# pwr.2p.test -------------------------------------------------------------

# two-sample test for proportions (ES=h) 

# We want to test if two proportions are equal. To use this function, we once 
# again need to calculate effect size using ES.h(). Or as we'll see, we can use
# the base R function, power.prop.test.

# EXAMPLE: Let's say I randomly sample male and female UVa undergrad students 
# and ask them if they consume alcohol at least once a week. My null hypothesis 
# is no difference in the proportion that answer yes. My alternative hypothesis 
# is that there is a difference. (two-sided; one gender has higher proportion, I
# don't know which.) I'd like to detect a difference as small as 5%. How many 
# students do I need to sample in each group if we want 80% power and a
# significance level of 0.05?

# Recall effect size depends on the two proportions we compare:
ES.h(p1 = 0.55, p2 = 0.50)
ES.h(p1 = 0.35, p2 = 0.30)
ES.h(p1 = 0.15, p2 = 0.10)

# 55% vs. 50%
pwr.2p.test(h = ES.h(p1 = 0.55, p2 = 0.50), sig.level = 0.05, power = .80)
# 35% vs. 30%
pwr.2p.test(h = ES.h(p1 = 0.35, p2 = 0.30), sig.level = 0.05, power = .80)
# 15% vs. 10%
pwr.2p.test(h = ES.h(p1 = 0.15, p2 = 0.10), sig.level = 0.05, power = .80)

# Sample size is per group. Always round sample size up.

# Base R has a function called power.prop.test that allows you to use the raw
# proportions in the function.
power.prop.test(p1 = 0.55, p2 = 0.50, sig.level = 0.05, power = .80)
power.prop.test(p1 = 0.35, p2 = 0.30, sig.level = 0.05, power = .80)
power.prop.test(p1 = 0.15, p2 = 0.10, sig.level = 0.05, power = .80)

# Notice the results are slightly different. It calculates effect size
# differently.

# If we don't have any preconceived estimates of proportions or don't feel
# comfortable making estimates, we can use a conventional effect size:
cohen.ES(test = "p", size = "small") # 0.2
cohen.ES(test = "p", size = "medium") # 0.5
cohen.ES(test = "p", size = "large") # 0.8


# Sample sizes for the conventional effects:
pwr.2p.test(h = 0.2, sig.level = 0.05, power = .80)
pwr.2p.test(h = 0.5, sig.level = 0.05, power = .80)
pwr.2p.test(h = 0.8, sig.level = 0.05, power = .80)

# Let's see how effect size affects sample size. The results of the pwr
# functions can be saved:
pout <- pwr.2p.test(h = 0.2, power = 0.80, sig.level = 0.05)
# Then various parts can be extracted:
str(pout)
pout$n
ceiling(pout$n) # round up

# We can also do this directly as follows:
ceiling(pwr.2p.test(h = 0.2, power = 0.80, sig.level = 0.05)$n)

# comparing effect size to sample size to help us get a feel for their
# relationship:

# generate a sequence of effect sizes
h <- seq(0.1, 0.9, 0.01)

# "apply" the pwr.p.test function to the effect sizes
n <- sapply(h, function(x)ceiling(pwr.2p.test(h = x, power = 0.80)$n))

# plot sample size vs. effect size
plot(h, n, type="l", main="Sample size vs Effect size h")

# add points representing "conventional" effect sizes
points(x = c(0.2,0.5,0.8), y = n[h %in% c(0.2,0.5,0.8)], 
       pch=19, cex=1, col=c("black","red","blue"))
legend("topright", legend = c("0.2 - small","0.5 - medium","0.8 - large"), 
       col = c("black","red","blue"), 
       pch = 19, title = "effect size")

# As we go below 0.2, the sample size takes off!

# What sample size do I need to detect effects of 0.2 and 0.1 with 80% power?
pwr.2p.test(h = 0.2, sig.level = 0.05, power = 0.8)
pwr.2p.test(h = 0.1, sig.level = 0.05, power = 0.8)

# YOUR TURN! Let's say we're only able to randomly sample 80 students (40 per 
# group). What's the power of our two-sided test test if we assume a small
# effect size of 0.2 and we set significance level to 0.05?
pwr.2p.test(h = 0.2, sig.level = 0.05, n = 40)


# pwr.2p2n.test -----------------------------------------------------------

# two-sample test for proportions, unequal sample sizes (ES=h) 

# This function allows us to calculate power when we have unequal sample sizes. 
# We can also use it to find a sample size for one group when we already know
# the size of the other. It has two "n" arguments: n1 and n2

# Let's return to our undergraduate survey of alcohol consumption. It turns out 
# we were able to survey 543 males and 675 females. What's the power of our
# test?

# I honestly have no idea if there would be a difference, or if there was, how
# big that difference would be. I also have no idea about what the proportions
# would be. In this case we could use a "conventional" effect size. (Small,
# Medium, or Large)

# Let's say we're interested in being able to detect a "small" effect size.

cohen.ES(test = "p", size = "small") # 0.2

# So instead of using ES.h() with guesses at p1 and p2, I can just use the
# effect size 0.2.
pwr.2p2n.test(h = 0.2, n1 = 543, n2 = 675, sig.level = 0.05)

# Let's say I previously surveyed 763 female undergraduates and found that p%
# said they consumed alcohol once a week. I'd like to survey some males and see
# if a significantly different proportion respond yes. How many do I need to
# sample to detect a small effect size in either direction with 80% power and a 
# significance level of 0.05?

pwr.2p2n.test(h = 0.2, n1 = 763, power = 0.8, sig.level = 0.05)
# or specify n2; it doesn't matter
pwr.2p2n.test(h = 0.2, n2 = 763, power = 0.8, sig.level = 0.05)



# pwr.t.test --------------------------------------------------------------

# one sample and two sample t tests for means (ES=d) 

# The effect size d is simply the difference in population means divided by the 
# standard deviation of either population (since they are assumed equal). Now we
# have to make a guess at the standard deviation!

# There is no function for effect size d. We have to calculate this ourselves.

# d = mean1 - mean2 / sd

# The cohen.ES function can return small, medium and large d values, which are
# 0.2, 0.5 and 0.8:
cohen.ES(test = "t", size = "small")
cohen.ES(test = "t", size = "medium")
cohen.ES(test = "t", size = "large")

# Two-sample t test

# EXAMPLE: I'm interested to know if there is a difference in the mean price of 
# what male and female students pay at the library coffee shop. Let's say I 
# randomly observe 30 male and 30 female students check out from the coffee shop
# and note their total purchase price. How powerful is this experiment if I want
# to detect a "medium" effect in either direction?

pwr.t.test(n = 30, d = 0.5, sig.level = 0.05) # n is per group

# How many do I need to observe for a test with 80% power?
pwr.t.test(d = 0.5, power = 0.80, sig.level = 0.05)

# What about a large effect?
pwr.t.test(n = 30, d = 0.8, sig.level = 0.05) # n is per group
pwr.t.test(d = 0.8, power = 0.8, sig.level = 0.05) 


# The default alternative is "two.sided". That is, we're looking for an effect 
# in either direction. We can specify the alternative as "greater" than 0 or
# "less" than 0. 

# alternative: greater than 0 (positive effect)
# what sample size do I need for 80% power and a medium effect?
pwr.t.test(d = 0.5, power = 0.80, sig.level = 0.05, alternative = "greater")

# alternative: less than 0 (negative effect)
# need to make the effect negative
# what sample size do I need for 80% power?
pwr.t.test(d = -0.5, power = 0.80, sig.level = 0.05, alternative = "less")

# Same sample size in either case. Again, in practice, probably best to conduct
# two-sided tests.

# EXAMPLE: Let's say we want to be able to detect a difference of at least 75
# cents in the mean purchase price. How can we convert that to an effect size?
# First we need to make a guess at the population standard deviation. If we have
# absolutely no idea, one rule of thumb is to take the difference between the 
# maximum and minimum values and divide by 4 (or 6). Let's say max is 10 and min
# is 1. So our guess at a standard deviation is 9/4 = 2.25. Therefore d is

d <- 0.75/2.25 # 0.333
pwr.t.test(d = d, power = 0.80, sig.level = 0.05)

# An effect size of 0.333 requires 143 per group 
# An effect size of 0.2 requires 394 per group.

# Let's see how effect size affects sample size. Create a plot of effect size
# versus required sample size for 80% power and 0.05 significance level:
d <- seq(0.1,0.9,0.01)
n <- sapply(d, function(x) ceiling(pwr.t.test(d = x, power = 0.80)$n))
plot(d,n,type="l", main="Sample size vs Effect size d")
points(x = c(0.2,0.5,0.8), y = n[d %in% c(0.2,0.5,0.8)], 
       pch=19, cex=1, col=c("black","red","blue"))
legend("topright", legend = c("0.2 - small","0.5 - medium","0.8 - large"), 
       col = c("black","red","blue"), 
       pch = 19, title = "effect size")

# We can see how crucial anticipated effect size is to determining sample size.

# If you don't like working with d, you can use the power.t.test function that 
# comes with base R. It allows you to specify "delta" (true difference in means)
# and "sd" (standard deviation):

power.t.test(delta = 0.75, sd = 2.25, sig.level = 0.05, power = 0.8)

# For any of the pwr functions we can provide multiple sample size values to get
# multiple power estimates. For example, let's see how power changes as we let n
# go from 100 to 500 by 100 using a "small" effect size of 0.2.

pwr.t.test(n = seq(100,500,100), d = 0.2, sig.level = 0.05)

# We can save this output and graph:
n <- seq(100,900,10)
pout <- pwr.t.test(n = n, d = 0.2, sig.level = 0.05)
plot(x = n, y = pout$power, type = "l")
abline(h = 0.8, col = "red")

# One-sample t-test

# To calculate power and sample size for one-sample t-test, I have to set the
# "type" argument to type = "one.sample"

# EXAMPLE: I think the average purchase price at the Library coffee shop is over
# $3 per student. My null is $3 or less; my alternative is greater than $3. If 
# the true average purchase price is $3.50, I would like to have 90% power to 
# declare my estimated average purchase price is greater than $3. How many 
# transactions do I need to observe assuming a significance level of 0.05? Let's
# say max purchase price is $10 and min is $1. So our guess at a standard
# deviation is 9/4 = 2.25. Therefore d is...

d <- 0.50/2.25
pwr.t.test(d = d, sig.level = 0.05, power = 0.90, alternative = "greater", 
           type = "one.sample")

# or with power.t.test:
power.t.test(delta = 0.50, sd = 2.25, power = 0.90, sig.level = 0.05, 
             alternative = "one.sided", type = "one.sample")

# Very important to remember type = "one.sample"

# What if I lower my significance level to 0.01?
pwr.t.test(d = d, sig.level = 0.01, power = 0.90, alternative = "greater", 
           type = "one.sample")

# Going to extremes...
pwr.t.test(d = d, sig.level = 0.001, power = 0.999, alternative = "greater", 
           type = "one.sample")

# Another type is "paired". A paired t-test is basically the same as a 
# one-sample t-test. Instead of one sample of individual observations, you have 
# one sample of pairs of observations, where you take the difference between 
# each pair to get a single sample of differences. These are commonly before and
# after measures on the same person.

# The effect size for a paired t-test can include correlation between the pairs 
# (This is because the pairs are dependent, not independent):

# d = (mean1 - mean2) / sd * sqrt(2 * (1 - r))   (Cohen, p. 48)

# Another definition of effect size for paired data (without correlation):

# d = (mean1 - mean2) / sd * sqrt(2)    (Dalgaard, p. 145)

# The second is more conservative and probably safer to use.

# Notice "paired" and "one.sample" return the same result, but the effect (d)
# means different things in each scenario.
pwr.t.test(d = 0.2, power = 0.90, type = "paired")
pwr.t.test(d = 0.2, power = 0.90, type = "one.sample")

power.t.test(delta = 0.75, sd = 3.75, power = 0.90, type = "paired")
power.t.test(delta = 0.75, sd = 3.75, power = 0.90, type = "one.sample")

# EXAMPLE: 24 high school boys are put on a ultraheavy rope-jumping program. 
# Does this increase their 40-yard dash time? We'll measure their 40 time before
# the program and after. We'll use a paired t-test to see if the difference in 
# times is greater than 0. Assume the standard deviation of the differences will
# be about 0.25. How powerful is the test to detect a difference of about 0.08
# with 0.05 significance?

pwr.t.test(n = 24, d = 0.08 / (0.25 * sqrt(2)), 
           type = "paired", alternative = "greater")

# or 
power.t.test(n = 24, delta = 0.08, sd = 0.25 * sqrt(2), 
             type = "paired", alternative = "one.sided")


# YOUR TURN! How many boys would I need to sample to detect a difference of 0.05
# in either direction with 80% power and the usual 0.05 significance level,
# assuming the standard deviation of the differences will be about 0.25?
pwr.t.test(d = 0.05 / (0.25 * sqrt(2)), power = 0.8, type = "paired")
power.t.test(delta = 0.05, sd = 0.25 * sqrt(2), power = 0.8, type = "paired")

# pwr.t2n.test ------------------------------------------------------------

# two sample t test for means, unequal sample sizes (ES=d) 

# Find power for a t-test with 28 in one group and 35 in the other group and a
# medium effect size. (sig.level defaults to 0.05.)
pwr.t2n.test(n1 = 28, n2 = 35, d = 0.5)

# Find n1 sample size when other group has 35, desired power is 0.80, effect 
# size is 0.5 and significance level is 0.05:
pwr.t2n.test(n2 = 35, d = 0.5, power = 0.8)



# pwr.chisq.test ----------------------------------------------------------

# chi-squared tests (ES=w)

# There are two chi-square tests this function addresses:

# 1. goodness of fit test
# 2. contingency test, or test for association

# In a goodness of fit test, a single dimension of proportions is tested against
# a prespecified set of proportions which constitutes the null hypothesis. 
# Rejecting the null means we have sufficient evidence to conclude the data 
# don't appear to "fit" the prespecified set of proportions. If we were hoping 
# to show our data "fit" the prespecified set of proportions, then failure to
# reject the Null is a good thing.

# In a contingency test, or test of association, a table of counts classified by
# two variables is tested against the expected table of counts given the two 
# variables are independent. Rejecting the null means the data appear to be 
# associated in some way.

# The pwr.chisq.test function requires specification of effect size "w". The
# effect size "w" differs depending on the test. The pwr package provides two 
# functions to calculate both versions:

# ES.w1	- goodness of fit
# ES.w2	- test for association

# In essence, the effect size w measures the discrepancy between null and 
# alternative proportions over the cells in a table. These formulas are provided
# in Chapter 7 of Cohen (1988).

# ES.w1(P0, P1)
# P0 - First set of k probabilities (null hypothesis)
# P1 - Second set of k probabilities (alternative hypothesis)

# ES.w2(P)
# P	- A two-way probability table (alternative hypothesis)

# As we can see calculating an effect size takes a bit of work. If you like, you
# can go with small, medium or large: 0.10, 0.30, 0.50

cohen.ES(test = "chisq", size = "small")
cohen.ES(test = "chisq", size = "medium")
cohen.ES(test = "chisq", size = "large")

# Finally, the pwr.chisq.test requires specifying degrees of freedom (df).

# goodness of fit: df = number of cells - 1
# test of association: (Var1 number of categories - 1) * (Var2 number of categories - 1)

# Let's work some examples!

# Goodness of fit

# EXAMPLE: (From Cohen, example 7.1) A market researcher is seeking to determine
# preference among 4 package designs. He arranges to have a panel of 100 
# consumers rate their favorite package design. He wants to perform a chi-square
# goodness of fit test against the null of equal preference (25% for each
# design) with a significance level of 0.05. What's the power of the test if 3/8
# of the population actually prefers one of the designs and the remaining 5/8
# are split over the other 3 designs?

# To calculate effect size, we need to create vectors of null and alternative
# proportions:
null <- rep(0.25, 4)
alt <- c(3/8, rep((5/8)/3, 3))
# Now use them in the effect size function
ES.w1(null,alt)

# To calculate power, specify effect size, N, and degrees of freedom (4-1).
pwr.chisq.test(w=ES.w1(null,alt), N=100, df=(4-1), sig.level=0.05)

# How many subjects do we need to achieve 80% power?
pwr.chisq.test(w=ES.w1(null,alt), df=(4-1), power=0.8, sig.level = 0.05)

# If our alternative is correct then we need to survey at least 131 people to
# detect this with 80% power.

# test of association

# EXAMPLE: I want to see if there's an association between gender and flossing
# teeth among UVa students. I randomly sample 100 students (male and female) and
# ask whether or not they floss daily. I want to carry out a chi-square test of 
# association to determine if there's an association between these two 
# variables. As usual I set my significance level to 0.05. To determine effect 
# size I need to propose an alternative hypothesis, which in this case is a 
# table of proportions. I propose the following:

#   Floss No Floss
# M   0.1      0.4
# F   0.2      0.3

prob <- matrix(c(0.10,0.20,0.40,0.30), ncol=2, 
               dimnames = list(c("M","F"),c("Floss","No Floss")))
prob

# Even proportion of male and female, but I want to detect if at least 10% more
# females floss.

# Now use the matrix to calculate effect size:
ES.w2(prob)

# We also need degrees of freedom.
# DF = (2 - 1) * (2 - 1) = 1

# And calculate power:
pwr.chisq.test(w = ES.w2(prob), N = 100, df = 1, sig.level = 0.05)

# How many students should I survey if I wish to achieve 90% power?
pwr.chisq.test(w = ES.w2(prob), power = 0.9, df = 1, sig.level = 0.05)

# Even though I constructed the matrix to show females flossing more often, the 
# results apply regardless of the direction of the association we might find. 
# With 221 people sampled and an effect of 0.218 truly present in the 
# population, we have 90% power to reject the null of no association between
# gender and flossing.

# If you don't suspect association in either direction, or you don't feel like
# building a matrix in R, you can go with a conventional effect size.

cohen.ES(test = "chisq", size = "small")
pwr.chisq.test(w = 0.1, N = 100, df = 1, sig.level = 0.05)
pwr.chisq.test(w = 0.1, power = 0.9, df = 1, sig.level = 0.05)


# pwr.r.test --------------------------------------------------------------

# correlation test (ES=r) 

# The correlation test allows us to test whether there is any linear 
# relationship between two continuous variables. In other words, can we reject
# the null hypothesis of the correlation coefficient being 0. 

# Here's an example of what appears to be a linear relationship. (Note: The iris
# dataset comes with R.)
head(iris)
x <- iris$Sepal.Length
y <- iris$Petal.Length
plot(x,y)

# The correlation is high. Recall correlation ranges from -1 to 1.
cor(x,y)

# We can fit a straight line to the data using simple linear regression:
lm(y ~ x)
coef(lm(y ~ x)) # just view coefficients

# and draw the fitted line
abline(lm(y ~ x))

# the slope of the line is related to the correlation coefficient:
coef(lm(y ~ x))[2] # slope
# same as...
cor(x, y) * sd(y)/sd(x)

# So testing if the correlation is 0 is the same as testing if the slope in the
# simple linear regression is 0.

# The nice thing about correlation is that it's already unitless, so we don't
# require a formula to calculate effect size.

# For what it's worth, here are suggested small, medium and large effect sizes:
cohen.ES(test = "r", size = "small")
cohen.ES(test = "r", size = "medium")
cohen.ES(test = "r", size = "large")

# EXAMPLE: Let's say I'm a web developer and I want to conduct an experiment 
# with one of my sites. I want to randomly select a group of people, ranging in 
# age from 18 - 65, and time them how long it takes them to complete a task, say
# locate some piece of information. I suspect there may be a "small" positive 
# linear relationship between time it takes to complete the task and age. How 
# many subjects do I need to detect this positive (ie, r > 0) relationship with
# 80% power and the usual 0.05 significance level?

pwr.r.test(r = 0.1, sig.level = 0.05, power = 0.8, alternative = "greater")

# The "arctangh transformation" approximates the normal distribution, which is 
# used in the power and sample size calculations.

# The default is a two-sided test. We specify alternative = "greater" since we 
# believe there is small positive effect. That is, our null is correlation <= 0
# vs the alternative that correlation is > 0. 

# What if I just want to detect a small effect in either direction (positive or
# negative correlation)? Use the default settings of "two.sided"
pwr.r.test(r = 0.1, sig.level = 0.05, power = 0.8, alternative = "two.sided")

# And what if I hypothesize a negative effect (decreased time as age increases)?
# We need to make the effect size negative and specify alternative = "less"
pwr.r.test(r = -0.1, sig.level = 0.05, power = 0.8, alternative = "less")

# Of course the number of subjects is the same as alternative = "greater".

# YOUR TURN:
# What's the power of my test if I recruit 50 people and I hypothesize a medium
# positive effect?
pwr.r.test(n = 50, r = 0.3, sig.level = 0.05, alternative = "greater")



# power.anova.test --------------------------------------------------------

# balanced one-way analysis of variance tests

# ANOVA, or Analysis of Variance, tests whether or not means differ between more
# than 2 groups. One-way means one explanatory variable. Balanced means we have
# equal sample size in each group. The null hypothesis is that the means are all
# equal.

# Let's start with the power.anova.test function that comes with base R. It's 
# easier to use than pwr.anova.test and does not require calculating an effect
# size.

# The power.anova.test function requires you to specify the number of groups, 
# the between group variance (between.var), and the within group variance
# (within.var), which we assume is the same for all groups.

# EXAMPLE: Let's say I'm a web developer and I'm interested in 3 web site 
# designs for a client. I'd like to know which design(s) help users find 
# information fastest, or which design requires the most time. I design an 
# experiment where I have 3 groups of randomly selected people use one of the 
# designs to find some piece of information and I record how long it takes. (All
# groups look for the same information.) How many people do I need in each group
# if I believe two of the designs will take 30 seconds and one will take 25
# seconds? Assume population standard deviation is 5 and that I desire power
# and significance levels of 0.8 and 0.05.

# The between group variance:
var(c(30, 30, 25))

# The within group variance (sd ^ 2):
5^2

power.anova.test(groups = 3, between.var = 8.3, within.var = 5^2, power = 0.8)


# The pwr.anova.test function requires you to provide an effect size. The effect
# size, f, for k groups is calculated as follows:

# f = sd_means / sd_populations

# Translation: standard deviation of the k means divided the common standard 
# deviation of the populations involved.

# Or just use conventional effect sizes: 0.1, 0.25, 0.4.

cohen.ES(test = "anov", size = "small")
cohen.ES(test = "anov", size = "medium")
cohen.ES(test = "anov", size = "large")

# Returning to example, How many people do I need in each group if I want to
# detect a medium effect with 80% power and a significance level of 0.05.
pwr.anova.test(k = 3, f = 0.25, sig.level = 0.05, power = 0.8)

# Or what's the power of my test to detect a medium effect if I get 50 for each
# group?
pwr.anova.test(k = 3, f = 0.25, sig.level = 0.05, n = 50)



# pwr.f2.test -------------------------------------------------------------

# test for the general linear model (ES=f2) 

# By "general linear model" we basically mean multiple regression. This is a 
# little tricky to use because not only do we have to supply an "effect size" 
# (f2), we also have to supply numerator (u) and denominator (v) degrees of 
# freedom instead of sample size. These last two refer to the F test that tests 
# whether all the model coefficients (except the intercept) are 0. We can also 
# think of this as a test that the proportion of variance explained by the model
# predictors is 0. 

# Example of F test using the stackloss data that comes with R. See ?stack.loss
head(stackloss)

# Regress stack.loss on the other 3 variables and view summary of results.
# Results of F test are last line in summary
summary(lm(stack.loss ~ ., data=stackloss))

# Multiple R-squared:  0.9136
# F-statistic:  59.9 on 3 and 17 DF,  p-value: 3.016e-09

# u = 3 (numerator DF)
# v = 17 (denominator DF)

# u is the number of coefficients you'll have in your model (again, minus the
# intercept).

# v is the number of error degrees of freedom. It equals n - u - 1. (17 = 21 - 3
# - 1) So if we want to determine sample size for a given power and effect size,
# we actually find v, which we then use to solve n = v + u + 1.


# The effect size is determined as follows:

# f2 = PV_s / PV_e

# where PV_s = proportion of variance explained by the predictors (signal) and
# PV_e = proportion of variance unexplained (error or noise)

# Another way to express this is 

# f2 = R-squared / (1 - R-squared)

# Where R-squared is the coefficient of determination, R^2. Also described as
# the "proportion of variance explained".

# To determine your effect size you hypothesize the proportion of variance your
# model explains, or the R-squared. For example, 0.45. This leads to an effect size of
0.45/(1 - 0.45) # ~0.81

# It should be noted we can reverse this. Given an effect size, we can determine
# R-squared as ES / (1 + ES)
0.81/(1 + 0.81) # ~0.45

# Again we have suggested small, medium and large effect sizes:

cohen.ES(test = "f2", size = "small") # 0.02
# R-squared:
0.02/(1 + 0.02) # 0.019

cohen.ES(test = "f2", size = "medium") # 0.15
# R-squared:
0.15/(1 + 0.15) # 0.130

cohen.ES(test = "f2", size = "large") # 0.35
# R-squared:
0.35/(1 + 0.35) # 0.259

# Obviously these are debatable! What's conventional in the behavioral and
# social sciences may not be in other fields.

# EXAMPLE: Let's say I'm a web developer and I want to conduct an experiment
# with one of my sites. I want to randomly select a group of people, ranging in
# age from 18 - 65, and time them how long it takes them to complete a task, say
# locate some piece of information. I know there will be variability in the
# observed times. I think age and years of education may explain this
# variability. How powerful is my experiment if I recruit 40 subjects and I want
# to be able to detect at least 30% explained variance (R^2 = .30) with a 0.05
# significance level?

# Three predictors, so u = 2
# 40 subjects, so v = 40 - 2 - 1 = 37
# R^2 = .30, so effect size f2 = 0.3/(1 - 0.3)

pwr.f2.test(u = 2, v = 37, f2 = 0.3/(1 - 0.3), sig.level = 0.05)

# How many subjects do I need if I want to be able to detect at least 30%
# explained variance (R^2 = .30) with 80% power and the usual 0.05 significance
# level? We have to find v and then derive n.

pwr.f2.test(u = 2, f2 = 0.3/(1 - 0.3), sig.level = 0.05, power = 0.8)
# now find n
23 + 2 + 1

# It's important to note that the alternative hypothesis here is that at least 
# one of the coefficients in my model is not 0. This doesn't mean we need at 
# least 26 subjects to have all coefficients significant. It just means 26 
# subjects gives us 80% chance of correctly detecting the effect of at least one
# of our predictors, provided one or more truly affect our response.

# Continuing with previous example, it would be of interest if using a mobile 
# device accounted for at least 5% beyond the variance explained by the model
# with age and years of education. We could think of this as a 0/1
# indicator in the model that takes the value 1 if the user is on a mobile
# device, and 0 otherwise.

# Call the mobile device indicator variable set "B" and the others variable set 
# "A". We'd like to know the power of this regression for detecting the 
# contribution of B to the model with A. This is sometimes called a Partial F 
# test. We test if a set of predictors explain variance above and beyond a
# second set of predictors. The null is no contribution.

# The effect size for this test:
  
# f2 = (R^2_AB - R^2_A) / (1 - R^2_AB)

# R^2_AB is percent variance explained by variable sets A and B. R^2_A is 
# percent variance explained by variable set A.

# In this case, f2 = (0.35 - 0.30)/(1 - 0.35)
  
# How many subjects would be needed to identify the mobile/tablet contribution
# to variance explained with 90% power and a signficance level of 0.05?

# u = number of variables in set "A"
# v = n - number of variables in sets "A" and "B" - 1

pwr.f2.test(u = 2, f2 = (0.35 - 0.30) /(1 - 0.35), sig.level = 0.05, power = 0.9)

# To calculate sample size:
# n = round(v) + number of variables in A & B + 1
165 + 3 + 1



# END WORKSHOP SCRIPT