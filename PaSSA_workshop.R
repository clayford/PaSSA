# Clay Ford
# Power and Sample Size Analysis workshop
# Fall 2016
# UVa StatLab

# Power and sample size calculations for various statistical tests using the
# "pwr" package and a few built-in R functions

install.packages("pwr")
library(pwr)


# pwr.p.test --------------------------------------------------------------

# one-sample test for proportions  (ES=h) 

# This applies to our name tag example. Say we think people place name tags on 
# the left side of their chest 75% percent of the time versus random chance
# (50%). What sample size do we need to show this assuming a significance level
# (Type I error) of 0.05 and a desired power of 0.80?

h <- ES.h(p1 = 0.75, p2 = 0.50)
pwr.p.test(h = h, sig.level = 0.05, power = 0.80, alternative = "greater")

# About 23 people; always round up.

# The previous example assumed a one-sided test: Null = 0.5, Alt > 0.5. 

# We typically take the conservative route and calculate sample size based on a 
# two-sided test. That is Null = 0.5, Alt != 0.5. If we don't specify an
# alternative, the default alternative is two.sided.

pwr.p.test(h = h, sig.level = 0.05, power = 0.80)

# Notice we need more people. 

# Say we think people place name tags on the left 70% percent of the time
# instead of 50%. What is the power of our test if we survey 30 people provided 
# we accept a significance level of 0.05? (Notice we can use the ES.h() function
# in pwr.p.test)
pwr.p.test(h = ES.h(p1 = 0.70, p2 = 0.50), n = 30, sig.level = 0.05)

# Again, notice the default is a "two.sided" test. This means we're simply
# testing that the alternative proportion is not 0.5. It could be lower or
# higher. Assuming "greater" increases the power:
pwr.p.test(h = ES.h(p1 = 0.70, p2 = 0.50), n = 30, sig.level = 0.05, 
           alternative = "greater")

# Equivalently we can think of one proportion being "less" than some value, say
# 0.25 versus random chance. Notice the effect is negative and resulting power is
# the same:
pwr.p.test(h = ES.h(p1 = 0.30, p2 = 0.50), n = 30, sig.level = 0.05, 
           alternative = "less")

# NOTE: Usually recommended to stick with the two-sided alternative.

# We can also save the result of a sample size estimate and plot it.
pout <- pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50), power = 0.80, sig.level = 0.05)
plot(pout)

# Your plot will look different from mine if you do not have the ggplot2 package
# installed.

# Conventional effect sizes for "h":
cohen.ES(test = "p", size = "small")
cohen.ES(test = "p", size = "medium")
cohen.ES(test = "p", size = "large")

# We can use these instead of specifying proportions:
pwr.p.test(h = 0.2, sig.level = 0.05, power = 0.8)
pwr.p.test(h = 0.5, sig.level = 0.05, power = 0.8)
pwr.p.test(h = 0.8, sig.level = 0.05, power = 0.8)

# YOUR TURN!
# Say we think people place name tags on the left 70% percent of the time instead
# of 50%. What sample size do we need to show this assuming a significance level
# of 0.01, a desired power of 0.90, and a one-sided "greater" alternative?



# pwr.2p.test -------------------------------------------------------------

# two-sample test for proportions (ES=h) 

# EXAMPLE: Let's say I want to randomly sample male and female UVa undergrad
# students and ask them if they consume alcohol at least once a week. My null
# hypothesis is no difference in the proportion that answer yes. My alternative
# hypothesis is that there is a difference. (two-sided; one gender has higher
# proportion, I don't know which.) I'd like to detect a difference as small as
# 5%. How many students do I need to sample in each group if we want 80% power
# and a significance level of 0.05?

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

# Sample size is PER GROUP. Always round sample size up.

# Base R has a function called power.prop.test that allows you to use the raw
# proportions in the function.
power.prop.test(p1 = 0.55, p2 = 0.50, sig.level = 0.05, power = .80)
power.prop.test(p1 = 0.35, p2 = 0.30, sig.level = 0.05, power = .80)
power.prop.test(p1 = 0.15, p2 = 0.10, sig.level = 0.05, power = .80)

# Notice the results are slightly different. It calculates effect size
# differently.

# If we don't have any preconceived estimates of proportions or don't feel 
# comfortable making estimates, we can use a conventional effect sizes of 0.2,
# 0.5 or 0.8

# Sample sizes for the conventional effects:
pwr.2p.test(h = 0.2, sig.level = 0.05, power = .80)
pwr.2p.test(h = 0.5, sig.level = 0.05, power = .80)
pwr.2p.test(h = 0.8, sig.level = 0.05, power = .80)

# What sample size do I need to detect effects of 0.2 and 0.1 with 80% power?
pwr.2p.test(h = 0.2, sig.level = 0.05, power = 0.8)
pwr.2p.test(h = 0.1, sig.level = 0.05, power = 0.8)

# YOUR TURN! Let's say we're only able to randomly sample 80 students (40 per 
# group). What's the power of our two-sided test test if we assume a small
# effect size of 0.2 and we set significance level to 0.05?


# pwr.2p2n.test -----------------------------------------------------------

# two-sample test for proportions, unequal sample sizes (ES=h) 


# Let's return to our undergraduate survey of alcohol consumption. It turns out 
# we were able to survey 543 males and 675 females. What's the power of our test
# if we're interested in being able to detect a "small" effect size.

cohen.ES(test = "p", size = "small") # 0.2

# So instead of using ES.h() with guesses at p1 and p2, I can just use the
# effect size 0.2.
pwr.2p2n.test(h = 0.2, n1 = 543, n2 = 675, sig.level = 0.05)

# Let's say I previously surveyed 763 female undergraduates and found that p% 
# said they consumed alcohol once a week. I'd like to survey some males and see 
# if a significantly different proportion respond yes. How many do I need to 
# sample to detect a small effect size (0.2) in either direction with 80% power
# and a significance level of 0.05?

pwr.2p2n.test(h = 0.2, n1 = 763, power = 0.8, sig.level = 0.05)
# or specify n2; it doesn't matter



# pwr.t.test --------------------------------------------------------------

# one-sample and two-sample t tests for means (ES=d) 

# Two-sample t test

# EXAMPLE: I'm interested to know if there is a difference in the mean price of
# what male and female students pay at the library coffee shop. Let's say I
# randomly observe 30 male and 30 female students check out from the coffee shop
# and note their total purchase price. How powerful is this experiment if I want
# to detect a "medium" effect in either direction?

pwr.t.test(n = 30, d = 0.5, sig.level = 0.05) # n is per group

# How many do I need to observe for a test with 80% power?
pwr.t.test(d = 0.5, power = 0.80, sig.level = 0.05)

# What about a large effect (0.8)?
pwr.t.test(n = 30, d = 0.8, sig.level = 0.05) # n is per group
pwr.t.test(d = 0.8, power = 0.8, sig.level = 0.05) 



# EXAMPLE: Let's say we want to be able to detect a difference of at least 75
# cents in the mean purchase price. How can we convert that to an effect size?
# First we need to make a guess at the population standard deviation. If we have
# absolutely no idea, one rule of thumb is to take the difference between the 
# maximum and minimum values and divide by 4 (or 6). Let's say max is 10 and min
# is 1. So our guess at a standard deviation is 9/4 = 2.25. Therefore d is

d <- 0.75/2.25 # 0.333
pwr.t.test(d = d, power = 0.80, sig.level = 0.05)

# An effect size of 0.333 requires 143 per group 

# If you don't like working with d, you can use the power.t.test function that 
# comes with base R. It allows you to specify "delta" (true difference in means)
# and "sd" (standard deviation):

power.t.test(delta = 0.75, sd = 2.25, sig.level = 0.05, power = 0.8)

# For any of the pwr functions we can provide multiple sample size values to get
# multiple power estimates. For example, let's see how power changes as we let n
# go from 25 to 150 by 25 using an effect size of 0.333.

pwr.t.test(n = seq(25,150,25), d = 0.333, sig.level = 0.05)

# We can also save the result of a pwr.t.test sample size estimate and plot it.
pout <- pwr.t.test(d = 0.333, power = 0.80, sig.level = 0.05)
plot(pout)



# One-sample t-test

# To calculate power and sample size for one-sample t-test, I have to set the
# "type" argument to type = "one.sample"

# EXAMPLE: I think the average purchase price at the Library coffee shop is over
# $3 per student. My null is $3 or less; my alternative is greater than $3. If 
# the true average purchase price is $3.50, I would like to have 90% power to 
# declare the estimated average purchase price is greater than $3. How many 
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


# "paired" t-test. 

# EXAMPLE: 24 high school boys are put on a ultraheavy rope-jumping program. 
# Does this decrease their 40-yard dash time (ie, make them faster)? We'll
# measure their 40 time before the program and after. We'll use a paired t-test
# to see if the difference in times is greater than 0 (before - after). Assume
# the standard deviation of the differences will be about 0.25. How powerful is
# the test to detect a difference of about 0.08 with 0.05 significance?

pwr.t.test(n = 24, d = 0.08 / 0.25, 
           type = "paired", alternative = "greater")

# or 
power.t.test(n = 24, delta = 0.08, sd = 0.25, 
             type = "paired", alternative = "one.sided")


# YOUR TURN! How many boys would I need to sample to detect a difference of 0.08
# in either direction with 80% power and the usual 0.05 significance level,
# assuming the standard deviation of the differences will be about 0.25?


# pwr.t2n.test ------------------------------------------------------------

# two-sample t test for means, unequal sample sizes (ES=d) 

# Find power for a t-test with 28 in one group and 35 in the other group and a
# medium effect size. (sig.level defaults to 0.05.)
pwr.t2n.test(n1 = 28, n2 = 35, d = 0.5)

# Find n1 sample size when other group has 35, desired power is 0.80, effect 
# size is 0.5 and significance level is 0.05:
pwr.t2n.test(n2 = 35, d = 0.5, power = 0.8)

# what about a large effect?
pwr.t2n.test(n2 = 35, d = 0.8, power = 0.8)



# pwr.chisq.test ----------------------------------------------------------

# chi-squared tests (ES=w)

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

prob <- matrix(c(0.1,0.2,0.4,0.3), ncol=2, 
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

# If you don't suspect association in either direction, or you don't feel like
# building a matrix in R, you can go with a conventional effect size.

cohen.ES(test = "chisq", size = "small")
pwr.chisq.test(w = 0.1, N = 100, df = 1, sig.level = 0.05)
pwr.chisq.test(w = 0.1, power = 0.9, df = 1, sig.level = 0.05)

# Could also reframe the question as a two-sample proportion test. What sample 
# size do I need to detect a small effect in gender on the proportion of
# students who floss with 90% power?
pwr.2p.test(h = 0.2, sig.level = 0.05, power = 0.9)

# Notice this returns the same answer, except divided by 2.

# pwr.r.test --------------------------------------------------------------

# correlation test (ES=r) 

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


# YOUR TURN:
# What's the power of my test if I recruit 50 people and I hypothesize a medium
# positive effect?



# power.anova.test --------------------------------------------------------

# balanced one-way analysis of variance tests

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

# n = 16

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

# EXAMPLE: Let's say I'm hired to survey a company's workforce about job 
# satisfaction. I ask employees to rate their satisfaction on a scale from 1 
# (hating life) to 10 (loving life). I know there will be variability in the 
# answers, but I think two variables that will explain this variability are 
# salary and age. In fact I think it will explain at least 30% (R^2 = .30) of
# the variance. How powerful is my "experiment" if I randomly recruit 40
# employees and accept a 0.05 significance level?

# Two predictors, so u = 2
# 40 subjects, so v = 40 - 2 - 1 = 37
# R^2 = .30, so effect size f2 = 0.3/(1 - 0.3)

pwr.f2.test(u = 2, v = 37, f2 = 0.3/(1 - 0.3), sig.level = 0.05)

# How many employees do I need to survey if I want to be able to detect at least
# 30% explained variance (R^2 = .30) with 80% power and the usual 0.05
# significance level? We have to find v and then derive n.

pwr.f2.test(u = 2, f2 = 0.3/(1 - 0.3), sig.level = 0.05, power = 0.8)
# now find n, which is n = v + u + 1
23 + 2 + 1 # 26

# It's important to note that the alternative hypothesis here is that at least 
# one of the coefficients in my model is not 0. This doesn't mean we need at 
# least 26 subjects to have all coefficients significant. It just means 26 
# subjects gives us 80% chance of correctly detecting the effect of at least one
# of our predictors, provided one or more truly affect our response.

# Continuing with previous example, it would be of interest if having your own 
# office accounted for at least 5% beyond the variance explained by the model 
# with salary and age. We could think of this as a 0/1 indicator in the model
# that takes the value 1 if an employee has his/her own office, and 0 otherwise.

# Call the office indicator variable set "B" and the others variable set 
# "A". We'd like to know the power of this regression for detecting the 
# contribution of B to the model with A. This is sometimes called a Partial F 
# test. We test if a set of predictors explain variance above and beyond a
# second set of predictors. The null is no contribution.

# The effect size for this test:
  
# f2 = (R^2_AB - R^2_A) / (1 - R^2_AB)

# R^2_AB is percent variance explained by variable sets A and B. R^2_A is 
# percent variance explained by variable set A.

# In this case, f2 = (0.35 - 0.30)/(1 - 0.35)
  
# How many employees would we need to survey to identify the office contribution
# to variance explained with 90% power and a signficance level of 0.05?

# u = number of variables in set "A"
# v = n - number of variables in sets "A" and "B" - 1

pwr.f2.test(u = 2, f2 = (0.35 - 0.30) /(1 - 0.35), sig.level = 0.05, power = 0.9)

# To calculate sample size:
# n = ceiling(v) + number of variables in A & B + 1
165 + 3 + 1 # 169


# time permitting material ------------------------------------------------

# sample size curves

# Sample size curves can be a useful visual aid in helping understand the 
# relationship between effect size, power, sample size and significance level.
# Let's make some for a chi-square test of association for a 2 x 2 table.

# generate a sequence of effect sizes and power values
es <- seq(0.1,0.5,0.01)
pow <- c(0.8, 0.85, 0.9, 0.95)

# combine these values into a data frame
vals <- expand.grid(es=es,pow=pow)

# create a function to calculate sample size and extract
getN <- function(es,pow,sl=0.05){
  ceiling(pwr.chisq.test(w = es, df = 1, power = pow, sig.level = sl)$N)
}

getN(vals$es, vals$pow)

# "apply" the function to the es and pow columns of the vals data frame
vals$n <- mapply(getN, vals$es, vals$pow)
# add a column to indicate significance level
vals$sl <- 0.05

# create temporary copy of vals to do the same for 0.01 significance level
tmp <- vals
tmp$n <- mapply(getN, vals$es, vals$pow, 0.01)
tmp$sl <- 0.01

# combine tmp and vals into vals
vals <- rbind(vals, tmp); rm(tmp)

# set "pow" as factor to aid in plotting
vals$pow <- factor(vals$pow)

# create sample size curves faceted by significance level
library(ggplot2)
ggplot(vals, aes(es, n, color=pow, group=pow)) + geom_line() +
  facet_wrap(~ sl) +
  labs(x="Effect Size", y="Sample Size", title="Sample Sizes for Chi-square Test (2 x 2)") +
  scale_color_discrete("Power") 



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
plot(h, n, type="l", main="Sample size vs Effect size h\n for 80% power and 0.05 significance level")

# add points representing "conventional" effect sizes
points(x = c(0.2,0.5,0.8), y = n[h %in% c(0.2,0.5,0.8)], 
       pch=19, cex=1, col=c("black","red","blue"))
legend("topright", legend = c("0.2 (small)","0.5 (medium)","0.8 (large)"), 
       col = c("black","red","blue"), 
       pch = 19, title = "effect size")

# As we go below 0.2, the sample size takes off!


# END WORKSHOP SCRIPT