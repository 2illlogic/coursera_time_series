Test_1_scores=round(rnorm(50, 78, 10))
Test_2_scores=round(rnorm(50, 78, 14))

#basic scatterplot
plot(Test_2_scores~Test_1_scores, main='Test scores for two exams (50 students)', xlab='Test_1_scores', ylab='Test 2 scores', col='blue')

#histogram
hist(Test_1_scores, xlab='Test 1 Scores', main='Histogram of my data', freq=F, col='green', breaks=10)

#plots density curve - smoothing of histogram
lines(density(Test_1_scores), col='red', lwd=5)

#-----basic linear models-------
co2.model <- lm(co2 ~ time(co2))
summary(co2.model)
plot(co2)
##plots fitted line over data
abline(co2.model)

#extract residuals
co2.residuals = resid(co2.model)

# Q-Q, (quantile-quantile), plot of residual quantiles
# assuming normality vs. theoretical normal quantiles
# (change 'norm' to test against other models)
qqnorm(co2.residuals)
qqline(co2.residuals)

# plot residuals againts time
plot(co2.residuals ~ time(co2))

# zoom in to see patterns by setting 'xlim' to a smaller
# interval
plot(co2.residuals ~ time(co2), xlim = c(1991, 1994))

#-----inference--------------------------------
plot(extra ~ group, data = sleep)
attach(sleep)
#subset to separate groups in study
extra.1 <- extra[group == 1]
extra.2 <- extra[group == 2]

# run a t-test, (two independent samples)
# (could also do z, X^2, F tests)
t.test(extra.1, extra.2, paired=TRUE, 
       alternative='two.sided')
## manually
d <- extra.1-extra.2
d_bar <- mean(d)
s_d <- sd(d)
n <- 10
t = (d_bar-0)/(s_d/sqrt(n))

## calculate probability that X <= t w/ df = n - 1 = 9
## i.e. p-value
2*pt(t, 9)

## confidence interval w/alpha = 0.05
t_alpha <- qt(0.975, 9)
std_err <- s_d/sqrt(n)
ci_lower <- d_bar - (t_alpha*std_err)
ci_upper <- d_bar + (t_alpha*std_err)

qqnorm(d, main='Normal Probability Plot')
qqline(d)

# plot linear relationship between drug 1 and drug 2
# note that the slope is greater than 1, suggesting drug 2
# is more effective than drug 1
plot(extra.2 ~ extra.1, xlab='Drug 1', ylab='Drug 2')
sleep.model <- lm(extra.2 ~ extra.1)
abline(sleep.model)

# perform residuals analysis on linear model
sleep.residuals <- resid(sleep.model)
qqnorm(sleep.residuals)
qqline(sleep.residuals)

#-----correlation-------------------------------
#produce paired plots
pairs(trees, pch=21, bg=c("red"))

#calculate covariance and correlation
cov(trees)
cor(trees)
#-----------------------------------------------
#playing with a non-linear model
co2.model2 <- lm(co2 ~ time(co2) + I(cos(time(co2))) + I(sin(time(co2))))
summary(co2.model2)
co2.residuals2 <- resid(co2.model2)
qqnorm(co2.residuals2)
qqline(co2.residuals2)
#residuals are still not normal for model withg trigonometric terms

