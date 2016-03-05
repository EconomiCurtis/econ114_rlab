# Week 7 - R-Lab Notes

# http://lmgtfy.com/?q=r+poisson+distribution
?ppois
# rpois for random sampling. 
# qpois for quantiles

# Given a value for lambda, what's the probabilty of a certain value, 
# or range of values? 

# P(X=4), ~Poisson(4)
dpois(4, lambda = 4)

# P(X <= 4) ~P(4)
sum(dpois(0:4, lambda = 4))      
ppois(4, lambda = 4, lower.tail = T) # Using prob dist function

# P(X >= 12) ~P(4)
ppois(12, lambda = 4, lower.tail = F) # Using prob dist function, upper tail



# (a) (10 points) Let λ = 4. What is the Fisher information and observed Fisher
# information?
#' Fisher information - that's just lambda...?
#' Observed Fisher information:

#expected val
mean(rpois(100000, lambda = 4)) #will vary, I got 4.00308, very close. 

sum(rpois(100000, lambda = 4)) / mean((rpois(100000, lambda = 4)))^2

# I(/lambda) = n / \lambda = 100,000 / 4 = 25,000 ? 
# or is it I(/lambda) = 1 / \lambda = 0.25 ?


# (b) (10 points) Simulate n = 100,000 observations from this distribution. Find
# the asymptotic 95% confidence interval for λ.

x <- rpois(n = 100000, lambda = 4)
x <- (glm(x~1, family="poisson"))
exp(confint.default(x, level = 0.90)) # read ?confint
exp(confint.default(x, level = 0.95)) # read ?confint


# (c) (10 points) Draw B = 1000 bootstrap samples from your simulated data.
# What is the bootstrap standard deviation of λˆ?
x <- rpois(100000, lambda = 4)
bin <- rep(NA, 1000)
for (i in 1:1000){
  x.nu <- sample(x = x, size = 1000, replace = T)
  bin[i] <- mean(x.nu)
  # x <- (glm(x~1, family="poisson"))
  # exp(x$coefficients)  #does same as mean
}
mean(bin)
sd(bin)

print(paste(
  "sd is", sd(bin)
))

# # alt, very slow
# bin <- c()
# for (i in 1:1000){
#   x <- rpois(100000, lambda = 4)
#   bin <- c(bin, mean(x))
# }


# (d) (10 points) Find a 95% confidence interval using the bootstrapped standard
# deviation.

# s.e. = {sample sd}/{sqrt(n)}
mean(bin) - 1.96 * sqrt(sd(bin) / (1000))
mean(bin) + 1.96 * sqrt(sd(bin) / (1000))

# (e) (10 points) Find a 95% confidence interval using the bootstrapped empirical
# quantile method.
# find the 2.5 percentile, and the 97.5 percentile of the bootstrap samples

quantile(bin, c(0.025,0.975))


# (f) (10 points) Discuss the differences between the two bootstraps and the asymptotic
# confidence intervals.