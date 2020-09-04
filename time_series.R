library(astsa)
help(jj)

plot(jj)
# trend and seasonality

help(flu)
plot(flu)
# slight trend, seasonality

help(globtemp)
plot(globtemp)
plot(globtempl)
# trend and seasonality

plot(star)
# seasonality but no trend

# stationary ts = no trend in mean and var, no periodic variations.
# non-stationary are transformed into stationary


# stochastic proc = sequence of random variables. 
# each might have their own distribution, own expectation, var.
# every step is random, don't know exactly where you will be.
# time series can be considered realisation of a stochastic process.

# autocovariance = taking covariance of two points at different steps
# autocovariance function only depends on time difference
# because you assume time series is stationary

# autocovariance coefficients are estimates of correlation.


rpts <- rnorm(100)

# autocorrelation coeffs
(acf(rpts))

# autocorrelation function shows coeffs at time lags
acf(rpts, lag.max = 40)


# random walk model: X_t = X_(t-1) + Z_t
# wherever you are, add noise, next step, repeat
# noise is normal with some mean and variance
# X_t is the sum of noises till time t

x = c()
curr = 0
for(i in 1:100)
{
  curr = curr + rpts[i]
  x[i] <- curr
}

r_walk = ts(x)
plot(r_walk)
acf(r_walk)
# very high correlation for random walk, even 10 steps behind

d_r_walk <- diff(r_walk)
plot(d_r_walk)
acf(d_r_walk)
# now it's non-stationary

# MOVEING AVERGAE MODEL

# if effect of noise lasts for 2 steps, it is a moving average model of order 2

# simulating MA(2) model
noise = rnorm(1000)

ma_2 = NULL

for(i in 3:1000)
{
  ma_2[i-2] = noise[i] + 0.7*noise[i-1] + 0.2*noise[i-2]
}

ma2_pro = ts(ma_2)
plot(ma2_pro)

acf(ma2_pro)
# autocorrelation cut off after 2

# strictly stationary - distribution of X_t1 same as that of X_(t1+T)
# ACF depends on lag only

# weakly stationary = mean is constant, ACF depends only on lag
# implies variance is constant

# white noise is stationary, random walks are not
# moving average processes are stationary


# backward shift operator
# B.X_t = X_(t-1)

# for MA1 process:
# X_t = X_(t-1) + Z_t
# X_t = B.X_t + Z_t

# MA2 process:
# X_t = Z_t + (0.2)Z_(t-1) + (0.4)Z_(t-2)
# X_t = Z_t + (0.2)B.Z_t + (0.4)B^2.Z_t

# also works with AR processes and MA processes with drift

# ivertability = MA(q) process that can be expressed as an AR(inf) process
# divergent series are not invertible, convergent ones are

# MA(q) processes are always stationary, AR(p) aren't always

# AR PROCESSES

# simulating AR(1)
set.seed(2016)
N = 1000; phi = 0.4;
Z = rnorm(N); X = NULL;
X[1] = Z[1]

for(t in 2:N)
{
  X[t] = Z[t] + phi*X[t-1]
}

X.ts = ts(X)
plot(X.ts)
acf(X.ts)

# AR(2) process

# X_t = Z_t + 0.7X_(t-1) + 0.2.X(t-2)
set.seed(2017)

X.ts2 <- arima.sim(list(ar = c(0.7,0.2)), n =1000 ) 

plot(X.ts2)
acf(X.ts2)

# Yule-Walker equations are used to obtain paramters of model

# PACF

# For MA processes, the ACF plot cuts off after the q (order) value
# For AR processes, use PACF and check number of significant spikes

# ar() function is used to estimate coefficients.
# selects the best p value on fitted data points

# plot ACF and PACF to get an idea about which type of process fits data better

# ARMA MODELS

# arma(p,q)
# X_t = Z_t + phi_1*X_(t-1) ... phi_p*X_(t-p) + theta_1*Z_(t-1) ...theta_q Z_(t-q)  
# ARMA models can be converted to AR and MA models

#           AR(p)      |    MA(q)      |  ARMA(p,q)
# ACF  | Tails off     | Cuts-off at p | Tails off
# PACF | Cuts-off at p | Tails off     | Tails off

acf(discoveries)
pacf(discoveries)

# ACF doesn't abruptly cut off, kinda tails off
# PACF tails off, has one/two spikes above the cutoff

# ARIMA PROCESSES

# arima(p,d,q)
# d = number of differences required to make it stationary
# If ACF decay is slow, differencing may be required

# Q-statistic
# Test null hypothesis that several autocorrelation coeffificents are 0

# load births dataset
births <- read.csv('cali.csv')
n_births <- births[,2]
n_births <- n_births[1:365]
plot(n_births, type = 'l')
Box.test(n_births, lag = log(length(n_births)))
# p-value is very small, so there is autocorrelation somewhere

# data has a trend (going upwards); Remove it
plot(diff(n_births), type = 'l')
Box.test(diff(n_births), lag = log(length(n_births)))
# p-value still very small

acf(diff(n_births), 50)
# two big spikes, but another further down the line (maybe noise)

pacf(diff(n_births), 50)
# five significant spikes

# try fitting arima with params - (0,1,1); (0,1,2); (7,1,1); (7,1,2);

# if p-value > 0.05, residuals have no autocorrelation

# SARIMA MODELS
# Apart from recent lags, there might be seasonal periodic component
# Which repeats after every t observations

# SARIMA process needs to be stationary and invertible

# sarima(p,d,q,P,D,Q) has two parts
# p,d,q = order of non-seasonal AR, diff, and MA terms
# P,D,Q = order of seasonal AR, diff and MA terms

# Ex: sarima(0,0,1,0,0,1) has only seasonal and non-seasonal MA terms

# ACF plot shows big spikes after gaps


# SIMPLE EXPONENTIAL SMOOTHING
# when data is too noisy and model cannot be fitted
# Weighs previous data point and updates the fresh data point

# Double exponential
# Holt-Winters smoothing 
# used for data with trend

# Triple exponential
# used for multiplicative seasonality (trend + increased scale)
# multiplicative -> additive achieved by taking log




