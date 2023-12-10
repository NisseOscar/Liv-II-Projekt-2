## Assignment 2 Livsförsärking

# Some intial notes on the project
# Valuations based on risk neutral probabilities and probability measure Q
# Makeham function
# at time 0 a single payment is made
# After fee withdrawment invested in risky asset X(t)
# X(t) is a geometric brownian motion
# X(t) = X(0) * exp((r - sigma^2/2)t + sigma * W(t))
# W(t) is a Wiener process brownian motion
# r is the risk free interest rate
# sigma is the volatility of the risky asset
# Deaths are independent and identically distributed

# Given that a person is alive at time tau, 
# the insured will receive the invested return on the risky asset.
# Moreover an inheritance gain paid by the death of other in the pool.
# Redistribution of gains will be done at the end of each calendar year.



# Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)


#### Parameters

# Makeham function
a <- 3.5*10^(-4)	
b <- 7*10^(-8)
c <- 0.157
makeham <- function(x){
  a + b * exp(c * x)
}

# Brownian motion
# Wiener process brownian motion
r <- 0.015
sigma <- 0.06


# A.1
# Portfolio of 10000 contracts, with 50 yer old individuals.
# All turned 50 at new years.
# Benefits payed at age of 70
# Assume no charge is paid on the initial investment but will collect a annual fee of 0.1 % each yera.
# Calculate the risk neutral value of the benefits for one such contract 
# and calculate the risk-neutral value of the total gain for the insurer

annual_fee <- 0.001
n <- 10000
t_0 = 50
tau = 70
t_step <- 1/12

time_steps <- seq(0, tau-t_0, t_step)

sim_X <- function(){
    W <- rnorm(length(time_steps), mean = 0, sd = 1)
    W_cum <- cumsum(W)
    X <- 1+exp((r - sigma^2/2)*time_steps + sigma * W_cum)
    return(X)
}
# Plot lineplot
plot(time_steps+t_0,sim_X(), type = "l", col = "black",alpha=0.1, xlab = "Time", ylab = "X(t)", main = "Simulated X(t)")

# Simulate 10000 times ant plot
Xs <- replicate(1000, sim_X())

Xs <- data.frame(Xs)
Xs$t <- time_steps+t_0
X_melt <- melt(Xs, id.vars="t")
colnames(X_melt) <- c("t", "run", "return")

p = ggplot(X_melt,aes(x=t, y=return, group=run)) +
    theme_minimal() +
    geom_line(size=0.2, alpha=0.1)
p

# Calculate the risk neutral value of the benefits for one such contrac

X = sim_X()
survival_rate <- makeham(time_steps)
