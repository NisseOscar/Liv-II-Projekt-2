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

# Parameters for simulation
t_0 = 50
tau = 70
t_step <- 1
n <- 10000
n_simulations <- 1000
time_steps <- seq(0, tau-t_0, t_step)

# Wiener process brownian motion
r <- 0.015
sigma <- 0.06

# Helper function to plot simulated brownian motions
plot_simulations <- function(simulations){
    Xs <- data.frame(simulations)
    Xs$t <- time_steps+t_0
    X_melt <- melt(Xs, id.vars="t")
    colnames(X_melt) <- c("t", "run", "return")
    mean_res <- mean(simulations[dim(simulations)[1],])

    p <- ggplot(data=X_melt,aes(x=t, y=return, group=run)) +
        theme_minimal() +
        geom_line(size=0.2, alpha=0.1,color='black')+ 
        stat_summary(fun.y=mean,geom="line",lwd=1,color='darkred',alpha=0.5,aes(group=1))+
        geom_text(aes(label=round(mean_res,2),y=mean_res,x=tau+1),
            vjust=0,col='darkred',size=3)
    
    return(p)
}

plot_histogram <- function(series){
    p  <- ggplot(data.frame(series), aes(x=series,y=..density..)) + 
        geom_histogram(bins=20) + 
        geom_density(alpha=.2)+
        theme_minimal()+
        geom_segment(aes(x = mean(series),xend=mean(series), y = 0, yend = 2),col='darkred',size=0.5)+
        geom_text(aes(label=round(mean(series),4),y=-0.1,x=mean(series)),
            vjust=0,col='darkred',size=3)
    return(p)
}


###### Simulate price development of the risky asset X(t)

sim_X <- function(){
    W <- rnorm(length(time_steps), mean = 0, sd = 1)
    W_cum <- cumsum(W)
    X <- exp((r - sigma^2/2)*time_steps)+ sigma * W_cum
    return(X)
}

############ A.1
annual_fee <- 0.001
X_sim <- sim_X()
S_t <- exp(-makeham(t_0+time_steps))
portfolio_value <- replicate(
    n_simulations,
    n*(sim_X()*(1 -annual_fee)^(time_steps))
)

n_survivors <- n*(exp(-sum(t_step*makeham(time_steps))))
policy_holder_value <- portfolio_value/n_survivors

p <- plot_simulations(policy_holder_value)
ggsave("plots/a1_sim.jpg", p, width = 15, height = 10, units = "cm")

risk_neutral_value_a1 <- policy_holder_value[length(time_steps),]*exp(-r*(length(time_steps)-1))
p <- plot_histogram(risk_neutral_value_a1)
ggsave("plots/a1_price.jpg", p, width = 15, height = 10, units = "cm")

################ A.2
initial_fee <- 1-(1 -annual_fee)^(tau-t_0)
X_sim <- sim_X()
S_t <- exp(-makeham(t_0+time_steps))
portfolio_value <- replicate(
    n_simulations,
    n*(sim_X()*(1-initial_fee))
)

n_survivors <- n*(exp(-sum(t_step*makeham(time_steps))))
policy_holder_value <- portfolio_value/n_survivors

p <- plot_simulations(policy_holder_value)
ggsave("plots/a2_sim.jpg", p, width = 15, height = 10, units = "cm")

risk_neutral_value_a1 <- policy_holder_value[length(time_steps),]*exp(-r*(length(time_steps)-1))
p <- plot_histogram(risk_neutral_value_a1)
ggsave("plots/a2_price.jpg", p, width = 15, height = 10, units = "cm")


############## A.3
inheritence_tax <- 0.20
n <- 10000
X_sim <- sim_X()
S_t <- exp(-makeham(t_0+time_steps))
portfolio_value <- replicate(1000,n*(sim_X()*(1 - (1-S_t)*inheritence_tax)))

n_survivors <- n*(exp(-sum(t_step*makeham(time_steps))))
policy_holder_value <- portfolio_value/n_survivors

p <- plot_simulations(policy_holder_value)
ggsave("plots/a3_sim.jpg", p, width = 15, height = 10, units = "cm")

risk_neutral_value_a1 <- policy_holder_value[length(time_steps),]*exp(-r*(length(time_steps)-1))
p <- plot_histogram(risk_neutral_value_a1)
ggsave("plots/a3_price.jpg", p, width = 15, height = 10, units = "cm")


# A.4

#2 Simulate 100 times ant plot
Xs <- replicate(10, sim_X())

p <- plot_simulations(Xs)
# Save plot
ggsave("plots/a4_ass_simulation.jpg", p, width = 15, height = 10, units = "cm")
p <- plot_histogram(Xs[20,])
ggsave("plots/a3_ass_distribution.jpg", p, width = 15, height = 10, units = "cm")

