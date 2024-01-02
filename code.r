## Assignment 2 Livsförsärking

# Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(reshape2)


#### Parameters

# Set Seed
set.seed(1234)

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
z <- qnorm(0.975)

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
        geom_text(aes(label=round(mean(series),2),y=-0.1,x=mean(series)),
            vjust=0,col='darkred',size=3)
    return(p)
}


###### Simulate price development of the risky asset X(t)

sim_X <- function(){
    W <- rnorm(length(time_steps), mean = 0, sd = 1)
    W_cum <- cumsum(W)
    X <- exp((r - sigma^2/2)*time_steps+ sigma * W_cum)
    return(X)
}


####### Simulate survival percentage
p_t <- function(t_0,t){
  exp( - (a*(t-t_0)+b/c*(exp(c*t)-exp(c*t_0))))
}

sim_S <- function(){
    S <- rep(n,length(time_steps))
    for (i in 2:length(time_steps)){
        p <- p_t(t_0+time_steps[i-1],t_0+time_steps[i])
        S[i] <- round(rbinom(1,S[i-1],p))
    }
    return(S/n)
}

expected_return_without_fee = 1/p_t(50,70)
expected_return_without_fee

############ A.1
annual_fee <- 0.001
X_sim <-  replicate(n_simulations,sim_X())
S_sim <- replicate(n_simulations,sim_S())
portfolio_value <- X_sim*(1 -annual_fee)^(time_steps)/S_sim

# Plot simulations
p <- plot_simulations(portfolio_value)
ggsave("plots/a1_sim.jpg", p, width = 15, height = 10, units = "cm")

# Plot distribution of risk neutral value
risk_neutral_value <- portfolio_value[length(time_steps),]*exp(-r*(length(time_steps)-1))
p <- plot_histogram(risk_neutral_value)
ggsave("plots/a1_price.jpg", p, width = 15, height = 10, units = "cm")
mean(risk_neutral_value)
z*sd(risk_neutral_value)

# Plot distribution of policy issuer value
acc_fee <- (1-(1 -annual_fee)^(tau-t_0))
policy_issuer_value = n*acc_fee*X_sim[length(time_steps),]*exp(-r*(length(time_steps)-1))
mean(policy_issuer_value)
z*sd(policy_issuer_value)

################ A.2
initial_fee <- 1-(1 -annual_fee)^(tau-t_0)
X_sim <-  replicate(n_simulations,sim_X())
S_sim <- replicate(n_simulations,sim_S())
portfolio_value <- X_sim*(1-initial_fee)/S_sim

p <- plot_simulations(portfolio_value)
ggsave("plots/a2_sim.jpg", p, width = 15, height = 10, units = "cm")

risk_neutral_value <- portfolio_value[length(time_steps),]*exp(-r*(length(time_steps)-1))
p <- plot_histogram(risk_neutral_value)
ggsave("plots/a2_price.jpg", p, width = 15, height = 10, units = "cm")
mean(risk_neutral_value)
z*sd(risk_neutral_value)


############## A.3
inheritence_tax <- 0.20
X_sim <-  replicate(n_simulations,sim_X())
S_sim <- replicate(n_simulations,sim_S())
portfolio_value <- X_sim*(1 - (1-S_sim)*inheritence_tax)/S_sim
S_sim

p <- plot_simulations(portfolio_value)
ggsave("plots/a3_sim.jpg", p, width = 15, height = 10, units = "cm")

risk_neutral_value <- portfolio_value[length(time_steps),]*exp(-r*(length(time_steps)-1))
p <- plot_histogram(risk_neutral_value)
ggsave("plots/a3_price.jpg", p, width = 15, height = 10, units = "cm")
mean(risk_neutral_value)
z*sd(risk_neutral_value)

X_sim_20 <- X_sim[length(time_steps),]
S_sim_20 <- S_sim[length(time_steps),]
policy_issuer_value = n*X_sim_20*(1-S_sim_20)*inheritence_tax*exp(-r*(length(time_steps)-1))
mean(policy_issuer_value)
z*sd(policy_issuer_value)

############## A.4
# 1 Simulate 100 times
S <- replicate(100, sim_S())*n
p <- plot_simulations(S)
# Save plot
ggsave("plots/a4_survival_simulation.jpg", p, width = 15, height = 10, units = "cm")
p <- plot_histogram(S[21,])
ggsave("plots/a4_survival_distribution.jpg", p, width = 15, height = 10, units = "cm")


#2 Simulate 100 times ant plot
Xs <- replicate(100, sim_X())
p <- plot_simulations(Xs)
# Save plot
ggsave("plots/a4_ass_simulation.jpg", p, width = 15, height = 10, units = "cm")
p <- plot_histogram(Xs[21,])
ggsave("plots/a4_ass_distribution.jpg", p, width = 15, height = 10, units = "cm")

# Calculate New Inheritence fee to match other fee structures
f_star = 0.0198/(1-p_t(50,70))
f_star