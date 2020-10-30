# IS-LS model ----
# A closed economy in a short-medium run

# Packages ----
library(tidyverse)

# Parameters ----
c <- 0.63 # marginal propensity to consume
b <- 1500 # sensibility of the investment to the interest rate
k <- 0.6 # sensibility of the money demand to the product
h <- 2700 # sensibility of the money demand to the interest rate

# exogenous variable ----
C_bar <- 55 # autonomous consumption
I_bar <- 75 # autonomous investment
G_bar <- 200 # government spending
T_bar <- 110 # tax on income
M_bar <- 200 # money supply
P_bar <- 1 # price level (fixed in the short-run)

# Matrix representation ----
A <- rbind(c(1, -1, -1, 0),
           c(-c, 1, 0, 0),
           c(0, 0, 1, b),
           c(k, 0, 0, -h))
d <- c(G_bar, C_bar-c*T_bar, I_bar, M_bar/P_bar)

# Solutions ----
x <- solve(A, d)

# Creating diagram ----
Y <- seq(0.95*x[1], 1.05*x[1], 1)
C <- C_bar + c*(Y - T_bar)
I <- Y - C - G_bar
IS <- (I_bar-I)/b
LM <- 1/h*(k*Y-M_bar/P_bar)

data <- tibble(Y, C, I, IS, LM)

# graphing ----
ggplot(data, aes(x = Y)) + 
  geom_line(aes(y = IS), size = 2, alpha = .7, colour = 'dodgerblue3') +
  geom_line(aes(y = LM), size = 2, alpha = .7, colour = 'tomato3') +
  theme_minimal()
