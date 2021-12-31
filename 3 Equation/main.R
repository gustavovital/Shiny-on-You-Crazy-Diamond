# Description: 3 equation model dynamic
# Author: gustavovital
# Date: 31-12-2021

# Modelo:
# 
# Monetary rule:    yt - ye = -alpha*beta*(pit - piT)
# Phillips curve:   pit = pit_1 + alpha*(yt - ye)
# IS curve:         yt = At - a*rt_1

# Packages:
library(tidyverse)
library(patchwork)

# Implementing the model ----
# Parameters and SS
alpha <- 0.9
beta <- 0.5
piT <- 2
ye <- 2.5
a <- .6
A <- 3


# inflation
pit_1 <- seq(-10, 10, .01)

# Variables
pit <- (1/(1 + alpha^2 * beta)) * (pit_1 - alpha^2 * beta * piT)
yt <- ye - alpha*beta*(pit - piT)
rt <- 1/a * A -a*yt




# Initial plots, MR e PC:
data <- data.frame(pit, yt, rt, pit_1)

data %>% 
  ggplot() +
  geom_line(aes(x = (pit_1 + alpha*(yt - ye)), y = pit), size = 2, alpha = .4, color = 'blue') +  # PC
  geom_line(aes(x = yt, y = (ye - -alpha*beta*(pit - piT))), size = 2, alpha = .4) +  # MR
  xlim(0, 10) +
  ylim(-2, 5) -> y_pi

data %>% 
  ggplot(aes(yt, rt)) +
  geom_line(, size = 2, alpha = .4, color = 'darkblue') +
  xlim(0, 10) -> y_rt


y_rt/y_pi


