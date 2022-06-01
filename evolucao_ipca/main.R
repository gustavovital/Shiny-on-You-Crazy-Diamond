# evolução IPCA - shiny
# 
# 0.0 - criar tabela

# @gustavovital

# pacotes necessarios ----
library(tidyverse)
library(tm)
library(sidrar)
library(viridis)

# get data ----
api <- '/t/7060/n1/all/v/63/p/all/c315/all/d/v63%202'
ipca_index <- get_sidra(api = api)

ipca <- ipca_index %>% 
  select(Valor, Mês, `Geral, grupo, subgrupo, item e subitem`) %>% 
  pivot_wider(names_from = `Geral, grupo, subgrupo, item e subitem`, values_from = Valor) 

colnames(ipca) <- colnames(ipca) %>% 
  removePunctuation() %>% 
  removeNumbers()

# Define function to cumulative data and percentage ----
acum <- function(serie, perc = TRUE) {
  
  # define a empty vector to create the cumulative series as follows: 1 + Y_i/100
  acum <- c()
  acum[1] <- 1 + serie[1]/100
  
  # generalize the condition to all the series
  for(indice in 2:length(serie)) {
    acum[indice] <- (acum[indice - 1])*(1 + serie[indice]/100) 
  }
  acum <- (acum - 1)
  
  # return the cumulative series in percentage rate as default or not:
  if(perc == TRUE){
    return(acum*100)
  } else{
    return(acum)
  }
}


ipca_acum <- sapply(ipca[, 2:ncol(ipca)], acum) %>% unlist() %>% as_tibble()
ipca_acum <- rbind(0, ipca_acum)
ipca_acum$date <- seq(as.Date('2019-12-01'), length.out = nrow(ipca_acum), by = 'm')

