#####################################################################

    ### BACKTESTING, EVAULUANDO SEÑALES


library(Quandl)
library(scales)
library(gridExtra)   
library(TTR)
library(jsonlite)
library(xtable)
library(gtable)
library(grid)
library(dplyr)

# 2. Load  
library(tidyverse)
library(httr)
library(readxl)
library(lubridate)
library(reshape2)
library(quantmod)

# 3. Set working directory. Estoy llamando mi codigo. 
setwd("/home/sap/Desktop/R/trading_r_taller_sap/")

# 4. Quandl authentication key. Se debe generar una api key en Quandl
#Quandl.api_key("9zUtYEM3Q9LZ39SGEv") # mostrar. Esta no funciona. Es un ejemplo.

#Evaluar la estrategia de long y short en cada cia.
PFL <- getSymbolsYahoo(c("KOF"))
filter(date >= "2015-01-01")

ILF <- getSymbolsYahoo("ILF") %>% 
  filter(date >= "2015-01-01")

# 5. Calcular senal de trading para la estrategia M01. 
PFL <- PFL %>%
  mutate(signal = ifelse(as.numeric(format(date, "%m")) <= 9,1,-1), ticker = "M01")

# 6. Calcular retornos diarios, senal de returno, retornos acumulados, 
#rolling retornos acumulados, drawdown, 
# y sharpe ratio. 
PFL <- PFL %>%
  mutate(daily_return = ifelse(row_number() == 1, 0, adjusted_close / lag(adjusted_close, 1) - 1), 
         signal_return = daily_return * signal, 
         cum_return = cumprod(1 + signal_return) - 1, 
         cum_return_3m = (cum_return + 1) / lag(cum_return + 1, 63) - 1, 
         cum_return_12m = (cum_return + 1) / lag(cum_return + 1, 252) - 1, 
         drawdown = (cum_return + 1) / cummax(cum_return + 1) - 1, 
         sd_12m = runSD(signal_return, n = 252)*sqrt(252), 
         sharpe_12m = SMA(cum_return_12m / sd_12m), 252)

ILF <- ILF %>% 
  mutate(daily_return = ifelse(row_number() == 1, 0, adjusted_close / lag(adjusted_close, 1) - 1), 
         cum_return = cumprod(1 + daily_return) - 1, 
         cum_return_3m = (cum_return + 1) / lag(cum_return + 1, 63) - 1, 
         cum_return_12m = (cum_return + 1) / lag(cum_return + 1, 252) - 1, 
         drawdown = (cum_return + 1) / cummax(cum_return + 1) - 1, 
         sd_12m = runSD(daily_return, n = 252)*sqrt(252), 
         sharpe_12m = SMA(cum_return_12m / sd_12m), 252)

combined <- bind_rows(PFL, ILF)

# 7. Plot equity curve versus benchmark.
(p1 <- ggplot(combined, aes(x = date, y = cum_return)) + 
    geom_line(aes(colour = ticker)) + 
    labs(title = "Equity Curve Versus Benchmark", 
         subtitle = "Evaluación de desempeno de la estrategia", 
         y = "Cumulative Return", 
         x = "Date") + 
    geom_hline(yintercept = 0) + 
    theme_alphaplot())

#####################################################################

### BACKTESTING, EVAULUANDO SEÑALES