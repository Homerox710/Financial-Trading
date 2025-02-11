#####################################################################

    ### BACKTESTING, EVAULUANDO SE�ALES


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
         subtitle = "Evaluaci�n de desempeno de la estrategia", 
         y = "Cumulative Return", 
         x = "Date") + 
    geom_hline(yintercept = 0) + 
    theme_alphaplot())

#####################################################################

### EVALUACION DE DESEMPE�O (PLOTS)

(p2 <- ggplot(PFL, aes(x = date, y = signal)) + 
   geom_line(size = 1, colour = "blue") + 
   labs(title = "Trading Signal", 
        subtitle = "Senal de mi estrategia.Entre  +1 y -1.", 
        y = "Position", 
        x = "Date") + 
   geom_hline(yintercept = 0) + 
   theme_alphaplot())

(p3 <- ggplot(PFL, aes(x = date, y = adjusted_close)) + 
    geom_line(aes(colour = signal)) + 
    scale_colour_gradient(low = "red") +
    labs(title = "PFL Closing Price con Trading Signal", 
         subtitle = "Cuando short y cuando long", 
         y = "Precio Cierre Ajustado", 
         x = "Fecha") + 
    geom_hline(yintercept = 0) + 
    theme_alphaplot())


(p4 <- ggplot(combined, aes(x = date, y = cum_return_3m)) + 
    geom_line(aes(colour = ticker)) + 
    labs(title = "Rolling Returns (3 Meses)", 
         y = "Retorno", 
         x = "Fecha") + 
    scale_y_continuous(labels = percent, limits = c(-0.5, 0.75)) + 
    geom_hline(yintercept = 0) + 
    theme_alphaplot())

(p5 <- ggplot(combined, aes(x = date, y = drawdown)) + 
    geom_line(aes(colour = ticker)) + 
    labs(title = "Drawdown", 
         subtitle = "Frecuencia de caidas, tamano de maximas caidas y tiempo de recuperacion", 
         y = "Porcentaje drawdown", 
         x = "Fecha") + 
    scale_y_continuous(labels = percent) + 
    geom_hline(yintercept = 0) + 
    theme_alphaplot())


(p6 <- ggplot(combined, aes(x = date, y = sharpe_12m)) + 
    geom_line(aes(colour = ticker)) + 
    labs(title = "Sharpe Ratio (12 Meses)", 
         subtitle = "Sharpe ratio. Retornos por unidad de riesgo.", 
         y = "Sharpe Ratio", 
         x = "Fecha") + 
    geom_hline(yintercept = 0) + 
    theme_alphaplot())

#####################################################################

### EVALUACION DE RENTABILIDAD

library(tidyverse)
library(tidyquant)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

stocks <- c("KOF", "TV", "BBD", "WALMEX.MX", "GFNORTEO.MX")

stock_data <- tq_get(stocks,
                     get = "stock.prices",
                     from = Sys.Date() - months(12),
                     to = Sys.Date())

init.investment <- 1000
growth <- mo_returns %>% arrange(date) %>%
  mutate(final_value = init.investment * cumprod(1 + returns)) %>%
  arrange(desc(final_value))
growth %>% filter(date == max(date)) %>% select(-date)

growth %>% ggplot(aes(x = date, y = final_value, color = symbol)) +
  geom_line() +
  # geom_smooth(method = "loess") +
  labs(
    title = "Portafolio individual: Comparando el crecimiento de US1000",
    subtitle = "Visualizacion de desempeno",
    x = "",
    y = "Valor inversion"
  ) +
  theme_tq() + theme(legend.position = "right") +
  scale_y_continuous(labels = scales::dollar)

growth %>% ungroup() %>% filter(date == max(date)) %>% 
  mutate(rank = row_number()) %>% top_n(5, final_value) %>% 
  select(rank, symbol, final_value)     