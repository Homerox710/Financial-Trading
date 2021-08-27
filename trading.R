library(quantmod)
library(PerformanceAnalytics)

#serie de tiempo y conseguir datos desde enero 15, 2019
date <- "2019-1-15"           

#Obtener símbolos de yahoo. por ej Televisa
tv <- getSymbols.yahoo("TV", from = date)

TVClose <- getSymbols.yahoo("TV", from=date, auto.assign = F)[,6]
TVClose

TVRets <- na.omit(dailyReturn(TVClose, type="log")) 
#TVRets <- na.omit(dailyReturn(TVClose, type="arithmetic")) 
chartSeries(TVRets)  

#Importar los datasets de cinco empresas de latam que seleccionamos 
#en yahoo finance 
#Despues de importar los archivos, unirlos por filas y despues 
#convertir a numeric
#crear columna a cada dataset con nombre de cia (ticker)
library(dplyr)
library(caret)
library(tidyverse)     

#dataset FEMSA
FEMSA <- mutate (FEMSA, Company= "Femsa", Company_Ticker="KOF") 
#cambiamos el orden de las variables (columnas) para tener primero
#la compañia
FEMSA_2 <- FEMSA[,c(9,8,1,2,3,4,5,6,7)]         

#dataset GFBANORTE
GFBANORTE.MX <- mutate (GFBANORTE.MX, Company= "Grupo Financiero Banorte", 
                        Company_Ticker="GFNORTEO.MX") 
#cambiamos el orden de las variables (columnas) para tener primero
#la compañia
GFBANORTE.MX_2 <- GFBANORTE.MX[,c(9,8,1,2,3,4,5,6,7)]  

#Televisa
TELEVISA <- mutate (TELEVISA, Company= "Televisa", 
                    Company_Ticker="TV") 
TELEVISA_2 <- TELEVISA[,c(9,8,1,2,3,4,5,6,7)] 

#walmart México
WALMEX.MX <- mutate (WALMEX.MX, Company= "Walmart Méx", 
                     Company_Ticker="WALMEX.MX") 
WALMEX.MX_2 <- WALMEX.MX[,c(9,8,1,2,3,4,5,6,7)] 

#Bradesco. banco brasil
BBD.bradesco <- mutate (BBD.bradesco, Company= "Banco Bradesco", 
                        Company_Ticker="BBD") 
BBD.bradesco_2 <- BBD.bradesco[,c(9,8,1,2,3,4,5,6,7)] 

#Crear un gran dataset agregando filas (rows)
Latam_GEI_Index <- rbind(FEMSA_2, GFBANORTE.MX_2, TELEVISA_2, WALMEX.MX_2, 
                         BBD.bradesco_2)      

#ver estructura de dataset
str (Latam_GEI_Index)   

#si algo estuviera en caracter, entonces tocaria usar el convertidor as.numeric asi
#ej: si open no estuviera numerico
Latam_GEI_Index$Open = gsub(",", "", Latam_GEI_Index$Open) %>%
  as.numeric()  

#promedios y desviaciones de los precios de cierre ajustados de las cinco acciones
summary(Latam_GEI_Index)

#desviacion estandar de precio de cierre ajustado de las cinco companias
sd(Latam_GEI_Index$Adj.Close)   

#subseting para sacar promedio, desviacion estandar y coeficiente por compania
library("dplyr")

FEMSA_3 <- Latam_GEI_Index %>%
  select(Company, Adj.Close) %>%
  filter(Company == "Femsa")     

WALMART_3 <- Latam_GEI_Index %>%  
  select(Company, Adj.Close) %>%
  filter(Company == "Walmart Méx")

BBD.bradesco_3 <- Latam_GEI_Index %>%  
  select(Company, Adj.Close) %>%
  filter(Company == "Banco Bradesco")

TELEVISA_3 <- Latam_GEI_Index %>%  
  select(Company, Adj.Close) %>%
  filter(Company == "Televisa")   

GFBANORTE.MX_3 <- Latam_GEI_Index %>%  
  select(Company, Adj.Close) %>%
  filter(Company == "Grupo Financiero Banorte")

#promedio, desviacion y coeficiente para cada cia.   
meanWALMART_3<- mean(WALMART_3$Adj.Close)
sdWALMART_3<- sd(WALMART_3$Adj.Close)  
Coef_W <- (sdWALMART_3/meanWALMART_3)*100  
