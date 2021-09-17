
#####################################################################
       
       ### SERIE DE TIEMPO

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

#####################################################################

      ### PROMEDIOS,DESVIACIONES, COEFICIENTES

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

  # FEMSA
meanFEMSA_3<- mean(FEMSA_3$Adj.Close)
sdFEMSA_3<- sd(FEMSA_3$Adj.Close)  
Coef_F <- (sdFEMSA_3/meanFEMSA_3)*100  

  # Walmart
meanWALMART_3<- mean(WALMART_3$Adj.Close)
sdWALMART_3<- sd(WALMART_3$Adj.Close)  
Coef_W <- (sdWALMART_3/meanWALMART_3)*100  

# Televisa
meanTELEVISA_3<- mean(TELEVISA_3$Adj.Close)
sdTELEVISA_3<- sd(TELEVISA_3$Adj.Close)  
Coef_W <- (sdTELEVISA_3/meanTELEVISA_3)*100  


#####################################################################

      ### COMPORTAMIENTO HISTORICO


#analisis que resume comportamiento historico de las cinco cias 
#desde marzo 2, 2019. Con precio cierre ajustado

date <- "2019-3-2"  
tickers <- c("GFNORTEO.MX", "TV", "WALMEX.MX", "BBD", "KOF")       

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(ticker,
                                                             from="2019-3-2", periodicity="daily",auto.assign=FALSE)[,6])  
}

portfolioPrices 

view(portfolioPrices)


#Graficar lineas con serie de tiempo precios cierre. con ggplot2
library ("ggplot2")
#Antes vamos a incluir el titulo de la variable tiempo en la columna 0  
#exportar archivo excel
library("dplyr")
colnames(portfolioPrices)
str(portfolioPrices)
library("tibble")

portfolioPrices <- as.data.frame(portfolioPrices) # verificamos que es un dataframe
class(portfolioPrices)    

portfolioPrices <- rownames_to_column(portfolioPrices, var="fecha") # Agregamos columna fecha
view(portfolioPrices)     

#extraer este dataset.al escritorio
#windows
write.csv2(portfolioPrices , "C:/Users/Desktop/portfolioPrices.csv")
#linux - Ejemplo de mi escritorio
write.csv2(portfolioPrices , "/home/sonia/Desktop/portfolioPrices.csv")  

#ajustar el dataframe para hacer la grafica de lineas
library("tidyverse") 
# Fila fecha a columna
df <- portfolioPrices %>% select(fecha,GFNORTEO.MX.Adjusted,TV.Adjusted,
                                 WALMEX.MX.Adjusted,BBD.Adjusted, KOF.Adjusted) %>%
  gather (key="variable", value="value", -fecha)
head(df)

#ahora grafica de lineas para ver comportamiento de precio de
#cierre ajustado 
ggplot(df, aes(x=fecha, y=value)) + 
  geom_line(aes(group=variable, linetype=variable))+
  scale_color_manual(values=c("red", "blue", "black", "purple", "green"))+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),    
        panel.grid.minor = element_blank())+
  labs(title="Fluctuación Precio Cierre Ajustado desde Marzo 2019")   

#####################################################################

      ### HISTORICO DE RETORNO DIARIO SOBRE PRECIOS DE CIERRE

date <- "2019-3-2"    
TVClose <- getSymbols.yahoo("TV", from=date, auto.assign = F)[,6]
TVClose

TVRets <- na.omit(dailyReturn(TVClose, type="log")) 
chartSeries(TVRets)    

#Femsa    
date <- "2019-3-2"  
KOFClose <- getSymbols.yahoo("KOF", from=date, auto.assign = F)[,6]
KOFClose   

KOFRets <- na.omit(dailyReturn(KOFClose, type="log")) 
chartSeries(KOFRets) 

#Grupo financiero banorte
date <- "2019-3-2"  
GFNORTEO.MXClose <- getSymbols.yahoo("GFNORTEO.MX", from=date, auto.assign = F)[,6]
GFNORTEO.MXClose

GFNORTEO.MXRets <- na.omit(dailyReturn(GFNORTEO.MXClose, type="log")) 
chartSeries(GFNORTEO.MXRets) 

#Walmart México
date <- "2019-3-2"  
WALMEX.MXClose <- getSymbols.yahoo("WALMEX.MX", from=date, auto.assign = F)[,6]
WALMEX.MXClose

WALMEX.MXRets <- na.omit(dailyReturn(WALMEX.MXClose, type="log")) 
chartSeries(WALMEX.MXRets) 

#Bradesco
date <- "2019-3-2"  
BBDClose <- getSymbols.yahoo("BBD", from=date, auto.assign = F)[,6]
BBDClose

BBDCloseRets <- na.omit(dailyReturn(BBDClose, type="log")) 
chartSeries(BBDCloseRets)     

#Ver con velas japonesas 
getSymbols( Symbols="BBF", src="yahoo",
            from = "2019-03-02",
            to = "2020-03-02")

barChart(BBF, theme = "white")  

candleChart(BBF, multi.col = TRUE, theme = "white")   
#
getSymbols( Symbols="WALMEX.MX", src="yahoo",
            from = "2019-03-02",
            to = "2020-03-02")

barChart(WALMEX.MX, theme = "white")  

candleChart(WALMEX.MX, multi.col = TRUE, theme = "white") 

getSymbols( Symbols="TV", src="yahoo",
            from = "2019-03-02",
            to = "2020-03-02")

barChart(TV, theme = "white")  

candleChart(TV, multi.col = TRUE, theme = "white") 

#
getSymbols( Symbols="BFNORTEO.MX", src="yahoo",
            from = "2019-03-02",
            to = "2020-03-02")

barChart(BFNORTEO.MX, theme = "white")  

candleChart(BFNORTEO.MX, multi.col = TRUE, theme = "white") 

#
getSymbols( Symbols="KOF", src="yahoo",
            from = "2019-03-02",
            to = "2020-03-02")

barChart(KOF, theme = "white")  

candleChart(KOF, multi.col = TRUE, theme = "white") 
install.packages("xts")

#####################################################################

      ### HISTOGRAMAS

hist(TV$TV.Close, breaks= 60, col="blue")      

hist(KOF$KOF.Close, breaks= 60, col="blue") 

hist(BBF$BBF.Close, breaks= 60, col="blue") 

hist(BFNORTEO.MX$BFNORTEO.MX.Close, breaks= 60, col="blue") 

hist(WALMEX.MX$WALMEX.MX.Adjusted, breaks= 60, col="blue") 

#####################################################################

      ### ANALISIS TECNICO

#TA: Technical analysis
#BBands es bandas de bollinger. Vo es volumen: 
#Nivel de actividad de un mercado. 
#MACD es moving average convergence divergence
KOF%>%Ad()%>%chartSeries()
KOF%>%chartSeries(TA='addBBands();addVo();addMACD()',
                  subset='2020')  


KOF%>%chartSeries(TA='addBBands();addVo();addMACD();
                  addRSI()',subset='2020') 


#y ATR
KOF%>%chartSeries(TA='addBBands();addMACD();addRSI();
                  addATR()',subset='2020')   


#####################################################################

      ### RENTABILIDAD DEL PORTAFOLIO SEGÚN RIESGO

library(PerformanceAnalytics)
library(PortfolioAnalytics)
tickers <- c("WALMEX.MX", "BBD", "KOF", "BBF", "TV")      

weights <- c(.20, .20, 0.20, 0.20, 0.20)     

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(ticker,
                                                             from="2019-3-2", periodicity="daily",auto.assign=FALSE)[,6])  
}

portfolioPrices 
#ROC: Rate of change. % de variacion entre precio actual y precio de
#periodo anterior. 
portfolioReturns <- na.omit(ROC(portfolioPrices))  
portfolioReturns

#Benchmark: con ILF> ishares Latin Funds

benchmarkPrices <- getSymbols.yahoo('ILF',
                                    from='2019-3-2', periodicity='daily', auto.assign=FALSE)[,6]

benchmarkReturns <- na.omit(ROC(benchmarkPrices))     
benchmarkReturns      

#Retornos del portafolio segun el peso de cada accion. 
#Sale una sola columna
#porque son los retornos del portafolio segun los pesos. 
#Funcion de returno de portafolio del paquete Performance analytics

RetornosPortafolio <- Return.portfolio(portfolioReturns)
RetornosPortafolio

#Grafica ROC
RetornosPortafolio %>% chartSeries(TA="addROC()",subset="2020")
benchmarkReturns %>% chartSeries(TA="addROC()",subset="2020")

#####################################################################
