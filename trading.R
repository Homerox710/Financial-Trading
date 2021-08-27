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
