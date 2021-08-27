library("quantmod")
library("PerformanceAnalytics")
# Gráfico con el comportamiento del BTC a lo largo de los meses hasta el día actual

date          = "2020-10-2" # Aquí ecogemos desde qué fecha queremos analizar
BTC_USD       = getSymbols.yahoo("BTC-USD", from=date, env = .GlobalEnv) 
BTC_USD_close = getSymbols.yahoo("BTC-USD", from=date, auto.assign = F)[,6]

BTC_USD_close

BTC_USD_RETS  = na.omit(dailyReturn(BTC_USD_close, type = "log"))
chart_Series(BTC_USD_RETS)
