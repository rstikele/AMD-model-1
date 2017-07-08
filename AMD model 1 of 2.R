library(quantmod)
library(XML)
library(Amelia)
library(TTR)
#library(plotly)
library(ggplot2)
library(binhf)
library(PerformanceAnalytics)
library(MuMIn)
library(glmulti)
library(mice)
library(VIM)
library(zoo)



percent.change = function(open,close){(((close-open)/(open))*100)}

paste0(1:85, collapse = ' ')#paste
fetch.stock.data = getQuote(c('AMD','NVDA','INTC','AMZN','XOM','EBAY','MSFT','NFLX','AAPL','DIS','TSM', 'DCTH', 'RNVA'), what = yahooQF())
attach(fetch.stock.data)
stock.data = data.frame(Symbol, `Ave. Daily Volume`, `Book Value`, `% Change`, `Earnings/Share`, `EPS Estimate Current Year`, `EPS Estimate Next Quarter`, `EPS Estimate Next Year`, `Float Shares`, EBITDA, `P/E Ratio`, `PEG Ratio`, `Price/Book`, `Price/Sales`, `Price/EPS Estimate Current Year`, `Price/EPS Estimate Next Year`, `Short Ratio`, `1 yr Target Price`)
View(stock.data)
detach(fetch.stock.data)
missmap(fetch.stock.data, main ='Missing Map', col = c('yellow', 'black'), legend = F)
missmap(stock.data, main ='Missing Map', col = c('yellow', 'black'), legend = F)
any(is.na(fetch.stock.data))
any(is.na(stock.data))

getSymbols(c('AMD','NVDA','INTC','AMZN','XOM','EBAY','MSFT','NFLX','AAPL','DIS','TSM', 'DCTH', 'RNVA'), src = 'google')
as.xts(merge(AMD,NVDA,INTC,AMZN,XOM,EBAY,MSFT,NFLX,AAPL,DIS,TSM, DCTH, RNVA))
df.watch = as.xts(merge(AMD,NVDA,INTC,AMZN,XOM,EBAY,MSFT,NFLX,AAPL,DIS,TSM, DCTH, RNVA))
df.watch = data.frame(df.watch)
missmap(df.watch, main ='Missing Map', col = c('yellow', 'black'), legend = F)
miss.plot = aggr(df.watch, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(df.watch), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


stock.big.data = merge.data.frame(stock.data, df.watch)
stock.big.data
missmap(stock.big.data, main ='Missing Map', col = c('yellow', 'black'), legend = F) #Impute missing data later





######################################################################################################################################################################################################################
model.names = c('AMD','NVDA','INTC','AMZN','XOM','EBAY','MSFT','NFLX','AAPL','DIS','TSM', 'DCTH', 'RNVA')
paste0(model.names, '.Close', collapse = ' + ')
model.close = c(AMD.Close + NVDA.Close + INTC.Close + AMZN.Close + XOM.Close + EBAY.Close + MSFT.Close + NFLX.Close + AAPL.Close  + DIS.Close + TSLA.Close + MU.Close + TSM.Close + DCTH.Close + RNVA.Close)


amd = AMD
amd.bb = BBands( amd[,c("AMD.High","AMD.Low","AMD.Close")] )
amd.macd = MACD(amd[,4])
amd.rsi = RSI(amd[,4])
amd.cci = CCI(amd[,c("AMD.High","AMD.Low","AMD.Close")])
amd.adx = ADX(amd[,c("AMD.High","AMD.Low","AMD.Close")])
amd.stoc = stoch(amd[,c("AMD.High","AMD.Low","AMD.Close")])
amd.cmf = CMF(amd[,c("AMD.High","AMD.Low","AMD.Close")], amd[,5])
amd.vwap = VWAP(amd[,4], amd[,5])
amd.evwma = EVWMA(amd[,4], amd[,5])
amd.will = WPR(amd[,c("AMD.High","AMD.Low","AMD.Close")])
amd.smi = SMI(amd[,c("AMD.High","AMD.Low","AMD.Close")])
amd.zlema = ZLEMA(amd[,4])
amd.percent.change = percent.change(amd[,1],amd[,4])
colnames(amd.percent.change) = 'Percent Change'

amd.data = data.frame(amd.percent.change, amd.bb, amd.macd, amd.rsi, amd.cci, amd.adx, amd.stoc, amd.cmf, amd.vwap, amd.evwma, amd.will, amd.smi, amd.zlema)
amd.data = na.contiguous(amd.data)
amd.data= amd.data[-20]
amd.data= amd.data[-21]

j = 2:length(names(amd.data))
paste0('amd.data[[',j,']]', collapse = ' + ' )


model1 = glm(amd.data[[1]] ~ amd.data[[2]] + amd.data[[3]] + amd.data[[4]] + amd.data[[5]] + amd.data[[6]] + amd.data[[7]] #Performed in long-hand due to downstream errors
             + amd.data[[8]] + amd.data[[9]] + amd.data[[10]] + amd.data[[11]] + amd.data[[12]] + amd.data[[13]] 
             + amd.data[[14]] + amd.data[[15]] + amd.data[[16]] + amd.data[[17]] + amd.data[[18]] + amd.data[[19]] 
             + amd.data[[20]] + amd.data[[21]], data = amd.data)#Set all variables to compare; Many models here, feel free to stop early

model1in = glmulti(model1, level = 1, crit = "aicc")
summary(model1in) 
weightable(model1in)
capture.output(weightable(model1in), file = "weights1.doc")

b1 = glm(amd.data[[1]]~1+amd.data[[6]]+amd.data[[7]]+amd.data[[8]]+amd.data[[9]]+amd.data[[10]]+amd.data[[11]]+amd.data[[15]], data = amd.data)#Choose best model
summary(b1)

###Average Top Models Here
b2 = glm(amd.data[[1]]~1+amd.data[[2]]+amd.data[[4]]+amd.data[[5]]+amd.data[[6]]+amd.data[[7]]+amd.data[[8]]+amd.data[[9]]+amd.data[[10]]+amd.data[[11]]+amd.data[[12]]+amd.data[[14]], data = amd.data)
b3 = glm(amd.data[[1]]~1+amd.data[[3]]+amd.data[[4]]+amd.data[[5]]+amd.data[[6]]+amd.data[[7]]+amd.data[[8]]+amd.data[[9]]+amd.data[[10]]+amd.data[[11]]+amd.data[[12]]+amd.data[[14]], data = amd.data)
b4 = glm(amd.data[[1]]~1+amd.data[[2]]+amd.data[[3]]+amd.data[[4]]+amd.data[[5]]+amd.data[[6]]+amd.data[[7]]+amd.data[[8]]+amd.data[[9]]+amd.data[[10]]+amd.data[[11]]+amd.data[[12]]+amd.data[[14]], data = amd.data)
b5 = glm(amd.data[[1]]~1+amd.data[[2]]+amd.data[[3]]+amd.data[[5]]+amd.data[[6]]+amd.data[[7]]+amd.data[[8]]+amd.data[[9]]+amd.data[[10]]+amd.data[[11]]+amd.data[[12]]+amd.data[[14]], data = amd.data)
ave= model.avg(b1,b2,b3,b4,b5)
summary(ave)
setnames(amd.data, 1:21, c(colnames(amd.data[1:21]))); names(amd.data)
capture.output(summary(ave), file = "best-predictor-variable.doc")
######################################################################################################################################################################################################################

AMD.EMA.20 = EMA(AMD$AMD.Close, n=20)#Exponential Moving Average
AMD.EMA.30 = EMA(AMD$AMD.Close, n=30)
AMD.EMA.80 = EMA(AMD$AMD.Close, n=80)
AMD.EMA.100 = EMA(AMD$AMD.Close, n=100)
AMD.EMA.50 = EMA(AMD$AMD.Close, n = 50)
AMD.chart = chartSeries(AMD, type = 'candlesticks', TA = 'addMACD()', subset = '2017')  
slow.diff = addTA(AMD.EMA.20 - AMD.EMA.100, col = 'red', type = 'h', legend = '20-100 MA'); slow.diff; addTA(AMD.EMA.80, on =1, col = 'red')
med.diff = addTA(AMD.EMA.50 - AMD.EMA.100, col = 'yellow', type = 'h', legend = '50-100 MA'); med.diff; addTA(AMD.EMA.50, on = 1, col = 'yellow')
fast.diff = addTA(AMD.EMA.20 - AMD.EMA.50, col = 'green', type = 'h', legend = '20-50 MA'); fast.diff; addTA(AMD.EMA.30, on = 1, col = 'green')
dev.off()
AMD.chart = chartSeries(AMD, type = 'candlesticks', TA = 'addMACD()', subset = '2017') 
addROC()
addBBands()
addRSI()
dev.off()
AMD.chart = chartSeries(AMD, type = 'candlesticks', TA = 'addMACD()', subset = '2017') 
addCCI()
#addMACD()
addADX()
slow.cross = (AMD.EMA.20 - AMD.EMA.100)
med.cross = (AMD.EMA.50 - AMD.EMA.100)
fast.cross = (AMD.EMA.20 - AMD.EMA.50)

VWAP.Slow = VWAP(price=AMD$AMD.Close, volume=AMD$AMD.Volume, n=100)
VWAP.Fast = VWAP(price=AMD$AMD.Close, volume=AMD$AMD.Volume, n=20)
VWAP.Diff = VWAP.Fast- VWAP.Slow

chartSeries(AMD, subset = '2017', TA="addVo();addTA(VWAP.Slow, on=1, col='red');addTA(VWAP.Fast, on=1, col='blue');addTA(VWAP.Diff, col='blue')") #Needs some tuning

data.amd=AMD[,4]
amd.macd = MACD(data.amd, nFast = 12, nSlow = 26, nSig = 9, maType = SMA, percent = F)
signal = Lag(ifelse(amd.macd$macd < amd.macd$signal, -1, 1))
returns = ROC(data.amd)*signal
returns = returns['2017-01-01/2017-06-22']
AMD.Performance = exp(cumsum(returns))
table.Drawdowns(returns, top=10)
table.DownsideRisk(returns)
charts.PerformanceSummary(returns)

plot(AMD.Performance)

dev.off()





