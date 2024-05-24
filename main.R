
# Author: Jose Alberto Hernandez
# Date: May 2024



# First you load libraries

if (!require(BatchGetSymbols)) install.packages('BatchGetSymbols')


library(BatchGetSymbols)


# This function returns the logorithm of daily returns for a batch symbol (ETF, stock, etc)
getLogReturns <- function (symbol, first.date, last.date) {
  
  #freq.data <- dailyReturn
  
  l.out <- BatchGetSymbols(tickers = symbol, 
                           first.date = first.date,
                           last.date = last.date, 
                           freq.data = 'daily',
                           cache.folder = file.path(tempdir(),'BGS_Cache') ) # cache in tempdir()
  aux = rev(l.out$df.tickers$price.adjusted)
  
  return (log(aux[1:(length(aux)-1)]/aux[2:length(aux)]))
}


# This function returns the daily returns (natural, not logarithmic) for a batch symbol (ETF, stock, etc)
getReturns <- function (symbol, first.date, last.date) {
  
  #freq.data <- dailyReturn
  
  l.out <- BatchGetSymbols(tickers = symbol, 
                           first.date = first.date,
                           last.date = last.date, 
                           freq.data = 'daily',
                           cache.folder = file.path(tempdir(),'BGS_Cache') ) # cache in tempdir()
  aux = rev(l.out$df.tickers$price.adjusted)
  
  return ((aux[1:(length(aux)-1)]/aux[2:length(aux)])-1)
}



# Example: you get the last 5 years of SP500

spy_log = getLogReturns(c('SPY'),Sys.Date()-5*252,Sys.Date())

spy = spy_log
spy_naturals = exp(spy)-1



mean(spy_naturals)
sharpe_daily = mean(spy_naturals)/sd(spy_naturals)
sharpe_1a = sharpe_daily*sqrt(252)

mean(spy_log)

sd(spy_log)


# Para obtener los logReturns, hay que hacer log(PrecioDiaN/PrecioDiaNanterior), y luego ya los puedes sumar y demás
# Para obtener el return en naturales: ReturnNaturales = exp(ReturnLog)-1 y eso te da la rentabilidad

# Rentabilidad Diaria logaritmica
# Rentabilidad Logaritmica 1año = suma(Rentabilidad logaritma de 252 días)
# Rentabilidad natural 1año = cojo el resultataod anterior y exp(RentLogAño)-1




# in 1 year I should have with the gaussian approx:
exp(sum(rnorm(252,mean=mean(spy_log),sd=sd(spy_log))))-1

hist(exp(rnorm(1e3,mean=252*mean(spy_log),sd = sqrt(252)*sd(spy_log)))-1)


mean_1a = mean(exp(rnorm(1e3,mean=252*mean(spy_log),sd = sqrt(252)*sd(spy_log)))-1)
sd_1a = sd(exp(rnorm(1e3,mean=252*mean(spy_log),sd = sqrt(252)*sd(spy_log)))-1)

sharpe_1a = mean_1a/sd_1a # sharpe teorico gausiano

cat("\nrent_1a = ",mean_1a,"\n")
cat("sd_1a = ",sd_1a,"\n")
cat("sharpe_1a = ",sharpe_1a,"\n")


cat("-----\n")



log(1+r1)+log(1+r2)+log(1+r3)








# set dates: 10a = 22*12*10 = 2640
first.date <- Sys.Date() - 252*5 # 10 years
last.date <- Sys.Date()
freq.data <- 'daily'
# set tickers


tickers <- c('FTEC')
l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()
ftec = l.out$df.tickers$price.adjusted

ftec_returns = ftec[2:length(ftec)]/ftec[1:(length(ftec)-1)] -1
#print(ftec_returns)

# 10a = 22*12*10

last_5d = tail(ftec_returns,5); hist(last_5d)
cat("Mean = ", mean(last_5d), " Sharpe = ", mean(last_5d)/sd(last_5d), "\n")

last_1m = tail(ftec_returns,22); hist(last_1m)
cat("Mean = ", mean(last_1m), " Sharpe = ", sd(last_1m)/mean(last_1m), "\n")


last_3m = tail(ftec_returns,22*3); hist(last_1m)
cat("Mean = ", mean(last_3m), " Sharpe = ", sd(last_3m)/mean(last_3m), "\n")

last_6m = tail(ftec_returns,22*6); hist(last_6m)
cat("Mean = ", mean(last_6m), " Sharpe = ", sd(last_6m)/mean(last_6m), "\n")

last_12m = tail(ftec_returns,22*12); hist(last_12m)
cat("Mean = ", mean(last_12m), " Sharpe = ", sd(last_12m)/mean(last_12m), "\n")





spy_log = getLogReturns(c('SPY'),"2023-01-01","2023-12-31")

spy = getReturns(c('SPY'),"2023-01-01","2023-12-31")


x.test <- shapiro.test(spy_log)
print(x.test)

plotn <- function(x,main="Histograma de frecuencias \ny distribución normal",
                  xlab="X",ylab="Densidad") {
  min <- min(x)
  max <- max(x)
  media <- mean(x)
  dt <- sd(x)
  hist(x,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media,dt), min, max,add = T,col="blue")
}

plotn(spy_log,main="Distribución normal")#Grafico de x






