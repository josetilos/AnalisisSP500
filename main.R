
# Author: Jose Alberto Hernandez
# Date: May 2024



# First you load libraries

if (!require(BatchGetSymbols)) install.packages('BatchGetSymbols')


library(BatchGetSymbols)


source("./supporting_functions.R")



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


### Historic rates


#SPY 
#setwd("~/Google Drive/Research/Rexperiments")
spyh.df = read.csv("HistoricDataSPY.csv")


spyh.df$Date = as.Date(spyh.df$Date)



index1957 = which(spyh.df$Date=="1957-01-01")


index1970 = which(spyh.df$Date=="1970-01-01"); print(index1970)
indextoday = dim(spyh.df)[1]

nrows = dim(spyh.df)[1]

rent1m = NA*c(1:2000); rent1y = NA*c(1:2000); rent10y = NA*c(1:2000)
for (ii in seq(index1970,indextoday)) {
  rent1m[ii-index1970+1] = spyh.df[ii,"SP500"]/spyh.df[ii-1,"SP500"]-1   
}
for (ii in seq(index1970,indextoday,by=12)) {
  rent1y[ii-index1970+1] = spyh.df[ii+12,"SP500"]/spyh.df[ii,"SP500"]-1   
}

for (ii in seq(index1970,indextoday,by=12*10)) {
  rent10y[ii-index1970+1] = spyh.df[ii+12*10,"SP500"]/spyh.df[ii,"SP500"]-1   
}





rent1m = na.omit(rent1m); rent1y = na.omit(rent1y)
summary(rent1m)
summary(rent1y)                                                     


## Antiguos

list.of.packages <- c("ggplot2", "dplyr","plotly","ggExtra","zoo","readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(ggplot2)
library(dplyr)
library(plotly)
library(ggExtra)
library(zoo)
library (readr)

# Lectura de datos y carga en memoria:

#urlfile = "https://github.com/josetilos/AnalisisSP500/blob/main/HistoricalData_SP500.csv"
#spyh.df = read_csv(url(urlfile))

spyh.df = read.csv("HistoricDataSPY.csv") # Historico desde 1871, 1 dia por mes
spyh.df$Date = as.Date(spyh.df$Date)

sincebeginning = 1
since1946 = which(spyh.df$Date=="1946-01-01") # Final II Guerra Mundial
since1962 = which(spyh.df$Date=="1962-01-01") # Construccion muro Berlin
since1990 = which(spyh.df$Date=="1970-01-01") # Caida muro Berlin
indextoday = dim(spyh.df)[1]

spyhWW2.df = spyh.df[since1946:indextoday,]
spyhGFini.df = spyh.df[since1962:indextoday,]
spyhGFfin.df = spyh.df[since1990:indextoday,]




nrows = dim(spyh.df)[1]

#spy.df = read.csv("SPY.csv") # Diario, desde 1993
#gld.df = read.csv("GLD.csv") # Oro
#nasdaq.df = read.csv("IXIC.csv") # Nasdaq
#salud.df = read.csv("FHLC.csv") # Fidelity Healthcare
#ftec.df = read.csv("FTEC.csv") # Fidelity Technology
#bonos.df = read.csv("TLT.csv") # Bonos Tesoro Long Term US
#mundo.df = read.csv("URTH.csv") # Mundo

mynamestheme <- theme(
  plot.title = element_text(family = "Helvetica", face = "bold", size = (15)),
  legend.title = element_text(colour = "steelblue", face = "bold.italic", family = "Helvetica"),
  legend.text = element_text(face = "italic", colour = "steelblue4", family = "Helvetica"),
  axis.title = element_text(family = "Helvetica", size = (20), colour = "steelblue4"),
  axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (20))
)


# SP500 historico




# Dummy data
data <- data.frame(
  date = as.Date(spyh.df$Date),
  value = spyh.df$SP500
)

# Most basic bubble plot
p <- ggplot(data, aes(x=date, y=value)) +
  geom_line() +
  xlab("Fecha") + ylab("SPY") + mynamestheme

p

#scale_x_continuous(name="Ano", limits=c(1871, 2020)) +
#scale_y_continuous(name="SPY", limits=c(0, 1000))


# Rentabilidades mensuales. Todos los meses:

#NmonthSPYh = length(spyh.df$SP500)
#rent1month = 0*c(1:NmonthSPYh)
#rent1month = NA*c(since1950:indextoday)
#for (ii in c(1:(length(rent1month)-1))) {
#  rent1month[ii] = spyh.df[ii+1,"SP500"]/spyh.df[ii,"SP500"]-1
#}


rent1m = NA*c(1:20000); rent1y = NA*c(1:20000); rent10y = NA*c(1:20000)
for (ii in seq(sincebeginning+1,indextoday)) {
  rent1m[ii-sincebeginning+1] = spyh.df[ii,"SP500"]/spyh.df[ii-1,"SP500"]-1   
}


rent1m = na.omit(rent1m)

length(spyhGFfin.df$SP500)

rent1mWW2 = rent1m[((indextoday-length(spyhWW2.df$SP500)+1):(indextoday-1))]
rent1mGFfin = rent1m[((indextoday-length(spyhGFfin.df$SP500)+1):(indextoday-1))]

data <- data.frame(rent1m = c(rent1m,         # Create example data
                              rent1mWW2,
                              rent1mGFfin),
                   periodos = c(rep("Desde 1871", length(rent1m)),
                                rep("Desde 1946", length(rent1mWW2)),
                                rep("Desde 1990", length(rent1mGFfin))))



ggplot(data, aes(x = rent1m, fill = periodos)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50) +
  annotate(geom="text", x=0.25, y=200, label=paste("Rent 1m = ",round(mean(rent1m),5)),color="red") +
  annotate(geom="text", x=0.25, y=190, label=paste("Sharpe 1m = ",round(mean(rent1m)/sd(rent1m),5)),color="red") +
  annotate(geom="text", x=0.25, y=150, label=paste("Rent 1m = ",round(mean(rent1mWW2),5)),color="green") +
  annotate(geom="text", x=0.25, y=140, label=paste("Sharpe 1m = ",round(mean(rent1mWW2)/sd(rent1mWW2),5)),color="green") +
  annotate(geom="text", x=0.25, y=100, label=paste("Rent 1m = ",round(mean(rent1mGFfin),5)),color="blue") +
  annotate(geom="text", x=0.25, y=90, label=paste("Sharpe 1m = ",round(mean(rent1mGFfin)/sd(rent1mGFfin),5)),color="blue") 


sum(rent1mGFfin<0)/length(rent1mGFfin)
summary(rent1mGFfin[which(rent1mGFfin<0)]*100)

sum(rent1mGFfin>0)/length(rent1mGFfin)
summary(rent1mGFfin[which(rent1mGFfin>0)]*100)

phist <-ggplot(data = data.frame(rent1m = rent1m), aes(x = rent1m)) +
  geom_histogram(bins=100) + xlab("Rent. 1m") + ylab("Frecuencia") + mynamestheme +
  annotate(geom="text", x=0.25, y=100, label=paste("Rent 1m media = ",round(mean(rent1m),5)),color="blue") +
  annotate(geom="text", x=0.25, y=95, label=paste("Rent 1m sd = ",round(sd(rent1m),5)),color="blue") +
  annotate(geom="text", x=0.25, y=90, label=paste("Sharpe 1m = ",round(mean(rent1m)/sd(rent1m),5)),color="blue") +
  annotate(geom="text", x=0.25, y=85, label=paste("Periodos neg 1m = ",round(sum(rent1m<0)/length(rent1m),5)),color="blue")
phist


# Funcion de autocorrelacion por meses
acf_result <- acf(rent1m)
plot(acf_result)


# Rentabilidades anuales: escogiendo 12 meses seguidos al azar

onemonth = 1; oneyear = 12*onemonth; threeyear = 3*oneyear; 
fiveyear = 5*oneyear; tenyear = 10*oneyear; twentyyear = 20*oneyear


tradingdays = oneyear

dd = sample(c(1:(length(spyh.df$SP500)-tradingdays)), size = 1e5, replace=T)

spyvector = spyh.df[,"SP500"]

rent1year = spyvector[dd+tradingdays]/spyvector[dd]-1


ggplot(data = data.frame(rent1y = rent1year), aes(x = rent1y)) +
  geom_histogram(bins=100) + xlab("Rent. 1a") + ylab("Frecuencia") + mynamestheme +
  annotate(geom="text", x=0.75, y=3000, label=paste("Rent 1a media = ",round(mean(rent1year),5)),color="blue") +
  annotate(geom="text", x=0.75, y=2750, label=paste("Rent 1a sd = ",round(sd(rent1year),5)),color="blue") +
  annotate(geom="text", x=0.75, y=2500, label=paste("Sharpe 1a = ",round(mean(rent1year)/sd(rent1year),5)),color="blue") +
  annotate(geom="text", x=0.75, y=2250, label=paste("Periodos neg 1a = ",round(sum(rent1year<0)/length(rent1year),5)),color="blue")



tradingdays = threeyear

dd = sample(c(1:(length(spyh.df$SP500)-tradingdays)), size = 1e5, replace=T)

spyvector = spyh.df[,"SP500"]

rent3year = spyvector[dd+tradingdays]/spyvector[dd]-1


ggplot(data = data.frame(rent3y = rent3year), aes(x = rent3y)) +
  geom_histogram(bins=100) + xlab("Rent. 3a") + ylab("Frecuencia") + mynamestheme +
  annotate(geom="text", x=1, y=3000, label=paste("Rent 3a media = ",round(mean(rent3year),5)),color="blue") +
  annotate(geom="text", x=1, y=2750, label=paste("Rent 3a sd = ",round(sd(rent3year),5)),color="blue") +
  annotate(geom="text", x=1, y=2500, label=paste("Sharpe 3a = ",round(mean(rent3year)/sd(rent3year),5)),color="blue") +
  annotate(geom="text", x=1, y=2250, label=paste("Periodos neg 3a = ",round(sum(rent3year<0)/length(rent3year),5)),color="blue")



tradingdays = tenyear

dd = sample(c(1:(length(spyh.df$SP500)-tradingdays)), size = 1e5, replace=T)

spyvector = spyh.df[,"SP500"]

rent10year = spyvector[dd+tradingdays]/spyvector[dd]-1


ggplot(data = data.frame(rent10y = rent10year), aes(x = rent10y)) +
  geom_histogram(bins=100) + xlab("Rent. 10a") + ylab("Frecuencia") + mynamestheme +
  annotate(geom="text", x=2, y=3000, label=paste("Rent 10a media = ",round(mean(rent10year),5)),color="blue") +
  annotate(geom="text", x=2, y=2750, label=paste("Rent 10a sd = ",round(sd(rent10year),5)),color="blue") +
  annotate(geom="text", x=2, y=2500, label=paste("Sharpe 10a = ",round(mean(rent10year)/sd(rent10year),5)),color="blue") +
  annotate(geom="text", x=2, y=2250, label=paste("Periodos neg 10a = ",round(sum(rent10year<0)/length(rent10year),5)),color="blue")

# Ahora con aportaciones mensuales durante un año


tradingdays = 12*10

dd = sample(c(1:(length(spyh.df$SP500)-tradingdays)), size = 1e5, replace=T)

spyvector = spyh.df[,"SP500"]

rent1ycont = 0*c(1:length(dd))
for (ii in c(1:length(dd))) {
  aaux = 0*c(1:tradingdays)
  for (jj in c(1:length(aaux))) {
    aaux[jj] = spyvector[dd[ii]+tradingdays]/spyvector[dd[ii]+jj-1]-1
  }
  rent1ycont[ii] = mean(aaux)
}


ggplot(data = data.frame(rent1yc = rent1ycont), aes(x = rent1yc)) +
  geom_histogram(bins=100) + xlab("Rent. 1a cont") + ylab("Frecuencia") + mynamestheme +
  annotate(geom="text", x=1.5, y=2000, label=paste("Rent. 10a media = ",round(mean(rent1ycont),5)),color="blue") +
  annotate(geom="text", x=1.5, y=1900, label=paste("Rent. 10a sd = ",round(sd(rent1ycont),5)),color="blue") +
  annotate(geom="text", x=1.5, y=1800, label=paste("Sharpe 10a = ",round(mean(rent1ycont)/sd(rent1ycont),5)),color="blue") +
  annotate(geom="text", x=1.5, y=1700, label=paste("Prob perder 10a = ",round(sum(rent1ycont<0)/length(rent1ycont),5)),color="blue")



N = 12*fiveyear; rr = 0*c(1:(dim(spyh.df)[1]-N)); dated = rr
for (ii in c(1:(dim(spyh.df)[1]-N))) {
  rr[ii] = spyh.df[ii+N,"SP500"]/spyh.df[ii,"SP500"]-1
  dated[ii] = as.Date(spyh.df[ii,"Date"])
}

print(length(rr))

ggplot(data=data.frame(rent = rr[500:length(rr)], fecha = dated[500:length(rr)]), aes(x=fecha,y=rent)) +
  geom_bar(stat="identity") 




# otras cosas
plot(rollmean(spyhGFfin.df$SP500, k = 21*2, fill = NA))





# Histogram with density plot

#ggplot(dfaux = data.frame(rent = rent1m, fecha = dated), aes(x=rr)) + 
#  geom_histogram(aes(y=..density..), colour="black", fill="white")+
#  geom_density(alpha=10.2, fill="#FF6666") 
#spyh.df





N = 12*1

rr = 0*c(1:(dim(spyhWW2.df)[1]-N)); dated = 0*c(1:(dim(spyhWW2.df)[1]-N));


data <- data.frame(
  day = as.Date(spyhWW2.df$Date),
  value = spyhWW2.df$SP500)

onemonth = 1
for (ii in seq(1,length(data$value-onemonth),by=onemonth)) {
  #for (ii in c(1:(dim(data)[1]-oneyear))) {
  data$rr1m[ii] = data$value[ii+onemonth]/data$value[ii]-1
}

ggplot(data, aes(x=day,y=rr1m)) +
  geom_bar(stat="identity") + ylab("Rent 1 month") + xlab("Fecha")


oneyear = 12*1
for (ii in seq(1,length(data$value-oneyear),by=onemonth)) {
  #for (ii in c(1:(dim(data)[1]-oneyear))) {
  data$rr1y[ii] = data$value[ii+oneyear]/data$value[ii]-1
}


p2 <- ggplot(data, aes(x=day,y=rr1y)) +
  geom_bar(stat="identity") + ylab("Rent 1a") + xlab("Fecha")
p2


p3 <- ggplot(data, aes(x=day,y=rr1y)) +
  geom_point() + ylab("Rent 1 año") + xlab("Fecha")

p4 <- ggMarginal(p3, type="density") # histogram, density, boxplot
p4


tenyear = 12*10
for (ii in seq(1,length(data$value-tenyear),by=onemonth)) {
  #for (ii in c(1:(dim(data)[1]-oneyear))) {
  data$rr10y[ii] = data$value[ii+tenyear]/data$value[ii]-1
}


ggplot(data, aes(x=day,y=rr10y)) +
  geom_bar(stat="identity") + ylab("Rent 10a") + xlab("Fecha")






#for (ii in c(1:(dim(spyh1970.df)[1]-N))) {
#  rr[ii] = spyh1970.df[ii+N,"SP500"]/spyh1970.df[ii,"SP500"]-1
#  dated[ii] = spyh1970.df[ii,"Date"]
#}

#dfaux = data.frame(rent = rr, fecha = dated)
#ggplot(data=dfaux, aes(x=fecha,y=rent)) +
#  geom_bar(stat="identity") 


#NyearSPYh = length(spyh.df$SP500)-oneyear 
#rent1yseq = 0*c(1:NyearSPYh); date1yseq = 0*c(1:NyearSPYh)
#for (ii in c(1:(length(rent1year)-1))) {
#  rent1yseq[ii] = spyh.df[ii+12,"SP500"]/spyh.df[ii,"SP500"]-1
#  date1yseq[ii] = as.Date(spyh.df[ii+12,"Date"])
#}

#p <- ggplot(data = data.frame(rent1y = rent1yseq, date = date1yseq), aes(y = rent1y, x = date)) +
#  geom_point() + xlab("Fecha") + ylab("Rent. 1m") + mynamestheme

# with marginal histogram
#p1 <- ggMarginal(p, type="density") # histogram, density, boxplot
#p1


#rr = spyh.df[dd+tradingdays,"SP500"]/spy.df[dd,"SP500"]-1

#head(rr)



#probganar = sum(rr>0)/length(rr); 
#cat("prob ganar a",tradingdays,"meses =",probganar)

#monthly_rate = exp((log(1+mean(rr)))/tradingdays)-1; 
#cat("beneficio medio mensual = ",monthly_rate*100, "%\n")






# desde 1970
data1970 <- data.frame(
  day = as.Date(spyh.df[1200:dim(spyh.df)[1],"Date"]),
  value = spyh.df[1200:dim(spyh.df)[1],"SP500"]
)

# Most basic bubble plot
p <- ggplot(data1970, aes(x=day, y=value)) +
  geom_line() +
  xlab("Año") + ylab("SPY")
p

head(spy.df)
tail(spy.df)


onemonth = 1; oneyear = 12*onemonth
threeyear = 3*oneyear; fiveyear = 5*oneyear; tenyear = 10*oneyear


tradingdays = tenyear


Ndays = dim(spyh.df)[1]

nsim = 1e5
dd = sample(c(1:(Ndays-tradingdays)),size = nsim, replace=T)

spyvector = spyh.df[,"SP500"]

rr = spyvector[dd+tradingdays,2]/spyvector[dd,2]-1
hist(rr)
print(mean(rr))
print(sd(rr))


probganar = sum(rr>0)/nsim; 
cat("prob ganar a",tradingdays,"meses =",probganar)


# Elegir el momento de entrada y el momento de salida

# El momento de entrada como un punto que este un 25% mas barato que la media de los ultimos 3 anos
# El momenot de salida un punto que este un 25% mas caro que la media de los ultimos 3 anos

# Cuanto tiempo habria que esperar hasta que se de la condicion?



rent1m = NA*c(1:2000); rent1y = NA*c(1:2000); rent10y = NA*c(1:2000)
for (ii in seq(index1970,indextoday)) {
  rent1m[ii-index1970+1] = spyh.df[ii,"SP500"]/spyh.df[ii-1,"SP500"]-1   
}
for (ii in seq(index1970,indextoday,by=12)) {
  rent1y[ii-index1970+1] = spyh.df[ii+12,"SP500"]/spyh.df[ii,"SP500"]-1   
}

for (ii in seq(index1970,indextoday,by=12*10)) {
  rent10y[ii-index1970+1] = spyh.df[ii+12*10,"SP500"]/spyh.df[ii,"SP500"]-1   
}


rent1m = na.omit(rent1m); rent1y = na.omit(rent1y); rent10y = na.omit(rent10y)
summary(rent1m)
summary(rent1y)                                   
summary(rent10y)          







# (1+0.085)^10-1.   8.5% si lo mantienes 10 anyos lo pondera bien
# (1+0.085)^5-1.   8.5% si lo mantienes 5 anyos lo pondera bien
# (1+0.085)^3-1.   8.5% si lo mantienes 3 anyos lo pondera bien


monthly_rate = exp((log(1+mean(rr)))/tradingdays)-1; print(monthly_rate)
cat("beneficio medio mensual = ",monthly_rate*100, "%\n")



# SPY
# Nasdaq = IXIC
# FTEC, FHLC
# Bonos TLT,
# Oro GLD
# Mundo URTH




spyyahoo.df = read.csv("SPY.csv") # primer dia del mes
spyvector = spyyahoo.df$Adj.Close

nasdaqyahoo.df = read.csv("IXIC.csv") # primer dia del mes
nasdaqvector = nasdaqyahoo.df$Adj.Close


healthyahoo.df = read.csv("FHLC.csv") # primer dia del mes
healthvector = healthyahoo.df$Adj.Close

ftecyahoo.df = read.csv("FTEC.csv") # primer dia del mes
ftecvector = ftecyahoo.df$Adj.Close

goldyahoo.df = read.csv("GLD.csv") # primer dia del mes
gldvector = goldyahoo.df$Adj.Close

worldyahoo.df = read.csv("URTH.csv") # primer dia del mes
worldvector = worldyahoo.df$Adj.Close

bonos20yyahoo.df = read.csv("TLT.csv") # primer dia del mes
bonos20yvector = bonos20yyahoo.df$Adj.Close



oneyeard = 252; onemonthd = round(oneyeard/12);
threeyeard = 3*oneyeard; fiveyeard = 5*oneyeard






trainingdaysdd = fiveyeard

cat("\n")
nsim = 1e5

# World MSCI
dd = sample(c(1:(length(worldvector)-trainingdaysdd)),size = nsim, replace=T)
rr = worldvector[dd+trainingdaysdd]/worldvector[dd]-1
hist(rr)

cat("World\n")
probganar = sum(rr>0)/nsim; 
cat("prob ganar a",trainingdaysdd/252*12,"meses =",probganar,"\n")
monthly_rate = exp((log(1+mean(rr)))/tradingdays)-1; 
cat("beneficio medio mensual = ",monthly_rate*100, "%\n")

# SPY
dd = sample(c(1:(length(spyvector)-trainingdaysdd)),size = nsim, replace=T)
rr = spyvector[dd+trainingdaysdd]/spyvector[dd]-1
hist(rr)

cat("SPY\n")
probganar = sum(rr>0)/nsim; 
cat("prob ganar a",trainingdaysdd/252*12,"meses =",probganar,"\n")
monthly_rate = exp((log(1+mean(rr)))/tradingdays)-1; 
cat("beneficio medio mensual = ",monthly_rate*100, "%\n")

# Nasdaq
dd = sample(c(1:(length(nasdaqvector)-trainingdaysdd)),size = nsim, replace=T)
rr = nasdaqvector[dd+trainingdaysdd]/nasdaqvector[dd]-1
hist(rr)

cat("Nasdaq\n")
probganar = sum(rr>0)/nsim; 
cat("prob ganar a",trainingdaysdd/252*12,"meses =",probganar,"\n")
monthly_rate = exp((log(1+mean(rr)))/tradingdays)-1; 
cat("beneficio medio mensual = ",monthly_rate*100, "%\n")

# FTEC
dd = sample(c(1:(length(ftecvector)-trainingdaysdd)),size = nsim, replace=T)
rr = ftecvector[dd+trainingdaysdd]/ftecvector[dd]-1
hist(rr)

cat("FTEC\n")
probganar = sum(rr>0)/nsim; 
cat("prob ganar a",trainingdaysdd/252*12,"meses =",probganar,"\n")
monthly_rate = exp((log(1+mean(rr)))/tradingdays)-1; 
cat("beneficio medio mensual = ",monthly_rate*100, "%\n")

# FHLC Health
dd = sample(c(1:(length(healthvector)-trainingdaysdd)),size = nsim, replace=T)
rr = healthvector[dd+trainingdaysdd]/healthvector[dd]-1
hist(rr)

cat("Health\n")
probganar = sum(rr>0)/nsim; 
cat("prob ganar a",trainingdaysdd/252*12,"meses =",probganar,"\n")
monthly_rate = exp((log(1+mean(rr)))/tradingdays)-1; 
cat("beneficio medio mensual = ",monthly_rate*100, "%\n")

# GOLD
dd = sample(c(1:(length(gldvector)-trainingdaysdd)),size = nsim, replace=T)
rr = gldvector[dd+trainingdaysdd]/gldvector[dd]-1
hist(rr)

cat("Gold\n")
probganar = sum(rr>0)/nsim; 
cat("prob ganar a",trainingdaysdd/252*12,"meses =",probganar,"\n")
monthly_rate = exp((log(1+mean(rr)))/tradingdays)-1; 
cat("beneficio medio mensual = ",monthly_rate*100, "%\n")


# Bonos 20Y
dd = sample(c(1:(length(bonos20yvector)-trainingdaysdd)),size = nsim, replace=T)
rr = bonos20yvector[dd+trainingdaysdd]/bonos20yvector[dd]-1
hist(rr)

cat("Bonos 20Y\n")
probganar = sum(rr>0)/nsim; 
cat("prob ganar a",trainingdaysdd/252*12,"meses =",probganar,"\n")
monthly_rate = exp((log(1+mean(rr)))/tradingdays)-1; 
cat("beneficio medio mensual = ",monthly_rate*100, "%\n")

# Data<-read.csv("https://raw.githubusercontent.com/curran/data/gh-pages/dataSoup/datasets.csv")


# OTRO Simbolos Fidelity

# Funcion optimize

require(graphics)

f <- function (x, a) (x - a)^2
xmin <- optimize(f, c(0, 1), tol = 0.0001, a = 1/3)
xmin

## See where the function is evaluated:
optimize(function(x) x^2*(print(x)-1), lower = 0, upper = 10)

## "wrong" solution with unlucky interval and piecewise constant f():
f  <- function(x) ifelse(x > -1, ifelse(x < 4, exp(-1/abs(x - 1)), 10), 10)
fp <- function(x) { print(x); f(x) }

plot(f, -2,5, ylim = 0:1, col = 2)
optimize(fp, c(-4, 20))   # doesn't see the minimum
optimize(fp, c(-7, 20))   # ok



if (!require(BatchGetSymbols)) install.packages('BatchGetSymbols')


library(BatchGetSymbols)


# set dates
first.date <- Sys.Date() - 60
last.date <- Sys.Date()
freq.data <- 'daily'
# set tickers
tickers <- c('FB','MMM','PETR4.SA','abcdef')

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(),
                                                  'BGS_Cache') ) # cache in tempdir()



# set dates
first.date <- Sys.Date() - 365*5
last.date <- Sys.Date()
freq.data <- 'daily'
# set tickers
# FHLC Fidelity MSCI Health Care index ETF -
# FENY Fidelity MSCI Energy Innovation - -29% a 5 años
# FTEC Fidelity MSCI Information Technologies - 251% a 5 años
# FDIS Fidelity Consumer Discrecionary - 164% a 5 años
# FREL - 37% a 5 años
# QQQ - Invesco QQQ Trust Series
# QTUM - Defiance Quantum
# DGRO ishares Core Dividend Growth
# BRK.B Berkshire
# DRDR - iShares Healthcare innovation
# PICK - iShares MSCI Global Metals and Mining
# URTH - iShares MSCI World ETF
# PBD - Invesco Global Clean Energy
# ITA - iShares US Aerospace and Defense

tickers <- c('FHLC')
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(),
                                                  'BGS_Cache') ) # cache in tempdir()
fhlc = l.out$df.tickers$price.adjusted



tickers <- c('FTEC')
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(),
                                                  'BGS_Cache') ) # cache in tempdir()
ftec = l.out$df.tickers$price.adjusted


tickers <- c('URTH')
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(),
                                                  'BGS_Cache') ) # cache in tempdir()
urth = l.out$df.tickers$price.adjusted


fhlc = fhlc/fhlc[1]
ftec = ftec/ftec[1]
urth = urth/urth[1]


len <- 24
x = runif(len)
y = x^3 + runif(len, min = -0.1, max = 0.1)
plot(x, y)
s <- seq(from = 0, to = 1, length = 50)
lines(s, s^3, lty = 2)

df <- data.frame(x, y)
m <- nls(y ~ I(x^power), data = df, start = list(power = 1), trace = T)

tt = sample(size=1e2,c(1:length(fhlc)),replace=F)


# fhlc


plot(tt,fhlc[tt])

fhlc.df = data.frame(x=tt,y=fhlc[tt])
fhlc.df <- fhlc.df[order(fhlc.df$x), ] # order it by increasing x


#fhlc.lm = lm(fhlc ~ poly(c(1:length(fhlc)),1))

fm0 <- lm(log(y) ~ I(-x), fhlc.df) # simpler model to get better starting values

summary(fm0)

st <- list(b = coef(fm0)[[2]])
fm <- nls(y ~ cbind(exp(b*x), 1), fhlc.df, start = st, alg = "plinear")

plot(fhlc.df, col = "blue", pch = 20)
#lines(fhlc.df$x, predict(fm0,fhlc.df$x))
lines(exp(fitted(fm0)) ~ x, fhlc.df)

print(summary(fm0))



plot(fhlc.df, col = "red", pch = 20)
lines(fitted(fm) ~ x, fhlc.df)

print(summary(fm))


#fhlc.nls = nls(y ~ a*exp(b*x), data = fhlc.df, start = list(a = 1, b = 0.05), trace = T)


#plot(tt,predict(fm0,tt))

#summary(fhlc.lm)
#print(fhlc.lm$coefficients[2])

# ftec
plot(ftec)
ftec.lm = lm(ftec ~ poly(c(1:length(ftec)),1))
summary(ftec.lm)
print(ftec.lm$coefficients[2])

# urth
plot(urth)
urth.lm = lm(urth ~ poly(c(1:length(urth)),1))
summary(urth.lm)
print(urth.lm$coefficients[2])


print(l.out$df.control)


library(ggplot2)

p <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close))
p <- p + geom_line()
p <- p + facet_wrap(~ticker, scales = 'free_y')
print(p)

library(BatchGetSymbols)

first.date <- Sys.Date()-365
last.date <- Sys.Date()

df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$Tickers

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date)

print(l.out$df.control)
print(l.out$df.tickers)



library(BatchGetSymbols)

first.date <- Sys.Date()-365
last.date <- Sys.Date()

df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$Tickers

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date)

print(l.out$df.control)
print(l.out$df.tickers)


















