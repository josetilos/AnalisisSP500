# Supporting functions



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



