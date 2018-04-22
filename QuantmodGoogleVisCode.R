#Use GoogleVis to show data Candlestick charts
library(googleVis)
library(quantmod)

getSymbols("PF", src = "yahoo", from = "2018-04-21", to = "2018-04-21")
getSymbols("ERIC", src = "yahoo", from = "2018-04-21", to = "2018-04-21")
getSymbols("TRU", src = "yahoo", from = "2018-04-21", to = "2018-04-21")

getSymbols("SKX", src = "yahoo", from = "2018-04-21", to = "2018-04-21")
getSymbols("GNTX", src = "yahoo", from = "2018-04-21", to = "2018-04-21")
getSymbols("SAGE", src = "yahoo", from = "2018-04-21", to = "2018-04-21")

#Gainers
win.df1 <- data.frame(Date=index(PF),
                      Tickers="PF",
                      Open=PF$PF.Open,
                      Close=PF$PF.Close,
                      Low=PF$PF.Low,
                      High=PF$PF.High)
win.df2 <- data.frame(Date=index(TRU),
                      Tickers="TRU",
                      Open=TRU$TRU.Open,
                      Close=TRU$TRU.Close,
                      Low=TRU$TRU.Low,
                      High=TRU$TRU.High)
win.df3 <- data.frame(Date=index(ERIC),
                      Tickers="ERIC",
                      Open=ERIC$ERIC.Open,
                      Close=ERIC$ERIC.Close,
                      Low=ERIC$ERIC.Low,
                      High=ERIC$ERIC.High)

plot.win1 <- gvisCandlestickChart(win.df1,
                                  xvar = "Tickers",
                                  low="PF.Low", high="PF.High",
                                  open="PF.Open", close="PF.Close",
                                  options = list(title="Largest Gainers for 4/20/18",
                                                 vAxis='{minValue:59, maxValue:62}',
                                                 legend='none',
                                                 height=400,
                                                 width=350))
plot.win2 <- gvisCandlestickChart(win.df2,
                                  xvar = "Tickers",
                                  low="TRU.Low", high="TRU.High",
                                  open="TRU.Open", close="TRU.Close",
                                  options = list(title="Largest Gainers for 4/20/18",
                                                 vAxis='{minValue:64, maxValue:68}',
                                                 legend='none',
                                                 height=400,
                                                 width=350))
plot.win3 <- gvisCandlestickChart(win.df3,
                                  xvar = "Tickers",
                                  low="ERIC.Low", high="ERIC.High",
                                  open="ERIC.Open", close="ERIC.Close",
                                  options = list(title="Largest Gainers for 4/20/18",
                                                 vAxis='{minValue:7, maxValue:8}',
                                                 legend='none',
                                                 height=400,
                                                 width=350))
plots2 <- gvisMerge(plot.win1,plot.win2,horizontal = TRUE)
plot.winning <- gvisMerge(plots2, plot.win3, horizontal = TRUE)
plot(plot.winning)
#Losers
loss.df2 <- data.frame(Date=index(SKX),
                       Tickers="SKX",
                       Open=SKX$SKX.Open,
                       Close=SKX$SKX.Close,
                       Low=SKX$SKX.Low,
                       High=SKX$SKX.High)
loss.df3 <- data.frame(Date=index(GNTX),
                       Tickers="GNTX",
                       Open=GNTX$GNTX.Open,
                       Close=GNTX$GNTX.Close,
                       Low=GNTX$GNTX.Low,
                       High=GNTX$GNTX.High)
loss.df4 <- data.frame(Date=index(SAGE),
                       Tickers="SAGE",
                       Open=SAGE$SAGE.Open,
                       Close=SAGE$SAGE.Close,
                       Low=SAGE$SAGE.Low,
                       High=SAGE$SAGE.High)

plot.loss <- gvisCandlestickChart(loss.df2,
                                  xvar = "Tickers",
                                  low="SKX.Low", high="SKX.High",
                                  open="SKX.Open", close="SKX.Close",
                                  options = list(title="Largest Losers for 4/20/18",
                                                 vAxis='{minValue:29, maxValue:33}',
                                                 legend='none',
                                                 height=400,
                                                 width=350))
plot.loss2 <- gvisCandlestickChart(loss.df3,
                                   xvar = "Tickers",
                                   low="GNTX.Low", high="GNTX.High",
                                   open="GNTX.Open", close="GNTX.Close",
                                   options = list(title="Largest Losers for 4/20/18",
                                                  vAxis='{minValue:20, maxValue:24}',
                                                  legend='none',
                                                  height=400,
                                                  width=350))
plot.loss3 <- gvisCandlestickChart(loss.df4,
                                   xvar = "Tickers",
                                   low="SAGE.Low", high="SAGE.High",
                                   open="SAGE.Open", close="SAGE.Close",
                                   options = list(title="Largest Losers for 4/20/18",
                                                  vAxis='{minValue:147, maxValue:169}',
                                                  legend='none',
                                                  height=400,
                                                  width=350))
plots <- gvisMerge(plot.loss,plot.loss2,horizontal = TRUE)
plot.losing <- gvisMerge(plots, plot.loss3, horizontal = TRUE)
plot(plot.losing)