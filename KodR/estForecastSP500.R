#source("KodR/estModelSP500.R")

h = nrow(sp500_2014["2014-04/"])
ttime = index(sp500_2014["2014-04/"])

sp2march = sp500_2014["/2014-03"]
filter = dlmFilter(sp2march, model_mle)
forecast = dlmForecast(filter, nAhead = h)


fsp = as.numeric(forecast$f)
#time = index(tail(sp2march,1))
ttime = as.POSIXct(ttime)


plot(sp500_2014, ylim = c(1720, 1950))
lines(sp2march, col = "red", lwd = 3)
lines(ttime, fsp, lwd = 3, col = "blue")

Q = sapply(forecast$Q, function(x) sqrt(x[1,1]))
lb <- forecast$f + qnorm(0.05, sd = Q)
ub <- forecast$f + qnorm(0.95, sd = Q)

lines(ttime, lb, col = "green", lwd = 3)
lines(ttime, ub, col = "green", lwd = 3)

