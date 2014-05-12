require(quantmod)
require(dlm)

# pobieranie danych
sp500 = getSymbols("^GSPC",auto.assign = FALSE)
sp500 = sp500[,4]
sp500 = sp500["2013/2014"]

#plot(sp500)

### Wyciecie okresu na ktorym bedzie estymowany model:
est_data = as.numeric(sp500["2013"])

# Funckja przeslana do dlmMLE - taki model bedzie estmowany
modFunc = function(x)
{
  ## Zamiast nakladac restrykcje na parametry (wariancja musi byc dodatnia!!!)
  ## wystarczy parametry podsnosci do potegi:
  dlmModPoly(dV = exp(x[1]) , dW = exp(x[2:3]))
}

### Estymacja modelu:
fit_mle = dlmMLE(est_data, rep(exp(-2),3),modFunc)

### Na podstawie funkcji ktora budowala model wewnatrz dlmMLE
### budowany jest obiekt dlm z odpowiednimi parametrami:
model_mle = modFunc(fit_mle$par)

### Najpierw przeprowadzana jest filtracja na danych na ktorych
### proces byl estymowany - koncowe wartosci zastana uzyte w nowym okresie
filter_est = dlmFilter(est_data, model_mle)

# pobranie ostatniego oszacowania zmiennych ukrytych:
m0 = as.numeric(tail(filter_est$m,1))
model_mle$m0 = m0
# pobranie macierzy kowariancji:
model_mle$C0 = dlmSvd2var( tail(filter_est$U.R,1)[[1]],  tail(filter_est$D.R,1))
 
# Filtracja dla danych z 2014:
sp500_2014 = sp500["2014"]
filter = dlmFilter(sp500_2014, model_mle)

time = index(sp500_2014)


layout(matrix(c(1,2,1,3), nc = 2))
par(mar = c(2,2,3,1))
plot(time,sp500_2014, type = "l", lwd = 2, main = "SP500")
lines(time, filter$f, lwd = 3, col = "brown")

plot(time, filter$m[-1,1], type = "l", lwd = 2, col = "red", main = "Alpha")
plot(time, filter$m[-1,2], type = "l", lwd = 2, col = "blue", main = "Beta")
