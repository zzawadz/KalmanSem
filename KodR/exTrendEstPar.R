### Wpierw nalezy uruchomic exTrend.R!!!
#source("KodR/exTrend.R")

model = dlmModPoly(dV = 3^2, dW = c(1,0.2)^2)
filt  = dlmFilter(y,model)


layout(matrix(c(1,2,1,3), nc = 2))
par(mar = c(2,2,3,1))

### Proces:
plot(y, type = "l", xlab = "T", main = "Proces", lwd = 3)
lines(filt$f[-1], col = "brown", lwd = 3)
#legend("topleft",legend = "Kolorem zaznaczono wartosc estymowaną filtrem Kalmana")

### Parametr alpha:
plot(alpha, type = "l", main = "Alpha", lwd = 3)
# Usuwam chwile zerowa:
lines(filt$m[-1,1], col = "red", lwd = 3)

### Parametr beta:
beta_f = filt$m[-1,2]
plot(beta, type = "l", main = "Beta", ylim = extendrange(beta_f), lwd = 3)
lines(beta_f, col = "blue", lwd = 2)
