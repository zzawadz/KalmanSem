set.seed(5)

require(dlm)
n = 100


beta  = cumsum(rnorm(n,sd = 0.2))
alpha = cumsum(beta) + cumsum(rnorm(n,sd = 1))



y = alpha + rnorm(n, sd = 3)


layout(matrix(c(1,2,1,3), nc = 2))
par(mar = c(2,2,3,1))
plot(y, type = "l", xlab = "T", main = "Proces", lwd = 3)
plot(alpha, type = "l", main = "Alpha", lwd = 3)
plot(beta, type = "l", main = "Beta", lwd = 3)

