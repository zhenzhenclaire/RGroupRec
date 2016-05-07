library(kernlab)
data(spirals)
sc <- specc(spirals, centers = 2)
plot(spirals, pch = (23 - 2 * sc))
