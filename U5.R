# Aufgabe 3 a)
# change lot(l) to take number of simulations as well - takes too long otherwise
# Jan approved!
lot <- function(lot, n) {
  alpha = runif(n, min=(-pi/2), max=(pi/2))
  xs = lot * tan(alpha)
  return (xs)
}

# b)
# simulate 10^6 times with a lot of length 0.5
results=lot(0.5, 10^6)
# cut off all results with absolute value >10
# dividing by 0.5 leads to prettier histogram...
results = results[abs(results)<=10] / 0.5
# plot histogram
hist(results,
     breaks=200,
     main="Aufgabe 2",
     xlab="values of x",
     ylab="relative frequency",
     xlim=c(-10,10),
     freq=FALSE)

# d)
M = sum(dcauchy(-10:10))
lines(-10:10, (1/M)*dcauchy(-10:10))


# Aufgabe 4 a)
box_muller <- function(n) {
  # pick a U and a V, uniform, n-times each
  Us = runif(n, min=0, max=1)
  Vs = runif(n, min=0, max=1)
  Zs = list()
  # for each independent pair of U and V, compute X and Y 
  # and store as 'tupel' in Zs
  for (i in 1:n) {
    X = sqrt((-2)*log(Us[i]))*cos(2*pi*Vs[i])
    Y = sqrt((-2)*log(Us[i]))*sin(2*pi*Vs[i])
    Zs[[i]] = c(X,Y)
  }
  return (Zs)
}

# b)
Xs = c()
Ys = c()
i=1
# vor every vector returned by box_muller, store it's X value in Xs, Y in Ys
# warning: slow!
for (vec in box_muller(10^5)) {
  Xs[i] = vec[1]
  Ys[i] = vec[2]
  i = i+1
}
# compute histograms and show in a 2x2 pattern
par(mfrow=c(2,2))
hist(Xs)
hist(Ys)
hist(Xs[Ys<0])
hist(Xs[Ys>=0])



#Anmerkung von Jan:
#3 ist hier sehr schnell, 4 langsamer als bei anderen Abgaben