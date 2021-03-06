# Funktion, die eine Zufallszahl zwischen 0 und 255
# generiert und die bin�re Darstellung zur�ckgibt
byte <- function() {
  x <- sample(0:255,1)
  y <-intToBits(x)[8:1]
  return(y)
}

# deklariere Array
array = c()
# f�hre byte() 10000 mal aus und speichere jedes Mal 
# Summe der bin�ren Darstellung in array
for (i in 1:10000) {
  x <- byte()
  z <- sum(as.numeric(x))
  array[i] = z
}
# erstelle eine Histogramm mit den Werten aus array
hist(array, breaks=0:9-0.5, xlab="Wert", ylab="relative H�ufigkeit", freq=T, main="Binomialverteilung")


