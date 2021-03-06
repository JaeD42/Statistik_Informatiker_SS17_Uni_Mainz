# Funktion, die f�r Wahrscheinlichkeit p, dass Kopf f�llt, 
# die Anzahl der Zahl- W�rfe vor dem ersten Kopf- Wurf z�hlt
muenze <- function(p) {
  counter = 0
  # generiere zuf�llig 0 oder 1 mit Wahrscheinlichkeit p f�r 1 und 1-p f�r 0
  x <- sample(0:1,1, prob=c(1-p,p))
  # solange 0 rauskommt, wirf M�nze erneut und z�hle counter hoch
  while(x == 0) {
    x <- sample(0:1,1, prob=c(1-p,p))
    counter = counter + 1
  }
  # gib counter zur�ck
  return(counter)
}

v <- c()
# f�hre 10000 mal Funktion aus und speichere jedesmal Anzahl der Zahl- W�rfe in Vector v
for (i in 1:10000) {
  x <- muenze(0.6)
  v[i] = x
}

# deklariere Array
array = c()
# f�r i von 0 bis 10: z�hle H�ufigkeit von i in v und speichere Wert/10000 in array
for (i in 0:10) {
  counter <- 0
  for (j in 1:10000) {
    if (v[j] == i) {
      counter = counter + 1
    }
  }
  array[i+1] = counter/10000
}
# gib array aus
print(array)

# plotte Werte von array
plot(array, x = 0:10, ylim=c(0,1), pch=19, main="Geometrische Verteilung", xlab="Anzahl Zahlw�rfe (60% Wahrsch. f�r Kopf)", ylab="relative H�ufigkeit")


