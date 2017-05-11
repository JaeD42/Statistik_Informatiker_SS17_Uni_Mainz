# Funktion, die für Wahrscheinlichkeit p, dass Kopf fällt, 
# die Anzahl der Zahl- Würfe vor dem ersten Kopf- Wurf zählt
muenze <- function(p) {
  counter = 0
  # generiere zufällig 0 oder 1 mit Wahrscheinlichkeit p für 1 und 1-p für 0
  x <- sample(0:1,1, prob=c(1-p,p))
  # solange 0 rauskommt, wirf Münze erneut und zähle counter hoch
  while(x == 0) {
    x <- sample(0:1,1, prob=c(1-p,p))
    counter = counter + 1
  }
  # gib counter zurück
  return(counter)
}

v <- c()
# führe 10000 mal Funktion aus und speichere jedesmal Anzahl der Zahl- Würfe in Vector v
for (i in 1:10000) {
  x <- muenze(0.6)
  v[i] = x
}

# deklariere Array
array = c()
# für i von 0 bis 10: zähle Häufigkeit von i in v und speichere Wert/10000 in array
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
plot(array, x = 0:10, ylim=c(0,1), pch=19, main="Geometrische Verteilung", xlab="Anzahl Zahlwürfe (60% Wahrsch. für Kopf)", ylab="relative Häufigkeit")


