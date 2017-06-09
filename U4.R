

# 4a)
# Simuliere den Zerfall von N Atomen, mit exponentialverteilten Zerfallszeiten 
# mit Parameter p, fuer K Intervalle der Laenge t.
zerfall = function (N, p, K, t) {
  
  # Simuliere expontentialverteilte Zerfallszeiten
  zerfallszeiten = rexp(n = N, rate = p)
  
  # Bestimme V   = alle V_k
  #          V_k = Anzahl der Atome die im Intervall ((k-1)*t, k*t] zerfallen
  V = rep(0,K)
  
  for(z in zerfallszeiten){
    ind = ceiling(z/t)
    if (ind<=K){
      V[ind]=V[ind]+1
      }
  }
  
  print(V)
  
  return(V)
}


# 4b)
# Fuehre eine Simulation durch.
# Bemerkung: bei N = 10^6 dauert das ausfuehren etwas laenger...
teilB = function() {
  T = 7.5
  K = 2500
  N = 2.5*10^6
  p = 3.8/(T*N)
  
  V = zerfall(N, p, K, T)
  print(V)
  hist(
    x = V,
    breaks= 0:(max(V)+1)-0.5,
    freq = F,
    xlab = "Zeitintervall Index",
    ylab = "Relative Haeufigkeit der Zerfallsereignisse"
  )
  points(
    x = seq(0, max(V)),
    y = dpois(x = seq(0, max(V)), lambda = 3.8)
  )
}


# 4c + 4d
teilCD = function() {
  # 4c)
  # I_count := Anzahl der Zeitintervalle im Experiment (dort "I")
  # I       := Tabelle mit den Ergebnissen des Experiments
  I_count = 2608
  I = matrix(
    c(0, 57,
      1, 203,
      2, 383,
      3, 525,
      4, 532,
      5, 408,
      6, 273,
      7, 139,
      8, 45,
      9, 27,
      10, 10,
      11, 4,
      12, 0,
      13, 1,
      14, 1
    ), ncol = 2, byrow = T)
  
  # Bestimme M = 0*57 + 1 * 203 + ... + 14 * 1
  # sowie lamba-hut und I-hut (Poisson-Verteilung)
  M = sum(I[1:15] * I[16:30])
  lambda_hat = M / I_count
  I_hat = dpois(x = seq(0, 9), lambda = lambda_hat) * M
  
  # 4d)
  # Plotte Forschungergebnisse (blau) vs erwartete Poisson-Verteilung (rot)
  plot(
    x = seq(0, 9),
    y = I[16:25],
    col = "blue",
    ylim = range(0:max(max(I), max(I_hat))),
    xlab = "Zeitintervall Index",
    ylab = "Anzahl der Zerfallsereignisse",
    pch = 19
  )
  points(
    x = seq(0, 9),
    y = I_hat[1:10],
    col = "red",
    pch = 19
  )
  
  # Es faellt auf: I und I_hat sind fast identisch, nur leichte Abweichungen und ein Faktor von rund 3.8
  # Extra: Forschungsergebnis skaliert um 3.8
  points(
    x = seq(0, 9),
    y = I[16:25] * 3.8,
    col = "pink",
    pch = 19
  )
  
  legend(
    "topright",
    legend = c("Forschungsergebnis",
               "Poisson-Verteilung mit lambda-hut",
               "Forschungsergebniss skaliert um 3.8"),
    col = c("blue", "red", "pink"),
    pch = 19
  ) 
}

#teilB()
#teilCD()
