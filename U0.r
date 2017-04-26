

aufgabe2 = function(alphabet,word,number_of_experiments){
  
  #erstelle einen zufaelligen string bestimmter laenge aus alphabet
  sample_string = function(alphabet,length){
    res=c()
    for(i in c(1:length)){
      res[i]=sample(alphabet,1)
    }
    res = paste(res,collapse="")
    return(res)
  }

  #einzelnes experiment
  #gib true zurueck falls wir zufaellig das
  #richtige wort erstellen
  experiment <- function(alphabet,word){
    return(sample_string(alphabet,nchar(word))==word)
  }

  #wiederhole experiment sehr oft und zaehle
  #wie oft wir das richtige wort erzeugen
  count=0
  for(i in c(1:number_of_experiments)){
    count =count+ experiment(alphabet,word)
  }
  print(count/number_of_experiments)

}


aufgabe2(c("a","b"),"aab",10000)



aufgabe4 = function(w1,w2,N){
  #testet ob ein wuerfel gegen den anderen in einem
  #spiel gewinnt
  test_w1_ggn_w2 = function(w1,w2){
    return(sample(w1,1)>sample(w2,1))
  }

  

  count=0
  #zaehle wie oft w1 gegen w2 gewinnt
  for (i in 1:N){
  count=count+test_w1_ggn_w2(w1,w2)
  }
  print(count/N)


}

#die 4 wuerfel
w1 = c(3,3,3,3,3,3)
w2 = c(4,4,4,4,0,0)
w3 = c(5,5,5,1,1,1)
w4 = c(6,6,2,2,2,2)

#in wieviel % der faelle gewinnt w1 gegen w2?
aufgabe4(w1,w2,10000)
