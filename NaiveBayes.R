mydata = read.csv("spam.csv")
mydata <- data.frame(lapply(mydata, as.character), stringsAsFactors=FALSE)
#Daten laden
samples = 5559


#listen initialisieren
counter_spam = list()
counter_ham = list()

#hilfsvariablen
num_spam = 0
num_ham = 0

#erste 4000 werden zum lernen benutzt
for(i in 1:4000){
  #hole satz
  words = strsplit(mydata[[2]][i],' ')
  
  #wenn kein spam
  if(mydata[[1]][i]=="ham"){
     #incrementiere zaehler fuer jedes wort
     for(w in words[[1]]){
      if(is.null(counter_ham[[w]])){
	counter_ham[[w]]=0
      }
      counter_ham[[w]]=counter_ham[[w]]+1
     }
     num_ham=num_ham+1
  #wenn spam
  }else{
     for(w in words[[1]]){
      if(is.null(counter_spam[[w]])){
	counter_spam[[w]]=0
      }
      counter_spam[[w]]=counter_spam[[w]]+1
     }
     num_spam=num_spam+1
  
  
  }
}


pred=c()
corr=c()
#berechne vorhersage
for(i in 4000:samples){
    spam_prob=1
    ham_prob=1
    words = strsplit(mydata[[2]][i],' ')
    for(w in words[[1]]){
      if(!is.null(counter_spam[[w]])){
         spam_prob=spam_prob*((counter_spam[[w]]+1)/num_spam)
      
      }else{
         spam_prob=spam_prob*(1/length(counter_spam))
      }
      if(!is.null(counter_ham[[w]])){
         ham_prob=ham_prob*((counter_ham[[w]]+1)/num_ham)
      }else{
         ham_prob=ham_prob*(1/length(counter_ham))
      }
    }
    spam_prob=spam_prob*(num_spam/(num_spam+num_ham))
    ham_prob = ham_prob*(num_ham/(num_spam+num_ham))
    pred[i-3999]=0
    if(spam_prob>ham_prob){
        pred[i-3999]=1;
    }
    corr[i-3999]=0
    if(mydata[[1]][i]=="spam"){
        corr[i-3999]=1;
    }
}

print(paste("Korrekt als spam: ",sum((pred==corr)[corr==1]), " von ",sum(corr==1),sep=""))

print(paste("Korrekt als ham:",sum((pred==corr)[corr==0]), " von ",sum(corr==0)))


