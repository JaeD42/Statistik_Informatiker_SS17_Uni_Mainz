#function that calculates whether we win or not
#expects: desired wealth, current ressources, probability to lose, betting method
gamble <- function(k,kn,p,case) {
    while(TRUE) {
        #if no money is left, the player loses
	    if(kn==0){
	        return(FALSE)
	    #if the desired wealth is reached, the player wins
	    } else if(kn>=k) {
	        return(TRUE)
	    } else {
	        #the stakes, depending on the betting method
	        if(case==0) {
		        x <- 1
	        } else if(case==1) {
		        x <- kn
	        } else if(case==2) {
		        x <- ceiling(kn/2)
	        } else if(case==3) {
		        x <- ceiling(kn*(k-kn)/k)
	        }
	        prob <- runif(1,0,1)
	        #stakes are lost, can be recovered at (1-p) probability
	        kn <- kn-x
	        if(prob>p) {
		        kn <- kn+2*x
	        }
	    }
    }
}

k <- 100 #can be altered to 200

ps <- c(18/37,0.5,0.75)

for(i in ps) {
    print("Ergebnis:")
    print(paste("p=",i,sep=""))
    for(j in 0:3) {
    	count <- 0
    	times <- 1000 #make smaller for quicker calculation
    	#count the number of times we win with given parameters
    	for(l in 1:times) {
    	    if(gamble(k,50,i,j)) count <- count+1
    	}
    	#relative frequency of winning
    	h <- count/times
    	print(paste("Strategie ",j+1,": ",h,sep=""))
    }
}


