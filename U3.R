
#a)
#function to generate multinomial distributed vector
#and return number of servers with users > 300 
server <- function(u,k){
  Z = 0;
  prob = rep(1/k,k);
  #create vector with rmultinom
  vec = rmultinom(1,u,prob);
  for(i in 1:length(vec[,1])){
    # check for 300 or more user servers
    if(vec[i,1] > 300){
      Z = Z + 1;
    }
  }
  return(Z);
}

#b)
#repeat experiment 1000 times
Z = replicate(10^4,server(60000,225));
hist(Z,
     breaks=min(Z):((max(Z)+1)-0.5),
     main = "Multinomialverteilung",
     xlab = "x-Werte",
     ylab = "Anzahl der Experimente",
     xaxt = 'n',
     col = 'cyan'
     );
axis(1,min(Z):max(Z)+0.5,min(Z):max(Z));

#c)
#counter to check if a server has at least 300 users
prob = c();
for(k in 0:30){
  print(k)
  counter = 0; 
  Z = replicate(10^3,server(60000,220+k));
  for(i in 1:length(Z)){
    if(Z[i] > 0){
      counter = counter + 1;
    }
  }
  #fill prob 
  prob[k+1] = counter / 10^3;
}
plot(220:250,
     prob,
     type='b',
     xlab='Anzahl der Server',
     ylab='P(Z > 0)',
     pch=20
     )

 