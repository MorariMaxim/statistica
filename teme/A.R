a1a=function(n, lambda, k, p)
{
  par(mfrow=c(1,3))
  barplot(dpois(k:n, lambda), space = 0.5, col="green", legend.text="dpois");
  barplot(dgeom(k:n, p), space = 0.5, col="blue",  legend.text="dgeom");
  barplot(dbinom(k:n, n, p), space = 0.5 , col="orange",  legend.text="dbinom");
}
a1a(20,1,5,0.5)
a1b = function(p){
  x = seq(1,1000,2)
  y = dgeom(x, p)
  res = sum(y)
  print(res)
  x = 0:3
  y = dgeom(x,p) 
  res = 1 - sum(y)
  print(res)
  x = 0:20
  y = dgeom(x,p)
  res = sum(y)
  print(res)
  
}
a1b(0.5)
a1c = function(l){
  
  lim = 0.0000001
  k=0
  while(1){
    k=k+1;
    x = dpois(0:(k-1),l)
    r = 1- sum(x)
    if(r<lim){
      print(k)
      break;
    }
  }
}
a1c(1)
a2a = function(f){
  
  tablou = read.csv("note.csv", header = T, sep=",")
  s1 = tablou[['P']];
  s2 = tablou[['S']];
  
  print("Statistics for the first sample")
  cat("Median: ",median(s1), " Mean: ",mean(s1)," SD: ",sd(s1),"\nQuartiles:\n");  

  i =0
  q = vector()
  for ( k in 2:5){
    print(as.vector(quantile(s1))[k])
  }
  print("Statistics for the second sample")
  cat("Median: ",median(s2), " Mean: ",mean(s2)," SD: ",sd(s2),"\nQuartiles:\n");
  
  i =0
  q = vector()
  for ( k in 2:5){
    print(as.vector(quantile(s2))[k])
  }
}
a2a("note.csv")
a2b = function(f,sample){
  tablou = read.csv("note.csv", header = T, sep=",")
  x = tablou[[sample]];
  
  m = mean(x)
  s = sd(x)
  trimmed = vector() 
  j = 0 
  for(i in 1:length(x))
    if(!(x[i] <m - 2*s || x[i] > m + 2*s)) {
      j = j + 1
      trimmed[j] = x[i]
    }  
  return(trimmed)
}
a2b("note.csv","P")
a2b("note.csv","S")
a2c = function(f){
  
  p = a2b(f,"P");
  s = a2b(f,"S");
  interval = 1:10;
  
  par(mfrow=c(1,2))
  hist(p,breaks = interval, right = T,include.lowest = F, col= "blue")
  hist(s,breaks = interval, right = T,include.lowest = F, col= "red")
  
}
a2c("note.csv")