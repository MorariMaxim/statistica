ex1=function(x)
{
  max=max(x)
  min=min(x)
  sum=sum(x)
  raport=min(x)/max(x)
  mari=length(x[x>=40])
  mici=length(x[x<40])/length(x)*100
  return(list(max,min,sum,raport,mari,mici))
}
ex2a = function(x){
  y = 1:length(x)
  for ( k in 1:length(x))
    y[k] = x[k]/sum(x)
  print(y);
  
}
ex2b = function(x){ 
  z = 1:length(x)
  for ( k in 1:length(x))
    z[k] = (x[k]-min(x))/max(x)
  print(z);
  
}
ex2c = function(x){ 
  z = 1:(length(x)-1)
  for ( k in 1:(length(x)-1))
    z[k] = sum(x[1:k])/sum(x[(k+1):length(x)])
  print(z) 
}
ex2d = function(x){ 
  z = 1:(length(x)-1)
  for ( k in 1:(length(x)-1))
    z[k] = min(x[1:k])/max(x[(k+1):length(x)])
  print(z) 
}
ex4=function(n, p)
{
  #print(dbinom(0:n,n,p))
  barplot(dbinom(0:(n-1),n,p), space=0, main="Binomial dist")
}
ex5a=function(n, p)
{
  return(max(dbinom(0:(n-1),n,p)))
} 

ex5b=function(n, p, k)
{
  suma=0;
  for(i in 1: k)
  {
    suma=sum(dbinom(0:(k-1),n,p))
  }
  print(suma)
}
ex5c=function(n,p,k,m)
{
  prob=0;
  d=dbinom(0:(n-1),n,p)
  for(i in k:m)
  {
    prob=prob+d[i];
  }
  print(prob)
}

ex6=function(p, n)
{
  x=dgeom(0:(n-1), p)
  print(x)
}

ex9 = function(n,p){
  g = dgeom(0:(n-1),p); 
  print(g);
  barplot(g,main="Geomteric distribution", space=0)
}
ex10 = function(n,l){
  p=dpois(0:(n-1),l)
  print(p);
  barplot(p,main="Poisson distribution", space=0)
}
