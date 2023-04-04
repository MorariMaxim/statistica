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
ex6=function(p, n)
{
  x=dgeom(0:(n-1), p)
  print(x)
}