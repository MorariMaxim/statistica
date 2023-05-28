ex21 = function(n,u,d,a){
  alfa = 1-a;
  ds = sqrt(d);
  z = qnorm(1 - alfa/2, 0, 1);
  
  a = u - z*ds/sqrt(n);
  b = u + z*ds/sqrt(n);
  interval = c(a,b);
  interval
}
ex26 = function(filename){
  
  x = scan(filename);
  n = length(x);
  u = mean(x);
  d = 5^2;
  a = 0.9;
  ex21(n,u,d,a);
  a = 0.95;
  ex21(n,u,d,a);
  a = 0.99;
  ex21(n,u,d,a)
  
}
ex31 = function(n,u,d,a){
  alfa = 1-a;
  ds = sqrt(d);
  t = qt(1-alfa/2,n-1);
  se = ds/sqrt(n);
  
  a= u - t*se;
  b= u + t*se;
  interval = c(a,b);
  interval
}
ex34 = function(filename){
  
  x = scan(filename);
  n = length(x);
  u = mean(x);
  d = (sd(x))^2;
  a = 0.9;
  ex31(n,u,d,a);
  a = 0.95;
  ex31(n,u,d,a);
  a = 0.99;
  ex31(n,u,d,a)
}
ex41 = function(n,s,a,p0){
  
  alt = s/n;
  
  zs = (alt - p0)/sqrt(p0*(1-p0)/n);
  cz = qnorm(1-a,0,1);
  r = c(zs,cz); 
  print(r);
  if(zs < cz) {
    print("Nu se respinge");
  }
  else {
    print("Se respinge");
  }
}
ex42 = function(){
  
  ex41(150,20,0.05,0.1);
  
}
 