ex1_1c = function(u,d,n){
  
  x = seq(-n,n,0.1);
  y = dnorm(x,u,d);
  plot(x,y);
}
ex2_2= function(r,n){
  
  return(mean(rt(n,r)));
}
ex2_2_comp = function(){
  for(r in 2:5){
    n = 100;
    for(i in 1:4){
      n=n*10;
      cat("r=",r," n=",n,"mean=",ex2_2(r,n),"\n");
    }
  }
}
ex3_2= function(a,l,n,N,z) {
  u = a/l;
  s = a/(l^2);
  ub= z * s/sqrt(n) + u;
  sum = 0;
  for(i in 1:N) {
    x_n = mean(rgamma(n, a,l));
    if(x_n <= ub) {
      sum = sum + 1;
    }
  }
  return(sum/N);
}
ex3_2_comp = function(a,l){
  z = -1.5;
  for(i in 1:3){
    N = 5000;
    for(j in 1:3){
      cat("z=",z," n=",50," N=",N," sum/N=",ex3_2(a,l,50,N,z),"\n"); 
      N= N*2;
    }
    z=z+1.5;
  }
}
ex4_1 = function(n,p,k){
  
  e = n*p;
  v = n*p*(1-p);
  s = sqrt(v);
  q = (k-0.5-e)/s;
  return(pnorm(q));
}
ex4_2 = function(n,p,k){
  
  e = n*p;
  v = n*p*(1-p);
  s = sqrt(v);
  q = (k+0.5-e)/s; 
  return(1-pnorm(q));
}