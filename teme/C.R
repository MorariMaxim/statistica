c1 = function(a,N){
  
  count = 0;
  for(i in 1:N){
    x = runif(1, -(a^(0.5)),a^(0.5));
    y = runif(1, -(a^(0.5)),a^(0.5));
    z = runif(1, 0,a);
    if(z>= x^2+y^2){
      count = count+1;
    }
  }
  est = 4*a^2 * count/N;
  real = pi*a^2/2;
  erabs = abs(real - est);
  errel = abs(erabs/real);
  cat("Est: ", est," Real: ",real, " Eroarea relativa ", errel,"\n");
#  return (est);
}
c1(2,10000)
c1(2,20000)
c1(2,50000)
c1(4,10000)
c1(4,20000)
c1(4,50000)
c1(8,10000)
c1(8,20000)
c1(8,50000)
c2 = function(N){
  
  count = 0;
  for(i in 1:N){
    x = runif(1,0,5);
    y = runif(1,0,5);
    
    if((3*y<= x+6)&&(y<=12-3*x)){
      count = count+1;
    }
  }
  est = 25 * count/N;
  return (est);
}
c2(1000);
c3a = function(N) { 
  sum = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    e = (x+1)/sqrt(4-x^2);
    sum = sum + e;
  }
  est = 2*sum/N;
  real = pi/3;
  cat("Estimare ",est,"\nValreala ", real ); 
}
c3a(1000);
c3b = function(N) {
  sum = 0;
  for(i in 1:N) {
    x = runif(1, -100, 0);
    e = 1/(x^2+4);
    sum = sum + e
    
  }
  est = 100*sum/N;
  real = pi/4;
  cat("Estimare ",est,"\nValreala ", real ); 
}
c3b(1000);
c3c = function(N){
  sum = 0;
  for(i in 1:N) {
    u = 1*rexp(1, 1);
    sum = sum + -1*u*exp(-1*u)/exp(-u);
  }
  est = sum/N;
  real = -1;
  cat("Estimare ",est,"\nValreala ", real ); 
}
c3c(1000);
c4a_helper = function(m,q,n,p){
  fals = m;
  d = 0;
  while(fals > 0){
    d = d+1;
    #cat("\nDay ",d,"\n");
    fals = fals + rbinom(1,n,p);
    #cat("Fals",fals,"\n");
    for(i in 1:fals){
      u = runif(1,0,1);
      if(u<= q){
        fals = fals - 1;
      }
    }
    #cat("AFter deletion",fals,"\n");
  }
  return (d);
}
c4a = function(m,q,n,p){
  
  total = 0;
  for (i in 1:100) {
    total = total + c4a_helper(m,q,n,p);
  }
  return (total/100);
}
c4a(100000,0.1,500,0.5)
c4bhelper = function(m,q,n,p){
  fals = m;
  d = 0;
  while(d<40){
    d = d+1;
    ad = rbinom(1,n,p);
    fals = fals + ad;
    for(i in 1:fals){
      u = runif(1,0,1);
      if(u<= q){
        fals = fals - 1;
      }
    }
  }
  if(fals <= 50000) return (1);
  return (0);
}

c4b = function(m,q,n,p,N){
  
  c = 0;
  for(i in 1:N){
    r = c4bhelper(m,q,n,p);
    c = c+r;
  }
  return (c/N);
}
c4b(100000,0.1,500,0.5,10);
c4c = function(m,q,n,p,N){
  
  alfa = 1 - 0.99;
  z = qnorm(alfa/2);
  epsilon = 0.01;
  nmin= ceiling((1/4)*(z/epsilon)^2);
  c4b(m,q,n,p,nmin);
}
c4c(100000,0.1,500,0.5,100);