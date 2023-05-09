ex1_2 = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, 0, 2);
    y = runif(1, 0, 2);
    e = -2*x^2 +5*x-2;
    if( x>=1/2 && y <= e ) 
      N_C = N_C+1;
  }
  est = 4*N_C/N;
  rel = (1.125 - est)/1.125;
  if(rel<0) rel = rel*-1;
  cat("Estimare ",4*N_C/N,"\nEroare realativa ", rel ); 
  
}

ex2_1b = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = runif(1, 1, 4);
    sum = sum + exp(u);
  }
  est = 3*sum/N;
  real = 51.87987;
  erabs = real - est;
  if(erabs<0) erabs = erabs*-1;
  errel = erabs /real;
  if(errel<0) errel = errel*-1;
  cat("Estimare ",est,"\nEroare realativa ", errel,"\nEroare absoluta ",erabs); 
}
ex2_1d = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = runif(1, 1, 10);
    sum = sum + 1/(4*u^2-1);
  }
  est = 9*sum/N; 
  real = 0.27465; 
  erabs = real - est; 
  if(erabs<0) erabs = erabs*-1;
  errel = erabs /real;
  if(errel<0) errel = errel*-1;
  cat("Estimare ",est,"\nEroare realativa ", errel,"\nEroare absoluta ",erabs); 
}

ex2_2 = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = rexp(1, 3);
    sum = sum + exp(-2*u*u)/(3*exp(-u*3));
  }
  real = 0.626657; 
  est = sum/N;
  erabs = real - est; 
  if(erabs<0) erabs = erabs*-1;
  errel = erabs /real;
  if(errel<0) errel = errel*-1;
  return(sum/N);
}
ex2_2k =  function(k, N) {
  estimates = 0;
  for(i in 1:k)
    estimates[i] = ex2_2(N);
  print(mean(estimates));
  print(sd(estimates));
}
ex3_2 = function(N){
  sum = 0;
  for(i in 1:N){
    u = runif(1,0,4);
    if(u <= 1){ 
      sum = sum + rexp(1,4);
    }
    else{
      sum = sum + rexp(1,12);
    }
  }
  return (sum/N);
} 

ex4_2 = function(){
  inf = 1;
  d = 0;
  maxinf = 1;
  while(inf > 0){
    d=d+1;
    for(i in 1:(40-inf) ){
      u = runif(1,0,5);
      if(u<= 1){
        inf = inf +1;
      }
    }
    if(inf > maxinf) maxinf = inf;
    cat("\nDay: ", d," Inf: ",inf);
    if(d>1){
       u = floor(runif(1,0,4))*2 + 4;
       inf = inf - u;
       cat("\nDeleted: ", u, " Inf: ", inf);
    }
  }
  return (maxinf);
}
ex4_2a = function(N){
  
  succes = 0;
  for(i in 1:N){
    r = ex4_2();
    #cat("\n",r);
    if (r == 40 ){
      succes = succes + 1;
      #cat("\nResult: ",r);
    }
  }
  #cat("\nResult: ",0);
  return (succes/N);
}
ex4_2b = function(N){
  succes = 0;
  for(i in 1:N){
    r = ex4_2();
    #cat("\n",r);
    if (r >= 15 ){
      succes = succes + 1;
      #cat("\nResult: ",r);
    }
  }
  #cat("\nResult: ",0);
  return (succes/N);
}
ex4_2c = function(){
    alfa = 1 - 0.95;
    z = qnorm(alfa/2);
    epsilon = 0.01;
    nmin= ceiling((1/4)*(z/epsilon)^2);
  
    ex4_2b(nmin);
}
