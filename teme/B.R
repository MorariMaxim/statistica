exb1= function(p,n){
  
  return(mean(rgeom(n,p))+1);
}
exb1_comp = function(){
  
  p =0.2;  
  cat("p=0.2, n=5000, mean=",exb1(0.2,5000), " Exact= ",1/p, "\n");
  cat("p=0.2, n=10000, mean=",exb1(0.2,10000)," Exact= ",1/p, "\n");
  cat("p=0.2, n=100000, mean=",exb1(0.2,100000)," Exact= ",1/p, "\n");
  cat("p=0.2, n=500000, mean=",exb1(0.2,500000)," Exact= ",1/p, "\n");
  p =0.6;
  cat("p=0.6, n=5000, mean=",exb1(0.6,5000)," Exact= ",1/p, "\n");
  cat("p=0.6, n=10000, mean=",exb1(0.6,10000)," Exact= ",1/p, "\n");
  cat("p=0.6, n=100000, mean=",exb1(0.6,100000)," Exact= ",1/p, "\n");
  cat("p=0.6, n=500000, mean=",exb1(0.6,500000)," Exact= ",1/p, "\n");
  p = 0.8;
  cat("p=0.8, n=5000, mean=",exb1(0.8,5000)," Exact= ",1/p, "\n");
  cat("p=0.8, n=10000, mean=",exb1(0.8,10000)," Exact= ",1/p, "\n");
  cat("p=0.8, n=100000, mean=",exb1(0.8,100000)," Exact= ",1/p, "\n");
  cat("p=0.8, n=500000, mean=",exb1(0.8,500000)," Exact= ",1/p, "\n");
}
exb1_comp();
exb2= function(r,n,N,z) {
  u = 0;
  s = r/(r-2);
  ub= z * s/sqrt(n) + u;
  sum = 0;
  for(i in 1:N) {
    x_n = mean(rt(n, r));
    if(x_n <= ub) {
      sum = sum + 1;
    }
  }
  return(sum/N);
}
exb2_comp = function(r){
  z = -1.5;
  for(i in 1:3){
    N = 5000;
    for(j in 1:3){
      res = exb2(r,50,N,z);
      errel = abs((res - pnorm(z))/pnorm(z));
      cat("\nr=",r," n=50"," N=",N," sum/N=",res,"\nEroarea relativa ",errel ); 
      N= N*2;
    }
    z=z+1.5;
  }
}
exb2_comp(3);
exb3 = function(n,p,h,k){
  
  e = n*p;
  v = n*p*(1-p);
  s = sqrt(v);
  q1 = (h-e)/s;
  q2 = (k-0.5-e)/s;
  return(pnorm(q2)-pnorm(q1))
}
exb3(100,0.5,0,50)