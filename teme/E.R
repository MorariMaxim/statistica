e1 = function(n,u,d,a){
  alfa = 1-a;
  ds = sqrt(d);
  z = qnorm(1 - alfa/2, 0, 1);
  
  a = u - z*ds/sqrt(n);
  b = u + z*ds/sqrt(n);
  interval = c(a,b);
  interval
}
e1(10,138,11^2,0.9)
e1(10,138,11^2,0.95)
e1(10,138,11^2,0.99)

e2 = function(n,u,d,a){
  alfa = 1-a;
  ds = sqrt(d);
  t = qt(1-alfa/2,n-1);
  se = ds/sqrt(n);
  
  a= u - t*se;
  b= u + t*se;
  interval = c(a,b);
  interval
}
e2(256,18,1.44,0.95)

e3 = function(n,s,a,p0){
  
  alt = s/n;
  
  zs = (alt - p0)/sqrt(p0*(1-p0)/n);
  cz = qnorm(1-a,0,1);
  r = c(zs,cz); 
  print(r);
  if (zs < cz)
    print("schimbarea a fost inutila")
  else 
  {
    if(p_prim<p0){
      print("schimbarea nu a fost inutila")
    }
    else{
      print("schimbarea a fost inutila")
    }
  }
}
e3(153,19,0.01,0.12)
e3(153,19,0.05,0.12)