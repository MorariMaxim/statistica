
ex1 = function(X,P){
  
  n = length(x)
  r = runif(1,0,1)
  s1 = 0;
  s2 = 0;
  for (i in 1:n) {
    s2 = s1+P[i]
    if(s1<=r && r<s2 ) return(X[i]);
    s1 = s2;
  }
}
ex1(1:3,c(0.1,0.2,0.7))
check = function(x,p){
  
  c = x;
  for (i in 1:length(x)) {
    c[i] = 0;
  }
  
  for (i in 1:1000) {
    r =ex1(x,p)
    for (j in 1:length(x)) {
      if(x[j] == r) {
        c[j] = c[j]+1;
        break;
      }
    }
  }
  c
}
check(1:3,c(0.1,0.2,0.7))