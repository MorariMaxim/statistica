exd1_a = function(x,k){
  
  l = length(x)
  ori = l/2+1;
  for (j in 1:k) {
    r = sample(1:l,1)
    c = 0;
    for (i  in 1:l) {
      if(x[i]==x[r]) c=c+1;
    }
    if(c>= ori) return (x[r]);  
  }
  return ("X nu are M-element")
}
x = 0:10
exd1_a(x,3)
exd1_b = function(){
  
  #1/2^k < 1/10^-7 => 2^k > 10^7
  #=> k > log(10^7)
  
  return (ceiling(log2(10^7)))
}
exd1_b()
exd3_a = function(s,a){
  
  n = length(s)
  m = ceiling(a*log2(n))
  #cat("m= ",m,"\n")
  x = sample(s,m);
  #print(x)
  x = sort(x)
  #print(x)
  return (median(x));
}
x = 0:10;
exd3_a(x,1);
exd3_b = function(){
  
  #n^2 >= 20 000 000
  
  return (ceiling(sqrt(2*10^7)))
  
}
exd3_b();

element_ith = function(ith, A)
{ 
  r = sample(1:length(A),1);
  z = A[r]; 
  a_less = vector()
  a_more = vector()
  j = 0
  k = 0
  for (i in A)
    if (i < z) {
      j = j + 1
      a_less[j] = i
    }
    else if (i > z) {
      k = k + 1
      a_more[k] = i
    } 
  if (j >= ith) { 
    return (element_ith(ith, a_less))
  }
  else {
    if (length(A) == ith + k)
      return (z)
    else
    { 
        return (element_ith(ith-1-j, a_more))}
    }
}

A = seq(0,1000,3)
element_ith(3, A)
