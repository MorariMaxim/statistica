ex1_1=function(){
  x = scan(file ="sample1.txt");
  stem(x);
  
}
ex1_2=function(){
  
  tablou = read.csv("unemploy2012.csv", header=TRUE,
                     sep=";");
  rate = tablou[['rate']];
  ival = c(0,4,6,8,10,12,14,30);
  hist(rate,breaks = ival,right=T);
  
}
ex1_3=function(){
  
  tablou = read.csv("life_expect.csv", header=TRUE,
                    sep=",");
  male = tablou[['male']];
  female = tablou[['female']];
  hist(male,breaks = 7,right=T,col="blue", main ="life expectancy", xlab="male/female");
  hist(female,add=TRUE,breaks = 7,right=T,col="red");
  
}
outliers_mean = function(sample){
  m = mean(sample)
  s = sd(sample)
  outliers = vector()
  j = 0
  for(i in 1:length(sample))
    if(sample[i] <m - 2*s || sample[i] > m + 2*s) {
      j = j + 1
      outliers[j] = sample[i]
      }
  print(outliers)
}
outliers_iqr = function(sample){
  
  m = mean(sample)
  q3 = as.vector(quantile(sample))[4]
  q1 = as.vector(quantile(sample))[2]
  iqr = q3-q1
  outliers = vector()
  j = 0
  for(i in 1:length(sample))
    if(sample[i] <m - 1.5*iqr || sample[i] > m + 1.5*iqr) {
      j = j + 1
      outliers[j] = sample[i]
    }
  print(outliers)
}
ex3_3  = function(){
  x = scan("sample2.txt");
  print("Summary")
  print(summary(x))
  print("Aberrants (M-2*sd)")
  outliers_mean(x)
  print("Aberrants (M-1.5*IQR)")
  outliers_iqr(x)
  
}