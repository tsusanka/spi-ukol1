#Vycisteni konzole
cat("\014")  

#Reprezentant: Jiri Nadvornik
K = 4
L = 9

#===========================================================================================
#                                         Part 1
#===========================================================================================

#1.1
n = 20;
alpha = 0.01;
x = rnorm(n, mean=10.5, sd=1.3);

hypothesisTest = t.test(x, mu=10, conf.level = 1-alpha);
print(hypothesisTest); # Printing of the result is useful if you execute a script file 

#1.1.a
probability = 0.995
degreesOfFreedom = n-1
quantile = qt(probability, degreesOfFreedom)
criticalValue = qt(probability, degreesOfFreedom, lower.tail = FALSE)

#konfidencni interval
#quantile * (gama/sqrt(n)) -  margin error
marginError = (sd(x)/sqrt(n)) * quantile
confIntLower = mean(x) - marginError
confIntUpper = mean(x) + marginError

#1.1.b
#H0: mu = 10
mu = 10
if((mu > confIntLower) && (mu < confIntUpper) ){
  print("H0 accepted")
}else{
  print("H0 rejected")
}

#1.1.c
tStat = (mean(x) - 10)/ (sd(x)/ sqrt(n))
if(tStat > abs(criticalValue)){
  print("H0 rejected by T-Statistic")
}else{
  print("H0 not rejected by T-Statistic")
}

#1.2
t.test(x, mu=10, alternative = "greater", conf.level = 1-alpha);
t.test(x, mu=10, alternative = "less", conf.level = 1-alpha);

#1.2b
#konf iterval pro lesser interval
quantile = qt(0.99, degreesOfFreedom)
marginError = (sd(x)/sqrt(n)) * quantile
confIntLower = - Inf
confIntUpper = mean(x) + marginError

#1.2a
#H0: mu = 10
#HA: mu < 10
mu = 10

if( mu < confIntUpper){
  print("H0 accepted: mu < confIntUpper")
}else{
  print("H0 rejected")
}

#1.2c
# tStat by vychazel stejne nebot jednostrannost testu na vysledek nema vliv

#===========================================================================================
#                                         Part 2
#===========================================================================================

#Vycisteni konzole
cat("\014")
#Vymazani promennych
#rm(list = ls(all = TRUE))

#2.1
n = 20;
alpha = 0.01
x = rnorm(n, mean=10, sd=1)
error = rnorm(n, mean=0.5, sd=0.8306624)
y = x + error

tTest2 = t.test(x, y=y, paired = TRUE, alternative = "less", conf.level = 1-alpha)
print(tTest2)

#2.1a
#H0: muX = muY proti HA: muX < muY
confIntUpper2 = tTest2$conf.int[2]

mu = 0
if( mu < confIntUpper2){
  print("H0 accepted")
}else{
  print("H0 rejected")
}

#2.1b
diff = x - y
meanDiff = mean(diff)

confIntUpper3 = t.test(diff, mu=0, alternative = "less", conf.level = 1-alpha)$conf.int[2]

#H0: meanDiff = 0 proti HA: meanDiff < 0
if(meanDiff < confIntUpper3){
  print("H0 accepted")
}else{
  print("H0 rejected")
}

#2.2
n1 = 20;
n2 = 25;
alpha = 0.01
x=rnorm(n1, mean=10, sd=1.3)
y=rnorm(n2, mean=11.25, sd=1.3)

t.test(x, y=y, paired = FALSE, var.equal = TRUE, conf.level = 1-alpha)

#2.2a
t.test(x, y=y, paired = FALSE, var.equal = TRUE, conf.level = 1-alpha, alternative="less")
#2.2b
s2x = sum( (x - mean(x))^2 ) / (length(x)-1)
s2y = sum( (y - mean(y))^2 ) / (length(y)-1)

Sxy = sqrt( ((length(x)-1)*s2x + (length(y)-1)*s2y)   /  (length(x)+length(y) -2) )

T_stat = (mean(x) - mean(y)) / (Sxy * sqrt(1/length(x) + 1/length(y))) 
df = length(x) + length(y) -2

t_criticalValue = qt(1-alpha, df, lower.tail = TRUE)

p_val = pt(T_stat, df = df)

#2.3
n1 = 20;
n2 = 25;
alpha = 0.01
x=rnorm(n1, mean=10, sd=1.3)
y=rnorm(n2, mean=11.28, sd=1.2)

t.test(x, y=y, paired = FALSE, var.equal = FALSE, conf.level = 1-alpha, alternative = "less")

s2x = sum( (x - mean(x))^2 ) / (length(x)-1)
s2y = sum( (y - mean(y))^2 ) / (length(y)-1)

sx_y = sqrt(s2x/n1 + s2y/n2)

T_stat = (mean(x) - mean(y)) / sx_y

df = ((s2x/n1 + s2y/n2)^2) / (((s2x/n1)^2) / (n1-1) + ((s2y/n2)^2) / (n2-1)) 

t_criticalValue = qt(1-alpha, df, lower.tail = TRUE)

p_val = pt(T_stat, df = df)

#===========================================================================================
#                                         Part 3
#===========================================================================================

#Vycisteni konzole
#cat("\014")
#Vymazani promennych
#rm(list = ls(all = TRUE))

#overeni rychlosti pocitace
sequenceLength = 2000000;
x = runif(sequenceLength, 0, 100)
print(system.time(sort(x)))

#3.2
sampleSize = L*40
sampleSize2 = L*35
time1 = time2 = numeric(sampleSize); # Declare an array
for(i in 1:sampleSize){
  x = runif(sequenceLength, 0, 100); # Generate the sequence to be sorted
  # Measure sort times. The user-space time is at system.time(...)[1]
  # Inside system.time we must use x1 <- value and not x = value. The latter syntax is reserved for parameters.
  time1[i] = system.time(x1 <- sort(x, method = "quick"),  gcFirst = TRUE)[1];
  time2[i] = system.time(x2 <- sort(x, method = "shell"), gcFirst = TRUE)[1];
}

meanSingleton = mean(time1)
meanSedgewick = mean(time2)

alpha = K / 100
t.test(time1, time2, paired = FALSE, var.equal = FALSE, conf.level = 1-alpha)

#3.3
sep_time1 = sep_time2 = numeric(sampleSize); # Declare an array
for(i in 1:sampleSize){
  x = runif(sequenceLength, 0, 100); # Generate the sequence to be sorted
  sep_time1[i] = system.time(x1 <- sort(x, method = "quick"),  gcFirst = TRUE)[1];
}
for(i in 1:sampleSize2){
  x = runif(sequenceLength, 0, 100); # Generate the sequence to be sorted
  sep_time2[i] = system.time(x2 <- sort(x, method = "shell"), gcFirst = TRUE)[1];
}

meanSingleton_sep = mean(sep_time1)
meanSedgewick_sep = mean(sep_time2)