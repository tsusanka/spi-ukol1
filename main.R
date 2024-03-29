# Ondřej Paška
K = 6
L = 5

## 1 I
n = 20;
alpha = 0.01;
x = rnorm(n, mean=10.5, sd=1.3);

hypothesisTest = t.test(x, mu=10, conf.level = 1-alpha);
print(hypothesisTest);

## 1 Ia

probability = 1 - (alpha / 2)
degreesOfFreedom = n-1
quantile = qt(probability, degreesOfFreedom)
criticalValue = qt(probability, degreesOfFreedom, lower.tail = FALSE)

# slide 50 lecture 17
marginError = (sd(x)/sqrt(n)) * quantile
intervalLowerBound = mean(x) - marginError
print("intervalLowerBound")
print(intervalLowerBound)
intervalUpperBound = mean(x) + marginError
print("intervalUpperBound")
print(intervalUpperBound)

## 1 Ib
mu = 10
if ((mu > intervalLowerBound) && (mu < intervalUpperBound)) {
	print("H0 is accepted")
} else {
	print("H0 is rejected")
}

## 1 Ic
# slide 46 lec 17
tStatisticValue = (mean(x) - mu) / (sd(x) / sqrt(n))
if (tStatisticValue > abs(criticalValue)) {
	print("H0 is rejected by T-statistic")
} else {
	print("H0 is accepted by T-statistic")
}



## 1 II
print("First one-sided tests:")
t.test(x, mu=10, alternative = "greater", conf.level = 1-alpha);

print("Second one-sided tests:")
t.test(x, mu=10, alternative = "less", conf.level = 1-alpha);

	## 1 IIa
	probability = 1 - alpha # 0.99
	quantile = qt(probability, degreesOfFreedom)
	criticalValue = qt(probability, degreesOfFreedom, lower.tail = FALSE)

	marginError = (sd(x)/sqrt(n)) * quantile
	intervalLowerBound = - Inf
	intervalUpperBound = mean(x) + marginError

	## 1 IIb
	if ((mu > intervalLowerBound) && (mu < intervalUpperBound)) {
		print("H0 is accepted")
	} else {
		print("H0 is rejected")
	}

	## 1 IIc
	tStatisticValue = (mean(x) - mu) / (sd(x) / sqrt(n))
	if (tStatisticValue > abs(criticalValue)) {
		print("H0 is rejected by T-statistic")
	} else {
		print("H0 is accepted by T-statistic")
	}

################################################################################
##################################### PART 2 #####################################
################################################################################

####################### 2 I ##########################
n = 20;
alpha = 0.01;
x = rnorm(n, mean=10, sd=1);
error = rnorm(n, mean=0.5, sd=0.8306624);
y = x + error;

tTestPair = t.test(x, y=y, paired = TRUE, alternative = "less", conf.level = 1-alpha)

## 2 Ia
print(tTestPair)

## 2 Ib
diff = x - y

test = t.test(diff, y=y, paired=TRUE, alternative = "greater", conf.level = 1-alpha, mu=0)
print(test)

####################### 2 II ##########################

n1 = 20;
n2 = 25;
alpha = 0.01
x=rnorm(n1, mean=10, sd=1.3)
y=rnorm(n2, mean=11.25, sd=1.3)

t.test(x, y=y, paired = FALSE, var.equal = TRUE, conf.level = 1-alpha)

## 2 IIa
t.test(x, y=y, paired = FALSE, var.equal = TRUE, conf.level = 1-alpha, alternative="less")

## 2 IIb
s2x = sum( (x - mean(x))^2 ) / (length(x)-1)
s2y = sum( (y - mean(y))^2 ) / (length(y)-1)

Sxy = sqrt( ((length(x)-1)*s2x + (length(y)-1)*s2y)   /  (length(x)+length(y) -2) )

TStat = (mean(x) - mean(y)) / (Sxy * sqrt(1/length(x) + 1/length(y))) 
print(TStat)
df = length(x) + length(y) -2
print(df)

tCriticalValue = qt(1-alpha, df, lower.tail = TRUE)

pVal = pt(TStat, df = df)
print(pVal)

####################### 2 III ##########################
 
n1 = 20;
n2 = 25;
alpha = 0.01
x=rnorm(n1, mean=10, sd=1.3)
y=rnorm(n2, mean=11.28, sd=1.2)
 
t.test(x, y=y, paired = FALSE, var.equal = FALSE, conf.level = 1-alpha)


########### 2 III a ###########

t.test(x, y=y, paired = FALSE, var.equal = FALSE, conf.level = 1-alpha, alternative="less")

########### 2 III b ###########


s2x = sum( (x - mean(x))^2 ) / (length(x)-1)
s2y = sum( (y - mean(y))^2 ) / (length(y)-1)

sxY = sqrt(s2x/n1 + s2y/n2)

TStat = (mean(x) - mean(y)) / sxY
print(TStat)
 
df = ((s2x/n1 + s2y/n2)^2) / (((s2x/n1)^2) / (n1-1) + ((s2y/n2)^2) / (n2-1)) 
print(df)
 
tCriticalValue = qt(1-alpha, df, lower.tail = TRUE)
pVal = pt(TStat, df = df)
print(pVal)


################################################################################
##################################### PART 3 #####################################
################################################################################


## 3 I

sequenceLength = 2500000; # 0.434s

## 3 II

sampleSize = L*40;
time1 = time2 = numeric(sampleSize); # Declare an array

for(i in 1:sampleSize) {
	x = runif(sequenceLength, 0, 100); # Generate the sequence to be sorted
	# Measure sort times. The user-space time is at system.time(...)[1]
	# Inside system.time we must use x1 <- value and not x = value. The latter syntax is reserved for parameters.
	time1[i] = system.time(x1 <- sort(x, method = "quick"),  gcFirst = TRUE)[1];
	time2[i] = system.time(x2 <- sort(x, method = "shell"), gcFirst = TRUE)[1];
}

meanSingleton = mean(time1)
print("meanSingleton: ")
print(meanSingleton)
meanSedgewick = mean(time2)
print("meanSedgewick: ")
print(meanSedgewick)

alpha = K / 100
t.test(time1, time2, paired = FALSE, var.equal = FALSE, conf.level = 1-alpha)

## 3 III
time3 = time4 = numeric(sampleSize); # Declare an array
for(i in 1:sampleSize){
	x = runif(sequenceLength, 0, 100); # Generate the sequence to be sorted
	time3[i] = system.time(x1 <- sort(x, method = "quick"),  gcFirst = TRUE)[1];
}
sampleSize2 = L*35;
for(i in 1:sampleSize2){
	x = runif(sequenceLength, 0, 100); # Generate the sequence to be sorted
	time4[i] = system.time(x2 <- sort(x, method = "shell"), gcFirst = TRUE)[1];
}

meanSingleton2 = mean(time3)
print("meanSingleton2: ")
print(meanSingleton2)
meanSedgewick2 = mean(time4)
print("meanSedgewick2: ")
print(meanSedgewick2)
