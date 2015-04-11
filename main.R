# Ondřej Paška
K = 6
L = 5
n = K*20

## 1

# The Uniform Distribution
u = runif(n, 0, 1)
u
hist(u, main="Histogram hodnot u")

# 1.I
# https://en.wikipedia.org/wiki/Exponential_distribution#Generating_exponential_variates
# log is natural log
x=-log(1-u)/L
x

# 1.II
hist(x, probability=TRUE, main="Histogram hodnot x ")
xWidth=max(x)-min(x)
xGrid=seq(min(x)-0.2*xWidth,max(x)+0.2*xWidth,length=n)
lines (xGrid,dexp(xGrid, rate=L), col='red')

# 1.III
plot(ecdf(x), verticals=TRUE, do.points = FALSE, main="Empirická distribuční funkce")
lines (xGrid,pexp(xGrid, rate=L), col='blue')

# 1.IV
y=rexp(1000, rate=L)
qqplot(x, y);
abline(0, 1, col='red', lwd=2)

# 1.V diskuse

# 1.VI :(


## ------------------------------------------------------------- ##
## 2

# 2.I + 2.II
lambda = function(t) { 100 + 50/exp((t - 420)^2/(3600*L)) + 100/exp((L*(t - 480 - 30*L)^2)/360000)}

size=24*60

# generates minutes
t=seq(0,size-1)
plot(t, lambda(t), type='l') 

# lambda maximum
lambdaMax=optimize(lambda, interval=c(0, size), maximum=TRUE)$objective

result=list()
t=0
i=0
while(t < size)
{
	lambdaT=lambda(t)

	t = t + rexp(1, lambdaMax)

	# random
	s = runif(1, min=0, max=1)
	if (s <= lambdaT/lambdaMax)
	{
		i = i + 1
		result[i] = t
	}
}

xWidth=max(unlist(result))
xGrid=seq(min(x)-0.1*xWidth,max(x)+0.1*xWidth,length=K*10)

firstTen = result[0:(K*10)]
plot(firstTen,rep(0, length(firstTen)), xlab="Čas", main="Prvních K*10 příchodů na časové ose")

# 2.III
t=seq(0,size-1)
#Vykresleni krivky za pomoci dat z histogramu
h=hist(unlist(result), breaks=size, plot=FALSE)
plot(h$mids, h$counts, type="l", xlab = "Čas[min]", ylab="Počet požadavků", main="Četnost příchodů pro celý den")
lines(t, lambda(t), col="red", lwd=3)


## ------------------------------------------------------------- ##
## 3

# 3.I

courierProb = K/(K+L)
postList = list()
courierList = list()
i=0

for (i in 0:size)
{
	lambdaT = lambda(i)
	courierList[i] = 0
	postList[i] = 0

	for (y in 0:lambdaT)
	{
		r = runif(1, min=0, max=1) # random()
		# either courier or post
		if (r <= courierProb)
		{
			courierList[i] = as.numeric(courierList[i]) + 1
		}
		else
		{
			postList[i] = as.numeric(postList[i]) + 1
		}
	}
}

# 3.II

# Post usage; postProb is opposite of courierProb
plot(t, postList, typ="l", col="green", xlab="Čas", ylab="Počet", main="Objednávky s použitím státní pošty")
lines(t, lambda(t)*(1-courierProb), col="red", lwd="3")

# Courier usage
plot(t, courierList, typ="l", col="violet", xlab="Čas", ylab="Počet", main="Objednávky s použitím kurýrní služby")
lines(t, lambda(t)*(courierProb), col="red", lwd="3")


# 3.III

# all together
plot(t, postList, typ="l", col="orange", xlab = "Čas", ylab="Počet", main="Četnost příchodů objednávek pro oba procesy pro celý den", ylim=c(20,160))
lines(t, lambda(t)*(1-courierProb), col="red", lwd="3")
lines(t, courierList, typ = "l", col="green")
lines(t, lambda(t)*(courierProb), col="red", lwd="3")

