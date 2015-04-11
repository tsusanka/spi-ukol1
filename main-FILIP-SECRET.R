#Vycisteni konzole
cat("\014")  

#Reprezentant: Jiri Nadvornik
K = 4
L = 9
n = K*20

#Rovnomerne rozdeleni 'n' hodnot
u = runif(n, min=0, max=1)
u
hist(u, main="Histogram hodnot u")

#transformace dat dle inverzní distribuční funkce. Dle vzorce T= -ln(U)/lambda
#viz http://en.wikipedia.org/wiki/Exponential_distribution#Generating_exponential_variates
x=-log(1-u)/L
x

#Histogram 'x'  + graf hustoty EXP(L)
hist(x, breaks=3*K, probability=TRUE, main="Hustota")
xWidth=max(x)-min(x)
xGrid=seq(min(x)-0.1*xWidth,max(x)+0.1*xWidth,length=K*20)
lines (xGrid,dexp(xGrid, rate=L), col='red')

#Empiricka dist. fce 'x' + Distribucni funkce EXP(L)
plot(ecdf(x), verticals=TRUE, do.points = FALSE, main="Distribuční funkce")
lines (xGrid,pexp(xGrid, rate = L), col='red')

#Q-Q plot pro vzorkova data a nove vygenerovana data
y=rexp(1000, rate=L)
qqplot(x,y, plot.it=TRUE, xlab="Original values(x)", ylab="Exponential new random data(y)", main="Pravdepodobnostni papir")
#referencni osa
abline(a=0, b=1)

#Part 2

#lambda ze zadani
lambda = function(t) { 100 + 50/exp((t - 420)^2/(3600*L)) + 100/exp((L*(t - 480 - 30*L)^2)/360000)}
#minuty ve 24 hodinach
t=seq(0,24*60-1)
plot (t,lambda(t), lty="solid", lwd=3, type='l', main="Intenzita dle Poissonova rozdeleni") 

#maximalni hodnota, ktere funkce nabyva
lambda_maximum = optimize(lambda, interval=c(0, 24*60), maximum=TRUE)$objective
t=0
i=0
result = list()
#Generovani prichodu pozadavku pro prvni den
#Po pruchodu cyklem budou v listu result() casy prichodu pro prvni den
while(t < 24*60){
  lambda_t = lambda(t)
  
  t = t + rexp(1, lambda_maximum)
  
  #dalsi vygenerovany cas by byl uz mimo ramec jednoho dne
  #ukoncime cyklus
  if(t > 24*60){
    break
  }
  
  s = runif(1, min=0, max=1)
  if(s <= lambda_t/lambda_maximum){
    i = i + 1
    result[i] = t
  }
  
}

#2.2
#Vybrani nejvyssi hodnoty ve vysledcich
xWidth=max(unlist(result))
xGrid=seq(min(x)-0.1*xWidth,max(x)+0.1*xWidth,length=K*10)
#Pozadujeme pouze prvnich K*10 vysledku z prvniho dne
result_firstTen = result[0:(K*10)]
plot(result_firstTen,rep(0, length(result_firstTen)), xlab= "cas[min]", ylab = "", main="Prvnich K*10 pozadavku za den")

#2.3
t=seq(0,24*60-1)
#plot(t,rpois(t, lambda(t)), type="l", col="black", xlab = "cas[min]", ylab="pocet pozadavku", main="Cetnost prichodu pozadavku na web. server")
#Vykresleni krivky za pomoci dat z histogramu
h=hist(unlist(result), breaks=24*60, plot=FALSE)
plot(h$mids, h$counts, type="l", xlab = "cas[min]", ylab="pocet pozadavku", main="Cetnost prichodu pozadavku na web. server")
lines(t, lambda(t), col="red", lwd=3)

#Part 3

kuryr_prob = K/(K+L)
kuryr_list = list()
posta_list = list()
index=0
#Vnejsi cyklus projede kazdou minutu dne a vygeneruje
#pocet objednavek pro danou minutu
while(index <= 24*60){
  lambda_t = lambda(index)
  index_inner=0
  kuryr_list[index] = 0
  posta_list[index] = 0
  #Pro kazdou objednavku z dane minuty rozhodneme pouzitou sluzbu
  while(index_inner < lambda_t){
    r = runif(1, min=0, max=1)
    if(r <= kuryr_prob){
      kuryr_list[index] = as.numeric(kuryr_list[index]) + 1
    }else{
      posta_list[index] = as.numeric(posta_list[index]) + 1
    }
    
    index_inner = index_inner + 1
  }
  index = index + 1
}

#Plot vyuziti posty
plot(t, posta_list, typ="l", col="orange", xlab = "cas[min]", ylab="pocet lidi", main="Cetnost vyuziti posty")
lines(t, lambda(t)*(1-kuryr_prob), col="red", lwd="3")
#Plot vyuziti kuryra
plot(t, kuryr_list, typ="l", col="green", xlab = "cas[min]", ylab="pocet lidi", main = "Cetnost vyuziti kuryra")
lines(t, lambda(t)*(kuryr_prob), col="red", lwd="3")


#Plot sloucenych grafu
plot(t, posta_list, typ="l", col="orange", xlab = "cas[min]", ylab="pocet lidi", main="Porovnání četnosti využití obou služeb"
     ,ylim=c(20,160))
lines(t, lambda(t)*(1-kuryr_prob), col="red", lwd="3")
lines(t, kuryr_list, typ = "l", col="green")
lines(t, lambda(t)*(kuryr_prob), col="red", lwd="3")



