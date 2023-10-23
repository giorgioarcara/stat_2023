# ES1
# slide 104 - Richiami 2022
x<-scan("x.txt")
summary(x)
par(mfrow=c(1,3))
hist(x,col="red")
boxplot(x,col="green")
qqnorm(x)
qqline(x)

# normality
shapiro.test(x)

# find statistic
mu0=150
n=length(x)
sigma=sqrt(5)
m=mean(x)
z<-(m-mu0)/(sigma/sqrt(n)) # slide 104 da Lezione1_Richiami
z
#R=(rifiuta per tutti valori di |z|> z(1-alpha/2))
alpha=0.05
zstar = qnorm(1-alpha/2) 

p=(1-pnorm(abs(z)))*2
p

# confidence interval
m-1.96*(1/sqrt(n))
m+1.96*(1/sqrt(n))


# ES 2. slide 106 (Richiami 2022)
s=sd(x)
m=mean(x)
n = length(x)
t<-(m-mu0)/(s/sqrt(n))
t
t.test(x,mu=150, conf.level=0.99)
m+qt(0.005, df = n-1, lower.tail = T)*(s/sqrt(n))
m+qt(1-0.005, df = n-1, lower.tail = T)*(s/sqrt(n))


# ES. 3 slide 106, richiami 2022
n = 100
mc = 115
mp = 120
sc = 24 
# calcolo t
tval = (mc - mp) / (sc/sqrt(n))
# calcolo p
pt(tval, df = n-1)

# ES. 4 slide 10
n = 10
mc = 13
mp = 15
sc = 4
# calcolo t
tval = (mc - mp) / (sc/sqrt(n))
# calcolo p
pt(tval, df = n-1)

