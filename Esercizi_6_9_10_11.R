# ESERCIZIO 6 p.112
n= 8
m  = 11
s = 11
mu0 = 24


tobs = (m-mu0)/(s/sqrt(n))

# trovo il t-critico per alpha = 0.01
qt(0.01, df = n-1, lower.tail=T)
# trovo il pvalue associato al mio t osservato (coda a sinistra)
pt(tobs, df = n - 1, lower.tail = T) 


### ESERCIZIO 9 p.114 Richiami
# a) - test z per probabilità di successi

N = 3615
n = 194
p = n/N
p0 = 0.04

# b) calcolo z
z = (p-p0)/ ( sqrt(p0*(1-p0)/N))

# calcolo p-value
pnorm(1-abs(z), lower.tail=T)*2

# IC superiore
p + qnorm(0.025)*sqrt(p*(1-p)/N)

# IC superiore
p + qnorm(1-0.025)*sqrt(p*(1-p)/N)


# Esercizio 10 p. 126 (Richiami)
m1 = 132.86
m2 = 127.44
s1 = 15.34
s2 = 18.23
n1 = 8
n2 = 21

sp_2 = ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2)
sp = sqrt(sp)

tobs = (m1-m2)/(sp*(sqrt( (1/n1) +(1/n2) )) )

# p osservato
2*(1 - pt(tobs, df = (n1+n2-2)))


# Esercizio 11 (richiami)
# spostarsi prima nella cartella corretta
dat = read.table("emicampo.txt", header=T)

head(dat) # vedi prime 6 righe del data.frame dat

t.test(dat$ED, dat$ES, paired=T, var.equal=F)

# in alternativa posso calcolare io il differenziale
dat$diff = dat$ED - dat$ES
t.test(dat$diff, var.equal = T)
             