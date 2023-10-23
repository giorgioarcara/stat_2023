#More or less from: http://www.fernandohrosa.com.br/en/P/shaded_areas_in_r/

######################@###########
# UPPER TAIL ONLY 1 - p(z) ######
#################################
zobs = -1

lower.x <- zobs 
upper.x <- 100 # put here a very high number
step <- (upper.x - lower.x) / 1000
sigma <- 1
mu <- 0
bounds <- c(mu-3*sigma, mu+3*sigma)

# colored area
cord.x <- c(lower.x,seq(lower.x,upper.x,step),upper.x)
cord.y <- c(0,dnorm(seq(lower.x,upper.x,step),mu,sigma),0)
# plot_title
plot_title = paste("prob = ", round( pnorm(zobs, mean=mu, sd = sigma, lower.tail=F), 2))
curve(dnorm(x,mu,sigma),xlim=bounds, main = plot_title) 
polygon(cord.x,cord.y,col='skyblue')

#####################
# LOWER TAIL  ONLY  ######
#####################
# this is the R default

zobs = 4.19

upper.x <- zobs 
lower.x <- -100 # put here a very high number
step <- (lower.x - upper.x) / 1000
sigma <- 1
mu <- 0
bounds <- c(mu-3*sigma, mu+3*sigma)

# colored area
cord.x <- c(upper.x,seq(upper.x,lower.x,step),lower.x)
cord.y <- c(0,dnorm(seq(upper.x,lower.x,step),mu,sigma),0)
# plot_title
plot_title = paste("prob = ", round( pnorm(zobs, mean=mu, sd = sigma, lower.tail=T), 2))
curve(dnorm(x,mu,sigma),xlim=bounds, main = plot_title) 
polygon(cord.x,cord.y,col='skyblue')



#####################
# TWO TAILS   ######
#####################
zobs = -1.4


## upper tail
zobs_upper = abs(zobs)
lower.x <- zobs_upper
upper.x <- 100 # put here a very high number
step <- (upper.x - lower.x) / 1000
sigma <- 1
mu <- 0
bounds <- c(mu-3*sigma, mu+3*sigma)

# colored area
cord.x <- c(lower.x,seq(lower.x,upper.x,step),upper.x)
cord.y <- c(0,dnorm(seq(lower.x,upper.x,step),mu,sigma),0)
# plot_title
prob1 = pnorm(zobs_upper, mean=mu, sd = sigma, lower.tail=F)
plot_title1 = paste("prob = ", round( prob1, 2))
curve(dnorm(x,mu,sigma),xlim=bounds, main = plot_title) 
polygon(cord.x,cord.y,col='skyblue')


## lower tail
zobs_lower= -abs(zobs)


upper.x <- zobs_lower 
lower.x <- -100 # put here a very high number
step <- (lower.x - upper.x) / 1000
sigma <- 1
mu <- 0
bounds <- c(mu-3*sigma, mu+3*sigma)

# colored area
cord.x <- c(upper.x,seq(upper.x,lower.x,step),lower.x)
cord.y <- c(0,dnorm(seq(upper.x,lower.x,step),mu,sigma),0)
# plot_title
prob2 = round( pnorm(zobs_lower, mean=mu, sd = sigma, lower.tail=F), 2)
plot_title2 = paste("prob = ", prob2)
polygon(cord.x,cord.y,col='skyblue')




  