setwd("~/Dropbox/Datascience/Universite_Johns-Hopkins/Inference_statistique/StatInference")

#-------------------------------------------------------------------------------
# Week 1 & 2
#-------------------------------------------------------------------------------


## p<nom_loi>(x, ...) pour Cumulative distribution function ie f(x) = P(X<=x)
## q<nom_loi>(p, ...) pour Quantile xth
## r<nom_loi>(n, ...) pour Random generation
## Exemple beta distribution
pbeta(0.75,2,1)         # P(X<=0.75)
qbeta(0.5,2,1)          # Quantile 50
rbeta(10,2,1)           # 10 tirage...
dbeta(0.75,2,1)         # density function p(0.75)

pnorm(q = 1, mean = 0, sd = 1) # P(X<=1) - cumulative distribution function
qnorm(p = 0.975)        # quantile 97,5%
rnorm(n = 10)           # 10 tirages
dnorm(x = 0)            # density 
?qnorm

?qunif
qunif(0.75)

## Simulation experiment
## Simulating normals with mean 0 and variance 1 versus averages
## of 10 normals from the same population
rnorm(20 * 5)
matrix(rnorm(20 * 5), 20)

library(ggplot2)
nosim <- 10000; n <- 10
dat <- data.frame(
        x = c(rnorm(nosim), apply(matrix(rnorm(nosim * n), nosim), 1, mean)),
        what = factor(rep(c("Obs", "Mean"), c(nosim, nosim))) 
)
ggplot(dat, aes(x = x, fill = what)) + geom_density(size = 2, alpha = .2); 

## Averages of x die rolls
dat <- data.frame(
        x = c(sample(1 : 6, nosim, replace = TRUE),
              apply(matrix(sample(1 : 6, nosim * 2, replace = TRUE), 
                           nosim), 1, mean),
              apply(matrix(sample(1 : 6, nosim * 3, replace = TRUE), 
                           nosim), 1, mean),
              apply(matrix(sample(1 : 6, nosim * 4, replace = TRUE), 
                           nosim), 1, mean)
        ),
        size = factor(rep(1 : 4, rep(nosim, 4))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.25, colour = "black") 
g + facet_grid(. ~ size)


## Averages of x coin flips
dat <- data.frame(
        x = c(sample(0 : 1, nosim, replace = TRUE),
              apply(matrix(sample(0 : 1, nosim * 10, replace = TRUE), 
                           nosim), 1, mean),
              apply(matrix(sample(0 : 1, nosim * 20, replace = TRUE), 
                           nosim), 1, mean),
              apply(matrix(sample(0 : 1, nosim * 30, replace = TRUE), 
                           nosim), 1, mean)
        ),
        size = factor(rep(c(1, 10, 20, 30), rep(nosim, 4))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth = 1 / 12, colour = "black"); 
g + facet_grid(. ~ size)

# Distribution de la sample variance de 10, 20, 30,...
library(ggplot2)
nosim <- 10000; 
dat <- data.frame(
        x = c(apply(matrix(rnorm(nosim * 10), nosim), 1, var),
              apply(matrix(rnorm(nosim * 20), nosim), 1, var),
              apply(matrix(rnorm(nosim * 30), nosim), 1, var)),
        n = factor(rep(c("10", "20", "30"), c(nosim, nosim, nosim))) 
)
ggplot(dat, aes(x = x, fill = n)) + geom_density(size = 2, alpha = .2) + geom_vline(xintercept = 1, size = 2) 

# Variance of x die rolls
dat <- data.frame(
        x = c(apply(matrix(sample(1 : 6, nosim * 10, replace = TRUE), 
                           nosim), 1, var),
              apply(matrix(sample(1 : 6, nosim * 20, replace = TRUE), 
                           nosim), 1, var),
              apply(matrix(sample(1 : 6, nosim * 30, replace = TRUE), 
                           nosim), 1, var)
        ),
        size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black") 
g <- g + geom_vline(xintercept = 2.92, size = 2)
g + facet_grid(. ~ size)


## Simulation example to test variance of mean close to ... sigma^2/n
## ou standard error of the mean close to ... sigma/sqrt(n)

## Normal distribution
## Standard normals have variance of 1, standart deviation of 1
## means of n standard normals should have variance of 1/n
## & standard deviation of 1\sqrt(n)
nosim <- 1000
n <- 10
sd(apply(matrix(rnorm(nosim * n), nosim), 1, mean)) # sd(1000 moyennes de 10 ...)
1 / sqrt(n)

## Uniform distribution as a variance of 1/12 et sd of 1/4
## moyenne de n uniforms : variance of 1/(12*n) 
nosim <- 1000
n <- 10
sd(apply(matrix(runif(nosim * n), nosim), 1, mean))
1 / sqrt(12 * n)

## Poisson variance of 4, sd of 2
## mean of n Poisson -> variance of 4/n - sd of 2/sqrt(n)
nosim <- 1000
n <- 10
sd(apply(matrix(rpois(nosim * n, 4), nosim), 1, mean))
2 / sqrt(n)

# fair coin flip variance of 0.25 (Bernoulli p=0.5)
nosim <- 1000
n <- 10
sd(apply(matrix(sample(0 : 1, nosim * n, replace = TRUE),
                nosim), 1, mean))
1 / (2 * sqrt(n))


library(UsingR); data(father.son); 
x <- father.son$sheight
n<-length(x)

g <- ggplot(data = father.son, aes(x = sheight)) 
g <- g + geom_histogram(aes(y = ..density..), fill = "lightblue", binwidth=1, colour = "black")
g <- g + geom_density(size = 2, colour = "black")
g

round(c(var(x), var(x) / n, sd(x), sd(x) / sqrt(n)),2)
## sd 2.81 inch mais sd of the mean 0.09 inch ... div par 32 avec n=1078
## c'est le sample strandard error of the mean ... donc une estimation du
## standard error of the mean

#-------------------------------------------------------------------------------
# Bernoulli distribution / Binomial distribution
#-------------------------------------------------------------------------------

## Rappel Bernoulli : p et 1-p, mean p, var p(1-p)
## mass function p^x*(1-p)^(1-x)

## Binomial : somme de n Bernoulli iid
## Mass function P(X=x) = (C x parmis n)*p^x*(1-p)^(n-x)
.5 ^ 8*8+.5 ^ 8
choose(8, 7) * .5 ^ 8 + choose(8, 8) *.5 ^ 8
?pbinom
pbinom(6, size = 8, prob = .5, lower.tail = FALSE) 
## lower.tail = FALSE pour P(X>6)

?pbinom
pbinom(3,prob = 0.5,size = 5,lower.tail = FALSE)
?ppois
ppois(q = 10,lambda = 15)
#-------------------------------------------------------------------------------
# Normal distribution
#-------------------------------------------------------------------------------

plot(function(x) dnorm(x,0,1), xlim=c(-3,3), col = "green")

# Quantiles
## Normal distribution
qnorm(0.90, mean = 0, sd = 1)
qnorm(0.95, mean = 0, sd = 1)
qnorm(0.975, mean = 0, sd = 1)
qnorm(0.99, mean = 0, sd = 1)

plot(function(x) qnorm(x,0,1), xlim=c(0,1), col = "green")
abline(v=0.90, lty = "dotted")
abline(h=qnorm(0.90,0,1), lty = "dotted")
## 1,28 pour 90%
abline(v=0.95, lty = "dotted")
abline(h=qnorm(0.95,0,1), lty = "dotted")
## 1,65 pour 95%
abline(v=0.975, lty = "dotted")
abline(h=qnorm(0.975,0,1), lty = "dotted")
## 1,96 (ie presque 2) pour 97,5%
abline(v=0.99, lty = "dotted")
abline(h=qnorm(0.99,0,1), lty = "dotted")
## 2,33 pour 99%



#-------------------------------------------------------------------------------
# Poisson distribution
#-------------------------------------------------------------------------------
# Discrète, x entier naurel
# P(X=x;lamnda)= lambda^x*exp^(-lambda)/x!
# Exemple : nb de pers per hours 2.5, watching 4 hours, prob x<=3 
ppois(3, lambda = 2.5 * 4)

# Approximation loie binomiale par poisson si size ++ p --
pbinom(2, size = 500, prob = .01)
ppois(2, lambda=500 * .01)


## Law of large numbers in action
n <- 10000
means <- cumsum(rnorm(n)) / (1  : n)

library(ggplot2)
g <- ggplot(data.frame(x = 1 : n, y = means), aes(x = x, y = y)) 
g <- g + geom_hline(yintercept = 0) + geom_line(size = 2) 
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
g


## Law of large numbers in action, coin flip... conv vers 0.5
means <- cumsum(sample(0 : 1, n , replace = TRUE)) / (1  : n)
g <- ggplot(data.frame(x = 1 : n, y = means), aes(x = x, y = y)) 
g <- g + geom_hline(yintercept = 0.5) + geom_line(size = 2) 
g <- g + labs(x = "Number of obs", y = "Cumulative mean")
g

## Rappel
## La loi faible des grand nombres stipule la convergence en probabilité
## de la moyenne de n variable aléatoires independantes de mêmes var et esp E(X) 
## vers E(X) 

## La loi forte stipule la convergence presque sûre si les X iid et intégrables (E(X)<+8)

## Central Limit Theorem
## Moyenne of iid v.a. converge en loi vers N(mu,sigma^2/n) 

## Dé à 6 faces
nosim <- 1000
cfunc <- function(x, n) sqrt(n) * (mean(x) - 3.5) / 1.71
dat <- data.frame(
        x = c(apply(matrix(sample(1 : 6, nosim * 10, replace = TRUE), 
                           nosim), 1, cfunc, 10),
              apply(matrix(sample(1 : 6, nosim * 20, replace = TRUE), 
                           nosim), 1, cfunc, 20),
              apply(matrix(sample(1 : 6, nosim * 30, replace = TRUE), 
                           nosim), 1, cfunc, 30)
        ),
        size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)


# Coin
nosim <- 1000
cfunc <- function(x, n) 2 * sqrt(n) * (mean(x) - 0.5) 
dat <- data.frame(
        x = c(apply(matrix(sample(0:1, nosim * 10, replace = TRUE), 
                           nosim), 1, cfunc, 10),
              apply(matrix(sample(0:1, nosim * 20, replace = TRUE), 
                           nosim), 1, cfunc, 20),
              apply(matrix(sample(0:1, nosim * 30, replace = TRUE), 
                           nosim), 1, cfunc, 30)
        ),
        size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)


# si p élevé, ... convergence lente
nosim <- 1000
cfunc <- function(x, n) sqrt(n) * (mean(x) - 0.9) / sqrt(.1 * .9)
dat <- data.frame(
        x = c(apply(matrix(sample(0:1, prob = c(.1,.9), nosim * 10, replace = TRUE), 
                           nosim), 1, cfunc, 10),
              apply(matrix(sample(0:1, prob = c(.1,.9), nosim * 20, replace = TRUE), 
                           nosim), 1, cfunc, 20),
              apply(matrix(sample(0:1, prob = c(.1,.9), nosim * 30, replace = TRUE), 
                           nosim), 1, cfunc, 30)
        ),
        size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)


# Confidence interval
## Give a confidence interval for the average height of sons
library(UsingR)
data(father.son)
x <- father.son$sheight
summary(x)
sd(x)
length(x)
sqrt(length(x))
qnorm(0.975) # 97,5th quantile, 95% au milieu, 1.96 approx
(mean(x) + c(-1, 1) * qnorm(.975) * sd(x) / sqrt(length(x))) 
(mean(x) + c(-1, 1) * qnorm(.975) * sd(x) / sqrt(length(x))) / 12
## Rmq : il y a une série d'approx
## le nombre n est fini ... approx CLT gauss
## standard error of the mean approchée par sample standard error of the mean


## Dans le cas d'une expérience de Bernoulli de param p
## la variance est p(1-p)
## une majoration simple de l'intervalle de confiance à 95% est :
## phats +/- 1/sqrt(n)
## (...p(1-p) max pour 0.5 et vaut 1/4...)
## par exemple avec n=100 la précision est +/- 0.1 ou +/- 10% 
## par exemple avec n=10000 la précision est +/_ 0.01
## Avec n=10000000 la précision est +/_ 0.001 soit +/- 1% 
## ie la moyenne empirique d'un echantillon de 100 d'une expérience de Bernoulli
## (taux d'un sondage, ...) est juste à +/-10% près avec conf de 95%
## ie la moyenne empirique d'un echantillon de 10.000 d'une expérience de Bernoulli
## (taux d'un sondage, ...) est juste à +/-1% près avec conf de 95%

# precision pour les loi binomiales avec echantillon de taille 10 100 1000, ... 
round(1 / sqrt(10 ^ (1 : 6)), 3)

## Exemple d'un sondage avec n=100 et phats=56
.56 + c(-1, 1) / sqrt(100)      # Approximation forte
.56 + c(-1, 1) * qnorm(.975) * sqrt(.56 * .44 / 100) # Calcul de WALD en remplacant p par p^
binom.test(56, 100)$conf.int    # exact avec loi ... pas approx CLT

# Simulation
n <- 20
pvals <- seq(.1, .9, by = .05)
pvals
nosim <- 1000
coverage <- sapply(pvals, function(p){
        phats <- rbinom(nosim, prob = p, size = n) / n
        ll <- phats - qnorm(.975) * sqrt(phats * (1 - phats) / n)
        ul <- phats + qnorm(.975) * sqrt(phats * (1 - phats) / n)
        mean(ll < p & ul > p)
})

ggplot(data.frame(pvals, coverage), aes(x = pvals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95) + ylim(.75, 1.0)

pvals
coverage

## NB le test "un intervalle de Wald donné contienne p" est 
## une expérience de Bernoulli....donc avec un echantillon de 1000
## on a une precision proche de +/-3% avec (confiance 95%)
## avec 10000, +/- 1%
1/sqrt(1000)


n <- 20
pvals <- seq(.01, .99, by = .001)
pvals
nosim <- 10000 # precision +/- 1% avec confiance 95%
coverage <- sapply(pvals, function(p){
        phats <- rbinom(nosim, prob = p, size = n) / n
        ll <- phats - qnorm(.975) * sqrt(phats * (1 - phats) / n)
        ul <- phats + qnorm(.975) * sqrt(phats * (1 - phats) / n)
        mean(ll < p & ul > p)
})

ggplot(data.frame(pvals, coverage), aes(x = pvals, y = coverage)) + geom_line(size = 1) + geom_hline(yintercept = 0.95) + ylim(0, 1.0)


## Quand p est proche de 0 ou de 1, la couverture est <<

## Test de l'intervalle phats +/- 1/ sqrt(n) pour n > 100 (largeur de l'int de 2*0.1)

rbinom(10, prob = 0.1, size = 1) 
1/sqrt(100)
n <- 100
pvals <- seq(.01, .99, by = .001)
nosim <- 10000 # precision +/- 1% avec confiance 95%
coverage <- sapply(pvals, function(p){
        phats <- rbinom(nosim, prob = p, size = n) / n
        ll <- phats -  sqrt(1 / n)
        ul <- phats +  sqrt(1 / n)
        mean(ll < p & ul > p)
})
ggplot(data.frame(pvals, coverage), aes(x = pvals, y = coverage)) + geom_line(size = 1) + geom_hline(yintercept = 0.95) + ylim(0.90, 1.0)

n <- 100
pvals <- seq(.49, .51, by = .001)
nosim <- 1000000 # precision +/- 0.1% avec confiance 95%
coverage <- sapply(pvals, function(p){
        phats <- rbinom(nosim, prob = p, size = n) / n
        ll <- phats -  sqrt(1 / n)
        ul <- phats +  sqrt(1 / n)
        mean(ll < p & ul > p)
})
ggplot(data.frame(pvals, coverage), aes(x = pvals, y = coverage)) + geom_line(size = 1) + geom_hline(yintercept = 0.95) + ylim(0.925, 0.975)

# Donc il extiste des valeurs de p (par ex 0.5) pour lequelles phats +/- 1/sqrt(n) est un intervalle de confiance à moins de 95%

n <- 1000
pvals <- seq(.01, .99, by = .001)
pvals
nosim <- 10000 # precision +/- 1% avec confiance 95%
coverage <- sapply(pvals, function(p){
        phats <- rbinom(nosim, prob = p, size = n) / n
        ll <- phats -  sqrt(1 / n)
        ul <- phats +  sqrt(1 / n)
        mean(ll < p & ul > p)
})
ggplot(data.frame(pvals, coverage), aes(x = pvals, y = coverage)) + geom_line(size = 1) + geom_hline(yintercept = 0.95) + ylim(0.9, 1.0)


#--------------------



n <- 100
pvals <- seq(.1, .9, by = .05)
nosim <- 1000
coverage2 <- sapply(pvals, function(p){
        phats <- rbinom(nosim, prob = p, size = n) / n
        ll <- phats - qnorm(.975) * sqrt(phats * (1 - phats) / n)
        ul <- phats + qnorm(.975) * sqrt(phats * (1 - phats) / n)
        mean(ll < p & ul > p)
})

ggplot(data.frame(pvals, coverage2), aes(x = pvals, y = coverage2)) + geom_line(size = 2) + geom_hline(yintercept = 0.95)+ ylim(.75, 1.0)
pvals
coverage2

## graph preécis à +/- 1%
n <- 100
pvals <- seq(.001, .999, by = .001)
nosim <- 10000 # precision +/- 1% confiance 95 %
coverage2 <- sapply(pvals, function(p){
        phats <- rbinom(nosim, prob = p, size = n) / n
        ll <- phats - qnorm(.975) * sqrt(phats * (1 - phats) / n)
        ul <- phats + qnorm(.975) * sqrt(phats * (1 - phats) / n)
        mean(ll < p & ul > p)
})

ggplot(data.frame(pvals, coverage2), aes(x = pvals, y = coverage2)) + geom_line(size = 1) + geom_hline(yintercept = 0.95)+ ylim(0, 1.0)


n <- 20; pvals <- seq(.1, .9, by = .05); nosim <- 1000
coverage <- sapply(pvals, function(p){
        phats <- (rbinom(nosim, prob = p, size = n) + 2) / (n + 4)
        ll <- phats - qnorm(.975) * sqrt(phats * (1 - phats) / n)
        ul <- phats + qnorm(.975) * sqrt(phats * (1 - phats) / n)
        mean(ll < p & ul > p)
})

ggplot(data.frame(pvals, coverage), aes(x = pvals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95)+ ylim(.75, 1.0)

## avec précision
n <- 20; pvals <- seq(.001, .999, by = .001); nosim <- 10000
coverage <- sapply(pvals, function(p){
        phats <- (rbinom(nosim, prob = p, size = n) + 2) / (n + 4)
        ll <- phats - qnorm(.975) * sqrt(phats * (1 - phats) / n)
        ul <- phats + qnorm(.975) * sqrt(phats * (1 - phats) / n)
        mean(ll < p & ul > p)
})

ggplot(data.frame(pvals, coverage), aes(x = pvals, y = coverage)) + geom_line(size = 1) + geom_hline(yintercept = 0.95)+ ylim(.95, 1.0)


## Poisson : nuclear pump failed 5 times over 94,32 days
## 95% CI for failure rate?
## X Poisson(lambda*t)
## Var(X) = lambda*t
## Estimate lambda = X/t
## Var(lambda) = Var(X/t)=Var(X)/t^2=lambda/t
x <- 5
t <- 94.32
lambda <- x / t
round(lambda + c(-1, 1) * qnorm(.975) * sqrt(lambda / t), 3)
poisson.test(x, T = 94.32)$conf # exacte CI avec loi


lambdavals <- seq(0.005, 0.10, by = .01); nosim <- 1000
t <- 100
coverage <- sapply(lambdavals, function(lambda){
        lhats <- rpois(nosim, lambda = lambda * t) / t
        ll <- lhats - qnorm(.975) * sqrt(lhats / t)
        ul <- lhats + qnorm(.975) * sqrt(lhats / t)
        mean(ll < lambda & ul > lambda)
})

ggplot(data.frame(lambdavals, coverage), aes(x = lambdavals, y = coverage)) + geom_line(size = 2) + geom_hline(yintercept = 0.95)+ylim(0, 1.0)

lambdavals <- seq(0.005, 0.10, by = .001); nosim <- 10000 # precision +/-1%
t <- 100
coverage <- sapply(lambdavals, function(lambda){
        lhats <- rpois(nosim, lambda = lambda * t) / t
        ll <- lhats - qnorm(.975) * sqrt(lhats / t)
        ul <- lhats + qnorm(.975) * sqrt(lhats / t)
        mean(ll < lambda & ul > lambda)
})

ggplot(data.frame(lambdavals, coverage), aes(x = lambdavals, y = coverage)) + geom_line(size = 1) + geom_hline(yintercept = 0.95)+ylim(0, 1.0)

lambdavals <- seq(0.005, 0.10, by = .01); nosim <- 1000
t <- 1000
coverage <- sapply(lambdavals, function(lambda){
        lhats <- rpois(nosim, lambda = lambda * t) / t
        ll <- lhats - qnorm(.975) * sqrt(lhats / t)
        ul <- lhats + qnorm(.975) * sqrt(lhats / t)
        mean(ll < lambda & ul > lambda)
})
ggplot(data.frame(lambdavals, coverage), aes(x = lambdavals, y = coverage)) + geom_line(size = 1) + geom_hline(yintercept = 0.95) + ylim(0, 1.0)


lambdavals <- seq(0.0001, 0.10, by = .0001); nosim <- 10000
t <- 1000
coverage <- sapply(lambdavals, function(lambda){
        lhats <- rpois(nosim, lambda = lambda * t) / t
        ll <- lhats - qnorm(.975) * sqrt(lhats / t)
        ul <- lhats + qnorm(.975) * sqrt(lhats / t)
        mean(ll < lambda & ul > lambda)
})
ggplot(data.frame(lambdavals, coverage), aes(x = lambdavals, y = coverage)) + geom_line(size = 1) + geom_hline(yintercept = 0.95) + ylim(0.5, 1.0)


# Etonnant, finalement n=1 ... pour utiliser l'approx du CLT .... !!!
# donc si lamba n'est pas trop petit, l'approx utilisant CLT pour construire CI
# fonctionne.... en fait c'est surtout 1.96... qui influ...var et mu indep...

# Mais évidemment l'intervalle est large avec un seul exemple, ...

## En résumé, l'approx Estimate +/-2 SEest fonctionne souvent bien pour donner 95% CI
## Dans les cas Binomial avec p proche de 0 ou de 1 et Poisson avec lambda <<..
## Utiliser les formiles extactes prennant en compte la loi
## ou pour le cas bin ajouter 2 succès sur 4 exp...



#-------------------------------------------------------------------------------
# T confidence interval
#-------------------------------------------------------------------------------

library(ggplot2)
library(manipulate)
k <- 1000
xvals <- seq(-5, 5, length = k)
myplot <- function(df){
        d <- data.frame(y = c(dnorm(xvals), dt(xvals, df)),
                        x = xvals,
                        dist = factor(rep(c("Normal", "T"), c(k,k))))
        g <- ggplot(d, aes(x = x, y = y)) 
        g <- g + geom_line(size = 2, aes(colour = dist))
        g
}
manipulate(myplot(mu), mu = slider(1, 20, step = 1))  

# T distribution toujours à queues plus épaisses

# Comparaison des quantiles > 50% de t distrib et n distrib
pvals <- seq(.5, .99, by = .01)
myplot2 <- function(df){
        d <- data.frame(n= qnorm(pvals),t=qt(pvals, df),
                        p = pvals)
        g <- ggplot(d, aes(x= n, y = t))
        g <- g + geom_abline(size = 2, col = "lightblue")
        g <- g + geom_line(size = 2, col = "black")
        g <- g + geom_vline(xintercept = qnorm(0.975))
        g <- g + geom_hline(yintercept = qt(0.975, df))
        g
}
manipulate(myplot2(df), df = slider(1, 20, step = 1))



data(sleep)
head(sleep)
summary(sleep)

mean1 <- mean(sleep[sleep$group ==1,]$extra)
mean2 <- mean(sleep[sleep$group ==2,]$extra)
mean1;mean2;mean2-mean1

g <- ggplot(sleep, aes(x = group, y = extra, group = factor(ID)))
g <- g + geom_line(size = 1, aes(colour = ID)) + geom_point(size =10, pch = 21, fill = "salmon", alpha = .5)
g

g1 <- sleep$extra[1 : 10]
g2 <- sleep$extra[11 : 20]
difference <- g2 - g1
mn <- mean(difference)
s <- sd(difference)
n <- 10

## Intervalle de confiance de la moyenne de la différence à 95%
mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)
t.test(difference)      # df degree of freedom n-1=9
pt(q = 4.0621,df = 9,lower.tail = FALSE)
pt(q = 4.0621,df = 9,lower.tail = FALSE)*2
# p-value two sided, ...

t.test(g2, g1, paired = TRUE) # g2 avec g1 en ref
t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)
?relevel
t.test(extra ~ group, paired = TRUE, data = sleep) # prend en référence le groupe 1

rbind(
        mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n),
        as.vector(t.test(difference)$conf.int),
        as.vector(t.test(g2, g1, paired = TRUE)$conf.int),
        as.vector(t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)$conf.int)
)



t.test(g2, g1, paired = TRUE)$conf.int
t.test(g2, g1, paired = FALSE)$conf.int
t.test(g2, g1, paired = FALSE,var.equal = FALSE)$conf.int


## Based on Rosner, Fundamentals of Biostatistics
## Deux groupes OC 8 et Control 21
## sd1 15.34 sd2 18.23
## Mean1 132.86, mean control 127.44
## Pooled standard deviation
sp <- sqrt((7 * 15.34^2 + 20 * 18.23^2) / (8 + 21 - 2))
132.86 - 127.44 + c(-1, 1) * qt(.975, 27) * sp * (1 / 8 + 1 / 21)^.5

n1 <- length(g1); n2 <- length(g2)
sp <- sqrt( ((n1 - 1) * sd(g1)^2 + (n2-1) * sd(g2)^2) / (n1 + n2-2))
md <- mean(g2) - mean(g1)
semd <- sp * sqrt(1 / n1 + 1/n2)
rbind(
        md + c(-1, 1) * qt(.975, n1 + n2 - 2) * semd,  
        t.test(g2, g1, paired = FALSE, var.equal = TRUE)$conf,
        t.test(g2, g1, paired = TRUE)$conf
)
# var.equal.... id en tre les deux groupes, ... hyp

g <- ggplot(sleep, aes(x = group, y = extra, group = factor(ID)))
g <- g + geom_line(size = 1, aes(colour = ID)) + geom_point(size =10, pch = 21, fill = "salmon", alpha = .5)
g


library(datasets); data(ChickWeight); library(reshape2)
##define weight gain or loss
ChickWeight
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
names(wideCW)[-(1 : 2)] <- paste("time", names(wideCW)[-(1 : 2)], sep = "")
library(dplyr)
wideCW <- mutate(wideCW,
                 gain = time21 - time0
)


g <- ggplot(ChickWeight, aes(x = Time, y = weight, 
                             colour = Diet, group = Chick))
g <- g + geom_line()
g <- g + stat_summary(aes(group = 1), geom = "line", fun.y = mean, size = 1, col = "black")
g <- g + facet_grid(. ~ Diet)
g


g <- ggplot(wideCW, aes(x = factor(Diet), y = gain, fill = factor(Diet)))
g <- g + geom_violin(col = "black", size = 2)
g

wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
rbind(
        t.test(gain ~ Diet, paired = FALSE, var.equal = TRUE, data = wideCW14)$conf,
        t.test(gain ~ Diet, paired = FALSE, var.equal = FALSE, data = wideCW14)$conf
)

# Hypothesis testing
library(UsingR); data(father.son)
t.test(father.son$sheight - father.son$fheight)


difference <- father.son$sheight - father.son$fheight
mn <- mean(difference)
sd <- sd(difference)
qt(.975,1077)
sqrt(1078)*(mn-0)/sd

## we reject the null hypothesis with 11.78 >> 1.96
## les chancces que la diff ne soit pas nulle sont > 5%



library(datasets); data(ChickWeight); library(reshape2)
##define weight gain or loss
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
names(wideCW)[-(1 : 2)] <- paste("time", names(wideCW)[-(1 : 2)], sep = "")
library(dplyr)
wideCW <- mutate(wideCW,
                 gain = time21 - time0
)


wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
t.test(gain ~ Diet, paired = FALSE, 
       var.equal = TRUE, data = wideCW14)

#-------------------------------------------------------------------------------
# Quizz 3
#-------------------------------------------------------------------------------

# Question 1 : In a population of interest, a sample of 9 men yielded a sample 
# average brain volume of 1,100cc and a standard deviation of 30cc. 
# What is a 95% Student's T confidence interval 
# for the mean brain volume in this new population?

1100 + c(-1,1) * qt(p = 0.975,df = 8) * 30 / sqrt(9)


# Question 2 : A diet pill is given to 9 subjects over six weeks. 
# The average difference in weight (follow up - baseline) is -2 pounds. 
# What would the standard deviation of the difference in weight have to be 
# for the upper endpoint of the 95% T confidence interval to touch 0?

# -2 + qt(p = 0.975,df = 8) * sd / sqrt(9) =0
sd <- sqrt(9)*2/ qt(p = 0.975,df = 8)
sd

# Question 3 : In an effort to improve running performance, 
# 5 runners were either given a protein supplement or placebo. 
# Then, after a suitable washout period, they were given the opposite treatment. 
# Their mile times were recorded under both the treatment and placebo, 
# yielding 10 measurements with 2 per subject. The researchers intend to use a 
# T test and interval to investigate the treatment. 
# Should they use a paired or independent group T test and interval?

# Paired

# Question 4 : In a study of emergency room waiting times, investigators consider 
# a new and the standard triage systems. To test the systems, administrators 
# selected 20 nights and randomly assigned the new triage system to be used on 
# 10 nights and the standard system on the remaining 10 nights. They calculated 
# the nightly median waiting time (MWT) to see a physician. The average MWT for 
# the new system was 3 hours with a variance of 0.60 while the average MWT for 
# the old system was 5 hours with a variance of 0.68. Consider the 95% confidence 
# interval estimate for the differences of the mean MWT associated with the new system. 
# Assume a constant variance. What is the interval? 
# Subtract in this order (New System - Old System).

pv <- (9*0.60 + 9*0.68)/(18)
sqrt(pv)
semd <- sqrt(pv)*sqrt(1/10+1/10)
3 - 5 + c(-1,1)*qt(p = 0.975,df = 18)*semd
???????


# Question 5 : Suppose that you create a 95% T confidence interval. 
# You then create a 90% interval using the same data. 
# What can be said about the 90% interval with respect to the 95% interval?


# Question 6 : To further test the hospital triage system, administrators 
# selected 200 nights and randomly assigned a new triage system to be used on 
# 100 nights and a standard system on the remaining 100 nights. 
# They calculated the nightly median waiting time (MWT) to see a physician. 
# The average MWT for the new system was 4 hours with a standard deviation of 
# 0.5 hours while the average MWT for the old system was 6 hours with a standard 
# deviation of 2 hours. Consider the hypothesis of a decrease in the mean MWT 
# associated with the new treatment. What does the 95% independent group 
# confidence interval with unequal variances suggest vis a vis this hypothesis? 
# (Because there's so many observations per group, just use the Z quantile instead of the T.)


semd <- sqrt((99*0.5^2+99*2^2)/(198))*sqrt(1/100+1/100)
6-4 + c(-1,1)*qnorm(p = 0.975)*semd


n1 <- n2 <- 100
xbar1 <- 4
xbar2 <- 6
s1 <- 0.5
s2 <- 2
xbar2 - xbar1 + c(-1, 1) * qnorm(0.975) * sqrt(s1^2/n1 + s2^2/n2)


# Question 7 : Suppose that 18 obese subjects were randomized, 
# 9 each, to a new diet pill and a placebo. Subjects’ body mass indices (BMIs) 
# were measured at a baseline and again after having received the treatment or 
# placebo for four weeks. The average difference from follow-up to the baseline 
# (followup - baseline) was −3 kg/m2 for the treated group and 1 kg/m2 for the 
# placebo group. The corresponding standard deviations of the differences 
# was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. 
# Does the change in BMI over the four week period appear to differ between 
# the treated and placebo groups? Assuming normality of the underlying data and 
# a common population variance, calculate the relevant *90%* t confidence interval. 

pv <- (8*1.5^2 + 8*1.8^2)/(16)
semd <- sqrt(pv)*sqrt(1/9+1/9)
-3 - 1 + c(-1,1)*qt(p = 0.95,df = 16)*semd


# Subtract in the order of (Treated - Placebo) with the smaller (more negative) number first.


# Homework 3
data(mtcars)
str(mtcars)
# 95% confidence interval for MPD (to the nearest MPG)
summary(mtcars$mpg)
# Sample mean and standard deviation of the dataset
mn <- mean(mtcars$mpg)
print(paste("Sample mean : ",round(mn,2)))
sd <- sd(mtcars$mpg)
print(paste("Sample sd : ",round(sd,2)))

# Sample standard error of the mean
n <- length(mtcars$mpg)
print(paste("size n = ",n))
sem <- sd/sqrt(n)
print(paste("Sample standard error of the mean :",round(sem,2)))
# 32 observations -> use t quantile
# NB on supposze la distribution symetrique, et en forme de tas, ...
CI <- mn + c(-1,1)*qt(p = 0.975,df = n-1)*sem
CI
# Arrondi
round(CI)

# Avec t.test
t.test(mtcars$mpg)
t.test(mtcars$mpg)$conf.int
round(t.test(mtcars$mpg)$conf.int)

# Suppose sd of 9 pairs diff is 1
# Average diff tq 95% Students t confidence interval upper limit =0  :
qt(p = 0.975,df = 8)*1/sqrt(9)


# comparaison de groupes indpendants
# cyl 4 et 6
g6 <- mtcars[mtcars$cyl==6,]$mpg
g4 <- mtcars[mtcars$cyl==4,]$mpg
g6
g4
n6 <- length(g6)
n4 <- length(g4)
n6
n4
mean(g6)
mean(g4)
# Sample mean difference
smd <- mean(g4) - mean(g6)
smd
# 95% T interval (assuming constant variance)
t.test(g4,g6,paired = FALSE,var.equal = TRUE)
# Manuellement
pv <- ((n6-1)*sd(g6)^2+(n4-1)*sd(g4)^2)/(n6+n4-2)
smd + c(-1,1)*qt(p = 0.975,df = n6+n4-2)*sqrt(pv)*sqrt(1/n4+1/n6)


# Pooled variance estimate

pv <- (8*1.5^2+8*1.8^2)/16
pv

#-------------------------------------------------------------------------------
# Week 4
#-------------------------------------------------------------------------------

## Calculating power for Gaussian data
alpha = 0.05
z = qnorm(1 - alpha)
pnorm(mu0 + z * sigma / sqrt(n), mean = mua, sd = sigma / sqrt(n), 
      lower.tail = FALSE)


mu0 = 30; mua = 32; sigma = 4; n = 16
z = qnorm(1 - alpha) # 95th quantile of normal distribution
pnorm(mu0 + z * sigma / sqrt(n), mean = mu0, sd = sigma / sqrt(n), 
      lower.tail = FALSE)
pnorm(mu0 + z * sigma / sqrt(n), mean = mua, sd = sigma / sqrt(n), 
      lower.tail = FALSE)



library(ggplot2)
nseq = c(8, 16, 32, 64, 128)
mua = seq(30, 35, by = 0.1)
z = qnorm(.95)
power = sapply(nseq, function(n)
        pnorm(mu0 + z * sigma / sqrt(n), mean = mua, sd = sigma / sqrt(n), 
              lower.tail = FALSE)
)
colnames(power) <- paste("n", nseq, sep = "")
d <- data.frame(mua, power)
library(reshape2)
d2 <- melt(d, id.vars = "mua")
names(d2) <- c("mua", "n", "power")    
g <- ggplot(d2, 
            aes(x = mua, y = power, col = n)) + geom_line(size = 2)
g            


library(manipulate)
mu0 = 30
myplot <- function(sigma, mua, n, alpha){
        g = ggplot(data.frame(mu = c(27, 36)), aes(x = mu))
        g = g + stat_function(fun=dnorm, geom = "line", 
                              args = list(mean = mu0, sd = sigma / sqrt(n)), 
                              size = 2, col = "red")
        g = g + stat_function(fun=dnorm, geom = "line", 
                              args = list(mean = mua, sd = sigma / sqrt(n)), 
                              size = 2, col = "blue")
        xitc = mu0 + qnorm(1 - alpha) * sigma / sqrt(n)
        g = g + geom_vline(xintercept=xitc, size = 3)
        g
}
manipulate(
        myplot(sigma, mua, n, alpha),
        sigma = slider(1, 10, step = 1, initial = 4),
        mua = slider(30, 35, step = 1, initial = 32),
        n = slider(1, 50, step = 1, initial = 16),
        alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05)
)


## Example

power.t.test(n = 16, delta = 2 / 4, sd=1, type = "one.sample",  alt = "one.sided")$power
power.t.test(n = 16, delta = 2, sd=4, type = "one.sample",  alt = "one.sided")$power
power.t.test(n = 16, delta = 100, sd=200, type = "one.sample", alt = "one.sided")$power

power.t.test(power = .8, delta = 2 / 4, sd=1, type = "one.sample",  alt = "one.sided")$n
power.t.test(power = .8, delta = 2, sd=4, type = "one.sample",  alt = "one.sided")$n
power.t.test(power = .8, delta = 100, sd=200, type = "one.sample", alt = "one.sided")$n

power.t.test(power = .8, n = 27, sd=200, type = "one.sample", alt = "one.sided")$delta




y <- rnorm(20)
x <- rnorm(20)
summary(lm(y ~ x))

summary(lm(y ~ x))$coeff[2,4]



set.seed(1010093)
pValues <- rep(NA,1000)
for(i in 1:1000){
        y <- rnorm(20)
        x <- rnorm(20)
        pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}

# Controls false positive rate
sum(pValues < 0.05)



set.seed(1010093)
pValues <- rep(NA,1000)
for(i in 1:1000){
        x <- rnorm(20)
        # First 500 beta=0, last 500 beta=2
        if(i <= 500){y <- rnorm(20)}else{ y <- rnorm(20,mean=2*x)}
        pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}
trueStatus <- rep(c("zero","not zero"),each=500)
table(pValues < 0.05, trueStatus)


x <- rnorm(20)
y <- rnorm(20,mean=2*x)
summary(lm(y ~ x))$coeff[2,4]


x <- rnorm(20)
y <- rnorm(20)
summary(lm(y ~ x))$coeff[2,4]



# Controls FWER 
table(p.adjust(pValues,method="bonferroni") < 0.05,trueStatus)
# Controls FDR 
table(p.adjust(pValues,method="BH") < 0.05,trueStatus)

par(mfrow=c(1,2))
plot(pValues,p.adjust(pValues,method="bonferroni"),pch=19)
plot(pValues,p.adjust(pValues,method="BH"),pch=19)



# Resampling

library(ggplot2)
library(gridExtra)
nosim <- 1000

cfunc <- function(x, n) mean(x)
g1 = ggplot(data.frame(y = rep(1/6, 6), x = 1 : 6), aes(y = y, x = x))
g1 = g1 + geom_bar(stat = "identity", fill = "lightblue", colour = "black")

dat <- data.frame(x = apply(matrix(sample(1 : 6, nosim * 50, replace = TRUE), 
                                   nosim), 1, mean))
g2 <- ggplot(dat, aes(x = x)) + geom_histogram(binwidth=.2, colour = "black", fill = "salmon", aes(y = ..density..)) 

grid.arrange(g1, g2, ncol = 2)


n = 50
B = 1000
## our data
x = sample(1 : 6, n, replace = TRUE)
## bootstrap resamples
resamples = matrix(sample(x,
                          n * B,
                          replace = TRUE),
                   B, n)
resampledMeans = apply(resamples, 1, mean)
g1 <- ggplot(as.data.frame(prop.table(table(x))), aes(x = x, y = Freq)) + geom_bar(colour = "black", fill = "lightblue", stat = "identity") 
g2 <- ggplot(data.frame(x = resampledMeans), aes(x = x)) + geom_histogram(binwidth=.2, colour = "black", fill = "salmon", aes(y = ..density..)) 
grid.arrange(g1, g2, ncol = 2)



library(UsingR)
data(father.son)
x <- father.son$sheight
n <- length(x)
B <- 10000
median(x)
resamples <- matrix(sample(x,
                           n * B,
                           replace = TRUE),
                    B, n)
dim(resamples)
resampledMedians <- apply(resamples, 1, median)
length(resampledMedians)
mean(resampledMedians)

g = ggplot(data.frame(x = resampledMedians), aes(x = x)) 
g = g + geom_density(size = 2, fill = "red")
#g = g + geom_histogram(alpha = .20, binwidth=.3, colour = "black", fill = "blue", aes(y = ..density..)) 
g = g + geom_vline(xintercept = median(x), size = 2)
g


B <- 10000
resamples <- matrix(sample(x,
                           n * B,
                           replace = TRUE),
                    B, n)
medians <- apply(resamples, 1, median)
sd(medians)
quantile(medians, c(.025, .975))


g = ggplot(data.frame(medians = medians), aes(x = medians))
g = g + geom_histogram(color = "black", fill = "lightblue", binwidth = 0.05)
g


data(InsectSprays)
g = ggplot(InsectSprays, aes(spray, count, fill = spray))
g = g + geom_boxplot()
g

subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"),]
y <- subdata$count
group <- as.character(subdata$spray)
testStat <- function(w, g) mean(w[g == "B"]) - mean(w[g == "C"])
observedStat <- testStat(y, group)
permutations <- sapply(1 : 10000, function(i) testStat(y, sample(group)))
observedStat
mean(permutations > observedStat)

g = ggplot(data.frame(permutations = permutations),
           aes(permutations))
g = g + geom_histogram(fill = "lightblue", color = "black", binwidth = 1)
g = g + geom_vline(xintercept = observedStat, size = 2)
g


#-------------------------------------------------------------------------------
# Homework 4
#-------------------------------------------------------------------------------
data(mtcars)
mean <- mean(mtcars$mpg)
sd <- sd(mtcars$mpg)
n <- length((mtcars$mpg))
mu0 <- mean - qnorm(0.05) * sd / sqrt(n)
mu0 <- round(mu0,2)

boxplot(mpg~cyl,data = mtcars )
g1 <- mtcars[mtcars$cyl == 4,]$mpg
g2 <- mtcars[mtcars$cyl == 6,]$mpg
t.test(g2,g1, paired = FALSE,var.equal = FALSE)

3 + qnorm(0.025)*1.1/10
3 + qnorm(0.975)*1.1/10

# tester H0 p=0.5 ou Ha P> 0.5
pbinom(54,100,prob = 0.5, lower.tail = FALSE)
# approx avec CLT
pnorm(0.55, mean = 0.5, sd = sqrt(0.25)/10,lower.tail = FALSE)

# Exact one sided P-value
ppois(q = 15800-1, lambda = 520*30,lower.tail = FALSE)
pnorm(15800, mean = 520*30,sd = sqrt(520*30), lower.tail = FALSE)

m1 <- 10
m2 <- 11
dif <- 11-10
sp <- 4
se <- 4 * sqrt(1/100+1/100)
z <- dif/se

# Power
mean <- 10
sd <- 4
n <- 100

# Power for a 5% one sided Z mean test if mean is 11?

# Stastistique (Xb - mean)/(sd/sqrt(n)
qnorm(p = 0.95)
10+qnorm(p = 0.95)*4/10
x1 <- 10+qnorm(p = 0.95)*4/10


pnorm(q = x1,mean = 10, sd = 4/10, lower.tail = FALSE)

pnorm(q = x1,mean = 11, sd = 4/10, lower.tail = FALSE)
# C'est la probabilité de rejeter si mu =11
# avec student
power.t.test(n = 100, delta = 1, sd=4, type = "one.sample",  alt = "one.sided",sig.level = 0.05)$power

power <- pnorm(10 + qnorm(.95) * .4, mean = 11, sd = .4, lower.tail = FALSE)

# Researchers would like to conduct a study of healthy adults to detect a four 
# year mean brain volume loss of .01 mm3.Assume that the standard deviation of 
# four year volume loss in this population is .04 mm3.
# What is necessary sample size for the study for a 5% one sided test versus a 
# null hypothesis of no volume loss to achieve 80% power? (Always round up)


# H0 mu = 0, Ha mu > 0
mu0 <- 0
mu1 <- 0.01
sd <- 0.04
power <- 0.8
# Z test...
# mean ~ N (0, 0.04/sqrt(n))
# We reject if mean is out of the tail ie 
# ie : mean > 0 + qnorm(0.95)*0.004/srqt(n)

qnorm(0.95)
# Probabilité d'avoir plus que cette limite sous l'hypothèses alternative 0.1
n <- 100
pnorm(q = qnorm(0.95)*0.04/sqrt(n),mean = 0.01,sd = 0.04/sqrt(n),lower.tail = FALSE)

n <- (qnorm(.95) + qnorm(.8)) ^ 2 * .04 ^ 2 / .01^2

data(mtcars)
# Give the p-value for a t-test comparing MPG for 6 and 8 cylinder cars assuming 
# equal variance, as a proportion to 3 decimal places.
c6 <- mtcars[mtcars$cyl == 6,]$mpg
c8 <- mtcars[mtcars$cyl == 8,]$mpg

mean(c6)
mean(c8)
sd(c6)
sd(c8)
length(c6)
length(c8)

t.test(c6,c8,var.equal = TRUE)


# a la main
pv <- (sd(c6)^2*(length(c6)-1)+sd(c8)^2*(length(c8)-1))/(length(c6)+length(c8)-2)
sqrt(pv)
# p-value : proba d'avoir plus extrême que mean(c6)-mean(c8)
mean <- mean(c6)-mean(c8)
# pv = 
pt(4.419,df = 19,lower.tail = FALSE)*2
mean
# t statistique
t <- mean/sqrt(1/7+1/14)/sqrt(pv)

pt(q = t,df = 19,lower.tail = FALSE)+pt(q = -t,df = 19,lower.tail = TRUE)

# Assotiated p value for a z test
# (dif-0) / (s/sqrt(n)) ~ N(0,1)
pnorm(q = t,lower.tail = FALSE)*2


sd <- sd(mtcars$mpg)

#-------------------------------------------------------------------------------
# Quizz 4
#-------------------------------------------------------------------------------

# Question 1 - P-value
diff <- c(-8,-3,1,-2,-5)
# Test t de Student adapté aux différences ...
t.test(diff)
g1 <- c(140,138,150,148,135)
g2 <- c(132,135,151,146,130)
g2-g1

t.test(g2,g1,paired = TRUE,var.equal = TRUE)

# Calcul manuel de la p-value
# la statistique
t <- (mean(diff)-0)/(sd(diff)/sqrt(5))
# P-value : ie probabilité d'obtenir plus extrême en val abs
pt(t,df = 4)*2
# 8%

# Question 2
# A sample of 9 men yielded a sample average brain volume of 1,100cc 
# and a standard deviation of 30cc. What is the complete set of values of μ0 
# that a test of H0:μ=μ0 would fail to reject the null hypothesis 
# in a two sided 5% Students t-test?
mean <- 1100
sd <- 30 
# c'est l'intervalle de confiance du mean à 95%
mean + c(-1,1) * qt(p = 0.975,df = 8)*sd/sqrt(9)

# !!!! avec 10%
mean + c(-1,1) * qt(p = 0.95,df = 8)*sd/sqrt(9)


# Question 3 
# Researchers conducted a blind taste test of Coke versus Pepsi. 
# Each of four people was asked which of two blinded drinks given in random 
# order that they preferred. The data was such that 3 of the 4 people chose Coke. 
# Assuming that this sample is representative, report a P-value for a test of 
# the hypothesis that Coke is preferred to Pepsi using a one sided exact test.

# c'est une experience de bernoulli
# H0 p = 0.5, Ha p > 0.5 (en en faveur de Coke)
pbinom(q = 2 ,size = 4,prob = 0.5 ,lower.tail = FALSE)
pbinom(q = 3 ,size = 4,prob = 0.5 )
?pbinom()


# Question 4
# Infection rates at a hospital above 1 infection per 100 person days at risk are
# believed to be too high and are used as a benchmark. A hospital that had 
# previously been above the benchmark recently had 10 infections over the last 
# 1,787 person days at risk. About what is the one sided P-value for the relevant 
# test of whether the hospital is *below* the standard?

# One sided test : H0 at standard ie 17.87, Ha < 17.87
ppois(q = 10,lambda = 17.87)

# Question 5
# Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and 
# a placebo. Subjects’ body mass indices (BMIs) were measured at a baseline and 
# again after having received the treatment or placebo for four weeks. The average 
# difference from follow-up to the baseline (followup - baseline) was −3 kg/m2 
# for the treated group and 1 kg/m2 for the placebo group. The corresponding standard 
# deviations of the differences was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 
# for the placebo group. Does the change in BMI appear to differ between the treated 
# and placebo groups? Assuming normality of the underlying data and a common 
# population variance, give a pvalue for a two sided t test.

# p-value
sp <- sqrt((1.5^2+1.8^2)/2)
t <- (1+3) / sp / sqrt(1/9+1/9)
?pt
pt(q = t,df = 16,lower.tail = FALSE)*2


# Question 7
# Researchers would like to conduct a study of 100 healthy adults to detect a 
# four year mean brain volume loss of .01 mm3. Assume that the standard deviation 
# of four year volume loss in this population is .04 mm3. About what would be the 
# power of the study for a 5% one sided test versus a null hypothesis of no volume loss?

mu0 <- 0
mu1 <- 0.01
sd <- 0.04
n <- 100


qt(0.95,df = 99)
qnorm(0.95)

qt(0.95,df = 99)*0.04/sqrt(100)
qnorm(0.95)*0.04/sqrt(100)
# Approx avec normal pour n=100
power <- pnorm(q = qnorm(0.95)*0.04/sqrt(100),mean = 0.01,sd = 0.04/sqrt(n), lower.tail = FALSE)

# Student's
power.t.test(n = 100, delta = 0.01, sd=0.04, type = "one.sample",  alt = "one.sided",sig.level = 0.05)$power



# Question 8
# Researchers would like to conduct a study of n healthy adults to detect a four 
# year mean brain volume loss of .01 mm3. Assume that the standard deviation of 
# four year volume loss in this population is .04 mm3. About what would be the 
# value of n needded for 90% power of type one error rate of 5% one sided test 
# versus a null hypothesis of no volume loss?

sd <- 0.04
mu1 <- 0.01
# Approx normal
n <- 138
power <- pnorm(q = qnorm(0.95)*0.04/sqrt(n),mean = 0.01, sd = 0.04/sqrt(n), lower.tail = FALSE)
(qnorm(0.95) - qnorm(0.10, mean = 0.01))^2*0.04^2/(0.01)^2



power.t.test(delta = 0.01, sd=0.04, type = "one.sample",  alt = "one.sided",sig.level = 0.05,power = 0.9)$n

