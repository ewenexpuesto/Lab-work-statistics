#Simulation et convergence
#Simulation dans R

#Exercice 1

mu = 20
sigma = sqrt(60)

p = 1 - pnorm(35,mu, sigma)

print(c("La probabilité d\'observer une valeur supérieure à 35 est",p))

#Exercice 2

n = 20
mu = 20
sigma = sqrt(60)
x = rnorm(n,mu, sigma)

p_chap = sum(x > 35) / n 

hist(x, probability = TRUE, main = paste("Emp.prob=", p_chap), ylim = c(0, 0.1),
     xlim = c(min(x), max(x)))
## evaluer la densite de la loi normale en grille xx
xx = seq(-10,50,by=1)
yy = dnorm(xx, mean=20, sd=sqrt(60))
## add density plot
lines(xx, yy, col="red")
abline(v=c(20,35), col=c("red","gray"), lty=5)

#Exercice 3 

n = 20
mu = 20
sigma = sqrt(60)

m <- 10
datmat <- matrix(c(0), nrow = n, ncol = m)
for(j in 1:m){
  datmat[,j] = rnorm(n, mean = mu, sd = sigma)
  p_chap = sum(datmat[, j] > 35) / n 
}

## plots en multi panel

ncol(datmat) #
par(mfrow=c(2,2)) # 2 x 2 panel
for (j in 1:4)
{
  hist(datmat[,j], xlim= c(-10,50), ylim = c(0,0.1), probability = TRUE,
       main=paste("histogram of X_",j ), xlab=paste("X_",j))
  xx = seq(-10,50,by=1)
  yy = dnorm(xx, mean=20, sd=sqrt(60))
  ## add density plot
  lines(xx, yy, col="red")
  abline(v=c(20,35), col=c("red","gray"), lty=5)
}

#Conclusion : Elles ne sont pas tout à fait proche de la valeur théorique

#Exercice 4 

hist(x, probability = TRUE, main = paste("Emp.prob=", p_chap), ylim = c(0, 0.1),
     xlim = c(min(x), max(x)))
## evaluer la densite de la loi normale en grille xx
xx = seq(-10,50,by=1)
yy = dnorm(xx, mean=20, sd=sqrt(60))
## add density plot
lines(xx, yy, col="red")
abline(v=c(20,35), col=c("red","gray"), lty=5)

n = 50
mu = 20
sigma = sqrt(60)

m <- 10
datmat <- matrix(c(0), nrow = n, ncol = m)
for(j in 1:m){
  datmat[,j] = rnorm(n, mean = mu, sd = sigma)
  p_chap = sum(datmat[, j] > 35) / n 
}

## plots en multi panel

ncol(datmat) #
par(mfrow=c(2,2)) # 2 x 2 panel
for (j in 1:4)
{
  hist(datmat[,j], xlim= c(-10,50), ylim = c(0,0.1), probability = TRUE,
       main=paste("histogram of X_",j ), xlab=paste("X_",j))
  xx = seq(-10,50,by=1)
  yy = dnorm(xx, mean=20, sd=sqrt(60))
  ## add density plot
  lines(xx, yy, col="red")
  abline(v=c(20,35), col=c("red","gray"), lty=5)
}

#Conclusion : on se rapproche de la courbe rouge qui est la loi N(20,60) qui est la valeur théorique, c'est une application
#du théorème centrale limite

#Estimation - Méthode de Monte Carlo

#Exercice 5

n <- 10

x <- runif(n,-1,1)

x_max <- max(x)
x_min <- min(x)

#Exercice 6

monte_carlo <- function(m) {
  for (i in 1:m) {
    n <- 10
    x <- runif(n,-1,1)
    x_max <- max(x)
    x_min <- min(x)
    print(c("Minimum and maximum",x_min,x_max))
  }
}

monte_carlo(50)

# Les maxima sont quasi tous positifs et souvent proches de 1

#Exercice 7

monte_carlo(100)

m <- 100

datmat_min <- matrix(c(0), nrow=1, ncol=m) #Loi M_n
datmat_max <- matrix(c(0), nrow=1, ncol=m) #Loi N_n

for (i in 1:m) {
  n <- 10
  x <- runif(n,-1,1)
  x_max <- max(x)
  x_min <- min(x)
  datmat_min[,i] <- x_min
  datmat_max[,i] <- x_max
  print(c("Minimum and maximum",x_min,x_max))
}

# La loi des minimums et des maximums ont la même densité que celle de l'exercice 2 du TD 1

# Affichage des graphiques
hist(datmat_min,breaks=20) # breaks pour voir plus précisément
hist(datmat_max,breaks=20)
boxplot(c(datmat_min)) # il faut convertir la liste en un vecteur pour afficher le boxplot correctement
boxplot(c(datmat_max))

# Affichage de la moyenne
mean_min <- mean(datmat_min)
mean_max <- mean(datmat_max)
print(c("Moyenne des minimums",mean_min))
print(c("Moyenne des maximums",mean_max))

hist(datmat_min, probability = TRUE)
curve(5, add = TRUE, color="red") 

#Théorème Central Limite

