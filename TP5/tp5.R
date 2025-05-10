#Ex1.** Nous voulons construire un test NP. Donner la statistique de test $T(\mathbf{X})$.

#T(X) = \bar{X}

#Ex3

alp <- 0.1
nsimu <- 100
n <-50
mu0 <- 0
mu1<-0.1
sig0 <- 1
Kalpha <- mu0 + (sig0 / sqrt(n)) * qnorm(1-alp)
beta <- pnorm(sqrt(n) * ( (Kalpha - mu1) / sig0 ) )
N_alpha <- c()
N_beta <- c()
for(simu in 1:nsimu){
  X0 <- rnorm(n,mu0,sig0)
  X1 <- rnorm(n,mu1,sig0)
  N_alpha <- append(N_alpha,mean(X0)>Kalpha)
  N_beta <- append(N_beta,mean(X1)>Kalpha)
}
alpha_estim <- mean(N_alpha)
beta_estim <- mean(N_beta)
print(c("estmation de alpha :",alpha_estim))
print(c("estmation de beta :",beta_estim))