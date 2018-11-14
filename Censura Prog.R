 require(rootSolve) #Pacote usando para gerar as amostras censuradas.

 #Informações Iniciais
  n = 50; m = 40           # m é o número de falhas

 p = 0.5; beta = 2 
 R1=c(10,rep(0,39)) #Esquema de Censura Progressiva

########################GERAÇÃO DA AMOSTRA##################################### 
 dados<-matrix(0,amostra,m) #São 100 amostras de tamnho "m"

 #Programa para gerar amostras progressivamente censuradas da EL.
 for (i in 1:amostra){
 W<-runif(m)
 V<-vector()
 U<-vector()
 for(j in 1:m){
 V[j]<-W[j]^(1/(j+sum(R1[(m-j+1):m])))
 }
 for(j in 1:m){
 U[j]<-1-prod(V[m:(m-j+1)])
 }
 fun <- function(x, u) (1-(log(1-(1-p)*exp(-beta*x))/log(p)))-u #acumulada
 dados[i,]<-sapply(U, function(u) uniroot(fun, c(0, 10), u=u)$root)
 }