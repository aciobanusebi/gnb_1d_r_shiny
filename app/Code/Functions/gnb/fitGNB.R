function(k,n,labels,data) {
  pi <- rep(0,k)
  mu <- rep(0,k)
  sigma <- rep(0,k)
  
  localData <- data[data[,2] == labels[1],]
  pi[1] <- nrow(localData)/n
  mu[1] <- mean(localData[,1])
  sigma[1] <- sqrt(sum((localData[,1] - mu[1])^2)/length(localData[,1]))
  yMax <- pi[1]*dnorm(mu[1],mu[1],sigma[1])
  
  for(i in 1:k) {
    if(i==1) {
      next
    }
    localData <- data[data[,2] == labels[i],]
    pi[i] <- nrow(localData)/n
    mu[i] <- mean(localData[,1])
    sigma[i] <- sqrt(sum((localData[,1] - mu[i])^2)/length(localData[,1]))
    aux <- pi[i]*dnorm(mu[i],mu[i],sigma[i])
    if(aux > yMax) {
      yMax <- aux
    }
  }
  list(pi=pi,mu=mu,sigma=sigma)
}