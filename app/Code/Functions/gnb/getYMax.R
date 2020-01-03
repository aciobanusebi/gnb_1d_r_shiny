function(pi,mu,sigma) {
  yMax <- pi[1]*dnorm(mu[1],mu[1],sigma[1])
  if(is.infinite(yMax)) {
    yMax <- 0.3
  }
  
  for(i in 1:length(pi)) {
    if(i==1) {
      next
    }
    aux <- pi[i]*dnorm(mu[i],mu[i],sigma[i])
    if(!is.infinite(aux) && aux > yMax) {
      yMax <- aux
    }
  }
  
  yMax
}