function(labels,n,k,x,pi,mu,sigma) {
  label <- 0
  values <- rep(0,k)
  for(j in 1:k) {
    values[j] <- pi[j] * dnorm(x,mu[j],sigma[j])
  }
  myMax <- max(values)
  if(is.infinite(myMax)) {
    label <- 0
  } else {
    indexes <- which(values == myMax)
    if(length(indexes) != 1) {
      label <- 0
    } else {
      label <- indexes[1]
    }
  }
  list(
    label = as.character(labels[label]),
    densities = values,
    normalizedDensities = values/sum(values))
}
