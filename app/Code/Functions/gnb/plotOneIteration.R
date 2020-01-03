function(D,zeros,numericLabels,xlim,ylim,k,pi,mu,sigma) {
  # PLOT
  f <- Functions$getPdfFromParameters(pi[1],mu[1],sigma[1])
  plot(f,ylab="p(x)",xlim=xlim,ylim=ylim,col=2)
  
  points(D,zeros,pch=16,lwd=2,col=numericLabels + 1,cex=2)
  points(mu,rep(0,length(mu)),pch=4,lwd=2,col=(1:length(mu))+1,cex=2)
  for(j in 2:k) {
    f <- Functions$getPdfFromParameters(pi[j],mu[j],sigma[j])
    plot(f,add=TRUE,xlim=xlim,col=j+1)#,lty=j)
  }
}
