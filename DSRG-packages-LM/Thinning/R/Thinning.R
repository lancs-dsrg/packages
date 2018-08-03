


#' Thinning - Generating arrivals from a NHPP
#'
#' @param f The intensity function we want to be thinned
#' @param x A sequence of points between 0 and Tend to thin
#' @param N The max number of points to return
#' @param max_rate  The maximum rate within the interval [0, Tend)
#' @param Tend The end time
#'
#' @return Event times from a NHPP with intensity function f
#' @export
#'
#' 
Thinning<-function(f,x,N,max_rate,Tend){
  #plot(x,f(x),typ="l",ylim=c(0,max_rate+1),xlab="Time (t)", ylab="lambda(t)",lwd=2, main="Simulation of a NSPP by Thinning")
  I<-rexp(N,max_rate)    #random exponential sample as they are poisson interarrival times
  A<-rep(0,N)     #The arrival time of the Nth customer
  A[1]<-I[1]
  for(i in 2:N){
    A[i]<-A[i-1]+I[i]
  } 
  MAX<-Tend
  n<-sum(A<MAX)
  
  A<-A[A<MAX]       #just gives the vector of the arrival times less than 10 
  Acc<-rbinom(n,1,f(A)/max_rate)       # f(A)/7 is lambda/lstar
  Aacc<-A[Acc==1]      #accepted values
  Anacc<-A[Acc==0]
  
  #points(Aacc,rep(0,length(Aacc)),col="darkolivegreen3",pch=16,cex=1)
  # for(i in 1:length(Aacc)){    
  #   lines(c(Aacc[i],Aacc[i]), c(0,f(Aacc[i])))
  # }
  return(Aacc)
}
