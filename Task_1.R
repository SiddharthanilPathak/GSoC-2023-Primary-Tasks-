
############ Task - 1 ######################################

#######################################################
########## Autoregressive model of order 1 ############


## Inputs are Autoregressive coefficient(alpha), standard deviation of Gaussian noise
## (I take mean to be 0 by default), size of time series, initial value X0

AR_Model_1 <- function(alpha, sd, n, x0)
{
  epsilon = rnorm(n,mean=0,sd = sd)
  x = vector(length = n+1)
  x[1] = x0
  
  for(i in 1:n)
  {
    x[i+1] = alpha*x[i]  + epsilon[i]
  }
  
  return(x)
  
}

# Example of Implementation:- 

ts = AR_Model_1(0.5,1,100,1)
plot(ts,type = "l", main="Simulations from AR(1) Model", col = "red", xlab = "Time",ylab = "Value")
ts1 = AR_Model_1(0.8,1,100,1)
lines(ts1,col = "blue")
ts2 = AR_Model_1(-0.6,1,100,1)
lines(ts2,col = "green")
legend("topright",legend = c("alpha=0.5","alpha=0.8","alpha=-0.6"),fill = c("red","blue","green"))



#######################################################################################################################
######################################################################################################################
