################################################# Task - 2 ######################################################

#####################################################################################################################
###################### Metropolis-Hastings algorithm for p-variate Normal distribution ##############################
library(mvtnorm)



Multivariate_normal_sample <- function(n,mean,sigma)
{
  accept = rep(0,n)
  p <- length(mean)
  sample = matrix(0,nrow =n, ncol = p)
  curr <- rmvnorm(1,mean = mean, sigma = sigma)  
  
  for (i in 1:n)
    {
    ## My Proposal distribution is p-variate distribution centered at "curr" and variance step 
    ## size I have taken is 0.3. I implementing Random walk Metropolis Hastings so ratio term 
    ## becomes trivial to calculate.
        y = rmvnorm(1,mean = curr, sigma = 0.3*sigma) ## Random Draw From Proposal Distribution
        ratio <- dmvnorm(y,mean = mean, sigma = sigma)/dmvnorm(curr,mean = mean, sigma = sigma)
      
        if(runif(1) <= ratio)
        {
          curr = y
          accept[i] = 1
        }
          sample[i,] = curr
    }
return(sample)
}

######################################
## Checking of Correctness
samp = Multivariate_normal_sample(1e4,c(2,3),matrix(c(1,0,0,1),nrow =2))

library(plotly)
x = samp[,1]
y = samp[,2]
plot(x,y) 


z = dmvnorm(samp,mean=c(2,3),sigma=matrix(c(1,0,0,1),nrow =2))
dat = cbind(x,y,z)
dat = as.data.frame(dat)
fig <- plot_ly(dat, x = ~x, y = ~y, z = ~z,
               marker = list(color = ~mpg, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'X-Axis'),
                                   yaxis = list(title = 'Y-Axis'),
                                   zaxis = list(title = 'Value of probability Distribution function')),
                      annotations = list(
                        x = 1.13,
                        y = 1.05,
                        text = 'Bivariate Normal Distribution',
                        xref = 'paper',
                        yref = 'paper',
                        showarrow = FALSE
                      ))
fig

#################################################################################################################
#################################################################################################################
#################################################################################################################
