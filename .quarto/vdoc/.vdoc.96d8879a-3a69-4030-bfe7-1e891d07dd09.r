#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| dir: ltr

data <- cbind(c(1:20), c(25,28,29,44,57,58,63,64,64, 76,87,93,111,126,136,139,157,168,189,212))
#
#
#
#
#| dir: ltr

y <- data[,2]
y
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| dir: ltr
f <- function(params, y){
  
  lambda <- params[1]
  beta <- params[2]
  n <- length(y)
  
  n*log(beta) + n*beta*log(lambda) + (beta - 1)*sum(log(y)) - 
    sum((lambda * y)^beta)
  
}
#
#
#
#
#
#| warning: false
#| dir: ltr
initialval <- c(1,1)
output <- optim(initialval, f,
      y = y,
      control = list(fnscale = -1),
      method = "BFGS",
      hessian = TRUE)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#


lm <- function(y,betaP){
  n <- length(y)
  lambda <- ( (1/n) * sum(y^betaP) )^(-1/betaP)
  lambda
}
#
#
#
lm(y,output$par[2])
```
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
