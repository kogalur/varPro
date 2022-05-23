###################################################################
### <May 01 2022>Hemant Ishwaran
### --------------------------------------------------------------- 
### simulate data and provide symbolic function 
### used for testing interaction detection algorithms
### now extended to handle correlations
### added more interesting simulations
### ---------------------------------------------------------------
###  Written by:
###
###  Hemant Ishwaran                     hemant.ishwaran@gmail.com
###  Division of Biostatistics           
###  Clinical Research Building
###  1120 NW 14th Street
###  University of Miami, Miami FL 33136
###
###  https://ishwaran.org
###  -------------------------------------------------------------
###  THIS PROGRAM SHOULD NOT BE COPIED, USED, MODIFIED, OR 
###  DISSEMINATED IN ANY WAY WITHOUT SPECIFIC WRITTEN PERMISSION 
###  FROM THE AUTHOR.
####################################################################

library(MASS)
library(copula)

simulation.sum <- list(

  cobra2 = function(n = 1000, d = 10, sd = .1, corrv = 0) {
    d <- max(10, d)
    X <- matrix(runif(n * d, -1, 1), ncol = d)
    paramlist <- lapply(1:d, function(j) {list(min=-1,max=1)})
    myCop <- normalCopula(param=rep(corrv,dim(combn(d,2))[2]), dim = d, dispstr = "un")
    myMvd <-  mvdc(copula=myCop, margins=rep("unif",d),paramMargins=paramlist)
    X[, 1:d] <- rMvdc(n, myMvd)
    dta <- data.frame(list(x = X, y = X[,1]*X[,2] + X[,3]^2 - X[,4]*X[,7] + X[,8]*X[,10] - X[,6]^2
                           + rnorm(n, sd = sd)))
    colnames(dta)[1:d] <- paste("x", 1:d, sep = "")
    f <- "x1 * x2 + x3 ^ 2 - x4 * x7 + x8 * x10 - x6 ^ 2"
    fs <- "I(x1 * x2) + I(x3 ^ 2) + I(-x4 * x7) + I(x8 * x10) - I(x6 ^ 2)"
    list(f = f, fs = fs, dta = dta)
  },

  cobra8 = function(n = 1000, d = 9, sd = .1, corrv = 0) {
    d <- max(9, d)
    X <- matrix(runif(n * d, -.25, 1), ncol = d)
    paramlist <- lapply(1:d, function(j) {list(min=-.25,max=1)})
    myCop <- normalCopula(param=rep(corrv,dim(combn(d,2))[2]), dim = d, dispstr = "un")
    myMvd <-  mvdc(copula=myCop, margins=rep("unif",d),paramMargins=paramlist)
    X[, 1:d] <- rMvdc(n, myMvd)
    ## use logistic approximation to step function
    kk <- 30
    y <- 1/(1 + exp(-kk * (X[,1] + X[,4]^3 + X[,9] + sin(X[,2]*X[,8]) + rnorm(n, sd = sd) -.38)))
    dta <- data.frame(list(x = X, y = y))
    colnames(dta)[1:d] <- paste("x", 1:d, sep = "")
    ## approximate the normal error with "0" the mean value
    f <- "1/(1+exp(-30*(x1+x4^3+x9+sin(x2*x8)-.38)))"
    fs <- "I(1 / (1 + exp(-30 *(x1 + x4 ^ 3 + x9 + sin(x2 * x8) - .38))))"
    list(f = f, fs = fs, dta = dta)
  },


  friedman1 = function (n, d = 5, sd = 1, corrv = 0) {
    d <- max(5, d)
    x <- matrix(runif(d * n), ncol = d)
    paramlist <- lapply(1:d, function(j) {list(min=0,max=1)})
    myCop <- normalCopula(param=rep(corrv,dim(combn(d,2))[2]), dim = d, dispstr = "un")
    myMvd <-  mvdc(copula=myCop, margins=rep("unif",d),paramMargins=paramlist)
    x[, 1:d] <- rMvdc(n, myMvd)
    y <- 10 * sin(pi * x[, 1] * x[, 2])
    y <- y + 20 * (x[, 3] - 0.5)^2 + 10 * x[, 4] + 5 * x[, 5]
    if (sd > 0) {
        y <- y + rnorm(n, sd = sd)
    }
    dta <- data.frame(list(x, y = y))
    colnames(dta)[1:d] <- paste("x", 1:d, sep = "")
    f <- "10 * sin(3.1415 * (x1 * x2)) + 20 * (x3 - 0.5)^2 + 10 * x4 + 5 * x5"
    fs <- "I(10 * sin(3.1415 * (x1 * x2))) + I(20 * (x3 - 0.5)^2) + I(10 * x4) + I(5 * x5)"
    list(f = f, fs = fs, dta = dta)
  },

  friedman3 = function (n, d = 4, sd = 0.1, corrv = 0) {
    d <- max(4, d)
    x <-  matrix(runif(n * d, 0, 1), ncol = d)
    paramlist <- lapply(1:d, function(j) {
      if (j==1) {
        list(min=0,max=100)
      }
      else if (j==2) {
        list(min=40*pi,max=560*pi)
      }
      else if (j==3) {
        list(min=0,max=1)
      }
      else if (j==4) {
        list(min=0,max=1)
      }
      else if (j>4) {
        list(min=0,max=1)
      }
    })
    myCop <- normalCopula(param=rep(corrv,dim(combn(d,2))[2]), dim = d, dispstr = "un")
    myMvd <-  mvdc(copula=myCop, margins=rep("unif",d),paramMargins=paramlist)
    x[, 1:d] <- rMvdc(n, myMvd)
    y <- atan((x[, 2] * x[, 3] - 1/(x[, 2] * x[, 4]))/x[, 1])
    if (sd > 0) {
      y <- y + rnorm(n, sd = sd)
    }
    dta <- data.frame(list(x, y = y))
    colnames(dta)[1:d] <- paste("x", 1:d, sep = "")
    f <- "atan((x2 * x3 - 1 / (x2 * x4))/x1)"
    fs <- "I(atan((x2 * x3 - 1 / (x2 * x4))/x1))"
    list(f = f, fs = fs, dta = dta)
  },

  inx1 = function(n = 1000, d = 6, sd = .1, corrv = 0) {
    d <- max(6, d)
    X <- matrix(runif(n * d, -1, 1), ncol = d)
    paramlist <- lapply(1:d, function(j) {list(min=-1,max=1)})
    myCop <- normalCopula(param=rep(corrv,dim(combn(d,2))[2]), dim = d, dispstr = "un")
    myMvd <-  mvdc(copula=myCop, margins=rep("unif",d),paramMargins=paramlist)
    X[, 1:d] <- rMvdc(n, myMvd)
    dta <- data.frame(list(x = X, y = X[,1]*(X[,2]^2)*sqrt(abs(X[,3])) + floor(X[,4]-X[,5]*X[,6]) + rnorm(n, sd = sd)))
    colnames(dta)[1:d] <- paste("x", 1:d, sep = "")
    ## approximate |x|=sqrt(x^2)
    ## write floor function as sum of step functions which can be approximated using logistic
    ## note that -2 <= x4-x5*x6 <= 2
    f0 <- "x1*(x2^2)*((x3^2)^(.5))+"
    f1 <- "-2/(1+exp(-30*(x4-x5*x6+2)))+"
    f2 <- "1/(1+exp(-30*(x4-x5*x6+1)))+"
    f3 <- "1/(1+exp(-30*(x4-x5*x6)))+"
    f4 <- "1/(1+exp(-30*(x4-x5*x6-1)))"
    f <- paste(f0, f1, f2, f3, f4)
    f0s <- "I(x1 * (x2 ^ 2) * ((x3 ^ 2) ^ (.5))) +"
    f1s <- "I(-2 / (1 + exp(-30 * (x4 - x5 * x6 + 2)))) +"
    f2s <- "I(1 / (1 + exp(-30 * (x4 - x5 * x6 + 1)))) +"
    f3s <- "I(1 / (1+ exp(-30 *(x4 - x5 * x6)))) +"
    f4s <- "I(1 / (1+exp(-30 *(x4 - x5 * x6 - 1))))"
    fs <- paste(f0s, f1s, f2s, f3s, f4s)
    list(f = f, fs = fs, dta = dta)
  },

  inx2 = function(n = 1000, d = 6, sd = .1, corrv = 0) {
    d <- max(6, d)
    X <- matrix(runif(n * d, -1, 1), ncol = d)
    paramlist <- lapply(1:d, function(j) {list(min=-1,max=1)})
    myCop <- normalCopula(param=rep(corrv,dim(combn(d,2))[2]), dim = d, dispstr = "un")
    myMvd <-  mvdc(copula=myCop, margins=rep("unif",d),paramMargins=paramlist)
    X[, 1:d] <- rMvdc(n, myMvd)
    dta <- data.frame(list(x = X, y = X[,3]*((X[,1]+1)^(abs(X[,2])))
                           - abs(X[,5])/sqrt(abs(X[,4])+abs(X[,5])+abs(X[,6]))
                           + rnorm(n, sd = sd)))
    colnames(dta)[1:d] <- paste("x", 1:d, sep = "")
    ## approximate |x|=sqrt(x^2)
    f <- "x3 * ((x1+1)^(sqrt(x2^2))) - sqrt((x5^2))/(sqrt(x4^2)+sqrt(x5^2)+sqrt(x6^2))"
    fs <- "I(x3 * ((x1 + 1)^(sqrt(x2 ^ 2)))) + I(-sqrt((x5 ^ 2)) / (sqrt(x4 ^ 2) + sqrt(x5 ^ 2) + sqrt(x6 ^ 2)))"
    list(f = f, fs = fs, dta = dta)
  },
  
  inx3 = function(n = 1000, d = 3, sd = .1, corrv = 0) {
    d <- max(3, d)
    X <- matrix(runif(n * d, -1, 1), ncol = d)
    paramlist <- lapply(1:d, function(j) {list(min=-1,max=1)})
    myCop <- normalCopula(param=rep(corrv,dim(combn(d,2))[2]), dim = d, dispstr = "un")
    myMvd <-  mvdc(copula=myCop, margins=rep("unif",d),paramMargins=paramlist)
    X[, 1:d] <- rMvdc(n, myMvd)
    dta <- data.frame(list(x = X, y = cos(X[,1]-X[,2]) 
                           +asin(X[,1]*X[,3])-atan(X[,2]-X[,3]^2) + rnorm(n, sd = sd)))
    colnames(dta)[1:d] <- paste("x", 1:d, sep = "")
    f <- "cos(x1-x2)+asin(x1*x3)-atan(x2-x3^2)"
    fs <- "I(cos(x1 - x2)) + I(asin(x1 * x3)) + I(-atan(x2 - x3 ^ 2))"
    list(f = f, fs = fs, dta = dta)
  },

  lm = function(n = 1000, d = 30, sd = 15, corrv = 0) {
    d0 <- 15
    d <- max(d0, d)
    Sigma <- diag(1, d)
    Sigma[1:5, 1:5] <- corrv
    Sigma[6:10, 6:10] <- corrv
    Sigma[11:15, 11:15] <- corrv
    diag(Sigma) <- 1
    signal <- 1
    beta <- rep(signal, d0)
    x <- mvrnorm(n, mu = rep(0, d), Sigma = Sigma)
    y <- x[, 1:d0] %*% beta + rnorm(n, sd = sd)
    dta <- data.frame(x, y = y)
    colnames(dta)[1:d] <- paste("x", 1:d, sep = "")
    f <- "1 * (x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15)"
    fs <- f
    list(f = f, fs = fs, dta = dta)
  },
 
  lmi = function(n = 1000, d = 20, sd = .1, corrv = 0) {
    d <- max(20, d)
    x <- matrix(runif(n * d, 0, 1), ncol = d)
    paramlist <- lapply(1:d, function(j) {list(min=0,max=1)})
    myCop <- normalCopula(param=rep(corrv,dim(combn(d,2))[2]), dim = d, dispstr = "un")
    myMvd <-  mvdc(copula=myCop, margins=rep("unif",d),paramMargins=paramlist)
    x[, 1:d] <- rMvdc(n, myMvd)
    signal1 <- .05
    signal2 <- .02
    y1 <-  (x[,1] + x[,2] + x[,3] + x[,4] + x[,5] +
            x[,6] + x[,7] + x[,8] + x[,9] + x[, 10])
    y2 <-   (x[,11] + x[,12] + x[,13] + x[,14] + x[,15] +
             x[,16] + x[,17] + x[,18] + x[,19] + x[, 20])
    y <- signal1 * y1 + exp(signal2 * y1 * y2) + rnorm(n, sd = sd)
    dta <- data.frame(x, y = y)
    colnames(dta)[1:d] <- paste("x", 1:d, sep = "")
    f1 <- ".05 * (x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)"
    f2 <- "(x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)*exp(.02 * (x11+x12+x13+x14+x15+x16+x17+x18+x19+x20))"
    f <- paste(f1, f2)
    fs <- f
    list(f = f, fs = fs, dta = dta)  
  },

  lmi2 = function(n = 1000, d = 30, sd = 15, corrv = 0) {
    d0 <- 15
    d <- max(d0, d)
    Sigma <- diag(1, d)
    Sigma[1:5, 1:5] <- corrv
    Sigma[6:10, 6:10] <- corrv
    Sigma[11:15, 11:15] <- corrv
    diag(Sigma) <- 1    
    beta <- rep(3, d0)
    x <- mvrnorm(n, mu = rep(0, d), Sigma = Sigma)
    y <- (x[, 1:d0])^2 %*% beta + rnorm(n, sd = sd)
    dta <- data.frame(x, y = y)
    colnames(dta)[1:d] <- paste("x", 1:d, sep = "")
    f <- "3 * (x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15)^2"
    fs <- f
    list(f = f, fs = fs, dta = dta)
  },

  sup = function(n = 1000, d = 6, sd = .5, corrv = 0) {
    d <- max(6, d)
    X <- matrix(runif(n * d, .05, 1), ncol = d)
    paramlist <- lapply(1:d, function(j) {list(min=.05,max=1)})
    myCop <- normalCopula(param=rep(corrv,dim(combn(d,2))[2]), dim = d, dispstr = "un")
    myMvd <- mvdc(copula=myCop, margins=rep("unif",d),paramMargins=paramlist)
    X[, 1:d] <- rMvdc(n, myMvd)
    y <- 10 * X[,1] * X[,2] + .25 / (X[,3] * X[,4]) + 10 * X[,5] * X[,6] + rnorm(n, sd = sd)
    dta <- data.frame(list(x = X, y = y))
    colnames(dta)[1:d] <- paste("x", 1:d, sep = "")
    f <- "10*x1*x2 + 0.25/(x3*x4) + 10*x5*x6"
    fs <- "I(10 * x1 * x2) + I(0.25 / (x3 * x4)) + I(10 * x5 *x6)"
    list(f = f, fs = fs, dta = dta)
  },

  sup2 = function(n = 1000, d = 10, sd = .5, corrv = 0){
    d <-  max(10, d)
    X <- matrix(runif(n * d, .5, 1), ncol = d)
    paramlist <- lapply(1:d, function(j) {list(min=.5,max=1)})
    myCop <- normalCopula(param=rep(corrv,dim(combn(d,2))[2]), dim = d, dispstr = "un")
    myMvd <- mvdc(copula=myCop, margins=rep("unif",d),paramMargins=paramlist)
    X[, 1:d] <- rMvdc(n, myMvd)
    y <- (pi ^ (X[, 1] * X[, 2]) * sqrt(2 * X[, 3])
      - asin(X[, 4]) + log(X[, 3] + X[, 5])
      - (X[, 9] / X[, 10]) * sqrt(X[, 7] / X[, 8]) - X[, 2] * X[, 7]
    )
    dta <- data.frame(list(x = X, y = y + rnorm(n, sd = sd)))
    colnames(dta)[1:d] <- paste("x", 1:d, sep = "")
    f <- "(3.1415 ^ (x1*x2)) * sqrt(2*x3) - asin(x4) + log(x3+x5) - (x9*sqrt(x7))/(x10*sqrt(x8)) - x2*x7"
    fs <- "I((3.1415 ^ (x1 * x2)) * sqrt(2 * x3)) + I(-asin(x4)) + I(log(x3 + x5)) + I(-(x9 * sqrt(x7))/(x10 * sqrt(x8))) + I(-x2 * x7)"
    list(f = f, fs = fs, dta = dta)
  }
   
)

