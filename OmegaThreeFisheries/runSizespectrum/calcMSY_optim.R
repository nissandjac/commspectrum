calcMSY_optim <- function(S0, param,minsize = W[1], maxsize = W[length(W)], Feq = 0.2){
  
  
  fn.msy < function(param,df)
  Feq <- Feq
  Yield <- matrix(NA, param$nSpecies, nruns)
  YieldSize <- array(NA, dim = c(param$nSpecies, length(S0$w), nruns))
  SSB <- matrix(NA, param$nSpecies, nruns)
  
  if(minsize != maxsize){
    sidx <- param$wInf >= minsize & param$wInf<= maxsize
  }else{
    sidx <- param$wInf == minsize
  }
    
  param$F0 <- rep(Feq, param$nSpecies)
  param$F0[param$wInf >= minsize & param$wInf<= maxsize] <- F0[i]
  
  param$tEnd <- 100
  SF <- IterateSpectrum(param, S0)
  Yield<- YieldCalc(param,SF)
  
  
  
  
  
  
  
  
}