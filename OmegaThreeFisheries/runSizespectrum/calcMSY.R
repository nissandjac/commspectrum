calcMSY <- function(S0, param, nruns = 30, Feq = 0.2, Fmax = 2, minsize = NA, maxsize = NA){
  
  F0 <- seq(0,Fmax, length.out = nruns)
  Feq <- Feq
  Yield <- matrix(NA, param$nSpecies, nruns)
  YieldSize <- array(NA, dim = c(param$nSpecies, length(S0$w), nruns))
  SSB <- matrix(NA, param$nSpecies, nruns)

  if(minsize != maxsize){
  sidx <- param$wInf >= minsize & param$wInf<= maxsize
  }else{
    sidx <- param$wInf == minsize
  }
  
    for(i in 1:length(F0)){
    param$F0 <- rep(Feq, param$nSpecies)
    param$F0[param$wInf >= minsize & param$wInf<= maxsize] <- F0[i]
    
    if(i == 1){ # Save a bit of time 
      param$tEnd <- 100
      SF <- IterateSpectrum(param, S0)
      Yield[,i] <- YieldCalc(param,SF)
      SSB[,i] <- calcSSB(param,SF, param$tEnd)
    }else{
      param$tEnd <- 40
      SF <- IterateSpectrum(param, SF)
      Yield[,i] <- YieldCalc(param,SF)
      SSB[,i] <- calcSSB(param,SF, param$tEnd)
    }
    
    
  }
  
  if(sum(sidx) >1){
    MSY.Yield <- colSums(Yield[sidx,])
  }else{
    MSY.Yield <- (Yield[sidx,])
  }
  
  MSY.idx <- which.max(MSY.Yield)
  
  Fmsy <- F0[MSY.idx]
  
return(list(Yield = MSY.Yield, SSB = SSB[,MSY.idx], Fmsy = Fmsy,
            F0 = F0))  
}