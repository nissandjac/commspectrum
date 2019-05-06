YieldCalcSize <- function(param,S){
  
  # Calculate the equilibrium yield from a size-spectrum run 
  
  tEnd<- param$tEnd/param$dt
  
  N <- S$N[tEnd,,]
  Yield <- matrix(0,param$nSpecies,length(S$w))  

  for (i in 1:param$nSpecies){
    
    Yield[i,] <- N[i,]*S$w*S$dw*S$Fin[i,]
    
  }
  
  return(Yield)
}
