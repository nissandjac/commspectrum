#' @param Parameterset Text string with the name of the parameter set to use
runFoodWebModel <- function(Parameterset='TraitBased', F0 = c(0.3, 0.3, 0.3)) 
{
  #
  # Choose the parameter set
  #
  if (Parameterset == 'Traitbased'){
    W <- 10^seq(log10(wInf[1]),log10(wInf[2]),length.out = nSpecies) # 10 species in logspace
    param <- baseparameters(W,kappa = 0.005,h = 20)
  }
  
  if (Parameterset == 'North Sea'){
    load('R/NorthSea.RData')
  }  
  
  if (Parameterset == 'Benguela Current'){
    load('R/Benguela.RData')
  }  
  
  if (Parameterset == 'Baltic Sea'){
    load('R/Baltic.RData')
  }  
  
  if (Parameterset == 'Northeast US Cont. Shelf'){
    load('R/NEUSCS.RData')
  }  
  
  if (Parameterset == 'Barents Sea'){
    load('R/Barents.RData')
  }  
  
  W <- param$wInf
  nSpecies <- param$nSpecies
  
  param$F0 <- rep(NA,nSpecies)
  param$F0[W <= 100] <- F0[1]
  param$F0[W <= 3000 & W > 100] <- F0[2]
  param$F0[W > 3000] <- F0[3]
  
  param$fishing <- "Trawl" # See "fishing.R" for definitions
  
  param$tEnd <- 40
  S <- IterateSpectrum(param,S = S) # Add S here to start at initial conditions from before
  S$Yield <- YieldCalc(param,S)
  
  return(S)
}