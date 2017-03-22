
baserun <- function(wInf = c(20,30000), nSpecies = 18 ,F0 = matrix(0.3,3,2), S= NA,Parameterset = 'Generic'){


# source('load_files.R')
# source('calcSSB.R')
# source('YieldCalc.R')

  
 # Choose the parameter set
if (Parameterset == 'Generic'){
  
W <- 10^seq(log10(wInf[1]),log10(wInf[2]),length.out = nSpecies) # 10 species in logspace
param <- baseparameters(W,kappa = 0.005,h = 20)
}

  
if (Parameterset == 'North Sea'){

  load('R/NorthSea.RData')
  W <- param$wInf
  nSpecies <- param$nSpecies
  
}  
  
  
if (Parameterset == 'Benguela Current'){
    
    load('R/Benguela.RData')
    W <- param$wInf
    nSpecies <- param$nSpecies
    
  }  

  
if (Parameterset == 'Baltic Sea'){
  
    load('R/Baltic.RData')
    W <- param$wInf
    nSpecies <- param$nSpecies
    
  }  
  
  if (Parameterset == 'Northeast US Cont. Shelf'){
    
    load('R/NEUSCS.RData')
    W <- param$wInf
    nSpecies <- param$nSpecies
    
  }  
  
  
  if (Parameterset == 'Barents Sea'){
    
    load('R/Barents.RData')
    W <- param$wInf
    nSpecies <- param$nSpecies
    
  }  

param$F0 <- rep(NA,nSpecies)
param$F0[W <= 100] <- F0[1,1]
param$F0[W <= 3000 & W > 100] <- F0[2,1]
param$F0[W > 3000] <- F0[3,1]
  
param$fishing <- "Trawl" # See "fishing.R" for definitions
  
param$tEnd <- 40
param$dt <- 0.5 # run the model faster
SF <- IterateSpectrum(param,S = S) # Add S here to start at initial conditions from before
SF$Yield <- YieldCalc(param,SF)

param$F0[W <= 100] <- F0[1,2]
param$F0[W <= 3000 & W > 100] <- F0[2,2]
param$F0[W > 3000] <- F0[3,2]

SF2 <- IterateSpectrum(param,SF)
SF2$Yield <- YieldCalc(param,SF2)

return(list(SFpre = SF,SFpost = SF2,param = param))


}



