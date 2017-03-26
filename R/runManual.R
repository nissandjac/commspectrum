#' Run the community spectrum simulator manually
#'
#' @param nSpecies Number of asymptotic size groups ("species").  Only relevant for trait-based model.
#' @param F0 Array with fishing mortality on each species
#' @param S Output from a previous simulation to use as initial conditions
#' @param param Set of parameters to use
#'
#' @return A structure with the output of the simulation
#'
#' @examples runTraitBasedModel()
#' 
#' 
#' @export
runTraitBasedModel <- function(nSpecies = 18 ,
                               F0 = 0.3+0*(1:nSpecies), 
                               S= NA, 
                               param = baseparameters(10^seq(log10(4),log10(30000),length.out = nSpecies),kappa = 1.3,h = 20))
{
  param$F0 <- F0
  param$fishing <- "Trawl" # See "fishing.R" for definitions
  
  S <- IterateSpectrum(param,S = S)

  return(S)
}



#' Run the food web size spectrum model manually
#'
#' @param Parameterset Text string with the name of the parameter set to use
#' @param F0 Array with fishing mortality on each fleet
#' @param S Output from a previous simulation to use as initial conditions
#'
#' @return A structure with the output of the simulation
#'
#' @examples runFoodWebModel('North Sea', F0 = c(0.1, 0.3, 0.7))
#' 
#' 
#' @export
runFoodWebModel <- function(Parameterset='TraitBased', F0 = c(0.3, 0.3, 0.3), S = NA) 
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
  
  return(S)
}