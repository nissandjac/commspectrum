#' Run the community spectrum simulator manually
#'
#' @param nSpecies Number of asymptotic size groups ("species").  Only relevant for trait-based model.
#' @param F0 Array with fishing mortality on each species
#' @param S Output from a previous simulation to use as initial conditions
#' @param param Set of parameters to use
#'
#' @return A structure with the output of the simulation
#'
#' @examples 
#' 
#' 
#' @export
runTraitBasedModel <- function(nSpecies = 18 ,
                               F0 = 0.3+0*(1:nSpecies), 
                               S= NA, 
                               param = baseparameters(10^seq(log10(4),log10(30000),length.out = nSpecies),kappa = 0.005,h = 20))
{
  param$F0 <- F0
  param$fishing <- "Trawl" # See "fishing.R" for definitions
  
  S <- IterateSpectrum(param,S = S)
  S$Yield <- YieldCalc(param,S)
  
  return(S)
}

