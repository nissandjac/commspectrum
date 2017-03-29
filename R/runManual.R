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
  S$param <- param
  
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
  S$param <- param
  
  return(S)
}
#' 
#' #' Plot the results from the food web model or the trait-based model
#'
#' @param Parameterset Text string with the name of the parameter set to use
#' @param F0 Array with fishing mortality on each fleet
#' @param S Output from a previous simulation to use as initial conditions
#'
#' @return -
#'
#' @examples plotResults( runTraitBasedModel() )
#' 
#' @export
plotResults <- function( S ) 
{
  param <- S$param
  idxEnd <- param$tEnd/param$dt
  w <- S$w
  W <- param$wInf
  xlimit <- c(0.02, max(w))
   
  par(mfcol = c(3, 2), 
      cex = 0.6,
      mar = c(2, 4, 0, 0), 
      oma = c(2, 0, 0.5, 0.5),
      tcl = -0.25,
      mgp = c(2, 0.6, 0))
  #layout(mat=matrix(c(1,1,2,3), 2, 2,byrow=TRUE),
  #       widths=c(3,1), heights=c(1,2))
  # -------------------------------
  # Size spectrum
  # -------------------------------
  B <- S$Ntot[idxEnd,]*w
  B[B == 0] <- NA # For plotting on log scale 

  yl <- param$kappaR * c(0.1*max(w)^(1+param$kR) , 0.02^(1+param$kR))
  #
  # Community spectrum:
  #
  plot(w,B, log = 'xy', type = 'l', col = 'black', lwd=3,
       ylab = 'Biomass density (-)', 
       xlab = 'Weight (g)', 
       ylim=yl, xlim=xlimit)
  #
  # Theoretical solution:
  #
  lines( w, param$kappaR*w^(1+param$kR), lty=2)
  #
  # Species spectra
  #
  N <- S$N[idxEnd,,]
  for (i in 2:param$nSpecies)
    lines(w,N[i,]*w, col = alpha('black',alpha = 0.3))
  #
  # Resource
  #
  wPP <- S$wPP
  lines(wPP, S$nPP[idxEnd,1,]*wPP, col='black', lwd=3, lty=2)
  # -------------------------------
  # Feeding levels
  # -------------------------------
  f <- S$f[idxEnd,,]
  plot(w, rep(param$fc[1],length(w)), type = 'l', log="x", lty=2,  # critical feeding level
       ylab = 'Feeding level',
       ylim=c(0,1),
       xlim= xlimit)
  lines(w, rep(param$f0, length(w)), lty=2) # Theoretical feeding level
  for (i in 2:param$nSpecies)
    lines(w, f[i,], lty=1)
  # -------------------------------
  # Mortality
  # -------------------------------
  M2 <- S$M2[idxEnd,,]
  plot(w, M2[1,], type='l', log="x", lty=1,lwd=3,
       ylab = 'Mortality (1/yr)',
       xlab = "Weight (g)",
       ylim = c(0,4),
       xlim = xlimit)
  lines(w, mean(param$a)*param$alpha*mean(param$h)*(param$f0-mean(param$fc)) * w^(param$n-1), lty=2)
  for (i in 2:param$nSpecies) {
    ix <- w<=param$wInf[i]
    lines(w[ix], S$Fi[i,ix], lty=1)
    points(param$wInf[i], S$Z0[i])
  }
  # -------------------------------
  # SSB
  # -------------------------------
  SSB <- S$SSB[idxEnd,]
  plot(W, SSB, type='l', log='xy', lty=1,
       ylab = 'SSB (ton)')
  points(W, SSB)
  # -------------------------------
  # R0
  # -------------------------------
  R0 <- S$Rp[idxEnd,]/S$R[idxEnd,]
  plot(W, R0, type='l', log='xy', lty=1,
       ylab = 'R0',
       ylim = c(0.5,max(R0)))
  points(W, R0)
  lines(W, rep(1,length(W)), lty=2)
  # -------------------------------
  # Yield
  # -------------------------------
  plot(W, S$Yield, type='l', log='xy', lty=1,
       ylab = 'Yield (ton/yr)',
       xlab = "Asymptotic weight (g)")
  points(W, S$Yield)
}
