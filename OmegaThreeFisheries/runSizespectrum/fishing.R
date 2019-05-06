fishing = function(param,iSpecies,w,type){  
  switch (type, 
          no      = Fin[iSpecies,] <- 0, # No fishing
          BH_Gill = w^(-1/4)*exp((-log(w/(param$nF*param$wInf[iSpecies]))^2/(2*param$gSigma))) ,
          # Balanced selective gillnet fishing
          BH_sel  = param$wInf[iSpecies]^(-1/4)*((1+(w/(param$nF * param$wInf[iSpecies]))^-param$myF)^-1),
          # Balanced selective trawl fishing
          BH_non  = (w^(-1/4)/max(w^(-1/4))),
          # Non-selective balanced fishing 
          Trawl   = (1+(w/(param$nF * param$wInf[iSpecies]))^-param$myF)^-1,

          unsel   = rep(1,length(w)),
          
          Kariba  = exp((-log(w/(param$aF))^2/(2*param$bF)))
        )
  
}