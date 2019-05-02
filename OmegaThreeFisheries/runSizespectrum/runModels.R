## Run size based model with different selectivities ## 

source('load_files.R')

W <- 10^seq(log10(10),log10(100000),length.out = 15) # 15 species in logspace 
param <- baseparameters(W,kappa = 0.005,h = 10)
param$F0 <- rep(0.2,param$nSpecies)
param$fishing <- "BH_non" # See "fishing.R" for definitions
param$wFstart <- 10

S0 <- IterateSpectrum(param,S = NA)
#plotSpectrum(param,S)
plotBiomasstime(param,S0) # Make sure the simulation has gone to equilibrium
plotSpectrum(param,S0)


### Calculate Fmsy of small, medium and large fish 
Fmax <- 1.5

Fmsy.forage <- calcMSY(S0, param,minsize = 0, maxsize = 999,nruns = 30, Fmax = Fmax)
Fmsy.medium <- calcMSY(S0, param,minsize = 1000, maxsize = 8000,nruns = 30, Fmax = Fmax)
Fmsy.large <- calcMSY(S0, param,minsize = 8000, maxsize = max(W),nruns = 30, Fmax = Fmax)

### Plot the MSY calculations \
df.plot <- data.frame(
  Yield = c(Fmsy.forage$Yield, Fmsy.medium$Yield, Fmsy.large$Yield),
  model = rep(c('forage','Medium','Large'), each = 30),
  F0 =  c(Fmsy.forage$F0,Fmsy.medium$F0,Fmsy.large$F0))

ggplot(df.plot,aes(x = F0, y = Yield, color = model))+geom_line(size = 1.5)+theme_classic()

# Try three different management strategies MSY, forage fish moratorium (0.5*Fmsy on forage fish), and Balanced harvesting 
param <- baseparameters(W,kappa = 0.005,h = 10)
param$fishing <- 'BH_non'
param$F0 <- 1

SF.BH <- IterateSpectrum(param, S = S0)

# Find the multispecies MSY during balanced harvesting 
Fmsy.all <- calcMSY(S0, param, minsize = 0, maxsize = max(W), Fmax = 4, nruns = 50)

plot(Fmsy.all$Yield)
