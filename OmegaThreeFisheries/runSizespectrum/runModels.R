## Run size based model with different selectivities ## 

source('load_files.R')
require(ggplot2)
require(dplyr)

W <- 10^seq(log10(10),log10(100000),length.out = 30) # 15 species in logspace 
param <- baseparameters(W,kappa = 0.005,h = 15)
param$F0 <- rep(0.2,param$nSpecies)
param$wFstart <- 10

S0 <- IterateSpectrum(param,S = NA)

#plotSpectrum(param,S)
param$fishing <- "BH_non" # See "fishing.R" for definitions
param$F0 <- 1
SF <- IterateSpectrum(param, S0)

plotBiomasstime(param,S0) # Make sure the simulation has gone to equilibrium
plot(SF$Fin[15,])
plotSpectrum(param,S0)



### Calculate Fmsy of small, medium and large fish 
Fmax <- 1.5

forage <- 1000
medium <- 8000
large <- max(W)
## Balanced harvesting 
param$fishing <- "BH_non" # See "fishing.R" for definitions

Fmsy.forage.bh <- calcMSY(S0, param,minsize = 0, maxsize = forage-0.1,nruns = 30, Fmax = Fmax)
Fmsy.medium.bh <- calcMSY(S0, param,minsize = forage, maxsize = medium,nruns = 30, Fmax = Fmax)
Fmsy.large.bh <- calcMSY(S0, param,minsize = medium, maxsize = large,nruns = 30, Fmax = Fmax)
Fmsy.all.BH <- calcMSY(S0, param,minsize = W[1], maxsize = W[length(W)],nruns = 50, Fmax = 3)

# Find the MMSY for Trawl

param$fishing <- 'Trawl'
Fmsy.forage.trawl <- calcMSY(S0, param,minsize = 0, maxsize = forage-0.1,nruns = 30, Fmax = Fmax)
Fmsy.medium.trawl <- calcMSY(S0, param,minsize = forage, maxsize = medium,nruns = 30, Fmax = Fmax)
Fmsy.large.trawl <- calcMSY(S0, param,minsize = medium, maxsize = large,nruns = 30, Fmax = Fmax)
Fmsy.all.trawl <- calcMSY(S0, param,minsize = W[1], maxsize = W[length(W)],nruns = 50, Fmax = 3)

## Plot the individual runs 
df.bh <- data.frame(
  Yield = c(Fmsy.forage.bh$Yield, Fmsy.medium.bh$Yield, Fmsy.large.bh$Yield),
  model = rep(c('forage','Medium','Large'), each = 30),
  F0 =  c(Fmsy.forage.bh$F0,Fmsy.medium.bh$F0,Fmsy.large.bh$F0)
  )
df.bh$sel <- 'bh'


## Plot the individual runs 
df.trawl <- data.frame(
  Yield = c(Fmsy.forage.trawl$Yield, Fmsy.medium.trawl$Yield, Fmsy.large.trawl$Yield),
  model = rep(c('forage','Medium','Large'), each = 30),
  F0 =  c(Fmsy.forage.trawl$F0,Fmsy.medium.trawl$F0,Fmsy.large.trawl$F0)
)
df.trawl$sel <- 'trawl'


p1 <- ggplot(df.bh,aes(x = F0, y = Yield, color = model))+
  geom_line(size = 1.5)+
  geom_line(data = df.trawl, size = 1.5, linetype = 2)+
  theme_classic()

png('Figures/Trait-Yield.png', width = 16, height = 12, res = 400, units = 'cm')
p1
dev.off()
p1

df.plot <- data.frame(
  Yield = c(Fmsy.all.trawl$Yield, Fmsy.all.BH$Yield),
  model = rep(c('Trawl','BH'), each = 50),
  F0 =  c(Fmsy.all.trawl$F0,Fmsy.all.BH$F0)
)

p2 <- ggplot(df.plot, aes(y = Yield, x = F0, color = model))+geom_line()+theme_classic()

p2

# Try different management strategies MSY, forage fish moratorium (0.5*Fmsy on forage fish), and Balanced harvesting 

### Trawl fishing - forage = 0.5 Fmsy 
param$fishing <- 'Trawl'
param$F0 <- rep(NA, param$nSpecies)
param$F0[param$wInf<forage] <- Fmsy.forage.trawl$Fmsy*0.5
param$F0[param$wInf>= forage & param$wInf < large] <- Fmsy.medium.trawl$Fmsy
param$F0[param$wInf>= medium] <- Fmsy.large.trawl$Fmsy

SF.FF <- IterateSpectrum(param, S0)
Yield.FF <- YieldCalcSize(param,SF.FF)

### BH across the board at Fmsy

param$fishing <- 'BH_non'

param$F0 <- rep(NA, param$nSpecies)
param$F0[param$wInf<forage] <- Fmsy.forage.bh$Fmsy
param$F0[param$wInf>= forage & param$wInf < large] <- Fmsy.medium.bh$Fmsy
param$F0[param$wInf>= medium] <- Fmsy.large.bh$Fmsy

SF.BH <- IterateSpectrum(param, S0)
Yield.BH <- YieldCalcSize(param,SF.BH)

## Standard Fmsy all over 

param$fishing <- 'Trawl'
param$F0 <- rep(NA, param$nSpecies)
param$F0[param$wInf<forage] <- Fmsy.forage.trawl$Fmsy
param$F0[param$wInf>= forage & param$wInf < large] <- Fmsy.medium.trawl$Fmsy
param$F0[param$wInf>= medium] <- Fmsy.large.trawl$Fmsy

SF.MSY <- IterateSpectrum(param, S0)
Yield.MSY <- YieldCalcSize(param,SF.MSY)

#### Plot the results 

### Print the total yield 
print(paste('Forage fish yield = ',round(sum(Yield.FF),5)*1000,
            'BH yield = ', round(sum(Yield.BH),5)*1000,
      'MSY yield = ', round(sum(Yield.MSY),5)*1000))

# Plot the yield as a function of individual size and trait size 


df.plot <- data.frame(Yield = c(colSums(Yield.FF), colSums(Yield.BH), colSums(Yield.MSY)),
                      w = rep(SF.MSY$w, 3),
                      mng = rep(c('Forage fishing', 'Balanced Harvesting', 'MSY'), each = length(SF.MSY$w))
                      )
                      
p3 <- ggplot(df.plot, aes(x = w, y = Yield, color = mng))+geom_line(size= 1.5)+scale_x_log10()+scale_y_continuous()+
  coord_cartesian(xlim = c(20,max(W)))+theme_classic()

png('Figures/size-Yield.png', width = 16, height = 12, res = 400, units = 'cm')

p3
dev.off()

df.plotYwInf <- data.frame(Yield = c(rowSums(Yield.FF), rowSums(Yield.BH), rowSums(Yield.MSY)),
                      wInf = rep(param$wInf, 3),
                      mng = rep(c('Forage fishing', 'Balanced Harvesting', 'MSY'), each = param$nSpecies)
)

p3 <- ggplot(df.plotYwInf, aes(x = wInf, y = Yield, color = mng))+geom_line(size= 1.5)+scale_x_log10()+scale_y_continuous()+
  coord_cartesian(xlim = c(20,max(W)))+theme_classic()

p3


df.plot2 <- data.frame(Yield = c(rowSums(Yield.FF), rowSums(Yield.BH), rowSums(Yield.MSY)),
                      wInf = rep(param$wInf, 3),
                      mng = rep(c('Forage fishing', 'Balanced Harvesting', 'MSY'), each = length(param$wInf))
)


p3 <- ggplot(df.plot2, aes(x = wInf, y = Yield, color = mng))+geom_line(size= 1.5)+scale_x_log10()+scale_y_continuous()+
  coord_cartesian(xlim = c(10,max(W)))+theme_classic()
png('Figures/wINf-Yield.png', width = 16, height = 12, res = 400, units = 'cm')

p3
dev.off()

# Plot the SSB/SSB0

SSB.FF <- calcSSB(param, S = SF.FF, param$tEnd/param$dt)
SSB.MSY <- calcSSB(param, S = SF.MSY, param$tEnd/param$dt)
SSB.BH <- calcSSB(param, S = SF.BH, param$tEnd/param$dt)
SSB0 <- calcSSB(param, S = S0, param$tEnd/param$dt)

df.plotSSB <- data.frame(SSB = c(SSB.FF/SSB0, SSB.MSY/SSB0, SSB.BH/SSB0),model =  rep(c('FF','MSY','BH'), each = param$nSpecies),
                         wInf = rep(param$wInf, 3))

p4 <- ggplot(df.plotSSB,aes(x = wInf, y = SSB, color = model))+geom_line(size = 1.5)+scale_x_log10()+theme_classic()+scale_y_log10()+
  geom_hline(aes(yintercept = 1), linetype = 2)+geom_hline(aes(yintercept = 0.2), linetype = 2)

png('Figures/wINf-SSB.png', width = 16, height = 10, res = 400, units = 'cm')
p4
dev.off()

Ntot <- S0$Ntot[param$tEnd/param$dt,]
Ntot.FF <- SF.FF$Ntot[param$tEnd/param$dt,]
Ntot.MSY <- SF.MSY$Ntot[param$tEnd/param$dt,]
Ntot.BH <- SF.BH$Ntot[param$tEnd/param$dt,]


df.plotN <- data.frame(N = c(Ntot.FF/Ntot, Ntot.MSY/Ntot, Ntot.BH/Ntot),model =  rep(c('FF','MSY','BH'), each = length(S0$w)),
                         w = rep(S0$w, 3))

p5 <- ggplot(df.plotN,aes(x = w, y = N, color = model))+geom_line(size = 1.5)+scale_x_log10()+theme_classic()+scale_y_log10()+
  geom_hline(aes(yintercept = 1), linetype = 2)+geom_hline(aes(yintercept = 0.2), linetype = 2)+coord_cartesian(ylim = c(1e-2,5),
                                                                                                                xlim = c(1,max(S0$w)))
png('Figures/w-N.png', width = 16, height = 12, res = 400, units = 'cm')
p5
dev.off()
