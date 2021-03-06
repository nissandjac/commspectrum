#install.packages('DATRAS',repos='http://www.rforge.net/',type='source')
require(DATRAS)
#datras <-  downloadExchange(survey = 'NS-IBTS')
#datras_cpue <-  downloadExchange(survey = 'CPUE per length per Hour and Swept Area', years = '2001')

specset <- c('Sprattus sprattus',
             "Ammodytes",
             "Ammodytes marinus",
             "Ammodytes tobianus",
             "Ammodytidae",
             'Trisopterus esmarkii',
             'Clupea harengus',
             'Merlangius merlangus',
             'Solea solea',
             'Solea vulgaris',
             'Pleuronectes platessa',
             'Melanogrammus aeglefinus',
             'Pollachius virens',
             'Gadus morhua',
             'Eutrigla gurnardus')


NS <- readExchange(sprintf('surveys/NS-IBTS_%d.zip', 1965:2016))
NSset <- subset(NS, Species %in% specset)

spec <- vector('list', 15); i=0
names(spec) <- specset
for (s in specset){
  i=i+1
  
  midpts <- seq(min(NSset[[3]]$LngtCm, na.rm = TRUE), 
                max(NSset[[3]]$LngtCm, na.rm = TRUE) + 4, by = 4)
  
  NS_spec <- addSpectrum(subset(NSset, Species==s),cm.breaks = midpts,by = 4)
  
  try <- NS_spec[[2]] %>% dplyr::select(Year, N) 
  nexttry <- data.frame(try$Year,as.data.frame(try$N))
  
  midpts <- (midpts[1:(length(midpts)-1)] + midpts[2:(length(midpts))])/2
  
  spec[[i]] <- nexttry %>% group_by(try.Year,sizeGroup) %>%
    summarise(N=mean(Freq)) %>% 
    mutate(midpt = midpts) %>%
    filter(try.Year!='1965')
}

meltspec <- reshape2::melt(spec, id.vars = c('try.Year','sizeGroup','midpt'))
names(meltspec) <-c('Year', 'sizeGroup', 'mid','n','N','Species')
spectra <- meltspec %>% dplyr::select(-n,-sizeGroup)

spectra %>% 
  filter(N>0) %>%
  ggplot() +
  geom_line(aes(x=mid,y=N,col=Year))+
  facet_wrap(~Species,scales = 'free_y') + 
  scale_y_log10() + 
  scale_x_log10()

spectra %>% 
  group_by(Species, Year) %>% 
  summarise(biomass=sum(N*0.01*midpts^3)) %>% 
  mutate(year = as.numeric(as.character(Year))) %>%
  filter(year>1972) %>%
  ggplot() + 
  scale_y_log10()+
  geom_line(aes(x=year,y=biomass)) + 
  facet_wrap(~Species, scales='free')

# group spectra of genus/species confusion

specsum_biomass <- spectra %>% 
  group_by(Species, Year) %>% 
  summarise(biomass=sum(N*0.01*midpts^3)) %>%
  tidyr::spread(Species,biomass) %>%
  mutate(Ammodytes = Ammodytidae + Ammodytes + `Ammodytes marinus` + `Ammodytes tobianus`,
         Solea = `Solea solea` + `Solea vulgaris`) %>% 
  dplyr::select(-Ammodytidae,
         -`Ammodytes marinus`,
         -`Ammodytes tobianus`,
         -`Solea solea`,
         -`Solea vulgaris`) %>%
  tidyr::gather(Species, biomass,-Year)

specsum_biomass %>%
  mutate(year = as.numeric(as.character(Year))) %>%
  filter(year>1972) %>%
  ggplot() + 
  scale_y_log10()+
  geom_line(aes(x=year,y=biomass)) + 
  facet_wrap(~Species, scales='free')

specsum <- spectra %>% 
  tidyr::spread(Species, N) %>%
  mutate(Ammodytes = Ammodytidae + Ammodytes + `Ammodytes marinus` + `Ammodytes tobianus`,
         Solea = `Solea solea` + `Solea vulgaris`) %>% 
  dplyr::select(-Ammodytidae,
                -`Ammodytes marinus`,
                -`Ammodytes tobianus`,
                -`Solea solea`,
                -`Solea vulgaris`) %>%
  tidyr::gather(Species, N,-Year,-mid)

specsum %>% 
  filter(N>0) %>%
  ggplot() +
  geom_line(aes(x=mid,y=N,col=as.numeric(as.character(Year)),group=Year))+
  facet_wrap(~Species,scales = 'free_y') + 
  viridis::scale_color_viridis('Year',discrete=F) +
  scale_y_log10() + 
  scale_x_log10() + 
  xlab('Length')

save(specsum, specsum_biomass, file='analysis/North-Sea/NS_survey_spectra.Rdata')

write.csv(specsum,'NS_numberspec.csv')
