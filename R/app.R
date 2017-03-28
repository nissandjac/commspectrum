alpha <- function(col, alpha) {
  adjustcolor(col, alpha.f = alpha)
}

#' Run the community spectrum simulator
#'
#' @param ... Not used at the moment
#'
#' @return Nothing, it is used to run the Shiny app
#' @export
#'
#' @examples
#' runCommunitySpectrum()
runCommunitySpectrum <- function(...) {
  runApp(shinyApp(ui = fluidPage(
    
    # Create all the inputs for running the model
    h1('Size spectrum simulator'),
    p('Calculate the expected ecosystem effect of a management plan involving changing the fishing mortality 
      on one aspect of the fish community from an "initial" fishing pattern to "new" fishing pattern. '),
    fluidRow(
      column(1),
      column(1,actionButton(inputId = 'click', label = 'Start simulation')))
    ,
    p(' - wait a few seconds and scroll down to see results of the simulation.')
    ,
    wellPanel(
      h4('Define ecosystem'),
      p('Select an ecosystem. Use "generic" for running the trait-based model which models a generic fish community.'),
      fluidRow(
        column(2, br(), p('Ecosystem')),
        column(2, selectInput(inputId = 'Parameterset', label = '',
                              choices =  c('Generic', 'North Sea', 'Baltic Sea', 
                                           'Benguela Current', 'Northeast US Cont. Shelf',
                                           'Barents Sea'))),
        column(8, htmlOutput("EcosystemDescription") )
      ))
    ,
    wellPanel(
      h4('Definition of fishing fleets'),
      p('There are three fleets: 1) forage fleet targeting small species; 
        2) pelagic fleet tageting medium sized species, and 3) a demersal fleet targeting large species. 
        The fleets are defined by largest fish targeted by the forage fish fleet, 
        and the smallest size targeted by the demersal fleet:'),
      fluidRow(
        column(2),
        column(3,sliderInput(inputId = 'wMiddle', label = 'Max. weight of forage fish', value = 100, min = 0,
                             max = 1000, step = 50)),
        column(3,sliderInput(inputId = 'wLarge', label = 'Min. weight of large fish', value = 3000, min = 1000,
                             max = 10000, step = 500))
      ))
    ,
    wellPanel(
      h4('Fishing mortalities (per year)\n'),
      
      fluidRow(
        column(2, br(),br(), p('Forage fleet')),
        column(3,sliderInput(inputId = 'Fsmall', label = '   Initial fishing level', value = 0.5, min = 0,
                             max = 3, step = 0.1)),
        column(3,sliderInput(inputId = 'Fsmall.after', label = '   New fishing level', value = 0.5, min = 0,
                             max = 3, step = 0.1))
      )
      ,
      fluidRow(
        column(2, br(),br(), p('Pelagic fleet')),
        column(3,sliderInput(inputId = 'Fmedium', label = '', value = 0.5, min = 0,
                             max = 3, step = 0.1)),
        column(3,sliderInput(inputId = 'Fmedium.after', label = '', value = 0.5, min = 0,
                             max = 3, step = 0.1))
      )
      ,
      fluidRow(
        column(2, br(),br(), p('Demersal fleet')),
        column(3,sliderInput(inputId = 'Flarge', label = '', value = 0.5, min = 0,
                             max = 3, step = 0.1)),
        column(3,sliderInput(inputId = 'Flarge.after', label = '', value = 0.5, min = 0,
                             max = 3, step = 0.1))
      ))
    ,
    # Plots:
    fluidRow(
      column(6,
             plotOutput(outputId = 'plotBiomass')),
      column(6,
             plotOutput(outputId = 'plotF')))
    ,
    fluidRow(
      column(6,
             plotOutput(outputId = 'plotYield')),
      column(6,
             plotOutput(outputId = 'plotSSB')))
    ,
    fluidRow(plotOutput(outputId = 'plotSpectrum', height = 600))
    ,
    wellPanel(
      p('The calculations are based on the model in ',
        a('Andersen et al (2016)', href='https://www.researchgate.net/publication/284514316_The_theoretical_foundations_for_size_spectrum_models_of_fish_communities'), 
        '. The calibrations for the specific ecosystem is described in ',
        a('Jacobsen et al (2016)', href='https://www.researchgate.net/publication/305110273_Efficiency_of_fisheries_is_increasing_at_the_ecosystem_level?_iepl%5BviewId%5D=dbdozbH76RI7ERE6VF6iTz8O&_iepl%5BprofilePublicationItemVariant%5D=default&_iepl%5Bcontexts%5D%5B0%5D=prfpi&_iepl%5BinteractionType%5D=publicationTitle'),
        '. Code by ',
        a("Nis Sand Jacobsen", href='mailto:nisjac@uw.edu'),
        ' and ',
        a('Ken Haste Andersen', href = 'mailto:kha@aqua.dtu.dk'),
        '. Output of the model should not be used in a practical management setting without first consulting the authors. ')
    )
  ),
  server = function(input,output){
    #
    # Define fleet sizes:
    #
    wMiddle <- 100
    wLarge <- 3000
    #
    # Make the description of the selected ecosystem:
    #
    output$EcosystemDescription <- renderText({
      
      if (input$Parameterset=='Generic')
        return("The generic trait-based model containing species with asymptotic sizes between 4 g and 50 kg.")
      
      if (input$Parameterset=='North Sea')
        return("North Sea ecosystem. Contains the following species:<br> 
               sandeel, sprat, norway pout, herring, sole, plaice, haddock, pollock, cod, whiting.")
      
      if (input$Parameterset == 'Benguela Current')
        return("Benguela current ecosystem. Contains the following species:<br>
               anchovy, sardine, kingklip, shallow-water hake, deep-water hake.")
      
      if (input$Parameterset == 'Baltic Sea')
        return("Baltic Sea ecosystem. Contains the following species:<br>
            sprat, herring, cod.")
      
      if (input$Parameterset == 'Northeast US Cont. Shelf')
        return("Northeast us continential shelf ecosystem. Contains the following species:<br>
  Atlantic butterfish, herring, menhaden, yellowtail flounder, acadian redfish, witch flounder, 
Atlantic croaker, winter flounder, black sea bass, american plaice, weakfish
spiny dogfish, bluefish, summer flounder, haddock, monkfish, cusk, tilefish, pollock,
white hake, striped bass, atlantic cod, atlantic halibut.")
      
      if (input$Parameterset == 'Barents Sea')
        return("Barents sea ecosystem. Contains the following species:<br>
capelin, pollock, golden redfish, greenland halibut, haddock, atlantic cod.")
    })
      #
      #   Run simulation when the button is clicked
      #
      simResults <- eventReactive(input$click,{
        #
        # Set sizes of fleets:
        #
        wMiddle <- input$wMiddle
        wLarge <- input$wLarge
        if (wMiddle > wLarge){
          wMiddle <- wLarge}
        wsize <- c(wMiddle, wLarge)
        #
        # Set fishing mortalities:
        #
        F0 <- matrix(NA,3,2)
        F0[1,1] <- input$Fsmall
        F0[2,1]<- input$Fmedium
        F0[3,1]<- input$Flarge
        F0[1,2]<- input$Fsmall.after
        F0[2,2]<- input$Fmedium.after
        F0[3,2]<- input$Flarge.after
        #
        # Run simulation:
        #
        SF <- baserun(
          nSpecies = 27,
          F0 = F0,
          S= NA, Parameterset = input$Parameterset, wsize)
        
        return(SF)
      })
      
      
      output$plotBiomass <- renderPlot(
        {
          
          SF <- simResults()[[1]] # Run before
          SF2 <- simResults()[[2]]  # Run after
          param <- simResults()[[3]] # params
          wMiddle <- input$wMiddle
          wLarge <- input$wLarge

          idx.biomass <- which(names(SF) == 'SSB')
          Biomass <- SF[[idx.biomass]]
          
          idx.time <- which(names(SF) == 't')
          time <- SF[[idx.time]]
          
          # Sum biomass in small medium and large
          if (length(which(param$wInf < wMiddle)) > 1){
            Biomass.small <- rowSums(Biomass[,which(param$wInf < wMiddle)])
          }else{Biomass.small <- Biomass[,which(param$wInf < wMiddle)]}
          
          if (length(which(param$wInf <= wLarge & param$wInf > wMiddle)) > 1){
            Biomass.medium <- rowSums(Biomass[,which(param$wInf <= wLarge & param$wInf > wMiddle)])
          }else{Biomass.medium <- Biomass[,which(param$wInf <= wLarge & param$wInf > wMiddle)]}
          
          if (length(which(param$wInf > wLarge)) > 1){
            Biomass.large <- rowSums(Biomass[,which(param$wInf > wLarge)])
          }else{Biomass.large <- Biomass[,which(param$wInf > wLarge)]}
          
          if(length(Biomass.small) == 0){
            Biomass.small <- rep(NA, length(time))
          }
          
          idx.biomass <- which(names(SF2) == 'SSB') # Overwrite and plot the after new fishing
          Biomass <- SF2[[idx.biomass]]
          # Sum biomass in small medium and large
          if (length(which(param$wInf < wMiddle)) > 1){
            Biomass.small.a <- rowSums(Biomass[,which(param$wInf < wMiddle)])
          }else{Biomass.small.a <- Biomass[,which(param$wInf < wMiddle)]}
          
          if (length(which(param$wInf <= wLarge & param$wInf > wMiddle)) > 1){
            Biomass.medium.a <- rowSums(Biomass[,which(param$wInf <= wLarge & param$wInf > wMiddle)])
          }else{Biomass.medium.a <- Biomass[,which(param$wInf <= wLarge & param$wInf > wMiddle)]}
          
          if (length(which(param$wInf > wLarge)) > 1){
            Biomass.large.a <- rowSums(Biomass[,which(param$wInf > wLarge)])
          }else{Biomass.large.a <- Biomass[,which(param$wInf > wLarge)]}
          
          if(length(Biomass.small.a) == 0){
            Biomass.small.a <- rep(NA, length(time))
          }
          # Plot the time varying biomass
          
          # axis limits
          minYl <- min(c(Biomass.small,Biomass.medium,Biomass.large,Biomass.small.a,Biomass.medium.a,Biomass.large.a), na.rm = T)
          maxYl <- max(c(Biomass.small,Biomass.medium,Biomass.large,Biomass.small.a,Biomass.medium.a,Biomass.large.a), na.rm = T)
          
          title <- 'Development in spawning stock biomass'
          plot(time-time[length(time)], Biomass.small, log = 'y', xlab = 'Time (years)', ylab = 'Biomass (ton)', type = 'l', ylim =c(minYl,maxYl),
               xlim = c(-20, time[length(time)]), col = alpha('black', alpha =  0.5), main = title)
          if(length(Biomass.medium > 0)){
            lines(time-time[length(time)],Biomass.medium, col = alpha('black', alpha =  0.5), lwd = 2)
          }
          lines(time-time[length(time)],Biomass.large, col = alpha('black', alpha =  0.5), lwd = 3)
          
          #time2 <- seq(time[length(time)]+param$dt,2*time[length(time)], length.out = length(time))
          lines(time,Biomass.large.a, col = alpha('red', alpha =  0.3), lwd = 3)
          lines(time,Biomass.small.a, col = alpha('red', alpha =  0.3))
          if(length(Biomass.medium > 0)){
            lines(time,Biomass.medium.a, col = alpha('red', alpha =  0.3), lwd = 2)
          }
          legend('bottomright', legend = c('Small', 'Medium', 'Large'), lty = c(1,1,1), lwd = c(1,2,3), bty = 'n')
          # Add diagonal line
          lines(rep(0, 100), seq(min(minYl), max(maxYl), length.out = 100), lty = 2, col = 'black', lwd = 3)
        })
      
      
      output$plotF <- renderPlot({
        SF <- simResults()[[1]]
        param <- simResults()[[3]]
        SF2 <- simResults()[[2]]
        
        idx.fishing <- which(names(SF) == 'Fin')
        fishing <- SF[[idx.fishing]]
        
        idx.fishing <- which(names(SF2) == 'Fin')
        fishing.after <- SF2[[idx.fishing]]
        
        yl <- c(0,max(c(fishing,fishing.after)))
        title <- 'Fisheries selectivity'
        plot(SF$w,fishing[1,], log = 'x', type = 'l', lwd = 2, xlim = c(0.1,max(param$wInf)),
             col = alpha('black',alpha = 0.5), ylab = 'Fishing mortality (per year)',
             xlab = 'Weight (g)', ylim = yl, main = title)
        lines(SF2$w, fishing.after[1,], lwd = 2, col = alpha('red', alpha =  0.3))
        for (i in 2:param$nSpecies){
          ix = SF$w < param$wInf[i]
          lines(SF$w[ix], fishing[i,ix], lwd = 2, col = alpha('black',alpha = 0.5))
          lines(SF2$w[ix], fishing.after[i,ix], col = alpha('red', alpha =  0.5))
        }
        legend('topleft', legend = c('Before', 'After'), lty = c(1,1), col = c('black','red'), bty = 'n')
        
      })
      
      output$plotYield <- renderPlot({
        
        SF <- simResults()[[1]]
        SF2 <- simResults()[[2]]      #param <- MM[[which(MM == 'param')]]
        param <- simResults()[[3]]
        
        idx.Yield <- which(names(SF) == 'Yield')
        Yield <- SF[[idx.Yield]]
        idx.Yield <- which(names(SF2) == 'Yield')
        Yield2 <- SF2[[idx.Yield]]
        
        # Sum biomass in small medium and large
        yl <- c(min(c(Yield[Yield>0],Yield2[Yield2>0])),
                max(c(Yield[Yield>0],Yield2[Yield2>0])))
        
        if (input$Parameterset == 'Generic'){
          plot(param$wInf,Yield, log = 'xy', col = alpha('black',alpha = 0.5), type = 'l', 
               xlab = 'Asymptotic weight (g)', 
               ylab = 'Yield (ton/km2/yr)',
               ylim = yl,
               xlim = c(min(param$wInf),max(param$wInf)),
               main = 'Yield (ton/km2/yr)', lwd = 3)
          lines(param$wInf,Yield2, col = alpha('red', alpha = 0.3), lwd = 3)
          lines(rep(input$wMiddle,100), seq(1e-15,yl[2]+1000, length.out = 100), lty = 2)
          lines(rep(input$wLarge, 100), seq(1e-15,yl[2]+1000, length.out = 100), lty = 2)
          legend('bottomright', legend = c('Before', 'After'), lty = c(1,1), 
                 col = c(alpha('black', alpha = 0.5),'red'), 
                 bty = 'n')
          fmt <- '%3.2f '
        }else{
          plot(param$wInf,Yield, log = 'xy', col = alpha('black',alpha = 0.5),
               xlab = 'Asymptotic weight (g)', 
               ylab = 'Yield (ton/yr)',
               ylim = yl,
               main = 'Yield (ton/yr)', pch = 16, lwd = 3, cex = 2)
          points(param$wInf,Yield2, col = alpha('red', alpha = 0.5), cex = 2, lwd = 3)
          lines(rep(input$wMiddle,100), seq(1e-15,yl[2]+1000, length.out = 100), lty = 2)
          lines(rep(input$wLarge,100), seq(1e-15,yl[2]+1000, length.out = 100), lty = 2)
          legend('bottomleft', legend = c('Before', 'After'), pch = c(16,1), col = c(alpha('black', alpha = 0.5),'red'), bty = 'n')
          # Print summed yields:
          fmt <- '%0.2e '
        }
        
        # Print summed yields:
        text(x=min(param$wInf)*sqrt(input$wMiddle/min(param$wInf)), 
             y=yl[1]*(yl[2]/yl[1])^0.95, adj=0.5,
             labels=sprintf(fmt, sum(Yield2[param$wInf<input$wMiddle])), col='red')
        text(x=input$wMiddle*sqrt(input$wLarge/input$wMiddle), 
             y=yl[1]*(yl[2]/yl[1])^0.95, adj=0.5,
             labels=sprintf(fmt, sum(Yield2[param$wInf>input$wMiddle & param$wInf<input$wLarge])), col='red')
        text(x=input$wLarge*sqrt(max(param$wInf)/input$wLarge), 
             y=yl[1]*(yl[2]/yl[1])^0.95, adj=0.5,
             labels=sprintf(fmt, sum(Yield2[param$wInf>=input$wLarge])), col='red')

        text(x=min(param$wInf)*sqrt(input$wMiddle/min(param$wInf)), 
             y=yl[1]*(yl[2]/yl[1])^0.85, adj=0.5,
             labels=sprintf(fmt, sum(Yield[param$wInf<input$wMiddle])), col='black')
        text(x=input$wMiddle*sqrt(input$wLarge/input$wMiddle), 
             y=yl[1]*(yl[2]/yl[1])^0.85, adj=0.5,
             labels=sprintf(fmt, sum(Yield[param$wInf>input$wMiddle & param$wInf<input$wLarge])), col='black')
        text(x=input$wLarge*sqrt(max(param$wInf)/input$wLarge), 
             y=yl[1]*(yl[2]/yl[1])^0.85, adj=0.5,
             labels=sprintf(fmt, sum(Yield[param$wInf>=input$wLarge])), col='black')
      })
      
      output$plotSSB <- renderPlot({
        SF <- simResults()[[1]]
        SF2 <- simResults()[[2]]      #param <- MM[[which(MM == 'param')]]
        param <- simResults()[[3]]
        
        SSB <- calcSSB(param,SF,length(SF$t))
        SSB2 <- calcSSB(param,SF2,length(SF2$t))
        # Sum biomass in small medium and large
        yl <- c(min(c(SSB,SSB2)),
                max(c(SSB,SSB2)))
        
        title <- 'Spawning stock biomass'
        
        if (input$Parameterset == 'Generic'){
          plot(param$wInf,SSB, log = 'xy', col = alpha('black',alpha = 0.5), type = 'l',
               xlab = 'Asymptotic weight (g)', 
               ylab = 'Spawning stock biomass',
               ylim = yl,
               xlim = c(min(param$wInf),max(param$wInf)),
               main = title, lwd = 3)
          lines(param$wInf,SSB2, col = alpha('red', alpha = 0.3), lwd = 3)
          lines(rep(input$wMiddle,100), seq(1e-15,yl[2]+1000, length.out = 100), lty = 2)
          lines(rep(input$wLarge,100), seq(1e-15,yl[2]+1000, length.out = 100), lty = 2)
          legend('bottomright', legend = c('Before', 'After'), lty = c(1,1), col = c(alpha('black', alpha = 0.5),'red'), bty = 'n')
        }else{
          plot(param$wInf,SSB, log = 'xy', col = alpha('black',alpha = 0.5),
               xlab = 'Asymptotic weight (g)', 
               ylab = 'Spawning stock biomass (ton)',
               ylim = yl,
               main = title, pch = 16, lwd = 3, cex = 2)
          points(param$wInf,SSB2, col = alpha('red', alpha = 0.5), cex = 2, lwd = 3)
          lines(rep(input$wMiddle,100), seq(1e-15,yl[2]+1000, length.out = 100), lty = 2)
          lines(rep(input$wLarge,wMiddle), seq(1e-15,yl[2]+1000, length.out = 100), lty = 2)
          legend('bottomleft', legend = c('Before', 'After'), pch = c(16,1), col = c(alpha('black', alpha = 0.5),'red'), bty = 'n')
        }
      })
      
      output$plotSpectrum <- renderPlot(expr={
        
        SF <- simResults()[[1]]
        SF2 <- simResults()[[2]]      #param <- MM[[which(MM == 'param')]]
        param <- simResults()[[3]]
        
        idx.N <- which(names(SF) == 'N')
        N <- SF[[idx.N]]
        
        idxEnd <- param$tEnd/param$dt
        Spectrum1 <- N[idxEnd,,]
        Spectrum1[Spectrum1 == 0] <- NA # For plotting on log scale 
        
        idx.N <- which(names(SF2) == 'N')
        N <- SF2[[idx.N]]
        Spectrum2 <- SF2$N[idxEnd,,]
        Spectrum2[Spectrum2 == 0] <- NA # For plotting
        
        w <- SF$w
        title <- 'Biomass density at equilibrium'
        
        #yl <- c(2*min(c(t(replicate(param$nSpecies, w))*Spectrum1,t(replicate(param$nSpecies, w))*Spectrum2), na.rm = T), 
        #        100*max(c(t(replicate(param$nSpecies, w))*Spectrum1,t(replicate(param$nSpecies, w))*Spectrum2), na.rm = T))
        yl <- param$kappaR * c(0.001*max(w)^(1+param$kR) , 0.1*0.02^(1+param$kR))
        
        plot(w,Spectrum1[1,]*w, log = 'xy', type = 'l', col = alpha('black',alpha = 0.3),
             ylab = 'Biomass density (-)', 
             xlab = 'Weight (g)', 
             main = title, 
             ylim=yl, xlim=c(0.02, max(w)))
        lines(w,Spectrum2[1,]*w, col = alpha('red',alpha = 0.3))
        
        for (i in 2:param$nSpecies){
          lines(w,Spectrum1[i,]*w, col = alpha('black',alpha = 0.3))
          lines(w,Spectrum2[i,]*w, col = alpha('red',alpha = 0.3))
        }
        #
        # Community spectrum:
        #
        lines(w, SF$Ntot[idxEnd,]*w, col=alpha('black',alpha = 0.3), lwd=3)
        lines(w, SF2$Ntot[idxEnd,]*w, col=alpha('red',alpha = 0.3), lwd=3)
        #
        # Resource
        #
        wPP <- SF$wPP
        lines(wPP, SF$nPP[idxEnd,1,]*wPP, col=alpha('black',alpha = 0.3), lwd=3, lty=2)
        lines(wPP, SF2$nPP[idxEnd,1,]*wPP, col=alpha('red',alpha = 0.3), lwd=3, lty=2)
      })
      
      
  }))
}
