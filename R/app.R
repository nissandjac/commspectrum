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
    h1('Size spectrum models'),
    wellPanel(
      h4('Fishing mortalities (per year)\n'),

      fluidRow(
        column(2, br(),br(), p('Small species')),
        column(3,sliderInput(inputId = 'Fsmall', label = '   Initial fishing level', value = 0.5, min = 0,
                             max = 2, step = 0.1)),
        column(3,sliderInput(inputId = 'Fsmall.after', label = '   New fishing level', value = 0.5, min = 0,
                             max = 2, step = 0.1)),
        column(3,selectInput(inputId = 'Parameterset', label = '   Parameter input',
                             choices =  c('Generic', 'North Sea', 'Baltic Sea', 
                                 'Benguela Current', 'Northeast US Cont. Shelf',
                                    'Barents Sea')))
      )
      ,
      fluidRow(
        column(2, br(),br(), p('Medium species')),
        column(3,sliderInput(inputId = 'Fmedium', label = '', value = 0.5, min = 0,
                             max = 2, step = 0.1)),
        column(3,sliderInput(inputId = 'Fmedium.after', label = '', value = 0.5, min = 0,
                             max = 2, step = 0.1))
      )
      ,
      fluidRow(
        column(2, br(),br(), p('Large species')),
        column(3,sliderInput(inputId = 'Flarge', label = '', value = 0.5, min = 0,
                             max = 2, step = 0.1)),
        column(3,sliderInput(inputId = 'Flarge.after', label = '', value = 0.5, min = 0,
                             max = 2, step = 0.1))
      ))
    ,
    actionButton(inputId = 'click', label = 'Start simulation'),

    #plotOutput(outputId = 'plotF'),
    fluidRow(
      column(6,
             plotOutput(outputId = 'plotBiomass')),
      column(6,
             plotOutput(outputId = 'plotF'))),


    fluidRow(
      column(6,
             plotOutput(outputId = 'plotYield')),
      column(6,
             plotOutput(outputId = 'plotSSB'))),

    fluidRow(6, 
             plotOutput(outputId = 'plotSpectrum')),

    wellPanel(
      helpText(a("code by Nis Sand Jacobsen",
                 href='mailto:nisjac@uw.edu')
      )
    )
  ),
  server = function(input,output){

    #
    #   data.small <- reactive({
    #     w <- 10^seq(log10(1),log10(input$maxsize),length.out = 100)
    #     sel <- input$Fsmall * (1+(w/(0.25 * 10))^-10)^-1
    #     return(data.frame(w = w, sel = sel))
    #   })

    runSim <- eventReactive(input$click,{
      # Run the size based model here
       # source('baserun.R')
      #
      F0 <- matrix(NA,3,2)
      F0[1,1] <- input$Fsmall
      F0[2,1]<- input$Fmedium
      F0[3,1]<- input$Flarge
      F0[1,2]<- input$Fsmall.after
      F0[2,2]<- input$Fmedium.after
      F0[3,2]<- input$Flarge.after

      SF <- baserun(
        nSpecies = 27,
        F0 = F0,
        S= NA, Parameterset = input$Parameterset)

      #Biomass<- SF$Biomass
      return(SF)
    })



    output$plotBiomass <- renderPlot(
      {

        SF <- runSim()[[1]] # Run before
        SF2 <- runSim()[[2]]  # Run after
        param <- runSim()[[3]] # params


        idx.biomass <- which(names(SF) == 'Biomass')
        Biomass <- SF[[idx.biomass]]
        
        idx.time <- which(names(SF) == 't')
        time <- SF[[idx.time]]
        
        # Sum biomass in small medium and large
        if (length(which(param$wInf < 100)) > 1){
          Biomass.small <- rowSums(Biomass[,which(param$wInf < 100)])
        }else{Biomass.small <- Biomass[,which(param$wInf < 100)]}

        if (length(which(param$wInf <= 3000 & param$wInf > 100)) > 1){
          Biomass.medium <- rowSums(Biomass[,which(param$wInf <= 3000 & param$wInf > 100)])
        }else{Biomass.medium <- Biomass[,which(param$wInf <= 3000 & param$wInf > 100)]}

        if (length(which(param$wInf > 3000)) > 1){
          Biomass.large <- rowSums(Biomass[,which(param$wInf > 3000)])
        }else{Biomass.large <- Biomass[,which(param$wInf > 3000)]}

        if(length(Biomass.small) == 0){
          Biomass.small <- rep(NA, length(time))
        }
        
        idx.biomass <- which(names(SF2) == 'Biomass') # Overwrite and plot the after new fishing
        Biomass <- SF2[[idx.biomass]]
        # Sum biomass in small medium and large
        if (length(which(param$wInf < 100)) > 1){
          Biomass.small.a <- rowSums(Biomass[,which(param$wInf < 100)])
        }else{Biomass.small.a <- Biomass[,which(param$wInf < 100)]}

        if (length(which(param$wInf <= 3000 & param$wInf > 100)) > 1){
          Biomass.medium.a <- rowSums(Biomass[,which(param$wInf <= 3000 & param$wInf > 100)])
        }else{Biomass.medium.a <- Biomass[,which(param$wInf <= 3000 & param$wInf > 100)]}

        if (length(which(param$wInf > 3000)) > 1){
          Biomass.large.a <- rowSums(Biomass[,which(param$wInf > 3000)])
        }else{Biomass.large.a <- Biomass[,which(param$wInf > 3000)]}

        if(length(Biomass.small.a) == 0){
          Biomass.small.a <- rep(NA, length(time))
        }
        # Plot the time varying biomass

        # axis limits
        minYl <- min(c(Biomass.small,Biomass.medium,Biomass.large,Biomass.small.a,Biomass.medium.a,Biomass.large.a), na.rm = T)
        maxYl <- max(c(Biomass.small,Biomass.medium,Biomass.large,Biomass.small.a,Biomass.medium.a,Biomass.large.a), na.rm = T)

        title <- 'Temporal Biomass evolution'
        plot(time-time[length(time)], Biomass.small, log = 'y', xlab = 'time (years)', ylab = 'Biomass (ton)', type = 'l', ylim =c(minYl,maxYl),
             xlim = c(-20, time[length(time)]), col = alpha('black', alpha =  0.5), main = title)
        lines(time-time[length(time)],Biomass.medium, col = alpha('black', alpha =  0.5), lwd = 2)
        lines(time-time[length(time)],Biomass.large, col = alpha('black', alpha =  0.5), lwd = 3)

        #time2 <- seq(time[length(time)]+param$dt,2*time[length(time)], length.out = length(time))
        lines(time,Biomass.large.a, col = alpha('red', alpha =  0.3), lwd = 3)
        lines(time,Biomass.small.a, col = alpha('red', alpha =  0.3))
        lines(time,Biomass.medium.a, col = alpha('red', alpha =  0.3), lwd = 2)
        legend('bottomright', legend = c('Small', 'Medium', 'Large'), lty = c(1,1,1), lwd = c(1,2,3), bty = 'n')
        # Add diagonal line
        lines(rep(0, 100), seq(min(minYl), max(maxYl), length.out = 100), lty = 2, col = 'black', lwd = 3)


      })
    output$plotF <- renderPlot({
      SF <- runSim()[[1]]
      param <- runSim()[[3]]
      SF2 <- runSim()[[2]]

      idx.fishing <- which(names(SF) == 'Fin')
      fishing <- SF[[idx.fishing]]

      idx.fishing <- which(names(SF2) == 'Fin')
      fishing.after <- SF2[[idx.fishing]]

      yl <- c(0,max(c(fishing,fishing.after)))
      title <- 'Fishing selectivity'
      plot(SF$w,fishing[1,], log = 'x', type = 'l', lwd = 2, xlim = c(0.1,max(param$wInf)),
           col = alpha('black',alpha = 0.5), ylab = 'Fishing mortality (per year)',
           xlab = 'weight (g)', ylim = yl, main = title)
      lines(SF2$w, fishing.after[1,], lwd = 2, col = alpha('red', alpha =  0.3))
      for (i in 2:param$nSpecies){
        lines(SF$w,fishing[i,], lwd = 2, col = alpha('black',alpha = 0.5))
        lines(SF2$w,fishing.after[i,], col = alpha('red', alpha =  0.5))
      }
      legend('topleft', legend = c('Before', 'after'), lty = c(1,1), col = c('black','red'), bty = 'n')

    })

    output$plotYield <- renderPlot({

      SF <- runSim()[[1]]
      SF2 <- runSim()[[2]]      #param <- MM[[which(MM == 'param')]]
      param <- runSim()[[3]]


      idx.Yield <- which(names(SF) == 'Yield')
      Yield <- SF[[idx.Yield]]
      idx.Yield <- which(names(SF2) == 'Yield')
      Yield2 <- SF2[[idx.Yield]]
      # Sum biomass in small medium and large
      yl <- c(min(c(Yield[Yield>0],Yield2[Yield2>0])),
              max(c(Yield[Yield>0],Yield2[Yield2>0])))

      title <- 'Yield at equilibrium'
      if (input$Parameterset == 'Generic'){
        plot(param$wInf,Yield, log = 'xy', col = alpha('black',alpha = 0.5), type = 'l', 
             xlab = 'Asymptotic weight (g)', 
             ylab = 'Yield (ton/yr)',
             ylim = yl,
           main = title, lwd = 3)
        lines(param$wInf,Yield2, col = alpha('red', alpha = 0.3), lwd = 3)
        lines(rep(100,100), seq(1e-15,yl[2]+100, length.out = 100), lty = 2)
        lines(rep(3000,100), seq(1e-15,yl[2]+100, length.out = 100), lty = 2)
        legend('bottomright', legend = c('Before', 'after'), lty = c(1,1), col = c(alpha('black', alpha = 0.5),'red'), bty = 'n')
      }else{
        plot(param$wInf,Yield, log = 'xy', col = alpha('black',alpha = 0.5),
             xlab = 'Asymptotic weight (g)', 
             ylab = 'Yield (ton/yr)',
             ylim = yl,
             main = title, pch = 16, lwd = 3, cex = 2)
        points(param$wInf,Yield2, col = alpha('red', alpha = 0.5), cex = 2, lwd = 3)
        lines(rep(100,100), seq(1e-15,yl[2]+1000, length.out = 100), lty = 2)
        lines(rep(3000,100), seq(1e-15,yl[2]+1000, length.out = 100), lty = 2)
        legend('bottomleft', legend = c('Before', 'after'), pch = c(16,1), col = c(alpha('black', alpha = 0.5),'red'), bty = 'n')
      }
    })

    output$plotSSB <- renderPlot({
      SF <- runSim()[[1]]
      SF2 <- runSim()[[2]]      #param <- MM[[which(MM == 'param')]]
      param <- runSim()[[3]]

      SSB <- calcSSB(param,SF,length(SF$t))
      SSB2 <- calcSSB(param,SF2,length(SF2$t))
      # Sum biomass in small medium and large
      yl <- c(min(c(SSB,SSB2)),
              max(c(SSB,SSB2)))

      title <- 'SSB at equilibrium'
      
      if (input$Parameterset == 'Generic'){
        plot(param$wInf,SSB, log = 'xy', col = alpha('black',alpha = 0.5), type = 'l',
             xlab = 'Asymptotic weight (g)', 
             ylab = 'Spawning stock biomass',
             ylim = yl,
             main = title, lwd = 3)
        lines(param$wInf,SSB2, col = alpha('red', alpha = 0.3), lwd = 3)
        lines(rep(100,100), seq(1e-15,yl[2]+100, length.out = 100), lty = 2)
        lines(rep(3000,100), seq(1e-15,yl[2]+100, length.out = 100), lty = 2)
        legend('bottomright', legend = c('Before', 'after'), lty = c(1,1), col = c(alpha('black', alpha = 0.5),'red'), bty = 'n')
      }else{
        plot(param$wInf,SSB, log = 'xy', col = alpha('black',alpha = 0.5),
             xlab = 'Asymptotic weight (g)', 
             ylab = 'Spawning stock biomass (ton)',
             ylim = yl,
             main = title, pch = 16, lwd = 3, cex = 2)
        points(param$wInf,SSB2, col = alpha('red', alpha = 0.5), cex = 2, lwd = 3)
        lines(rep(100,100), seq(1e-15,yl[2]+1000, length.out = 100), lty = 2)
        lines(rep(3000,100), seq(1e-15,yl[2]+1000, length.out = 100), lty = 2)
        legend('bottomleft', legend = c('Before', 'after'), pch = c(16,1), col = c(alpha('black', alpha = 0.5),'red'), bty = 'n')
      }
      
      
      
    })

    output$plotSpectrum <- renderPlot(expr={

      SF <- runSim()[[1]]
      SF2 <- runSim()[[2]]      #param <- MM[[which(MM == 'param')]]
      param <- runSim()[[3]]

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

      yl <- c(2*min(c(t(replicate(param$nSpecies, w))*Spectrum1,t(replicate(param$nSpecies, w))*Spectrum2), na.rm = T), 
              100*max(c(t(replicate(param$nSpecies, w))*Spectrum1,t(replicate(param$nSpecies, w))*Spectrum2), na.rm = T))
      
      plot(w,Spectrum1[1,]*w, log = 'xy', type = 'l', col = alpha('black',alpha = 0.3),
           ylab = 'Biomass density (-)', 
           xlab = 'Weight (g)', 
           main = title, 
           ylim=yl, xlim=c(0.02, 1e5))
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


  }, options = list(height = 2080)))
}
