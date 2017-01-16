#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(xtable)
library(knitr)
library(plotly)
library(Rcpp)
#carga funcion R
source("Funciones/funciones.R")

sourceCpp("Funciones/funciones.cpp")

shinyServer(function(input, output) {
  
  #############################################################
  #############################################################
  #############################################################
  ######FUNCION INVERSA
  #############################################################
  
  #FUNCION INVERSA HECHA EN CLASE
  ExpInv <- function(n.sims, lambda){
    u <- runif(n.sims)
    -(log(1-u))*(lambda^(-1))
  }
  
  #simulaciÃÂÃÂ³n del vector
  dataInput2 <- reactive({
                sims.vec <- data.frame(
                                      simulaciona = ExpInv(
                                      n.sims = input$n.sims2, 
                                      lambda = input$lambda2),
                                      dist = 'dist. 1')
                data.frame(rbind(sims.vec))
  })
  
  #histograma de las simulaciones
  output$gghisto <- renderPlotly({
                        datahisto <- dataInput2()
                        gg <- ggplot(datahisto, 
                                    aes(x = simulaciona)) + 
                              geom_histogram(alpha = .5, fill = '#D81B60', 
                                      bins = 45, show.legend = T) + 
                              ylab('frecuencia') +
                              xlab('Valor Simulado') 
                        ggplotly(gg)
  })
  
  #tabla con los datos de la simulacion
  output$stats <- renderPrint({
                  tab<-dataInput2()
                  summary(tab$simulaciona)
  })
  
  #acumulaciÃÂÃÂ³n de teÃÂÃÂ³rica contra empÃÂÃÂ­rica
  output$ggacumu<-renderPlotly({
    tab <- dataInput2()
    data.cum <- data.frame(
                  x = sort(tab$simulaciona)
                ) %>% 
                mutate(
                      n = 1/length(x),
                      acum = cumsum(n)
                      )
    
    gg.acum<-ggplot(data.cum, aes(x = x, y = acum)) + 
              geom_line(color = '#D81B60') + 
              stat_function(fun = 'pexp', args = list(rate = input$lambda2), 
                    color = '#006064') + 
              ylab('Dist. acumulada') + 
              xlab('Simulaciones')
    
    ggplotly(gg.acum, tooltip = c("y", "x"))
  })
  
  #prueba kolmogorov smirnov
  output$ksprueba2<- renderText({
    tab <- dataInput2()
    
    ksprueba <- ks.test(x = tab$simulaciona, 
                        y = "pexp", 
                        rate = input$lambda2)
    
    valorpi <- round(as.numeric(ksprueba$p.value),2)
    
    ifelse( valorpi < .05, 
            paste("Con p-value de ", valorpi,
                  " ,dada la muestra simulada, no se rechaza que esta tenga 
                    una distribucionn distinta de la exponencial."), 
            paste("Con p-value ", valorpi, 
                  " ,dada la muestra simulada, se rechaza que esta tenga 
                    una distribucionn distinta de la exponencial.")
            )
  })
  
  #tabla de estadisticos
  output$tabesta2 <- renderTable({
    datasimu <- dataInput2()
    tabesta <- data.frame(
                estadistico = c('E(X)= 1/lambda','V(x)= 1/lambda^2'),
                teorico = c(1/input$lambda2, 1/input$lambda2^2),
                empirico = round(c(mean(datasimu$simulaciona), 
                                    var(datasimu$simulaciona)
                                   ),2)  
                )
    rownames(tabesta) <- c("E[x]", "Var[X]")
    
    tab <- print( xtable(tabesta
                  ),
                  floating=FALSE, tabular.environment="array", 
                  comment=FALSE, print.results=FALSE)
    html <- paste0("$$", tab, "$$")
    
    withMathJax(HTML(html))
    
    tabesta
  })
  
  
  #qq plot teorico vs empirico
  output$ggqq <- renderPlotly({
                datasimus <- dataInput2()
                grafqq <- ggplot(datasimus, aes(sample = simulaciona)) + 
                          geom_abline(slope = 1, color = '#006064') +
                          stat_qq(distribution = qexp, dparams = input$lambda2, 
                          size = .5, color = '#D81B60') 
                #objeto gg
                ggplotly(grafqq, tooltip = c("Simulacion", "teorica"))
  })
  
  output$downloadsiminver <- downloadHandler(
    filename = function() {
      paste("simulacion_exponencial_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {

      impridatos <- dataInput2()

      write.csv(impridatos, file)
    }
  )
  
  #############################################################
  #############################################################
  #############################################################
  ######INTEGRACION MONTECARLO
  #############################################################

  
  Integrafun <- reactive({
    texto <- paste("aux <- function(x) (",
                   input$valb3, "-", input$vala3,")*", 
                   input$exprf3)
    eval(parse(text = texto))
    aux
  })
  
  Intreact <- reactive({
    fint <- Integrafun()
    montemastersim <- function(n.sims){

      montesim <- data.frame(
        unis = runif(n.sims, min = input$vala3, max = input$valb3)
      )%>% 
        mutate(unisf = fint(unis))
      estims <- montesim %>% 
        summarise(simid = n.sims,
                  muest = abs(mean(unisf)), 
                  sdest = sd(unisf),
                  confiinter = qnorm(input$alpha3/2, lower.tail = F)*sdest/sqrt(n.sims),
                  lowint = muest - confiinter,
                  uppint = muest + confiinter)
      estims
    }
    dat.ints <- lapply( seq(10, input$nsimsmax3, by = 10), montemastersim) %>% 
    bind_rows()
    dat.ints
  })
  
  output$gg.funf3 <- renderPlot({
    num.mu <- filter(Intreact(), simid == max(simid)) %>% .$muest %>% unique
    lab.mu <- paste("Area estimada: ", round(num.mu, 2)  )
    gg <- ggplot(data.frame(x = c(input$vala3, input$valb3)), aes(x)) +
      stat_function(fun = Integrafun(), geom = "line", 
                    size = 1.5, color = '#D81B60') +
      stat_function(fun = Integrafun(), geom = "ribbon",
                    mapping = aes(ymin = 0, ymax = ..y..),
                    fill = "#D81B60", alpha = .3) + 
      geom_label(x = (-input$vala3 + input$valb3)/2, y = 0.3, 
                 label = lab.mu,
                 colour = "#D81B60", size = 6 )+ 
      ylab('f(x)')+ 
      xlab("Rango de integracion") + 
      ggtitle("Funcion") 
    print(gg)
  })
  
  output$xtab.sim3 <- renderUI({
    tab <- Intreact() %>% 
      filter(simid == max(simid)) %>% 
      .[, c(-1, -4)] 
    names(tab) <- c("Val. Esperado", "Desv. Estandar",
                    'Lim. Inf', 'Lim. Sup')
    tab <- print( xtable(tab, 
                         align=rep("c", ncol(tab)+1), 
                         digits = c(0, 2, 2, 2, 2)),
                  include.rownames=FALSE,
                  floating=FALSE, tabular.environment="array", 
                  comment=FALSE, print.results=FALSE)
    html <- paste0("$$", tab, "$$")
    withMathJax(HTML(html))
    
  })
  

  output$gg.simsint3 <- renderPlot({
    gg <- ggplot(Intreact(), aes(x = simid, y = muest)) + 
      geom_ribbon(aes(ymin = lowint, ymax = uppint), 
                  alpha =.3, fill = '#D81B60') + 
      geom_line(color = 'gray50', size = 1) + 
      ylab('Estimacion de integracion') + 
      scale_x_log10()+
      xlab("Simulaciones")+ 
      ggtitle("Intervalos de Confianza")
    print(gg)
  })
  
  output$downloadintemc <- downloadHandler(
    filename = function() {
      paste("integracion_mc_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      impridatos <- Intreact()
      write.csv(impridatos, file)
    }
  )
  
  #############################################################
  #############################################################
  #############################################################
  ######REGRESION BAYESIANA
  #############################################################
  dataInput33 <- reactive({
    datos <- read_csv('Datos/ensatarea3final.csv')
    datos
  })
  
  
  #tabla ensanut
  output$datatableensanut<- DT::renderDataTable(DT::datatable({
    #datos <- read_csv('Datos/ensatarea3final.csv')
    datos<-dataInput33()
    datos
  }))#tabla ensanut
  
  #dipsersion de datos
  output$ggensadisp<- renderPlot({
    #datos <- read_csv('Datos/ensatarea3final.csv')
    datos<-dataInput33()
     scater <- ggplot(datos, aes(x = PESON, y = TALLAN)) + 
                  geom_point(colour='#D81B60')+
                  xlab("Peso")+
                  ylab("Talla")
     #objeto gg
     print(scater)
   })#dispersion de datos
   
  #funciones a priori tarea 4
  output$ggprioris <- renderPlot({
    
  #  gg1 <- ggplot(data.frame(x = rnorm(100, input$muaui, input$saui)), 
  #                aes(x = x)) + 
  #    stat_function(fun = dnorm, colour = "#00E676", 
  #                  size = 2,
  #                  args = list(mean = input$muaui, 
  #                              sd = input$saui)) + 
  #    ggtitle("Funcion apriori de 'a', normal: ") +
  #    ylab("Densidad") + 
  #    xlab("Soporte") 
    
    gg1 <- ggplot(data.frame(x = runif(100,min = 0,max=input$lsaui)), 
                                 aes(x = x)) + 
                     stat_function(fun = dunif, colour = "#00E676", 
                                   size = 2,
                                   args = list(min = 0,max=input$lsaui)) + 
                     ggtitle("Funcion apriori de 'a', uniforme: ") +
                     ylab("Densidad") + 
                     xlab("Soporte") 
  
    
    gg2 <- ggplot(data.frame(x = rnorm(100, input$mubui, input$sbui)), 
                  aes(x = x)) + 
      stat_function(fun = dnorm, colour = "#00E676", 
                    size = 2,
                    args = list(mean = input$mubui, 
                                sd = input$sbui)) + 
      ggtitle("Funcion apriori de 'b', normal: ") +
      ylab("Densidad") + 
      xlab("Soporte") 
      
    
    #gg3 <- ggplot(data.frame(x = rgamma(100, shape = input$ss, 
    #                                    rate = input$rs)),
    gg3<- ggplot(data.frame(x=runif(n = 100,min = 0,max = input$rs)),
                  aes(x = x)) + 
      stat_function(fun = dunif, colour = "#00E676", 
                    size = 2,
                    args = list(min = 0,max = input$rs)) + 
      ggtitle("Funcion apriori de 's', uniforme: ") + 
      ylab("Densidad") + 
      xlab("Soporte")
    
    gridExtra::grid.arrange(gg1, gg2, gg3, nrow = 1)
  }) #funciones apriori tarea 4
  
  #simulacion mcmc
  sims_mcmcserv<- reactive({
    #datos <- read_csv('Datos/ensatarea3final.csv')
    datos<-dataInput33()
    
    cadenastot<- lapply(1:input$ncadeui,function(i){
      

      sims.mat<-runMCMC(datos$PESON,
                        datos$TALLAN,
                        startValue = c(1,2,3),
                        iterations = as.numeric(input$nsimbayeui),
                        lsa3 = input$lsaui,
                        mub3 = input$mubui,
                        sdb3 = input$sbui,
                        lss3 = input$rs
                        )%>% 
        
       
        as.data.frame() %>% 
        as.tbl() %>% 
        rename(a = V1, b = V2, s = V3) %>% 
        mutate(num.sim = parse_number(rownames(.)), 
               cadena = i) 
      
      print(tail(sims.mat))
      sims.mat
    })%>% 
      rbind_all() %>%
      mutate(cadena = factor(cadena))
    
    cadenastot
  })
  
  output$ggsimusmcmc<- renderPlot({
    sims.matall<- sims_mcmcserv() %>%
      gather(param, sim, a:s) %>%
      mutate(num.sim = parse_number(num.sim))
    
    ggplot(sims.matall, aes(x=sim, y=..density.., fill=cadena))+
      geom_histogram(alpha=.4)+
      facet_wrap(~param, scales = "free") + 
      xlab("Estimacion") + 
      ylab("Densidad")
  })
  
  output$gglinearmcmc <- renderPlot({
    sims.matall <- sims_mcmcserv() %>%
      gather(param, sim, a:s) %>%
      mutate(num.sim = parse_number(num.sim))
    
    ggplot(sims.matall, aes( x= num.sim, y = sim,
                            color = cadena, group = cadena)) +
      geom_line() +
      facet_wrap(~param, scales = "free") +
      ylab("Simulacion")
  })
  
  output$ggadjustsim<-renderPlot({
    #datos <- read_csv('Datos/ensatarea3final.csv')
    datos<-dataInput33()
    cadenastot <- sims_mcmcserv()
    datosburn <- floor(max(cadenastot$num.sim)*input$burninui)
    
    
    tablesimus <- cadenastot  %>%
      gather(param, sim, a:s) %>% 
      mutate(num.sim = parse_number(num.sim)) %>% 
      filter( num.sim >= datosburn)
      newa<-mean(tablesimus[tablesimus$param=='b',]$sim)
      newb<-mean(tablesimus[tablesimus$param=='a',]$sim)
      news<-mean(tablesimus[tablesimus$param=='s',]$sim)

    

      scaterad <- ggplot(datos, aes(x = PESON, y = TALLAN)) + 
        geom_point(colour='#D81B60')+
        geom_abline(intercept = newa, slope = newb,show.legend = T)+
        geom_smooth(method = "lm", se = FALSE,show.legend = T)+
        annotate("text",label="ajuste RL", colour='blue',x=65,y=175)+
        annotate("text",label="ajuste MH", colour='black',x=20,y=200)+
        xlab("Peso")+
        ylab("Talla")
      print(scaterad)
    
  })
  
  
  BajaSimus <- reactive({
    sims_mcmcserv()  %>%
      gather(param, sim, a:s) %>% 
      mutate(num.sim = parse_number(num.sim)) %>% 
      filter(param == input$bajaparam)
  })
  
  
  output$bajasimu <- downloadHandler(
    filename = function() { "simulacion_MH.csv" },
    content = function(file) {
      write.csv(BajaSimus(), file)
    }
  )
  
  output$tablasimu <- renderDataTable(
    BajaSimus() ,
    options = list(
      pageLength = 10
    )
  ) 
  
  
  
  
  
  
  
  output$resumcmcsim <- renderUI({
    
    cadenastot <- sims_mcmcserv()
    datosburn <- floor(max(cadenastot$num.sim)*input$burninui)
    
    tablesimus <- cadenastot  %>%
      gather(param, sim, a:s) %>% 
      mutate(num.sim = parse_number(num.sim)) %>% 
      filter( num.sim >= datosburn)
    

    tabsumma <- tablesimus %>% 
      group_by(param) %>% 
      summarize(
        promedio = mean(sim),
        sd = sd(sim),
        `p_2.5%` = quantile(sim, probs = .025),
        `p_25%` = quantile(sim, probs = .25),
        `p_50%` = quantile(sim, probs = .5),
        `p_75%` = quantile(sim, probs = .75),
        `p_97.5%` = quantile(sim, probs = .975),
        `p_99.5%` = quantile(sim, probs = .995)
      ) 
    
    if(input$ncadeui > 1){

      tabsumma <- tabsumma %>% 
        left_join(
          RhatFun(tablesimus, 
                  n.sims= input$nsimbayeui, 
                  n.cadenas = input$ncadeui ), 
          by = "param"
        )
    }
    
    columnilla <- paste("rr", 
                      paste(rep("c", ncol(tabsumma)-1 ), 
                            collapse = ""), 
                      collapse = "", sep = "")
    imprimetab <- print( tabsumma %>% 
                    rename(parametro = param)  %>% 
                    xtable(align = columnilla),
                  include.rownames = F, floating = FALSE, 
                  tabular.environment = "array", 
                  comment = FALSE, print.results = FALSE)
    html <- paste0("$$", imprimetab, "$$")
    withMathJax(HTML(html))
  }) 
  
  output$statsensa1 <- renderPrint({
    #datos <- read_csv('Datos/ensatarea3final.csv')
    datos<-dataInput33()
    summary(datos$PESON)
  })
  
  output$statsensa2 <- renderPrint({
    #datos <- read_csv('Datos/ensatarea3final.csv')
    datos<-dataInput33()
    summary(datos$TALLAN)
  })
  
  output$statsensa3 <- renderPrint({
    #datos <- read_csv('Datos/ensatarea3final.csv')
    datos<-dataInput33()
    x<- datos$PESON
    y<- datos$TALLAN
    dataaux<-lm(y~x)
    summary(dataaux)
  })
  
})
