#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(xtable)
library(knitr)
library(plotly)
library(DT)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  
  
  # html
  
  tags$head(
    tags$style(HTML("
                   @import url('https://fonts.googleapis.com/css?family=Dosis');
 
                    h1 {
                    font-family: 'Dosis';
                    font-weight: 1000;
                    line-height: 1.1;
                    color: #C51162;
                    }
                    h2 {
                    font-family: 'Dosis';
                    font-weight: 200;
                    line-height: 1.1;
                    color: #607D8B;
                    }
                    h3{
                    font-family: 'Dosis';
                    font-weight: 700;
                    line-height: 1.1;
                    color: #000000;
                    
                    }
                    h4{
                    font-family: 'Dosis';
                    font-weight: 400;
                    line-height: 1.1;
                    color: #607D8B;
                    
                    }
                    body {
                    background-color: #fff;
                    }
                    "))
    ),
  
  
  # Application title
  headerPanel("Estadistica Computacional"),
  #
  h3("Tareas"),
  br(),
  br(),  
  
        navbarPage(title = "Menu",
             tabPanel(title = "Home",
                      h4("Victor Manuel Montoya M"),
                      h4("CU: 126459"),
                      h2("Tareas del Semestre:"),
                      h2("1. Funcion Inversa"),
                      h2("2. Integracion Monte Carlo"),
                      h2("3. Regresion Bayesiana")
                      ),#panel home
             #EMPIEZA SIMULACION INVERSA
             tabPanel(title = "Exponencial Inversa",
                      headerPanel("1. Funcion Inversa"),
                      h2("Simulacion de una Distribucion Exponencial mediante Funcion 
                         Inversa de la Distribucion Acumulada"),
                      #cuerpo inversa
                      sidebarPanel(width = 3,
                                   strong(h3("Parametro de la distribucion")),
                                   numericInput("lambda2", "lambda:", value = 4, 
                                                min = .0001, step = 1),
                                   br(),
                                   strong(h3("Numero de Simulaciones")),
                                   sliderInput("n.sims2", "Simulaciones:", 
                                               min = 10, max = 5000, value = 1107, 
                                               step = 10),
                                   br(),
                                   submitButton("Update View", icon("refresh")),
                                   br(),
                                   strong(h3("Descarga simulacion")),
                                   downloadButton('downloadsiminver', 'Download ')
                      ), #SidebAR PANEL INVERSA
                      mainPanel(
                        h2("Histograma de las simulaciones:"),
                        plotlyOutput("gghisto"),
                        br(),
                        h2("Datos centrales de la simulacion"),
                        verbatimTextOutput("stats"),
                        br(),
                        h2("Grafica de ajuste"),
                        plotlyOutput("ggacumu"),
                        br(),
                        h2("Prueba Kolmogorov-Smirnov:"),
                        textOutput("ksprueba2"),
                        br(),
                        h2("Estadisticos:"),
                        tableOutput("tabesta2"),
                        br(),
                        h2("QQ Empirica vs Teorica"),
                        plotlyOutput("ggqq")
                      )
                      
            ),#panel integracion
             tabPanel(title = "Integracion Monte Carlo",
                      headerPanel("2. Integracion Monte Carlo"),
                      h2("Integracion numerica de una funcion definida, dado
                         limite inferior y superior, mediante simulacion Monte Carlo"),
                      #cuerpo montecarlo
                      sidebarPanel(width = 3,
                                   h2('Parametros: '),
                                   strong(h3("Funcion a integrar f(x)")),
                                   textInput(inputId = "exprf3", 
                                             label = 'Escribe una funcion', 
                                             value = "sin(x)"),
                                   br(),
                                   strong(h3("Nivel de significancia")),
                                   sliderInput("alpha3", "Ingresa alpha", value = 0.05,
                                               min = .001, max = .9, step = .001),
                                   br(),
                                   strong(h3("Limitestes de integral")),
                                   numericInput("vala3", "Ingresa limite inferior", 0),
                                   numericInput("valb3", "Ingresa limite superior", 4),
                                   br(),
                                   strong(h4("Simulaciones")),
                                   sliderInput("nsimsmax3", "Numero Maximo: ",
                                               min = 30, max = 5000, value = 1107, step = 10),
                                   br(),
                                   submitButton("Update View", icon("refresh")),
                                   br(),
                                   strong(h3("Descarga simulacion")),
                                   downloadButton('downloadintemc', 'Download ')
                      ),
                      mainPanel(
                        h2("Funcion y Area bajo la Curva:"),
                        plotOutput("gg.funf3"),
                        h2("Valores de estimacion Monte Carlo:"),
                        uiOutput("xtab.sim3"),
                        #tableOutput("xtesttest"),
                        h2("Intervalos de confianza para diferentes numero de simulacionan:"),
                        plotOutput("gg.simsint3")
                      )
                      
            ),#panel montecarlo
             tabPanel(title = "Regresion Bayesiana",
                      headerPanel("3. Regresion Lineal Bayesiana"),
                      h2("Analisis Bayesiano de Regresion Lineal Simple"),
                      #cuerpo regresion lineal
                      #sidebar bayesiana
                      sidebarPanel(width = 3,
                                   h2('Parametros funcion ApriorI: '),
                                   strong(h3("Parametros de a")),
                                   fluidRow(
                                     column(5, numericInput(inputId	="muaui", label = "Media a:", 
                                                            width = 100,
                                                            min = -100, max = 100, value = 0
                                     )),
                                     column(6, numericInput(inputId	="saui", label = "D. Estandar a:",
                                                            width = 100,
                                                            min = 0.01, max = 100, value = 10
                                     ))
                                   ),
                                   strong(h3("Parametros de b")),
                                   fluidRow(
                                     column(5, numericInput(inputId	="mubui", label = "Media b:", 
                                                            width = 100,
                                                            min = -100, max = 100, value = 0
                                     )),
                                     column(6, numericInput(inputId	="sbui", label = "D. Estandar b:",
                                                            width = 100,
                                                            min = 0.01, max = 100, value = 10
                                     ))
                                   ),
                                   strong(h3("Parametros de s")),
                                   fluidRow(
                                     column(5, numericInput(inputId = "rs", "Rate b:", 
                                                            width = 100,
                                                            min = 0.01, max = 100, value = 1
                                     )),
                                     column(6, numericInput(inputId = "ss", "Shape b:", 
                                                            width = 100,
                                                            min = 0.01, max = 100, value = 1
                                     ))
                                   ),
                                   br(),
                                   h2("Parametros Metropolis"),
                                   strong(h4("Numero de Simulaciones")),
                                   sliderInput("nsimbayeui", "Elige el numero: ",
                                               min = 30, max = 5000, value = 380, step = 10),
                                   br(),
                                   strong(h4("Numero de Cadenas")),
                                   sliderInput("ncadeui", "Elige el numero: ",
                                               min = 1, max = 5, value = 2, step = 1),
                                   sliderInput("burninui", label = "Proporcion burn-in",
                                               min = 0, max = .99, value = .13, step = .01),
                                   br(),
                                   submitButton("Update View", icon("refresh"))
                      ),
                      mainPanel(
                        h2("ENSANUT MUJERES 2006, PESO Y TALLA:"),
                        h3("Datos provenientes de la Encuesta Nacional de Salud 2006, Mujeres"),
                        h4("http://datos.gob.mx/busca/dataset/encuesta-nacional-de-salud-y-nutricion-2006-ensanut"),
                        DT::dataTableOutput("datatableensanut"),
                        h2("Dispersion de los datos:"),
                        plotOutput("ggensadisp", width = 450, height = 350),
                        br(),
                        h2("Datos centrales de datos de peso ensanut"),
                        verbatimTextOutput("statsensa1"),
                        h2("Datos centrales de datos de talla ensanut"),
                        verbatimTextOutput("statsensa2"),
                        h2("Estimacion lineal por minimos cuadrados X=PESO, Y=TALLA"),
                        verbatimTextOutput("statsensa3"),
                        h2("Funciones a priori de parÃ¡metros a, b y s:"),
                        plotOutput("ggprioris", width = 900, height = 300),
                        h2("SimulaciÃ³n MCMC"),
                        h4("Densidad Simulaciones"),
                        plotOutput("ggsimusmcmc", width = 900, height = 300),
                        h4("Cadena"),
                        plotOutput("gglinearmcmc", width = 900, height = 300),
                        h4("Resumen de parametros"),
                        uiOutput("resumcmcsim")
                      )#Panel central bayesiana
                 )#panel bayesiana
        )#acaban todos los paneles
  ) # fluidPage
) # shinyUI