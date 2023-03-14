#CARGAR LIBRERIAS
library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(tinytex)
library(dplyr)
library(plotly)
library(ggeasy)


#DATASET A USAR R AIRQUALITY
data("airquality")
#DATAFRAME
datosairq = data.frame(airquality)
#OMITIR VALORES NA DEL DATAFRAME
dtna <-datosairq %>% na.omit()
#FUNCION FACTOR PARA DATA CATEGORICA MONTH
dtna$Month=factor(dtna$Month,ordered=TRUE,labels = c("May", "June", "July", "August", "September"))

#INFORMACION SOBRE EL DATASET
help(airquality)


#SHINY APP

shinyApp(
  ui = fluidPage(
    theme = bs_theme(version = 5, bootswatch = 'minty'),
    titlePanel(h1("Examen final",tags$br(),"Dataset: airquality", align ="center")),
    br(),
    sidebarLayout(
      sidebarPanel(shinyjs::useShinyjs(),
                   id = "side-panel",
                   selectInput("variab", label = "Seleccionar variable para histograma:", c("Ozone"=1, "Solar.R"=2,"Wind"=3, "Temp"=4),
                               selected=1, selectize=FALSE),
                   selectInput("variab1", label = "Seleccionar variable para boxplot, scatterplot, line chart y regresión lineal:", c("Ozone"="Ozone", "Solar.R"= "Solar.R","Wind"="Wind"),
                                      selected=1),
                   sliderInput("ancho", "Seleccionar ancho de la gráfica:",min = 500, max = 750, value = 250),
                   sliderInput("alto", "Seleccionar alto de la gráfica:",min = 300, max = 420, value = 250),
                   
                   radioButtons(inputId="color",label="Seleccionar color de histograma, line chart y línea de regresión lineal",
                                choices=c("#F3969A","#FF7851","#78C2AD"),
                                selected="#F3969A"),
                   splitLayout(
                     radioButtons(inputId="plotext",label="Exportar plot:",choices=c("png","pdf"),selected="png"),
                     radioButtons(inputId="tableext",label="Reporte:",choices=c("Word", "HTML"),selected="Word")),
                   br(),
                   actionButton("reset_input", "Reestablecer valores", class="btn btn-secondary", icon = icon("refresh")),
                   
      ),
      mainPanel(tabsetPanel(type="tab",
                            tabPanel("Sobre la app",
                                     tags$br(),
                                     tags$div(
                                       tags$h2("Shiny app usando el dataset airquality"), 
                                       "Esta aplicación usa como fuente el conjunto de datos ‘airquality’, presente en el paquete ‘datasets’ de RStudio.",
                                       tags$br(),
                                       "En él se presentan datos sobre las mediciones de la calidad del aire en Nueva York de mayo a setiembre de 1973.",
                                       tags$br(),
                                       "Presenta 153 observaciones y 6 variables:",
                                       paste0(names(dtna), collapse = " , "),
                                       tags$br(),
                                       tags$br(),
                                       tags$br(),
                                       tags$h5("DATA PREVIEW omitiendo valores NA")
                                       ),
                                     tags$br(),
                                     DT::dataTableOutput(outputId="datapreview")
                                    ),
                            tabPanel("Gráficos",
                                     tags$h2("Gráficos con dataframe omitiendo valores NA", align = "center"),
                                     tags$br(),
                                     tags$br(),
                                     tags$br(),
                                     fluidRow(
                                       column(6,plotOutput(outputId="plot")),
                                       column(6,plotOutput(outputId="plot1"))
                                       ),
                                     fluidRow(
                                       column(6,plotOutput(outputId="plot2")),
                                       column(6,plotOutput(outputId="plot3")
                                              )),
                                     tags$br(),
                                     tags$br(),
                                     downloadButton(outputId="downplot",label="Descargar gráficos",class="btn btn-secondary"),
                                     downloadButton(outputId="downrep1",label="Descargar reporte",class="btn btn-success"),
                                     tags$br(),
                                     tags$br()
                                    ),
                            
                            tabPanel("Regresión lineal",
                                     tags$h2("Gráfico interactivo con plotly", align = "center"),
                                     tags$br(),
                                     tags$br(),
                                     tags$br(),
                                     plotlyOutput(outputId="reg"),
                                     tags$br(),
                                     tags$br(),
                                     tags$h2("Tabla con los valores usados en la gráfica", align = "center"),
                                     DT::dataTableOutput(outputId="tablereg"),
                                     tags$br(),
                                     tags$br(),
                                     downloadButton(outputId="downgrap",label="Descargar gráfico",class="btn btn-secondary"),
                                     downloadButton(outputId="downrep",label="Descargar reporte",class="btn btn-success"),
                                     tags$br(),
                                     tags$br(),
                                     tags$br()
                                     ),
                            tabPanel("Dataset",
                                     tags$h2("Datos del dataset airquality omitiendo valores NA", align = "center"),
                                     tags$br(),
                                     tags$br(),
                                     DT::dataTableOutput(outputId="tabledata"),
                                     tags$br(),
                                     tags$br(),
                                     downloadButton(outputId="downdata",label="Descargar dataset",class="btn btn-secondary"),
                                     tags$br(),
                                     tags$br(),
                                     tags$br()
                              
                            ),
                            
                            
                            
      )
      )
      
    )),
  server = function(input, output,session) {
    
    #Para la acción asociada al botón de input reset_input
    observeEvent(input$reset_input, {
      shinyjs::reset("side-panel")
    })
    
    #PANEL 1: SOBRE LA APP 
    
    #Tabla Data preview
    output$datapreview=DT::renderDataTable({
      DT::datatable(head(dtna),options = list(searching = FALSE,paging = FALSE))
      })
    
    
    # PANEL 2 : GRÁFICOS
    
    #Data reactiva para histograma
    colm=reactive({
      as.numeric(input$variab)
    })
    
    
    plothis=reactive({
      colm=as.numeric(input$variab)
      hist(dtna[,colm],col=input$color,
           main=paste("Histograma de", names(dtna[colm])),
           xlab=colm) 
    })
    #PLOT HISTOGRAMA 
    output$plot =renderPlot(
      width = function() input$ancho,
      height = function() input$alto,
      res = 96,
      {
        plothis()
      })
    
    
    #PLOT BOXPLOT
   
    output$plot1 =renderPlot(
      width = function() input$ancho,
      height = function() input$alto,
      res = 96,
      { 
        ggplot(dtna, aes(x= Month, y = !!as.name(input$variab1), color = as.factor(Month))) +
        geom_boxplot() +
        ggtitle(
          paste(
            "Boxplot ",
            input$variab1, " vs Month")
        )+
          ggeasy::easy_center_title()
        
      })
    
    #PLOT SCATTERPLOT
    
    output$plot2 =renderPlot(
      width = function() input$ancho,
      height = function() input$alto,
      res = 96,
      
      { ggplot(dtna, aes(x= Temp, y= !!as.name(input$variab1), color= as.factor(Month))) + 
          geom_point(size=2, shape=23)+
          ggtitle(
            paste(
              "Scatterplot ",
              input$variab1, " vs Temp")
          )+
          ggeasy::easy_center_title()
      })
  
    
    #PLOT LINE CHART
    
    output$plot3 =renderPlot(
      width = function() input$ancho,
      height = function() input$alto,
      res = 96,
      { 
      ggplot(dtna, aes(x= Temp, y= !!as.name(input$variab1))) +
        geom_line(color= input$color, linewidth=1, alpha=0.9, linetype=1)+
          ggtitle(
            paste(
              "Line chart ",
              input$variab1, " vs Temp")
          )+
          ggeasy::easy_center_title()
      })
    
    
    #Boton de descarga de graficos con inputs PDF o PNG
    
    output$downplot=downloadHandler(
      filename=function(){
        paste("graficos",input$plotext,sep=".")},
      content=function(file){
        if (input$plotext=="png")
          png(file)
        else
          pdf(file)
        colm=as.numeric(input$variab)
        hist(dtna[,colm],col=input$color,
             main=paste("Histograma de", names(dtna[colm])),
             xlab=colm)
        plot(ggplot(dtna, aes(x= Month, y = !!as.name(input$variab1), color = as.factor(Month))) +
               geom_boxplot() +
               ggtitle(
                 paste(
                   "Boxplot ",
                   input$variab1, " vs Month")
               )+
               ggeasy::easy_center_title())
        plot(ggplot(dtna, aes(x= Temp, y= !!as.name(input$variab1), color= as.factor(Month))) + 
               geom_point(size=2, shape=23)+
               ggtitle(
                 paste(
                   "Scatterplot ",
                   input$variab1, " vs Temp")
               )+
               ggeasy::easy_center_title())
        plot(ggplot(dtna, aes(x= Temp, y= !!as.name(input$variab1))) +
               geom_line(color= input$color, linewidth=1, alpha=0.9, linetype=1)+
               ggtitle(
                 paste(
                   "Line chart ",
                   input$variab1, " vs Temp")
               )+
               ggeasy::easy_center_title())
        
        
        dev.off()
      }
    )
    
    #Boton de descarga de reporte Gráficos
    
    output$downrep1=downloadHandler(
      filename = function() {
        paste('Reporte_graficos', sep = '.', switch(
          input$tableext, HTML = 'html', Word = 'docx'
          
        ))
      },
      
      content = function(file) {
        src <- normalizePath('reporte.Rmd')
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'reporte.Rmd', overwrite = TRUE)
        
        out <- rmarkdown::render('reporte.Rmd', switch(
          input$tableext,
           HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
      })
    
    
    #PANEL 3: REGRESION LINEAL
    
    #Grafico dentro de output reg USANDO PLOTLY para interactividad
    
    output$reg =renderPlotly(
      {
        ggplot(dtna, aes(x=log(Temp), y=!!as.name(input$variab1)))+geom_point() + stat_smooth(method="lm", level=0.99, color=input$color)+
          ggtitle(
            paste(
              "Regresión lineal ",
              input$variab1, " vs Temp")
          )+
          ggeasy::easy_center_title()
        
      })
    
    #Tabla 
    
    output$tablereg=DT::renderDataTable({
      DT::datatable(select(dtna, Temp, !!as.name(input$variab1)), options = list(searching = FALSE))
     })
    
    #Boton de descarga de graficos con inputs PDF o PNG
    
    output$downgrap=downloadHandler(
      filename=function(){
        paste("regresion_lineal",input$plotext,sep=".")},
      content=function(file){
        if (input$plotext=="png")
          png(file)
        else
          pdf(file)
        plot(ggplot(dtna, aes(x=log(Temp), y=!!as.name(input$variab1)))+geom_point() + stat_smooth(method="lm", level=0.99, color=input$color)+
          ggtitle(
            paste(
              "Regresión lineal ",
              input$variab1, " vs Temp")
          )+
          ggeasy::easy_center_title()) 
        dev.off()
      }
    )
    
    #Boton de descarga de reporte 
    
    output$downrep=downloadHandler(
      filename = function() {
        paste('Reporte', sep = '.', switch(
          input$tableext, PDF = 'pdf', HTML = 'html', Word = 'docx'
       
        ))
      },
      
      content = function(file) {
        src <- normalizePath('report.Rmd')
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        
        out <- rmarkdown::render('report.Rmd', switch(
          input$tableext,
           HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
      })
  
    #PANEL 4: DATASET
    
    #Tabla omitiendo valores NA
    output$tabledata=DT::renderDataTable({
      DT::datatable(dtna, options = list(pageLength = 25,searching = FALSE))
    })
    
    #Boton de descarga dataset csv
    output$downdata=downloadHandler(
      filename="data.csv",
      content=function(file){
          dtna
        write.csv(dtna,file,row.names = FALSE)}
    )
         
  }
)
