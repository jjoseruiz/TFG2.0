library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shinyjs)
library(neurobase)
library(ANTsR)
library(extrantsr)
library(caret)
library(randomForest)
library(tree)
library(dplyr)
library(modeest)
library(papayar)
library(papayaWidget)
library(OpenImageR)
library(malf.templates)
source("eligeVoxelPaciente.R")
source("preprocesadoPaciente.R")
source("obtenCoord.R")
source("predice.R")
source("aplicaFuncion.R")
source("resultado.R")
source("recorreImagenes.R")
source("valoresImagen.R")

# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("flatly"),  shinyjs::useShinyjs(),
                navbarPage("MS Lesion Detection",
                           tabPanel("Inicio",
                                    sidebarLayout(
                                        sidebarPanel(titlePanel("Introducción"),
                                                     tags$html( "La Esclerosis Múltiple (MS) es una enfermedad autoinmune 
                               que degrada las vainas de mielina de las neuronas del Sistema Nervioso Central 
                               afectado a las conexiones nerviosas. Esto hace que la enfermedad se manifieste 
                               en cada persona de manera diferente y con síntomas impredecibles tales como dolores, 
                               pérdida de memoria, hormigueo o incluso parálisis.  A pesar de los grandes avances 
                               en medicina y tecnología, aún se desconoce el origen de la enfermedad, sin embargo, 
                               podemos estudiarla y diagnosticarla a través de herramientas como las Imágenes de Resonancia Magnética. 
                               Es por esto, que en ocasiones los médicos expertos en diagnóstico por imágenes, tienen serias dificultades para interpretar las lesiones ocasionadas por la enfermedad en el cerebro."
                                                    ,align = "justify"),
                                                    titlePanel("Descripción"),
                                                    tags$html("MS LESION DETECTION es una aplicación web enfocada a la 
                               detección de lesiones cerebrales relacionadas con la Esclerosis Múltiple 
                               vía Imágenes de resonancia Magnética basándose en modelos de",em("Machine Learning.")
                                                  ,align = "justify")),
                                        mainPanel(titlePanel("Flujo de funcionamiento de la aplicación"),
                                                withSpinner(plotOutput("imagenINICIO",width=800,height = 650))
                                        )
                            )),
                           tabPanel("Subida de imágenes",
                                    sidebarLayout(
                                        sidebarPanel(titlePanel("Sube aquí sus imágenes"),
                                                   tags$html("Recuerde que sus imágenes deben ser del mismo paciente y tener extensión/formato .nii o .nii.gz"),width = 3
                                      ),
                                      
                                      mainPanel(
                                          splitLayout(
                                              fileInput(inputId = "ImagenFlair",tags$html("FLAIR"), multiple = FALSE, accept = c(".nii",".nii.gz")),
                                              fileInput(inputId = "ImagenT1",tags$html("T1"), multiple = FALSE, accept = c(".nii",".nii.gz")),
                                              fileInput(inputId = "ImagenT2",tags$html("T2"), multiple = FALSE, accept = c(".nii",".nii.gz"))
                                        ),actionButton("botonSubir",tags$html("Subir imagenes")),
                                        tags$h6(textOutput(outputId="clics")),
                                          splitLayout(
                                              hidden(div(id = "plotflair",withSpinner(type = 8,plotOutput("plotFLAIR")))),
                                              hidden(div(id = "plott1",withSpinner(type = 8,plotOutput("plotT1")))),
                                              hidden(div(id="plott2",withSpinner(type = 8,plotOutput("plotT2"))))
                                        )
                                      )
                                    )),
                           tabPanel("Preprocesado",
                                    #Primer proceso: Corrección de inhomogeneidad
                                    sidebarLayout(
                                      
                                        sidebarPanel(titlePanel("Corrección de inhomogeneidad"),
                                                     tags$html("En ocasiones, las MRI tomadas tienen inhomogeneidades, ya sea debido a las propias antenas
de las máquinas de resonancia, a algún movimiento del paciente o debido a otro tipo de
artefactos. Suelen aparecer como una señal superpuesta a la imagen y se le conoce como
bias_field. Se ha revisado el estado del arte de los algoritmos más utilizados para la corrección
de este tipo de deficiencias, eligiendo la corrección N4 la cual es una mejora del algoritmo N3 el cual se considera robusto y estable
en Marinetto et al. 2011. Esta corrección realiza un proceso iterativo basado en la intensidad
recogida en el histograma de la imagen",align = "justify"),
                                                     # hidden(div(id="corrigiendo",withSpinner(type = 8, plotOutput("plotCorrigiendo"))))
                                        ),
                                        mainPanel(
                                          splitLayout(plotOutput(outputId = "imagenCorreccion",height = "250px",width = "900px"))
                                          
                                        )
                                    ),
                                    #Segundo proceso: Registro
                                    sidebarLayout(
                                      sidebarPanel(titlePanel("Registro"),
                                                   tags$html("El proceso de registro de una imagen es esencial para tener imágenes en el mismo espacio. Cuando hablamos de registrar las imágenes, queremos decir que estableceremos un espacio común para todas las imágenes. En nuestro caso, hemos decidido que se utilizará una transformación afín, la cual consiste en un movimiento rígido",
                                                             align = "justify")
                                        
                                      ),
                                      mainPanel(
                                        div(plotOutput(outputId = "imagenRegistro",height = "250px",width = "900px"))
                                        
                                      )
                                      
                                    ),
                                    #Tercer Proceso
                                    sidebarLayout(
                                      sidebarPanel(titlePanel("Extracción de masa Cerebral"),
                                                   tags$html("La extracción de la masa cerebral del cráneo es una técnica de neuroimagen bastante
estandarizada cuya finalidad es la de aislar y concretar exclusivamente el tejido cerebral
                                                             ",align = "justify")
                                        
                                      ),
                                      mainPanel(
                                        
                                      )
                                      
                                    ),
                                    #Cuarto proceso
                                    sidebarLayout(
                                      sidebarPanel(titlePanel("Normalización"),
                                                   tags$html("La normalización de los niveles de gris consiste en la adaptación los niveles de gris a un intervalo. Este proceso es útil para discriminar de una forma más sólida los tejidos que aparecen en la imagen. Habitualmente, las MRI se adquieren en unidades arbitrarias, dependiendo directamente del escáner utilizado, por lo que si queremos comparar imágenes,
es necesario tenerlas todas en la misma escala.
                                                             ",align = "justify")
                                        
                                      ),
                                      mainPanel(
                                        
                                      )
                                      
                                    )),
                           tabPanel("Obtención de dataset",
                                    sidebarLayout(
                                        sidebarPanel(
                                        ),
                                        mainPanel(
                                            
                                        )
                               
                           )),
                           tabPanel("Carga de Modelos",
                                    sidebarLayout(
                                        sidebarPanel(
                                        ),
                                        mainPanel(
                                            
                                        )
                            )),
                           tabPanel("Predicción y Resultados",
                                    sidebarLayout(
                                        sidebarPanel(
                                        ),
                                        mainPanel(
                                            
                                        )
                                    )
                           )
                )
)

server <- function(input, output) {
  options(shiny.maxRequestSize = 500*1024^2)
  toggle(id = "plotFLAIR", condition = FALSE)
  toggle(id = "plotT1", condition = FALSE)
  toggle(id = "plotT2", condition = FALSE)
  
    output$imagenINICIO<-renderPlot({
        flux = OpenImageR::readImage("Flujo de aplicacion.jpg")
        imageShow(flux)
    })
    #lectura
    app_imagenes<-eventReactive(input$botonSubir,{
      withProgress(
        if(!is.null(input$ImagenFlair) & !is.null(input$ImagenT1) & !is.null(input$ImagenT2)){
          datapath=input$ImagenFlair$datapath
          datapath2=input$ImagenT1$datapath
          datapath3=input$ImagenT2$datapath
          if(tools::file_ext(datapath)=="gz" & tools::file_ext(datapath2)=="gz" & tools::file_ext(datapath3)=="gz"){
            datapath=sub("gz$","nii.gz",datapath)
            datapath2=sub("gz$","nii.gz",datapath2)
            datapath3=sub("gz$","nii.gz",datapath3)
            file.rename(input$ImagenFlair$datapath,datapath)
            file.rename(input$ImagenT1$datapath,datapath2)
            file.rename(input$ImagenT2$datapath,datapath3)
          }
          FLAIR=antsImageRead(datapath)
          T1=antsImageRead(datapath2)
          T2=antsImageRead(datapath3)
          lista=list(FLAIR,T1,T2)
          return(lista)
        }else{
          "Suba ambas imágenes porfavor"
        }
        ,message = "Cargando imágenes al sistema")
    })
    observeEvent(input$botonSubir,{

      print(app_imagenes()[1])
      print("he clickeado")
      output$clics<-renderText({
        "Imágenes subidas con éxito. Continue en la ventana de Preprocesado."
      })
      output$plotFLAIR<-renderPlot({
          ortho2(app_imagenes()[[1]])
      })
      #Muestra la Imagen T1 Subida
      output$plotT1<-renderPlot({
          ortho2(app_imagenes()[[2]])
      })
      # Muestra la Imagen T2 Subida
      output$plotT2<-renderPlot({
          ortho2(app_imagenes()[[3]])
      })
    })
    
    
    output$clics<-renderText({
      if(!is.null(input$ImagenFlair) & !is.null(input$ImagenT1) & !is.null(input$ImagenT2)){
        "Haga clic en el botón de arriba."
      }else{
        "Suba las imágenes por favor"
      }
    })

    observeEvent(input$botonSubir,{
      show("plotflair")
      show("plott1")
      show("plott2")
      toggle(id ="plotFLAIR",condition = TRUE)
      toggle(id ="plotT1",condition = TRUE)
      toggle(id ="plotT2",condition = TRUE)
    })
    
    output$imagenCorreccion<-renderPlot({
      flux = OpenImageR::readImage("sinCorregir.jpeg")
      imageShow(flux)
    })
    output$imagenRegistro<-renderPlot({
      flux = OpenImageR::readImage("corregida.jpeg")
      imageShow(flux)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
