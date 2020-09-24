library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("flatly"),
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
                           tabPanel("Preprocesado",
                                    sidebarLayout(
                                        sidebarPanel(
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
    output$imagenINICIO<-renderPlot({
        flux = OpenImageR::readImage("Flujo de aplicacion.jpg")
        imageShow(flux)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
