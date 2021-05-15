library(shiny)
library(leaflet)
library(htmltools)
library(RColorBrewer)
library(scales)
library(lattice)

library(openxlsx)
library(readxl)
library(tidyverse)
library(DT)

source('www/distancia.R', local = T)

ui <- fluidPage(
    
    navbarPage('Cálculo de Distancias',
               id = 'distancias',
               
               tabPanel('Localización',
                        div(class='outer',
                            
                            tags$head(includeCSS('www/styles.css'),
                                      includeScript('www/gomap.js')),
                            
                            leafletOutput('mapa', width = '100%', height = '100%'),
                            
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = T,
                                          draggable = T, top = 60, left = "auto", right = 20, bottom = "auto",
                                          width = 330, height = "auto",
                                          
                                          h2("Carga de los Datos"),
                                          
                                          fileInput('base', 
                                                    'Seleccione la Base con los Datos',
                                                    accept = '.xlsx',
                                                    multiple = F,
                                                    buttonLabel = 'Buscar...',
                                                    placeholder = 'Ningún archivo seleccionado'),
                                          
                                          selectInput('hoja_excel',
                                                      'Hoja del archivo de Excel',
                                                      choices = ''),
                                          
                                          selectInput('latitud',
                                                      'Variable de la Latitud',
                                                      choices = ''),
                                          
                                          selectInput('longitud',
                                                      'Variable de la Longitud',
                                                      choices = ''),
                                          
                                          selectInput('id',
                                                      'ID de los datos',
                                                      choices = ''),
                                          
                                          actionButton('cargar_datos', 
                                                       'Cargar datos',
                                                       class = 'butt'),
                                          
                                          # helpText('Cálculo de las distancias'),
                                          # actionButton('descargar_distancia', 
                                          #              'Descargar',
                                          #              class = 'butt')
                                          ))), # FIN DEL tabPanel
               tabPanel('Matriz Calculada',
                        
                        helpText('Cálculo de las distancias con fórmula'),
                        actionButton('generar_distancia', 
                                     'Generar',
                                     class = 'butt'),
                        hr(style = 'border-top: 0px'),
                        dataTableOutput('matrizcaldulada')),
               tabPanel('Matriz Consultada')
    ) # FIN DEL navbarPage
) # FIN DEL ui


server <- function(input, output, session) {
    
    observeEvent(input$base,{
        
        if (is.null(input$base)) {
            return(NULL)
        }
        
        updateSelectInput(session,
                          'hoja_excel',
                          label = 'Hoja del archivo de Excel',
                          choices = excel_sheets(input$base$datapath))
    })
    
    observeEvent(input$hoja_excel,{
        
        if (is.null(input$base)) {
            return(NULL)
        }
        
        ARCHIVO <- read_excel(input$base$datapath, sheet = input$hoja_excel)
        
        updateSelectInput(session,
                          'latitud',
                          label = 'Variable de la Latitud',
                          choices = colnames(ARCHIVO))
        
        updateSelectInput(session,
                          'longitud',
                          label = 'Variable de la Longitud',
                          choices = colnames(ARCHIVO))
        
        updateSelectInput(session,
                          'id',
                          label = 'ID de los datos',
                          choices = colnames(ARCHIVO))
    })
    
    BASE <- eventReactive(input$cargar_datos,{
        
        if (is.null(input$base) & is.null(input$hoja_excel)) {
            return(NULL)
        }
        
        ARCHIVO <- read_excel(input$base$datapath, sheet = input$hoja_excel)
        
        ARCHIVO
    })
    
    output$mapa <- renderLeaflet({
        
        leaflet(BASE(), options = leafletOptions(minZoom = 3, maxZoom = 10)) %>%
            #setView(lng = -102.552784, lat = 23.634501, zoom = 5) %>%
            addProviderTiles('CartoDB.Positron', options = providerTileOptions(noWrap = T)) %>%
            # addCircleMarkers(
            #     lng =~ longitud, # Longitude coordinates
            #     lat =~ latitud, # Latitude coordinates
            #     radius = 5, # Total count
            #     stroke = F, # Circle stroke
            #     fillOpacity = 0.5) %>%
            addMarkers(~`Coordenada Y`,
                       ~`Coordenada X`,
                       label =~ htmlEscape(ID))
    })
    
    BASE_MATRIZ_CALCULADA <- eventReactive(input$generar_distancia,{
        
        AUX_BASE <- BASE()
        
        if(is.null(AUX_BASE)) {return(NULL)}
        
        notificacion <- showNotification(
            'Iniciando el cálculo de las distancias con fórmula', 
            duration = 10,
            closeButton = F
        )
        on.exit(removeNotification(notificacion), add = T)
        
        MATRIZ_CALCULADA <- data.frame(ID = AUX_BASE$ID)
        AUX_BASE$latitud <- AUX_BASE$`Coordenada X`
        AUX_BASE$longitud <- AUX_BASE$`Coordenada Y`
        
        l <- nrow(AUX_BASE)-1
        for (j in 1:l) {
            
            DISTANCIAS <- c(replicate(j,0))
            LATITUD_A <- AUX_BASE$latitud[j]
            LONGITUD_A <- AUX_BASE$longitud[j]
            
            for (i in j:l) {
                
                print(paste0('Coordenadas: [',j,', ',i+1,']'))
                
                LATITUD_B <- AUX_BASE$latitud[i+1]
                LONGITUD_B <- AUX_BASE$longitud[i+1]
                
                DISTANCIA <- distancia(LATITUD_A, LONGITUD_A, LATITUD_B, LONGITUD_B)
                DISTANCIAS <- c(DISTANCIAS, DISTANCIA)
                
            }
            
            DISTANCIAS <- as.numeric(DISTANCIAS)
            MATRIZ_CALCULADA <- cbind(MATRIZ_CALCULADA, DISTANCIAS)
            colnames(MATRIZ_CALCULADA)[j+1] <- AUX_BASE$ID[j]
            
        }
        
        MATRIZ_CALCULADA
    })
    
    output$matrizcaldulada <- renderDataTable(datatable({
        #---------------------------------------------------------------------------
        BASE <- BASE_MATRIZ_CALCULADA()
        
        if(is.null(BASE)) {return(NULL)}
        
        BASE
        
    }, extensions = 'Buttons',
    options = list(pageLength = 200,
                   dom = 'Bfrtip', 
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
    rownames = F
    ))
    
}


shinyApp(ui = ui, server = server)