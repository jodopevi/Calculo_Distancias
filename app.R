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
library(RSelenium)

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
                                                       class = 'butt')))), # FIN DEL tabPanel
               
               tabPanel('Matriz Calculada',
                        
                        helpText('Cálculo de las distancias con fórmula'),
                        actionButton('generar_distancia', 
                                     'Generar',
                                     class = 'butt'),
                        hr(style = 'border-top: 0px'),
                        dataTableOutput('matrizcaldulada')), # FIN DEL tabPanel
               
               tabPanel('Matriz de Internet',
                        
                        helpText('Cálculo de las distancias usando la página'),
                        actionButton('descargar_distancia', 
                                     'Descargar',
                                     class = 'butt'),
                        hr(style = 'border-top: 0px'),
                        dataTableOutput('matrizinternet')) # FIN DEL tabPanel
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

        BASE <- BASE_MATRIZ_CALCULADA()
        
        if(is.null(BASE)) {return(NULL)}
        
        BASE
        
    }, extensions = 'Buttons',
    options = list(pageLength = 200,
                   dom = 'Bfrtip', 
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
    rownames = F
    ))
    
    BASE_MATRIZ_INTERNET <- eventReactive(input$descargar_distancia,{
        
        COORDENADAS <- BASE()
        
        if(is.null(COORDENADAS)) {return(NULL)}
        
        notificacion <- showNotification(
            'Iniciando el cálculo con ayuda de internet', 
            duration = 10,
            closeButton = F
        )
        on.exit(removeNotification(notificacion), add = T)
        
        colnames(COORDENADAS)[2] <- 'latitud'
        colnames(COORDENADAS)[3] <- 'longitud'
        COORDENADAS$NS <- ifelse(COORDENADAS$latitud > 0,'N','S')
        COORDENADAS$EW <- ifelse(COORDENADAS$longitud > 0,'E','W')
        COORDENADAS$latitud <- abs(COORDENADAS$latitud)
        COORDENADAS$longitud <- abs(COORDENADAS$longitud)
        COORDENADAS$latitud <- as.character(COORDENADAS$latitud)
        COORDENADAS$longitud <- as.character(COORDENADAS$longitud)
        
        # CALCULO DE LA DISTANCIA CON LA CONSULTA DE LA PAGINA DE INTERNET
        MATRIZ_CONSULTA <- data.frame(ID = COORDENADAS$ID)
        
        # SE UTILIZA EL NAVEGADOR FIREFOX
        profile <- makeFirefoxProfile(list(browser.download.folderList = 2L,
                                           browser.download.manager.showWhenStarting = FALSE,
                                           browser.helperApps.neverAsk.openFile = "text/plain",
                                           browser.helperApps.neverAsk.saveToDisk = "text/plain"))
        rD <- RSelenium::rsDriver(browser = "firefox",
                                  port = 4444L,
                                  verbose = F,
                                  extraCapabilities = profile) 
        remDr <- rD[["client"]]
        remDr$navigate("https://www.nhc.noaa.gov/gccalc.shtml")
        # SE SELECCIONAN LAS UNIDADES EN KILOMETROS
        # SE DEJA AFUERA YA QUE AL MOMENTO DE RESETEAR LOS PARAMETROS NO CAMBIA LA SELECCION
        remDr$findElement(using = "name", value = "Dunit")$sendKeysToElement(list('km'))
        
        l <- nrow(COORDENADAS)-1
        tiempo = proc.time()
        for (j in 1:l) {
            
            DISTANCIAS <- c(replicate(j,0))
            
            Sys.sleep(1)
            
            # COORDENADA 1
            remDr$findElement(using = "name", value = "lat1")$clearElement()
            Sys.sleep(1)
            remDr$findElement(using = "name", value = "lat1")$sendKeysToElement(list(COORDENADAS$latitud[j]))
            Sys.sleep(1)
            remDr$findElement(using = "name", value = "NS1")$sendKeysToElement(list(COORDENADAS$NS[j]))
            Sys.sleep(1)
            remDr$findElement(using = "name", value = "lon1")$clearElement()
            Sys.sleep(1)
            remDr$findElement(using = "name", value = "lon1")$sendKeysToElement(list(COORDENADAS$longitud[j]))
            Sys.sleep(1)
            remDr$findElement(using = "name", value = "EW1")$sendKeysToElement(list(COORDENADAS$EW[j]))
            
            for (i in j:l) {
                
                print(paste0('Coordenadas: [',j,', ',i+1,']'))
                
                Sys.sleep(1)
                # COORDENADA 2
                remDr$findElement(using = "name", value = "lat2")$clearElement()
                Sys.sleep(1)
                remDr$findElement(using = "name", value = "lat2")$sendKeysToElement(list(COORDENADAS$latitud[i+1]))
                Sys.sleep(1)
                remDr$findElement(using = "name", value = "NS2")$sendKeysToElement(list(COORDENADAS$NS[i+1]))
                Sys.sleep(1)
                remDr$findElement(using = "name", value = "lon2")$clearElement()
                Sys.sleep(1)
                remDr$findElement(using = "name", value = "lon2")$sendKeysToElement(list(COORDENADAS$longitud[i+1]))
                Sys.sleep(1)
                remDr$findElement(using = "name", value = "EW2")$sendKeysToElement(list(COORDENADAS$EW[i+1]))
                Sys.sleep(1)
                # SE CALCULA LA DISTANCIA
                remDr$findElements(using = 'xpath', "//*/input[@value = 'Compute']")[[1]]$clickElement()
                Sys.sleep(1)
                # SE OBTIENE EL VALOR CALCULADO DE LA DISTANCIA
                DISTANCIA <- remDr$findElement(using = "name", value = "d12")$getElementAttribute('value')[[1]]
                DISTANCIAS <- c(DISTANCIAS, DISTANCIA)
                # NO ES NECESARIO DE REINICIAN LOS PARAMETROS
            }
            
            DISTANCIAS <- as.numeric(DISTANCIAS)
            MATRIZ_CONSULTA <- cbind(MATRIZ_CONSULTA, DISTANCIAS)
            colnames(MATRIZ_CONSULTA)[j+1] <- COORDENADAS$ID[j]
            
        }
        
        remDr$quit()
        system("taskkill /im java.exe /f", intern = F, ignore.stdout = F)
        
        MATRIZ_CONSULTA
    })
    
    output$matrizinternet <- renderDataTable(datatable({
        
        BASE <- BASE_MATRIZ_INTERNET()
        
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