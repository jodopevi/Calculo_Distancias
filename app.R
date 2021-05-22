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
                                                         ''),
                                          
                                          selectInput('longitud',
                                                         'Variable de la Longitud',
                                                         ''),
                                          
                                          selectInput('id',
                                                         'ID de los datos',
                                                         ''),
                                          
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
                             'Variable de la Latitud',
                             colnames(ARCHIVO))
        
        updateSelectInput(session,
                             'longitud',
                             'Variable de la Longitud',
                          colnames(ARCHIVO))
        
        updateSelectInput(session,
                             'id',
                             'ID de los datos',
                          colnames(ARCHIVO))
    })
    
    
    BASE <- eventReactive(input$cargar_datos,{
        
        if (is.null(input$base) & is.null(input$hoja_excel)) {
            return(NULL)
        }
        
        ARCHIVO <- read_excel(input$base$datapath, sheet = input$hoja_excel)
        
        ARCHIVO
    })
    
    
    output$mapa <- renderLeaflet({
        
        AUX_BASE <- BASE()
        
        if(is.null(AUX_BASE)) {return(NULL)}
        
        ID <- AUX_BASE %>%
            select(input$id) %>%
            rename(ID = input$id)
        LATITUD <- AUX_BASE %>%
            select(input$latitud) %>%
            rename(latitud = input$latitud)
        LONGITUD <- AUX_BASE %>%
            select(input$longitud) %>%
            rename(longitud = input$longitud)
        
        AUX_BASE <- cbind(ID, LATITUD, LONGITUD)
        
        leaflet(AUX_BASE, options = leafletOptions(minZoom = 3, maxZoom = 10)) %>%
            addProviderTiles('CartoDB.Positron', options = providerTileOptions(noWrap = T)) %>%
            addMarkers(~longitud,
                       ~latitud,
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
        
        MATRIZ_CALCULADA <- AUX_BASE %>%
            select(input$id) %>%
            rename(ID = input$id)
        LATITUD <- AUX_BASE %>%
            select(input$latitud) %>%
            rename(latitud = input$latitud)
        LONGITUD <- AUX_BASE %>%
            select(input$longitud) %>%
            rename(longitud = input$longitud)
        
        AUX_BASE <- cbind(MATRIZ_CALCULADA, LATITUD, LONGITUD)
        
        l <- nrow(AUX_BASE)-1
        
        withProgress(message = 'Calculando distancia', value = 0, {
            n <- l
            
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
            
            incProgress(1/n, 
                        detail = paste(round((j/n)*100,1),' %'))
        }})
        
        MATRIZ_CALCULADA$Var <- 0.00
        colnames(MATRIZ_CALCULADA)[ncol(MATRIZ_CALCULADA)] <- MATRIZ_CALCULADA$ID[nrow(MATRIZ_CALCULADA)]
        rownames(MATRIZ_CALCULADA) <- MATRIZ_CALCULADA$ID
        MATRIZ_CALCULADA$ID <- NULL
        
        MATRIZ_CALCULADA <- MATRIZ_CALCULADA + t(MATRIZ_CALCULADA)
        
        write.xlsx(MATRIZ_CALCULADA, 'www/Res_Matriz_Calculada.xlsx', rownames = T)
        
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
    rownames = T
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
        MATRIZ_CONSULTA <- slice(MATRIZ_CONSULTA,1:10)
        
        # SE UTILIZA EL NAVEGADOR FIREFOX
        system("taskkill /im java.exe /f", intern = F, ignore.stdout = F)
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
        
        #l <- nrow(COORDENADAS)-1
        l <- 9
        
        withProgress(message = 'Calculando distancia', value = 0, {
            n <- l
            
        for (j in 1:l) {
            
            DISTANCIAS <- c(replicate(j,0))
            
            Sys.sleep(0.5)
            
            # COORDENADA 1
            remDr$findElement(using = "name", value = "lat1")$clearElement()
            Sys.sleep(0.5)
            remDr$findElement(using = "name", value = "lat1")$sendKeysToElement(list(COORDENADAS$latitud[j]))
            Sys.sleep(0.5)
            remDr$findElement(using = "name", value = "NS1")$sendKeysToElement(list(COORDENADAS$NS[j]))
            Sys.sleep(0.5)
            remDr$findElement(using = "name", value = "lon1")$clearElement()
            Sys.sleep(0.5)
            remDr$findElement(using = "name", value = "lon1")$sendKeysToElement(list(COORDENADAS$longitud[j]))
            Sys.sleep(0.5)
            remDr$findElement(using = "name", value = "EW1")$sendKeysToElement(list(COORDENADAS$EW[j]))
            
            for (i in j:l) {
                
                print(paste0('Coordenadas: [',j,', ',i+1,']'))
                
                Sys.sleep(0.5)
                # COORDENADA 2
                remDr$findElement(using = "name", value = "lat2")$clearElement()
                Sys.sleep(0.5)
                remDr$findElement(using = "name", value = "lat2")$sendKeysToElement(list(COORDENADAS$latitud[i+1]))
                Sys.sleep(0.5)
                remDr$findElement(using = "name", value = "NS2")$sendKeysToElement(list(COORDENADAS$NS[i+1]))
                Sys.sleep(0.5)
                remDr$findElement(using = "name", value = "lon2")$clearElement()
                Sys.sleep(0.5)
                remDr$findElement(using = "name", value = "lon2")$sendKeysToElement(list(COORDENADAS$longitud[i+1]))
                Sys.sleep(0.5)
                remDr$findElement(using = "name", value = "EW2")$sendKeysToElement(list(COORDENADAS$EW[i+1]))
                Sys.sleep(0.5)
                # SE CALCULA LA DISTANCIA
                remDr$findElements(using = 'xpath', "//*/input[@value = 'Compute']")[[1]]$clickElement()
                Sys.sleep(0.5)
                # SE OBTIENE EL VALOR CALCULADO DE LA DISTANCIA
                DISTANCIA <- remDr$findElement(using = "name", value = "d12")$getElementAttribute('value')[[1]]
                DISTANCIAS <- c(DISTANCIAS, DISTANCIA)
                # NO ES NECESARIO DE REINICIAN LOS PARAMETROS
            }
            
            DISTANCIAS <- as.numeric(DISTANCIAS)
            MATRIZ_CONSULTA <- cbind(MATRIZ_CONSULTA, DISTANCIAS)
            colnames(MATRIZ_CONSULTA)[j+1] <- COORDENADAS$ID[j]
            
            incProgress(1/n, 
                        detail = paste(round((j/n)*100,1),' %'))
            
        }}) # TERMINA CICLO FOR Y EL PROGRESO
        
        remDr$quit()
        system("taskkill /im java.exe /f", intern = F, ignore.stdout = F)
        
        MATRIZ_CONSULTA$Var <- 0.00
        colnames(MATRIZ_CONSULTA)[ncol(MATRIZ_CONSULTA)] <- MATRIZ_CONSULTA$ID[nrow(MATRIZ_CONSULTA)]
        rownames(MATRIZ_CONSULTA) <- MATRIZ_CONSULTA$ID
        MATRIZ_CONSULTA$ID <- NULL
        
        MATRIZ_CONSULTA <- MATRIZ_CONSULTA + t(MATRIZ_CONSULTA)
        
        write.xlsx(MATRIZ_CONSULTA, 'www/Res_Matriz_Internet.xlsx', rownames = T)
        
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
    rownames = T
    ))
    
}


shinyApp(ui = ui, server = server)