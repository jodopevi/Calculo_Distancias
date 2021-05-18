rm(list = ls())
library(pacman)
p_load(RSelenium,
       tidyverse,
       readxl,
       openxlsx)
cat('\014')
#===============================================================================
# CARGA DEL ARCHIVO DE EXCEL
#===============================================================================
COORDENADAS <- read_excel('Coordenadas.xlsx',sheet = 'Datos')
#===============================================================================
# SE RENOMBRAN LAS VARIABLES DE LA latitud Y longitud
#===============================================================================
colnames(COORDENADAS)[2] <- 'latitud'
colnames(COORDENADAS)[3] <- 'longitud'
COORDENADAS$NS <- ifelse(COORDENADAS$latitud > 0,'N','S')
COORDENADAS$EW <- ifelse(COORDENADAS$longitud > 0,'E','W')
COORDENADAS$latitud <- abs(COORDENADAS$latitud)
COORDENADAS$longitud <- abs(COORDENADAS$longitud)
COORDENADAS$latitud <- as.character(COORDENADAS$latitud)
COORDENADAS$longitud <- as.character(COORDENADAS$longitud)
#===============================================================================
# CALCULO DE LA DISTANCIA CON LA CONSULTA DE LA PAGINA DE INTERNET
#===============================================================================
MATRIZ_CONSULTA <- data.frame(ID = COORDENADAS$ID)
#===============================================================================
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
proc.time()-tiempo

remDr$quit()
system("taskkill /im java.exe /f", intern = F, ignore.stdout = F)

write.xlsx(MATRIZ_CONSULTA,'Res_Matriz_Internet.xlsx',row.names = F)
#===============================================================================
# CALCULO DE LA DISTANCIA CON FORMULA
#===============================================================================
distancia <- function(LATITUD_A, LONGITUD_A, LATITUD_B, LONGITUD_B) {
  RESULTADO <- (acos(sin(LATITUD_A * 0.01745329252) * sin(LATITUD_B * 0.01745329252) + (cos(LATITUD_A * 0.01745329252) * cos(LATITUD_B * 0.01745329252) * cos(LONGITUD_A * 0.01745329252 - LONGITUD_B * 0.01745329252))) * 57.29577951 * 111.194)
  RESULTADO <- as.numeric(formatC(round(RESULTADO,2),2,format = 'f'))
  RESULTADO
}
#===============================================================================
# SE CARGA DE NUEVO LA BASE PARA CAMBIAR EL TIPO DE DATO EN LA LATITUD Y LONGITUD
#===============================================================================
COORDENADAS <- read.xlsx('Coordenadas.xlsx',sheet = 'Datos')
#===============================================================================
colnames(COORDENADAS)[2] <- 'latitud'
colnames(COORDENADAS)[3] <- 'longitud'
COORDENADAS$latitud <- as.numeric(COORDENADAS$latitud)
COORDENADAS$longitud <- as.numeric(COORDENADAS$longitud)
#===============================================================================
MATRIZ_CALCULADA <- data.frame(ID = COORDENADAS$ID)
#===============================================================================
l <- nrow(COORDENADAS)-1

tiempo = proc.time()
for (j in 1:l) {
  
  DISTANCIAS <- c(replicate(j,0))
  LATITUD_A <- COORDENADAS$latitud[j]
  LONGITUD_A <- COORDENADAS$longitud[j]
  
  for (i in j:l) {
    
    print(paste0('Coordenadas: [',j,', ',i+1,']'))
    
    LATITUD_B <- COORDENADAS$latitud[i+1]
    LONGITUD_B <- COORDENADAS$longitud[i+1]
    
    DISTANCIA <- distancia(LATITUD_A, LONGITUD_A, LATITUD_B, LONGITUD_B)
    DISTANCIAS <- c(DISTANCIAS, DISTANCIA)
    
  }
  
  DISTANCIAS <- as.numeric(DISTANCIAS)
  MATRIZ_CALCULADA <- cbind(MATRIZ_CALCULADA, DISTANCIAS)
  colnames(MATRIZ_CALCULADA)[j+1] <- COORDENADAS$ID[j]
  
}
proc.time()-tiempo

write.xlsx(MATRIZ_CALCULADA,'Res_Matriz_Calculada.xlsx',row.names = F)


