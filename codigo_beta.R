rm(list = ls())
library(pacman)
p_load(data.table,
       tidyverse,
       RSelenium,
       keras,
       tools,
       openxlsx)
#===============================================================================
COORDENADAS <- read.xlsx('Coordenadas.xlsx',
                         sheet = 'Datos')
#===============================================================================
gradmd <- function(coordenada){
  grados <- floor(coordenada)
  minutos <- (coordenada - floor(coordenada))*60
  segundos <- minutos - floor(minutos)
  minutos <- floor(minutos)
  segundos <- round(segundos*60,0)
  coordenada <- paste0(grados,':',minutos,'.',segundos)
  coordenada
}
#===============================================================================
COORDENADAS$latitud <- gradmd(COORDENADAS$Coordenada.X)
COORDENADAS$longitud <- gradmd(COORDENADAS$Coordenada.Y)
COORDENADAS$NS <- ifelse(COORDENADAS$Coordenada.X > 90,'S','N')
COORDENADAS$EW <- ifelse(COORDENADAS$Coordenada.Y > 90,'W','E')
#===============================================================================
MATRIZ <- data.frame(ID = COORDENADAS$ID)
#MATRIZ <- slice(MATRIZ, 1:5)
#===============================================================================
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
#l <- 4
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
    #Sys.sleep(1)
    # FINALMENTE SE REINICIAN LOS PARAMETROS
    #remDr$findElements(using = 'xpath', "//*/input[@value = 'Reset']")[[1]]$clickElement()
  }
  #aux <- aux + 1
  
  DISTANCIAS <- as.numeric(DISTANCIAS)
  MATRIZ <- cbind(MATRIZ, DISTANCIAS)
  colnames(MATRIZ)[j+1] <- COORDENADAS$ID[j]
  
}
proc.time()-tiempo
remDr$quit()

save(MATRIZ, file = 'MATRIZ_INTERNET.rda')


# dist2 <- function(lat_a, lon_a, lat_b, lon_b) {
#   res=(acos(sin(lat_a * 0.01745329252) * sin(lat_b * 0.01745329252) + (cos(lat_a * 0.01745329252) * cos(lat_b * 0.01745329252) * cos(lon_a * 0.01745329252 - lon_b * 0.01745329252))) * 57.29577951 * 111.194)
#   res
# }
# 
# dist2(lat_a = 14.7267,
#       lon_a = 92.3984,
#       lat_b = 18.1244,
#       lon_b = 94.4042)
# 
# library(sp)
# library(geosphere)
# puntos <- data.frame(id=c('a','b'), lat=c(14.7267, 92.3984), lon=c(18.1244, 94.4042))
# coordinates(puntos) <- c('lon','lat')
# # proj4string(puntos) <- CRS("+init=epsg:3857")
# proj4string(puntos) <- CRS('+init=epsg:4326')
# distHaversine(coordinates(puntos)[1,], coordinates(puntos)[2,]) / 1000
