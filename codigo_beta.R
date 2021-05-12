library(openxlsx)

COORDENADAS <- read.xlsx('Coordenadas.xlsx',
                         sheet = 'Datos')

gradmd <- function(coordenada){
  grados <- floor(coordenada)
  minutos <- (coordenada - floor(coordenada))*60
  segundos <- minutos - floor(minutos)
  minutos <- floor(minutos)
  segundos <- round(segundos*60,0)
  coordenada <- paste0(grados,':',minutos,'.',segundos)
  coordenada
}

COORDENADAS$latitud <- gradmd(COORDENADAS$Coordenada.X)
COORDENADAS$longitud <- gradmd(COORDENADAS$Coordenada.Y)
COORDENADAS$NS <- ifelse(COORDENADAS$Coordenada.X > 90,'S','N')
COORDENADAS$EW <- ifelse(COORDENADAS$Coordenada.Y > 90,'W','E')


coordenada <- 14.7267
gradmd(94.4042)

gradms("43:14:25")

dist2 <- function(lat_a, lon_a, lat_b, lon_b) {
  res=(acos(sin(lat_a * 0.01745329252) * sin(lat_b * 0.01745329252) + (cos(lat_a * 0.01745329252) * cos(lat_b * 0.01745329252) * cos(lon_a * 0.01745329252 - lon_b * 0.01745329252))) * 57.29577951 * 111.194)
  res
}

dist2(lat_a = 14.7267,
      lon_a = 92.3984,
      lat_b = 18.1244,
      lon_b = 94.4042)

library(sp)
library(geosphere)
puntos <- data.frame(id=c('a','b'), lat=c(14.7267, 92.3984), lon=c(18.1244, 94.4042))
coordinates(puntos) <- c('lon','lat')
# proj4string(puntos) <- CRS("+init=epsg:3857")
proj4string(puntos) <- CRS('+init=epsg:4326')
distHaversine(coordinates(puntos)[1,], coordinates(puntos)[2,]) / 1000
