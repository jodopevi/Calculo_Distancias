gradmd <- function(COORDENADA){
  grados <- floor(COORDENADA)
  minutos <- (COORDENADA - floor(COORDENADA))*60
  segundos <- minutos - floor(minutos)
  minutos <- floor(minutos)
  segundos <- round(segundos*60,0)
  coordenada <- paste0(grados,':',minutos,'.',segundos)
  coordenada
}