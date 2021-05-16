# Script y aplicación para el cálculo de distancias, usando fórmula y [web scraping](https://www.nhc.noaa.gov/gccalc.shtml)

La aplicación tiene la siguiente interfaz de usuario

![app](https://github.com/jodopevi/Calculo_Distancias/blob/main/www/app_Mapa.png)

donde se obtiene la matriz de distancias usando fórmula

![matriz_formula](https://github.com/jodopevi/Calculo_Distancias/blob/main/www/app_Matriz_Calculada.png)

y la matriz de distancias con ayuda de [web scraping](https://www.nhc.noaa.gov/gccalc.shtml)

![matriz_internet](https://github.com/jodopevi/Calculo_Distancias/blob/main/www/app_Matriz_Internet.png)

## Instruciones:

1. Clonar el repositorio en un nuevo proyecto en RStudio.
2. Compilar el script app.R y seleccionar los parámetros de:
- Base con los datos
- Variable de la latitud
- Variable de la longitud
- Variable del ID de las coordenadas geográficas.
3. Dar clic en el boton de **Cargar Datos**.
4. Se mostraran las coordenadas geográficas en el mapa, dar clic en la pestaña **Matriz Calculada**.