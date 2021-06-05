# Cargar datos
#install.packages ("readr")

library(readr)

datos <- read.csv("https://raw.githubusercontent.com/rpizarrog/Industrias-4.0/main/datos/participantes.csv",
                    encoding = "ISO-1851")
datos

######### 
# Nombre aleatorio o una muestra de una sola persona
persona <- sample(x = datos$NOMBRE, size = 1)
persona
