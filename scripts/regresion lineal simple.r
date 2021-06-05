# Regresión lineal simple
# Para predecir la cantidad de achaques de una persona 
# conforme a la edad


# Librerías
# install.packages("ggplot2")
library(ggplot2) # Gráficos amigables

# <- es igua que el signo de igual <- es = como asignación

edades <- c(10, 15, 20, 30, 40, 56, 67, 82, 45, 46, 58, 66, 70, 19, 43, 45, 46, 46, 47, 48, 42, 23)
achaques <- c(5, 7, 8, 10, 15, 20, 22, 16, 28, 26, 18, 23, 24, 22, 18, 16, 18, 20, 22, 24, 16, 18) # escala 5-30



edades
achaques


datos <- data.frame(edades, achaques)
datos



# Gráfica de dispersión 
ggplot(data = datos, mapping = aes(x = edades, y = achaques)) +
  geom_point(colour = "blue")



# Construir un modelo  de Regresión Lineal Simple
modelo <- lm(data = datos, formula = achaques ~ edades)
sm <- summary(modelo) # El resumen de los estadísticos

sm 



ggplot(data = datos, aes(x = edades, y = achaques)) +
  geom_point(colour = "blue") +
  geom_line(aes(x=edades, y = modelo$fitted.values), color = 'red')
  
  

# nuevos datos de edades

nuevas.edades <- c(55, 60, 20)


# Predecir las edades de 55, 60 y 20 para las personas nuevas
predict(object = modelo, newdata = data.frame(edades = nuevas.edades))


        




