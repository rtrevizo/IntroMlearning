#Ejemplo de análisis de regresión lineal múltiple en R

#Se desea modelar las calificaciones de un grupo de estudiantes en
#relación con su IQ y la cantidad de horas que dedican a estudiar
#a la semana

#Adicional al modelo se busca contestar la pregunta de: Cuál es la
#calificación promedio de un estudiante con IQ de 130 que dedica
#3 horas de estudio a la semana?

choose.files()

datosiq <- read.csv("C:\\Users\\rodol\\Desktop\\Final Machine Learning\\Regresion\\Multiple\\IQR.csv")

modeliq <- lm(datosiq$Promedio ~ datosiq$ï..IQ+datosiq$Tiempo, data = datosiq)

summary(modeliq)

#Para graficar regresión múltiple -------------------------------
#install.packages('scatterplot3d')
#install.packages('FactoClass')
library(scatterplot3d)
library(FactoClass)

#Llamo a la función de graficación 3D, porque tengo 3 variables
#IQ, Tiempo y Promedio
#mover el ángulo para ver a 60 y leer el sexto punto de 125-6-4.5
#type "l" para que vean gráfico de lineas, o "o" para gráfico de 
#punto y línea, "h" lineas verticales, "p" puntos por defecto

scatterplot3d::scatterplot3d(x = datosiq$ï..IQ,
                             y = datosiq$Promedio,
                             z = datosiq$Tiempo, 
                             xlab = "IQ",
                             ylab = "Promedio",
                             zlab = "Tiempo",
                             pch = 16,
                             angle = 40,
                             type = "p") 

#Le añado las rejillas completas al cubo
FactoClass::addgrids3d(x = datosiq$ï..IQ,
                       y = datosiq$Promedio,
                       z = datosiq$Tiempo,
                       grid = c("xy", "xz", "yz"))


#ver los datos de la tabla en referencia al grafo
datosiq


#porcentaje de variabilidad explicado por el modelo R2 es de
#0.91, es decir, de 91% (es decir, tiene 91% de efectividad)
#Según el valor p de 0.00001972 < 0.05 podemos deducir
#existe evidencia estadística de que los datos se ajustan al modelo
#de regresión

#De acuerdo a los valores p de la prueba t de IQ y de Tiempo
#de 0.001 y 0.002, ambos < 0.05, por lo que existe evidencia
#estadística de que los dos influyen en el promedio de la calificación

#Para contestar la pregunta del modelo de regresión múltiple...

#Cuál es la calificación promedio de un estudiante con IQ de 130 
#que dedica 3 horas de estudio a la semana?

#basta con utilizar el valor del intercepto + los valores estimados
#como a y b, multiplicados por el IQ y el Tiempo, quedaría como:


#Intercepto Estimado + IQEstimado * 130IQ + TiempoEstimado * 3 horas
-13.05239 + 0.12281*130 + 0.29665*3

#La calificación promedio estimada con IQ130 según el modelo 
#sería de 3.8

#install.packages('rmarkdown') para generar reportes
  