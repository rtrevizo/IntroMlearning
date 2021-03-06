#Ejemplo de an�lisis de regresi�n lineal m�ltiple en R

#Se desea modelar las calificaciones de un grupo de estudiantes en
#relaci�n con su IQ y la cantidad de horas que dedican a estudiar
#a la semana

#Adicional al modelo se busca contestar la pregunta de: Cu�l es la
#calificaci�n promedio de un estudiante con IQ de 130 que dedica
#3 horas de estudio a la semana?

choose.files()

datosiq <- read.csv("C:\\Users\\rodol\\Desktop\\Final Machine Learning\\Regresion\\Multiple\\IQR.csv")

modeliq <- lm(datosiq$Promedio ~ datosiq$�..IQ+datosiq$Tiempo, data = datosiq)

summary(modeliq)

#Para graficar regresi�n m�ltiple -------------------------------
#install.packages('scatterplot3d')
#install.packages('FactoClass')
library(scatterplot3d)
library(FactoClass)

#Llamo a la funci�n de graficaci�n 3D, porque tengo 3 variables
#IQ, Tiempo y Promedio
#mover el �ngulo para ver a 60 y leer el sexto punto de 125-6-4.5
#type "l" para que vean gr�fico de lineas, o "o" para gr�fico de 
#punto y l�nea, "h" lineas verticales, "p" puntos por defecto

scatterplot3d::scatterplot3d(x = datosiq$�..IQ,
                             y = datosiq$Promedio,
                             z = datosiq$Tiempo, 
                             xlab = "IQ",
                             ylab = "Promedio",
                             zlab = "Tiempo",
                             pch = 16,
                             angle = 40,
                             type = "p") 

#Le a�ado las rejillas completas al cubo
FactoClass::addgrids3d(x = datosiq$�..IQ,
                       y = datosiq$Promedio,
                       z = datosiq$Tiempo,
                       grid = c("xy", "xz", "yz"))


#ver los datos de la tabla en referencia al grafo
datosiq


#porcentaje de variabilidad explicado por el modelo R2 es de
#0.91, es decir, de 91% (es decir, tiene 91% de efectividad)
#Seg�n el valor p de 0.00001972 < 0.05 podemos deducir
#existe evidencia estad�stica de que los datos se ajustan al modelo
#de regresi�n

#De acuerdo a los valores p de la prueba t de IQ y de Tiempo
#de 0.001 y 0.002, ambos < 0.05, por lo que existe evidencia
#estad�stica de que los dos influyen en el promedio de la calificaci�n

#Para contestar la pregunta del modelo de regresi�n m�ltiple...

#Cu�l es la calificaci�n promedio de un estudiante con IQ de 130 
#que dedica 3 horas de estudio a la semana?

#basta con utilizar el valor del intercepto + los valores estimados
#como a y b, multiplicados por el IQ y el Tiempo, quedar�a como:


#Intercepto Estimado + IQEstimado * 130IQ + TiempoEstimado * 3 horas
-13.05239 + 0.12281*130 + 0.29665*3

#La calificaci�n promedio estimada con IQ130 seg�n el modelo 
#ser�a de 3.8

#install.packages('rmarkdown') para generar reportes
  