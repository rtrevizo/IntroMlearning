#Regresi�n lineal simple

#Framing: Edad/Peso de un grupo de ni�os
#nos interesa resolver: Cu�l es el peso que se espera tenga un
#ni�o de 6 a�os de edad?

#Esto se da en t�rminos de: Peso = a * Edad + b
#es decir, el Peso siendo x es igual a la pendiente o slope (a)
#multiplicado por la Edad, + el intercepto de y (b)

#por lo que el modelo buscar� ajustarse para identificar los mejores
#valores de a y de b para contestar la pregunta original
#el procedimiento se har� a trav�s de m�nimos cuadrados

#Cargar paquetes y llamar librerias
#install.packages('tidyverse')
library(tidyverse)

#Cargar datos
rdata <- read.csv("C:\\Users\\rodol\\Desktop\\Final Machine Learning\\Regresion\\Rlineal.csv")

#Leer datos
rdata

View(rdata)

#Con un slope igual al intercepto

rdata %>%
ggplot(aes(x = Edad, 
           y = Peso)) +
  geom_point() +
geom_abline(intercept = 1,
            slope = 1,
            col = "blue")

#Probar con un slope m�s grande

rdata %>%
ggplot(aes(x = Edad, 
           y = Peso)) +
  geom_point() +
  geom_abline(intercept = 1,
              slope = 3,
              col = "blue")

#Para el mejor ajuste de regresion lineal simple, se usa la funci�n 
#lm

#ella utiliza un m�todo matem�tico denominado m�nimos cuadrados
#para resolver la mejor linea recta en relaci�n a la distribuci�n
#de puntos del diagrama de dispersi�n

#Para armar correctamente la funci�n lm debe de asignarse primero
#la variable "y" luego una tilde de e�e (objeto tipo formula)
#seguido de la variable "x"

#Donde el primer valor es el resultado o variable dependiente
#mientras que el segundo representa la causa o variable independiente
#en resumen, estamos infiriendo que el peso varia con la edad...

lm(Peso ~ Edad, data = rdata)
#Una vez aplicada la funci�n lm se ajusta el modelo a los mejores
#valores posibles
?lm

#Los valores que regresa la funci�n lm deber�n usarse para construir
#una l�nea que se ajuste lo mejor posible al grupo de puntos
#pasando lo m�s enmedio (ajustado) que se pueda para que represente 
#mejor el comportamiento original de los datos

rdata %>%
  ggplot(aes(x = Edad, 
             y = Peso)) +
  geom_point() +
  geom_abline(intercept = -0.2454,
              slope = 4.6,
              col = "blue")

#Para contestar la pregunta lo mejor es graficar una linea que parta
#el set de datos en 6 a�os de edad para identificar lo que andabamos 
#buscando

rdata %>%
  ggplot(aes(x = Edad, 
             y = Peso)) +
  geom_point() +
  geom_abline(intercept = -0.2454,
              slope = 4.6,
              col = "blue") +
geom_vline(xintercept = 6,
           col = "red")

#Para calcular el valor exacto sustituimos la f�rmula en R
#Peso = a * Edad + b, donde primero determinamos la Edad Objetivo

Edad <- 11

#Luego para obtener el Peso, se sustituyen los valores de slope y
#de intercepto y

Peso <- 4.6 * Edad + -0.2454
Peso

#Con esto se contesta la pregunta, viendo que nuestro modelo 
#nos indica que un ni�o de 6 a�os muy probablemente tendr� un peso
#promedio de 27 kilogramos

#La regresi�n lineal permite predecir pesos para edades que no se
#encuentran en los datos

#Por ejemplo si quisiera saberse cuanto ser�a el peso promedio
#esperado de un ni�o de 14 a�os, pudiera saberse en base al
#comportamiento de los datos en el dataset

rdata

rdata %>%
  ggplot(aes(x = Edad, 
             y = Peso)) +
  geom_point() +
  geom_abline(intercept = -0.2454,
              slope = 4.6,
              col = "blue") +
  geom_vline(xintercept = 14,
             col = "red")

Edad <- 14
Peso <- 4.6 * Edad + -0.2454
Peso

#sin embargo la predicci�n de esta forma es limitada a 1 dato
#m�s adelante veremos como predecir en intervalo y con nivel de 
#tolerancia

#Otra forma de comparar variables para determinar correlaci�n
#con funci�n pairs
pairs(rdata)

#Corregida con las variables de inter�s
edad_peso <- rdata[2:3]
pairs(edad_peso)


#REGRESAR A EXPLICACI�N TE�RICA....

#Ahora, c�mo tener la mayor certeza posible de que el comportamiento
#del peso puede ser explicado por la edad?

#O como tener certeza de que la relaci�n de dos variables pudiera 
#explicar su comportamiento...

#Para ello se realizan pruebas estad�sticas, primero se determina
#el tipo de prueba que se realizar� en funci�n de las variables
#como Peso/Edad son variables n�mericas las dos, puede aplicarse
#un an�lisis de correlaci�n de Pearson, siempre y cuando 
#se presuma normalidad en la distribuci�n de datos (puntos)

#Para determinar la normalidad de una distribuci�n puede utilizarse
#el test shapiro-wilk 

#n�tese que para las pruebas es necesario ingresar las variables
#en forma de vectores

shapiro.test(rdata$Edad)
shapiro.test(rdata$Peso)

#valores p mayores de 0.05 quiere decir que la hipotesis nula 
#no se rechaza, lo que indica normalidad en la distribuci�n de datos
#lo que hace sentido si consideramos que a mayor edad mayor peso
#y el incremento se perfila relativamente gradual

#una vez que se determina que la distribuci�n es normal, se procede
#a calcular el coeficiente de correlaci�n con la funci�n cor
#usando las variables x y y para realizarlo

cor(rdata$Edad,rdata$Peso, method = "pearson")

#Recordando la teor�a: el cuadrado del coeficiente de 
#correlaci�n (R2) se denomina coeficiente de determinaci�n 
#y representa el el porcentaje de cambio de la variable "y" 
#que es explicado por la variable "x"

#es decir, si el cuadrado de lo que arroja la funci�n cor()
#es alto entonces quiere decir que a mayor edad se presume
#habr� un peso mayor y m�s a�n, que las predicciones futuras
#pudieran ser m�s precisas

0.9952291^2

?cor


#Veamos otro ejemplo, solo para contrastar los resultados ---------

data("Orange")

#Revisamos el data set "Orange"
?Orange

view(Orange)

#Son datos de �rboles de naranja, su crecimiento en relaci�n a 
#su tiempo de vida en d�as y su circunferencia en milimetros


#Veamos su diagrama de dispersi�n
Orange %>%
  ggplot(aes(x = age, 
             y = circumference))+
  geom_point()

#vista alternativa
age_circunference <- (Orange[2:3])
pairs(age_circunference)

#Que tipo de correlaci�n presentan esos datos?
#De ver esa gr�fica crees que ser� igual de fuerte que la del
#ejercicio Peso/Edad anterior?

shapiro.test(Orange$age)

#Pueden realizarse pruebas alternativas para corroborar el
#resultado

library(nortest)
ad.test(Orange$age)
sf.test(Orange$age)
cvm.test(Orange$age)
lillie.test(Orange$age)
pearson.test(Orange$age)

#Como se quiere determinar el grado de asociaci�n lineal entre dos
#variables cuantitativas pero en una distribuci�n no normal
#entonces se usa el coeficiente de correlaci�n Tau-B de Kendall

cor(Orange$age,Orange$circumference, method = "kendall")

0.7948496^2

#Como era de esperarse seg�n el diagrama de dispersi�n, se observa
#que la relaci�n entre el tiempo de vida de los �rboles y su 
#circunferencia es positiva pero es media, pues seguro varios
#varios valores estar�n por debajo de la l�nea recta de cualquier
#modelo, lo que har� dificil predecir valores futuros con
#precisi�n


#Para realizar predicciones sobre un modelo -----------------------

#Supongase que se quiere saber m�s que solo el peso de un ni�o de 6
#a�os, que era la pregunta original del problem framing
#con la regresi�n podemos determinar un intervalo de edades 
#para calcular los pesos que se espera tuvieran de acuerdo al 
#modelo

#Convertimos el modelo a objeto tipo variable

modelreg <- lm(Peso ~ Edad, data = rdata)

#Dise�amos el objeto del intervalo de edades que nos interesa
#en este ejemplo vemos de 11 a 15 a�os de edad

nueva_edad <- data.frame(Edad = seq(11, 15))

#Llamamos la funci�n predict con el modelo como objeto y 
#el intervalo de edad nuevo, lo que nos regresa los valores
#promedio esperados de peso en relaci�n con el intervalo 11-15

predict(modelreg, nueva_edad)

#La funci�n predict adicionalmente nos permite calcular el peso
#dentro de un nivel de tolerancia que por defecto es 90%
#para dar margen de error de -5% y +5% a la estimaci�n

predict(modelreg, nueva_edad, interval = "prediction")

#Modelo de predicci�n
set.seed(22)
{
x <- rnorm(5)
y <- x * rnorm(5, mean = 3, sd = 1)
xy <- data.frame(x,y)
mdl <- lm(y ~ x, data = xy)
predx <- data.frame(x = seq(from = -2, to = 3, by = 0.1))
pred.int <- cbind(predx, predict(mdl, newdata = predx, interval = "prediction", level = 0.95))
}

install.packages('ggthemes')
library(ggthemes)

#Graficar el modelo de predicci�n
library(ggplot2)
ggplot(pred.int, aes(x = x, y = fit)) +
  theme_bw() +
  ggtitle("Intervalo de predicci�n para futuras observaciones") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(data = xy, aes(x = x, y = y)) +
  geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity")+
  xlab("Observaciones")+
  ylab("Ajuste")

#Comparar puntos 11, 12, 13, 14 y 15 con  el modelo
predict(modelreg, nueva_edad, interval = "prediction")
