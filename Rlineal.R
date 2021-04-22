#Regresión lineal simple

#Framing: Edad/Peso de un grupo de niños
#nos interesa resolver: Cuál es el peso que se espera tenga un
#niño de 6 años de edad?

#Esto se da en términos de: Peso = a * Edad + b
#es decir, el Peso siendo x es igual a la pendiente o slope (a)
#multiplicado por la Edad, + el intercepto de y (b)

#por lo que el modelo buscará ajustarse para identificar los mejores
#valores de a y de b para contestar la pregunta original
#el procedimiento se hará a través de mínimos cuadrados

#Cargar paquetes y llamar librerias
#install.packages('tidyverse')
library(tidyverse)

#Cargar datos
rdata <- read.csv("C:\\Users\\rodol\\Desktop\\Final Machine Learning\\Regresion\\dataregresion.csv")

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

#Probar con un slope más grande

rdata %>%
ggplot(aes(x = Edad, 
           y = Peso)) +
  geom_point() +
  geom_abline(intercept = 1,
              slope = 3,
              col = "blue")

#Para el mejor ajuste de regresion lineal simple, se usa la función 
#lm

#ella utiliza un método matemático denominado mínimos cuadrados
#para resolver la mejor linea recta en relación a la distribución
#de puntos del diagrama de dispersión

#Para armar correctamente la función lm debe de asignarse primero
#la variable "y" luego una tilde de eñe (objeto tipo formula)
#seguido de la variable "x"

#Donde el primer valor es el resultado o variable dependiente
#mientras que el segundo representa la causa o variable independiente
#en resumen, estamos infiriendo que el peso varia con la edad...

lm(Peso ~ Edad, data = rdata)
#Una vez aplicada la función lm se ajusta el modelo a los mejores
#valores posibles
?lm

#Los valores que regresa la función lm deberán usarse para construir
#una línea que se ajuste lo mejor posible al grupo de puntos
#pasando lo más enmedio (ajustado) que se pueda para que represente 
#mejor el comportamiento original de los datos

rdata %>%
  ggplot(aes(x = Edad, 
             y = Peso)) +
  geom_point() +
  geom_abline(intercept = -0.2454,
              slope = 4.6,
              col = "blue")

#Para contestar la pregunta lo mejor es graficar una linea que parta
#el set de datos en 6 años de edad para identificar lo que andabamos 
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

#Para calcular el valor exacto sustituimos la fórmula en R
#Peso = a * Edad + b, donde primero determinamos la Edad Objetivo

Edad <- 11

#Luego para obtener el Peso, se sustituyen los valores de slope y
#de intercepto y

Peso <- 4.6 * Edad + -0.2454
Peso

#Con esto se contesta la pregunta, viendo que nuestro modelo 
#nos indica que un niño de 6 años muy probablemente tendrá un peso
#promedio de 27 kilogramos

#La regresión lineal permite predecir pesos para edades que no se
#encuentran en los datos

#Por ejemplo si quisiera saberse cuanto sería el peso promedio
#esperado de un niño de 14 años, pudiera saberse en base al
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

#sin embargo la predicción de esta forma es limitada a 1 dato
#más adelante veremos como predecir en intervalo y con nivel de 
#tolerancia

#Otra forma de comparar variables para determinar correlación
#con función pairs
pairs(rdata)

#Corregida con las variables de interés
edad_peso <- rdata[2:3]
pairs(edad_peso)


#REGRESAR A EXPLICACIÓN TEÓRICA....

#Ahora, cómo tener la mayor certeza posible de que el comportamiento
#del peso puede ser explicado por la edad?

#O como tener certeza de que la relación de dos variables pudiera 
#explicar su comportamiento...

#Para ello se realizan pruebas estadísticas, primero se determina
#el tipo de prueba que se realizará en función de las variables
#como Peso/Edad son variables númericas las dos, puede aplicarse
#un análisis de correlación de Pearson, siempre y cuando 
#se presuma normalidad en la distribución de datos (puntos)

#Para determinar la normalidad de una distribución puede utilizarse
#el test shapiro-wilk 

#nótese que para las pruebas es necesario ingresar las variables
#en forma de vectores

shapiro.test(rdata$Edad)
shapiro.test(rdata$Peso)

#valores p mayores de 0.05 quiere decir que la hipotesis nula 
#no se rechaza, lo que indica normalidad en la distribución de datos
#lo que hace sentido si consideramos que a mayor edad mayor peso
#y el incremento se perfila relativamente gradual

#una vez que se determina que la distribución es normal, se procede
#a calcular el coeficiente de correlación con la función cor
#usando las variables x y y para realizarlo

cor(rdata$Edad,rdata$Peso, method = "pearson")

#Recordando la teoría: el cuadrado del coeficiente de 
#correlación (R2) se denomina coeficiente de determinación 
#y representa el el porcentaje de cambio de la variable "y" 
#que es explicado por la variable "x"

#es decir, si el cuadrado de lo que arroja la función cor()
#es alto entonces quiere decir que a mayor edad se presume
#habrá un peso mayor y más aún, que las predicciones futuras
#pudieran ser más precisas

0.9952291^2

?cor


#Veamos otro ejemplo, solo para contrastar los resultados ---------

data("Orange")

#Revisamos el data set "Orange"
?Orange

view(Orange)

#Son datos de árboles de naranja, su crecimiento en relación a 
#su tiempo de vida en días y su circunferencia en milimetros


#Veamos su diagrama de dispersión
Orange %>%
  ggplot(aes(x = age, 
             y = circumference))+
  geom_point()

#vista alternativa
age_circunference <- (Orange[2:3])
pairs(age_circunference)

#Que tipo de correlación presentan esos datos?
#De ver esa gráfica crees que será igual de fuerte que la del
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

#Como se quiere determinar el grado de asociación lineal entre dos
#variables cuantitativas pero en una distribución no normal
#entonces se usa el coeficiente de correlación Tau-B de Kendall

cor(Orange$age,Orange$circumference, method = "kendall")

0.7948496^2

#Como era de esperarse según el diagrama de dispersión, se observa
#que la relación entre el tiempo de vida de los árboles y su 
#circunferencia es positiva pero es media, pues seguro varios
#varios valores estarán por debajo de la línea recta de cualquier
#modelo, lo que hará dificil predecir valores futuros con
#precisión


#Para realizar predicciones sobre un modelo -----------------------

#Supongase que se quiere saber más que solo el peso de un niño de 6
#años, que era la pregunta original del problem framing
#con la regresión podemos determinar un intervalo de edades 
#para calcular los pesos que se espera tuvieran de acuerdo al 
#modelo

#Convertimos el modelo a objeto tipo variable

modelreg <- lm(Peso ~ Edad, data = rdata)

#Diseñamos el objeto del intervalo de edades que nos interesa
#en este ejemplo vemos de 11 a 15 años de edad

nueva_edad <- data.frame(Edad = seq(11, 15))

#Llamamos la función predict con el modelo como objeto y 
#el intervalo de edad nuevo, lo que nos regresa los valores
#promedio esperados de peso en relación con el intervalo 11-15

predict(modelreg, nueva_edad)

#La función predict adicionalmente nos permite calcular el peso
#dentro de un nivel de tolerancia que por defecto es 90%
#para dar margen de error de -5% y +5% a la estimación

predict(modelreg, nueva_edad, interval = "prediction")

#Modelo de predicción
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

#Graficar el modelo de predicción
library(ggplot2)
ggplot(pred.int, aes(x = x, y = fit)) +
  theme_bw() +
  ggtitle("Intervalo de predicción para futuras observaciones") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(data = xy, aes(x = x, y = y)) +
  geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity")+
  xlab("Observaciones")+
  ylab("Ajuste")

#Comparar puntos 11, 12, 13, 14 y 15 con  el modelo
predict(modelreg, nueva_edad, interval = "prediction")
