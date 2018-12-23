---
title: 'Práctica 2: Limpieza y validación de los datos'
author: "ANTONIO SANCHEZ NAVARRO"
tuthor: "Cristina Palomares Bonache"
date: '`r format(Sys.Date(),"%e de %B %Y")`'
output: 
 html_document:
    toc: yes
    number_sections: yes
    toc_depth: 2
    toc_float: true 
lang: es    # speak language: ca is catalan, es is spanish
nocite: |   # cites do not include in the text but include in bibilography references
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r load_libraries, include=FALSE}
library(knitr)
library(lubridate)
library(VIM)
library(stringr)
library(psych)
library(ROCR)
library(dplyr)
library(nortest)
```

```{r lectura, echo=FALSE}
# read data
myfile <- "C:/Users/Antonio/Desktop/UOC/Tipologýa y ciclo de vida de los datos/PRAC2/winequality-red.csv" 
mydata <- read.csv(myfile)
n.var <- names(mydata)
#summary(mydata)
```

```{r chunck0, eval=FALSE, echo=FALSE}
# read data
kable(names(mydata))
toString(names(mydata))
```
****
# Carga de los datos
****

  En esta actividad se usará el fichero winequality-red.csv del repositorio Github, el cual precisa tareas de
  preprocesado (limpieza, integración y validación) para posterior análisis. Los datos a tratar corresponden a
  variables físicoquímicas correspondientes a variantes rojas del vino portugués "Vinho Verde", las cuales se prestan
  a tareas de clasificación o análisis de regresión. Las clases están ordenadas y no son equilibradas (por ejemplo,
  hay muchos más vinos normales que vinos excelentes o pobres).
  
  El archivo se denomina *`r myfile`*, contiene `r nrow(mydata)` registros y `r ncol(mydata)` variables. Estas  
  variables son: `r toString(names(mydata))`

```{r chumck1}
# Cargo el archivo de datos "winequality-red.csv" y valido que los tipos
# de datos se interpretan correctamente
winequality.red <- read.csv("C:/Users/Antonio/Desktop/UOC/Tipologýa y ciclo de vida de los datos/PRAC2/winequality-red.csv", stringsAsFactors = FALSE, header = TRUE)
head(winequality.red[,1:5])
```

****
# Resolución
****

  Examinamos el tipo de dato asociado a cada campo

```{r chumck2}
# Tipo de dato asignado a cada campo
sapply(winequality.red, function(x) class(x))
```

  Histograma de frecuencias absolutas para cada variable fisicoquímica

```{r chumck3}
# Histograma de frecuencias absolutas de las variables fisicoquímicas
hist(winequality.red$fixed.acidity, main="fixed acidity", xlab="Class brands", ylab="Frequency", col="blue")
hist(winequality.red$volatile.acidity, main="volatile acidity", xlab="Class brands", ylab="Frequency", col="blue")
hist(winequality.red$citric.acid, main="citric acid", xlab="Class brands", ylab="Frequency", col="blue")
hist(winequality.red$residual.sugar, main="residual sugar", xlab="Class brands", ylab="Frequency", col="blue")
hist(winequality.red$chlorides, main="chlorides", xlab="Class brands", ylab="Frequency", col="blue")
hist(winequality.red$free.sulfur.dioxide, main="free sulfur dioxide", xlab="Class brands", ylab="Frequency", col="blue")
hist(winequality.red$total.sulfur.dioxide, main="total sulfur dioxide", xlab="Class brands", ylab="Frequency", col="blue")
hist(winequality.red$density, main="density", xlab="Class brands", ylab="Frequency", col="blue")
hist(winequality.red$pH, main="pH", xlab="Class brands", ylab="Frequency", col="blue")
hist(winequality.red$sulphates, main="sulphates", xlab="Class brands", ylab="Frequency", col="blue")
hist(winequality.red$alcohol, main="alcohol", xlab="Class brands", ylab="Frequency", col="blue")
hist(winequality.red$quality, main="quality", xlab="Class brands", ylab="Frequency", col="blue", breaks = 5)
```

****
## Integración y selección de los datos de interés
****
  En esta sección, eliminamos columnas innecesarias o redundantes y fijamos el número de cifras
  decimales que deben contemplar. Se establece la columna "quality" de tipo numérico para facilitar
  cálculos y resultados posteriores.

```{r chumck4}
# Eliminación de datos de columnas redundantes
winequality.red <- winequality.red[, -(6:6)]
# Unimos las dos columnas de acidez (fija y volátil) en una sola columna
winequality.red$fixed.acidity<-winequality.red$fixed.acidity + winequality.red$volatile.acidity
winequality.red$fixed.acidity<-round(winequality.red$fixed.acidity,2)
colnames(winequality.red)[colnames(winequality.red)=="fixed.acidity"] <- "acidity"
# Ahora que ya disponemos de la acidez total, eliminamos la columna "volatile.acidity":
winequality.red <- winequality.red[, -(2:2)]
head(winequality.red)
```

```{r chumck5}
# Establecemos el número de cifras decimales en las columnas "acidity", "citric.acid" "chlorides" y "density"
winequality.red$acidity<-round(winequality.red$acidity, 2)
winequality.red$citric.acid<-round(winequality.red$citric.acid, 2)
winequality.red$chlorides<-round(winequality.red$chlorides, 3)
winequality.red$density<-round(winequality.red$density, 4)
# Convertimos la columna "quality" a tipo "numeric":
winequality.red$quality<-as.numeric(winequality.red$quality)
class(winequality.red$quality)
```

****
## Detección de ceros y elementos vacíos por campo
****

  En esta sección, se lleva a cabo la detección de ceros y elementos vacíos por campo

```{r chumck6}
# Número de valores desconocidos por campo
sapply(winequality.red, function(x) sum(is.na(x)))
```

****
## Valores extremos
****

  Identificamos outliers de cada variable fisicoquímica mediante diagramas de caja y usando la función
  boxplots.stats() de R.

```{r chumck7}
boxplot(winequality.red$acidity,main="Box plot of acidity", col="gray",ylab="values")
boxplot.stats(winequality.red$acidity)$out
boxplot(winequality.red$citric.acid,main="Box plot of citric acid", col="gray",ylab="values")
boxplot.stats(winequality.red$citric.acid)$out
boxplot(winequality.red$residual.sugar,main="Box plot of residual sugar", col="gray",ylab="values")
boxplot.stats(winequality.red$residual.sugar)$out
boxplot(winequality.red$chlorides,main="Box plot of chlorides", col="gray",ylab="values")
boxplot.stats(winequality.red$chlorides)$out
boxplot(winequality.red$total.sulfur.dioxide,main="Box plot of total sulfur dioxide", col="gray",ylab="values")
boxplot.stats(winequality.red$total.sulfur.dioxide)$out
boxplot(winequality.red$density,main="Box plot of density", col="gray",ylab="values")
boxplot.stats(winequality.red$density)$out
boxplot(winequality.red$pH,main="Box plot of pH", col="gray",ylab="values")
boxplot.stats(winequality.red$pH)$out
boxplot(winequality.red$sulphates,main="Box plot of sulphates", col="gray",ylab="values")
boxplot.stats(winequality.red$sulphates)$out
boxplot(winequality.red$alcohol,main="Box plot of alcohol", col="gray",ylab="values")
boxplot.stats(winequality.red$alcohol)$out
```

```{r chumck8}
# Eliminamos valores outliers de cada una de las variables fisicoquímicas.

outliers.acidity <- boxplot(winequality.red$acidity, plot=FALSE)$out
winequality.red <- winequality.red[-which(winequality.red$acidity %in% outliers.acidity),]

outliers.citric.acid <- boxplot(winequality.red$citric.acid, plot=FALSE)$out
winequality.red <- winequality.red[-which(winequality.red$citric.acid %in% outliers.citric.acid),]

outliers.residual.sugar <- boxplot(winequality.red$residual.sugar, plot=FALSE)$out
winequality.red <- winequality.red[-which(winequality.red$residual.sugar %in% outliers.residual.sugar),]

outliers.chlorides <- boxplot(winequality.red$chlorides, plot=FALSE)$out
winequality.red <- winequality.red[-which(winequality.red$chlorides %in% outliers.chlorides),]

outliers.total.sulfur.dioxide <- boxplot(winequality.red$total.sulfur.dioxide, plot=FALSE)$out
winequality.red <- winequality.red[-which(winequality.red$total.sulfur.dioxide %in% outliers.total.sulfur.dioxide),]

outliers.density <- boxplot(winequality.red$density, plot=FALSE)$out
winequality.red <- winequality.red[-which(winequality.red$density %in% outliers.density),]

outliers.pH <- boxplot(winequality.red$pH, plot=FALSE)$out
winequality.red <- winequality.red[-which(winequality.red$pH %in% outliers.pH),]

outliers.sulphates <- boxplot(winequality.red$sulphates, plot=FALSE)$out
winequality.red <- winequality.red[-which(winequality.red$sulphates %in% outliers.sulphates),]

outliers.alcohol <- boxplot(winequality.red$alcohol, plot=FALSE)$out
winequality.red <- winequality.red[-which(winequality.red$alcohol %in% outliers.alcohol),]
```

```{r chumck9}
# Número de columnas y registros o filas del nuevo dataset
ncol(winequality.red)
nrow(winequality.red)
```

****
## Exportación de los datos preprocesados
****

  Una vez limpiados los datos, los guardamos en un fichero llamado winequality-red_data_clean.csv

```{r chumck10}
write.csv(winequality.red, "C:/Users/Antonio/Desktop/UOC/Tipologýa y ciclo de vida de los datos/PRAC2/winequality-red_data_clean.csv")
```

****
# Análisis de resultados
****

****
## Selección de los grupos de datos que se quieren comparar
****

  Establecemos grupos dentro del conjunto de datos para posteriores análisis y comparaciones.


```{r chumck11}
# Agrupación por valores de densidad
low.density <- winequality.red[winequality.red$density <= mean(winequality.red$density),]
high.density <- winequality.red[winequality.red$density > mean(winequality.red$density),]

# Agrupación por porcentaje de alcohol en vino
low.alcohol.percentage<- winequality.red[winequality.red$alcohol <= 11.5,]
high.alcohol.percentage <- winequality.red[winequality.red$alcohol > 11.5,]

# Agrupación por cantidad de sal presente en el vino
low.clhorides <- winequality.red[winequality.red$chlorides <= mean(winequality.red$chlorides),]
high.clhorides <- winequality.red[winequality.red$chlorides > mean(winequality.red$chlorides),]

```

****
## Pruebas de normalidad y homogeneidad de la varianza
****

 Pruebas de normalidad de Anderson-Darling

```{r chumck12}
library(nortest)
alpha = 0.05
col.names = colnames(winequality.red)
for (i in 1:ncol(winequality.red)) {
if (i == 1) cat("Listado de variables fisicoquímicas que no siguen una distribución normal:\n")
if (is.integer(winequality.red[,i]) | is.numeric(winequality.red[,i])) {
p_val = ad.test(winequality.red[,i])$p.value
if (p_val < alpha) {
cat(col.names[i])
# Format output
if (i < ncol(winequality.red) - 1) cat(", ")
if (i %% 3 == 0) cat("\n")
}
}
}
```

  Test de normalidad Shapiro-Wilk y gráficos Q-Q
  
```{r chumck13}
qqnorm(winequality.red$acidity, main = "Normal Q-Q Plot for acidity")
qqline(winequality.red$acidity, col = "red")
shapiro.test(winequality.red$acidity)

qqnorm(winequality.red$citric.acid, main = "Normal Q-Q Plot for citric acid")
qqline(winequality.red$citric.acid, col = "red")
shapiro.test(winequality.red$citric.acid)

qqnorm(winequality.red$residual.sugar, main = "Normal Q-Q Plot for redidual sugar")
qqline(winequality.red$residual.sugar, col = "red")
shapiro.test(winequality.red$residual.sugar)

qqnorm(winequality.red$chlorides, main = "Normal Q-Q Plot for chlorides")
qqline(winequality.red$chlorides, col = "red")
shapiro.test(winequality.red$chlorides)

qqnorm(winequality.red$total.sulfur.dioxide, main = "Normal Q-Q Plot for total sulfur dioxide")
qqline(winequality.red$total.sulfur.dioxide, col = "red")
shapiro.test(winequality.red$total.sulfur.dioxide)

qqnorm(winequality.red$density, main = "Normal Q-Q Plot for density")
qqline(winequality.red$density, col = "red")
shapiro.test(winequality.red$density)

qqnorm(winequality.red$pH, main = "Normal Q-Q Plot for pH")
qqline(winequality.red$pH, col = "red")
shapiro.test(winequality.red$pH)

qqnorm(winequality.red$sulphates, main = "Normal Q-Q Plot for sulphates")
qqline(winequality.red$sulphates, col = "red")
shapiro.test(winequality.red$sulphates)

qqnorm(winequality.red$alcohol, main = "Normal Q-Q Plot for alcohol")
qqline(winequality.red$alcohol, col = "red")
shapiro.test(winequality.red$alcohol)

qqnorm(winequality.red$quality, main = "Normal Q-Q Plot for quality")
qqline(winequality.red$quality, col = "red")
shapiro.test(winequality.red$quality)
```

  Estudio de la homogeneidad de las varianzas. Test de Fligner-Kileen 

```{r chumck14}
fligner.test(quality ~ density, data = winequality.red)
fligner.test(quality ~ alcohol, data = winequality.red)
fligner.test(quality ~ chlorides, data = winequality.red)
```

****
# Pruebas estadísticas
****

****
## Influencia de las variables fisicoquímicas en la calidad de los vinos
****

```{r chumck15}
corr_matrix <- matrix(nc = 2, nr = 0)
colnames(corr_matrix) <- c("estimate", "p-value")
# Calcular el coeficiente de correlación para cada variable fisicoquimica
# con respecto al campo "quality"
for (i in 1:(ncol(winequality.red) - 1)) {
if (is.integer(winequality.red[,i]) | is.numeric(winequality.red[,i])) {
spearman_test = cor.test(winequality.red[,i],
winequality.red[,length(winequality.red)],
method = "spearman")
corr_coef = spearman_test$estimate
p_val = spearman_test$p.value
# Add row to matrix
pair = matrix(ncol = 2, nrow = 1)
pair[1][1] = corr_coef
pair[2][1] = p_val
corr_matrix <- rbind(corr_matrix, pair)
rownames(corr_matrix)[nrow(corr_matrix)] <- colnames(winequality.red)[i]
}
}
```

```{r chumck16}
print(corr_matrix)
```

## Matriz de correlación entre variables

```{r chumck17}
library(PerformanceAnalytics)
# Guardamos datos en un data.frame
acidity<-winequality.red$acidity
citric.acid<-winequality.red$citric.acid
residual.sugar<-winequality.red$residual.sugar
chlorides<-winequality.red$chlorides
total.sulfur.dioxide<-winequality.red$total.sulfur.dioxide
density<-winequality.red$density
pH<-winequality.red$pH
sulphates<-winequality.red$sulphates
alcohol<-winequality.red$alcohol
quality<-winequality.red$quality
data <- data.frame(acidity, citric.acid, residual.sugar, chlorides, total.sulfur.dioxide, density, pH, sulphates, alcohol, quality)
colnames(data) <- c("acidity","citric acid","residual sugar","chlorides","total sulfur dioxide","density", "pH", "sulphates", "alcohol", "quality")
cor(data)
chart.Correlation(data)
library(corrplot)
M<-cor(winequality.red)
corrplot(M, method = "ellipse")
```

## Contrastes de hipótesis

  ¿La calidad de los vinos con densidad inferior a la media supera la de los vinos con densidad
  por encima de la media?

```{r chumck18}
low.density.quality <- winequality.red[winequality.red$density <= mean(winequality.red$density),]$quality
high.density.quality <- winequality.red[winequality.red$density > mean(winequality.red$density),]$quality
```

```{r chumck19}
t.test(low.density.quality, high.density.quality, alternative = "less", conf.level = 0.95)
```

  ¿La calidad de los vinos con menos sal es igual o diferente a la de los vinos más salados?

```{r chumck20}
low.chlorides.quality <- winequality.red[winequality.red$chlorides <= mean(winequality.red$chlorides),]$quality
high.chlorides.quality <- winequality.red[winequality.red$chlorides > mean(winequality.red$chlorides),]$quality
```

```{r chumck21}
t.test(low.chlorides.quality, high.chlorides.quality, alternative = "two.sided", conf.level = 0.95)
```

## Modelo de regresión lineal

  Modelo de regresión multilineal para predecir la calidad

```{r chumck22}
# Regresores cuantitativos más influyentes en la calidad de los vinos
alcohol<-winequality.red$alcohol
sulphates<-winequality.red$sulphates
citric.acid<-winequality.red$citric.acid
density<-winequality.red$density
chlorides<-winequality.red$chlorides
total.sulfur.dioxide<-winequality.red$total.sulfur.dioxide
# Variable que se quiere predecir
quality<-winequality.red$quality
# Modelos de regresión lineal
modelo1 <- lm(quality ~ alcohol + sulphates + citric.acid, data = winequality.red)
modelo2 <- lm(quality ~ alcohol + sulphates + citric.acid + chlorides, data = winequality.red)
modelo3 <- lm(quality ~ alcohol + sulphates + citric.acid + chlorides + density, data = winequality.red)
modelo4 <- lm(quality ~ alcohol + sulphates + citric.acid + density + total.sulfur.dioxide, data = winequality.red)
```

```{r chumck23}
# Tabla con los coeficientes de determinación de cada modelo
tabla.coeficientes <- matrix(c(1, summary(modelo1)$r.squared,
2, summary(modelo2)$r.squared,
3, summary(modelo3)$r.squared,
4, summary(modelo4)$r.squared),
ncol = 2, byrow = TRUE)
colnames(tabla.coeficientes) <- c("Modelo", "R^2")
tabla.coeficientes
```

```{r chumck24}
summary(modelo4)
```

  Predicción de la calidad con el modelo de regresión lineal

```{r chumck25}
newdata <- data.frame(
alcohol = mean(winequality.red$alcohol),
sulphates = mean(winequality.red$sulphates),
citric.acid = mean(winequality.red$citric.acid),
density = mean(winequality.red$density),
total.sulfur.dioxide = mean(winequality.red$total.sulfur.dioxide)
)
# Predecir el precio
predict(modelo4, newdata)
```

  Modelo de regresión multilineal para predecir la acidez 

```{r chumck26}
# Regresores cuantitativos más influyentes en la calidad de los vinos
citric.acid<-winequality.red$citric.acid
density<-winequality.red$density
pH<-winequality.red$pH
# Variable que se quiere predecir
acidity<-winequality.red$acidity
# Modelo de regresión lineal
modelo <- lm(acidity ~ citric.acid + density + pH)
summary(modelo)
```

  Predecimos la acidez para unos valores de ácido cítrico, densidad y pH

```{r chumck27}
data <- data.frame(citric.acid = 0.489, density = 0.998, pH = 3.8)
# Predicción de la acidez
predict(modelo, data)
```

## Modelo de regresión logístico

```{r chumck28}
# Creación de la variable binaria "high.density"
winequality.red$density[winequality.red$density >= 1]<1
winequality.red$density[winequality.red$density < 1]<-0
high.density<-winequality.red$density
high.density<-factor(high.density)
# Variables explicativas de la densidad
acidity<-winequality.red$acidity
alcohol<-winequality.red$alcohol
residual.sugar<-winequality.red$residual.sugar
chlorides<-winequality.red$chlorides
# Estimación del modelo de regresión logística
reglog <- glm(high.density ~ acidity+alcohol+residual.sugar+chlorides, data = winequality.red, family = binomial, control = list(maxit = 1000))
summary(reglog)
# Creación del dataset con los datos necesarios para la predicción
newdata = data.frame(acidity = 6.36,alcohol = 8.496, residual.sugar = 2.226, chlorides=0.198)
# Usamos la función predict() para calcular la probabilidad predicha. Para obtener la predicción, se incluye el argumento type = "response"
predict(reglog, newdata, type="response")
```

## Tabla resumen de los datos preprocesados y represntación en forma de boxplots

```{r chumck29}
# Tabla resumen de las principales variables fisicoquímicas del conjunto de datos
summary(winequality.red)
```

```{r chumck30}
boxplot(winequality.red$acidity,main="Box plot of acidity", col="gray",ylab="values")
boxplot(winequality.red$citric.acid,main="Box plot of citric acid", col="gray",ylab="values")
boxplot(winequality.red$residual.sugar,main="Box plot of residual sugar", col="gray",ylab="values")
boxplot(winequality.red$chlorides,main="Box plot of chlorides", col="gray",ylab="values")
boxplot(winequality.red$total.sulfur.dioxide,main="Box plot of total sulfur dioxide", col="gray",ylab="values")
boxplot(winequality.red$density,main="Box plot of density", col="gray",ylab="values")
boxplot(winequality.red$pH,main="Box plot of pH", col="gray",ylab="values")
boxplot(winequality.red$sulphates,main="Box plot of sulphates", col="gray",ylab="values")
boxplot(winequality.red$alcohol,main="Box plot of alcohol", col="gray",ylab="values")
```

# Referencias

  Squire, Megan (2015). Clean Data. Packt Publishing Ltd.
  
  Jiawei Han, Micheine Kamber, Jian Pei (2012). Data mining: concepts and techniques. Morgan
  Kaufmann.
  
  Jason W. Osborne (2010). Data Cleaning Basics: Best Practices in Dealing with Extreme Scores.
  Newborn and Infant Nursing Reviews; 10 (1): pp. 1527-3369.
  
  Peter Dalgaard (2008). Introductory statistics with R. Springer Science & Business Media.
  
  Wes McKinney (2012). Python for Data Analysis. O'Reilley Media, Inc.
  
  Tutorial de Github (https://guides.github.com/activities/hello-world/)
  
  



