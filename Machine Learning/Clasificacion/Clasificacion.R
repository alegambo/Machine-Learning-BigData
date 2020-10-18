# Datos epidemiologicos
# tomados entre miembros de un determinado grupo de América del Norte.
# Esta población se caracteriza por una alta predisposición a desarrollar diabetes.
# En este estudio se evaluaron diferentes parámetros fisiológicos y bioquímicos en mujeres, 
# y se utilizaron como posibles predictores de aparición de diabetes en los cinco años que 
# siguieron a los análisis.
# Siguiendo el criterio de la OMS, se diagnosticó diabetes si la concentración de glucosa
# en plasma es mayor a 200 mg/dl a las dos horas de un test de tolerancia a la glucosa. 
# En este dataset la mayor concentración de glucosa registrada fue 199 mg/dl



# npreg: número de embarazos
# glucose: concentración de glucosa en plasma a las dos horas de un test de tolerancia a la glucosa (mg/dl)
# diastolic: presión sanguínea diastólica (mm Hg)
# triceps: espesor de la piel en el pliegue del triceps
# insulin: niveles de insulina en suero (micro U / ml)
# bmi: Indice de Masa Corporal (peso en kg / altura en m^2)
# pedi: función de pedigree de diabetes
# Age: edad en años
# test: diagnóstico de diabetes (0: negativo; 1: positivo)

#-----------------------------------------------------------------------------------------------------
# Eliminar variables de otros proyectos.
#-----------------------------------------------------------------------------------------------------

rm(list = ls())

options(scipen=999)

#-----------------------------------------------------------------------------------------------------
# Función para carga de librerías.
#-----------------------------------------------------------------------------------------------------

Packages_FN <- function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only = TRUE)){
    install.packages(pkgs = x, dependencies = TRUE)
    library(x,character.only = TRUE)
  }
}

#install.packages('xxxx', dependencies = TRUE)

#-----------------------------------------------------------------------------------------------------
# Carga de Librerías
#-----------------------------------------------------------------------------------------------------

Packages_FN(e1071)
Packages_FN(tidyverse)
Packages_FN(GGally)
Packages_FN(caTools)
Packages_FN(viridis)
Packages_FN(hrbrthemes)
Packages_FN("caret")
Packages_FN(ggpubr)
Packages_FN(reshape2)
Packages_FN(ggthemes)
Packages_FN(ISLR)
Packages_FN(kknn)
Packages_FN(ggplot2)
Packages_FN(psych)


# Corregir Outliers.
AjustarOutliers_FN <- function(x, removeNA = TRUE){
  qrts <- quantile(x, probs = c(0.25, 0.75), na.rm = removeNA)
  caps <- quantile(x, probs = c(.05, .95), na.rm = removeNA)
  iqr  <- qrts[2]-qrts[1]
  h    <- 1.5 * iqr
  x[x<qrts[1]-h] <- caps[1]
  x[x>qrts[2]+h] <- caps[2]
  x
}

# Cargar archivos.
CargarArchivo_FN <- function (x) {
  read.csv(x,
           header = TRUE,
           sep = ',',
           dec=".",
           stringsAsFactors = FALSE,
           na="NA")
}

# Generar gráficos boxplot.
VisualizarBoxPlot_FN <- function (Datos.Boxplot) {
  vCantidad <- 0
  for (i in 1:ncol(Datos.Boxplot)) {
    if (is.numeric(Datos.Boxplot[,i])) {
      vCantidad <- vCantidad + 1
    }
  }
  if (vCantidad %% 2) {
    vCuadricula <- matrix(1:(vCantidad+1), byrow = TRUE, nrow = 2)
  } else {
    vCuadricula <- matrix(1:(vCantidad), byrow = TRUE, nrow = 2)
  }
  layout(vCuadricula)
  par(mar=c(3,3,3,3))
  for (i in 1:ncol(Datos.Boxplot)) { 
    if (is.numeric(Datos.Boxplot[,i])) {
      boxplot(Datos.Boxplot[,i],
              main      = paste('Boxplot: ', names(Datos.Boxplot)[i]),
              xlab      = names(Datos.Boxplot)[i],
              ylab      = 'Cantidad de Observaciones',
              cex.main  = 1,
              col.main  = "Darkblue",
              font.main = 6)
    }
  }
  par(mfrow=c(1,1))
}

#-----------------------------------------------------------------------------------------------------
# Cargar los datos.
#-----------------------------------------------------------------------------------------------------

setwd('C:/Users/Alejandro/Documents/Curso Modelos Clasificacion/Proyecto')

Datos.Carga <- CargarArchivo_FN('diabetes.data.csv')

str(Datos.Carga)

summary(Datos.Carga)

#-----------------------------------------------------------------------------------------------------
# Explorar los datos.
#-----------------------------------------------------------------------------------------------------

str(Datos.Carga)

summary(Datos.Carga)


Datos.Analisis <- Datos.Carga



str(Datos.Analisis)
VisualizarBoxPlot_FN(Datos.Analisis)


#-----------------------------------------------------------------------------------------------------
# Se procede a ajustar outliers.
#-----------------------------------------------------------------------------------------------------

boxplot(Datos.Analisis$npreg, main = 'npreg: número de embarazos')
boxplot(Datos.Analisis$diastolic, main = 'presión sanguínea diastólica (mm Hg)')
boxplot(Datos.Analisis$triceps, main = 'espesor de la piel en el pliegue del triceps')
boxplot(Datos.Analisis$insulin, main = ' niveles de insulina en suero (micro U / ml)')
boxplot(Datos.Analisis$bmi, main = 'Indice de Masa Corporal (peso en kg / altura en m2)')
boxplot(Datos.Analisis$pedi, main = 'función de pedigree de diabetes')
boxplot(Datos.Analisis$age, main = 'Age: edad en años')

Datos.Analisis$npreg    <- AjustarOutliers_FN(Datos.Analisis$npreg)
Datos.Analisis$diastolic <- AjustarOutliers_FN(Datos.Analisis$diastolic)
Datos.Analisis$triceps <- AjustarOutliers_FN(Datos.Analisis$triceps)
Datos.Analisis$insulin <- AjustarOutliers_FN(Datos.Analisis$insulin)
Datos.Analisis$bmi <- AjustarOutliers_FN(Datos.Analisis$bmi)
Datos.Analisis$pedi <- AjustarOutliers_FN(Datos.Analisis$pedi)
Datos.Analisis$age <- AjustarOutliers_FN(Datos.Analisis$age)

VisualizarBoxPlot_FN(Datos.Analisis)


#---------------------------------------------------------------------------------------------------
# Correlación
#---------------------------------------------------------------------------------------------------
cor(Datos.Analisis)

pairs.panels(x = Datos.Analisis,
             ellipses = FALSE,
             lm = TRUE,
             method = "pearson")


Datos.Analisis$diabetes     <- factor(Datos.Analisis$diabetes,
                                      levels = c(0,1),
                                      labels = c('Negativo','Positivo'))
#---------------------------------------------------------------------------------------------------
# Crear conjuntos de datos de entrenamiento y de prueba.
#---------------------------------------------------------------------------------------------------

set.seed(1000)

Div.Observaciones   <- sample.split(Datos.Analisis$diabetes, SplitRatio = 0.7)

Datos.Entrenamiento <- Datos.Analisis[Div.Observaciones, ]

Datos.Prueba        <- Datos.Analisis[!Div.Observaciones,]

rm(Div.Observaciones)

#--------------------------------------------------------------------------------------------------
#
# Modelo NAIVE BAYES
#

#-----------------------------------------------------------------------------------------------------
# Crea probabilidades con los datos históricos y asigna predicción a los nuevos clientes
#-----------------------------------------------------------------------------------------------------

Datos.Modelo.Prob <- naiveBayes(diabetes ~., data = Datos.Entrenamiento) # Crea Probabilidades

Datos.Modelo.Pred <- predict(Datos.Modelo.Prob , Datos.Prueba[-9]) # Crea predicción

#-----------------------------------------------------------------------------------------------------
#
# Evaluación del Modelo.
#
#-----------------------------------------------------------------------------------------------------
# Matriz de confusión
#-----------------------------------------------------------------------------------------------------
# Exactitud:                Cantidad de predicciones positivas que fueron correctas
# Sensibilidad:             Tasa de Verdaderos positivos.
# Especificidad:            Tasa de Verdaderos negativos.
# Valor pred. Pos:          Tasa de predicción de valores positivos.
# Valor pred de Neg:        Tasa de predicción de valores negativos.
# Prevalencia:              Probabilidad de existencia de valores positivos en las muestras.  
#-----------------------------------------------------------------------------------------------------

MatrizConfusion <- table(Datos.Prueba$diabetes, Datos.Modelo.Pred, dnn = c("Actual", "Prediccion"))

MatrizConfusion


round(prop.table(MatrizConfusion) * 100, 2) # Proporciones con relación a todos los datos.

round(prop.table(MatrizConfusion, 1) * 100, 2) # Proporciones Redondeadas, Lo hace por filas.


round(prop.table(MatrizConfusion, 2) * 100, 2) # Proporciones Redondeadas, Lo hace por columnas.


# Librería reshape2 para utilizar melt
x <- melt(MatrizConfusion)

ggplot(x, aes(Actual, Prediccion)) +
  geom_point(aes(size = value), alpha = 0.8, color="darkgreen", show.legend = FALSE) +
  geom_text(aes(label = value), color="white") +
  scale_size(range = c(15,50)) +
  theme_bw()

# Librería caret.
confusionMatrix(MatrizConfusion)

#--------------------------------------------------------------------------------------------------------
# VP: cantidad de positivos clasificados correctamente como positivos (verdaderos positivos).
# VN: cantidad de negativos clasificados correctamente como negativos (verdaderos negativos).
# FN: cantidad de positivos clasificados incorrectamente como negativos. Error tipo 2 (Falsos Negativos).
# FP: cantidad de negativos clasificados incorrectamente como positivos. Error tipo 1 (Falsos positivos).
#
#  +----------+----------+----------+
#  |          | Negativo | Positivo |
#  +----------+----------+----------+
#  | Negativo |    VN    |    FP    |
#  +----------+----------+----------+
#  | Positivo |    FN    |    VP    |
#  +----------+----------+----------+
#
#--------------------------------------------------------------------------------------------------------

VN <- MatrizConfusion[1,1]
VP <- MatrizConfusion[2,2]
FN <- MatrizConfusion[2,1]
FP <- MatrizConfusion[1,2]

#--------------------------------------------------------------------------------------------------------
# Precisión Global (Exactitud) (P): Porcentaje de aciertos para las clases positivas y negativas,
# también conocido como acierto global.
#--------------------------------------------------------------------------------------------------------

P = ( VN + VP ) / ( VN + FP + FN + VP )

#--------------------------------------------------------------------------------------------------------
# Tasa de Error: Porcentaje de error para clases positivas y negativas, conocido como error global.
#--------------------------------------------------------------------------------------------------------

Error <- 1 - P

#--------------------------------------------------------------------------------------------------------
# Precisión Positiva (Sensibilidad) (PP): como el modelo clasifica los casos positivos como positivos.
#--------------------------------------------------------------------------------------------------------

PP = VP / ( FN + VP )

#--------------------------------------------------------------------------------------------------------
# Precisión Negativa (Especifidad) (PN): como el modelo clasifica los casos negativos como negativos.
#--------------------------------------------------------------------------------------------------------

PN = VN / ( VN + FP )

#--------------------------------------------------------------------------------------------------------
# Falsos Positivos (PFP): observaciones determinas como positivas pero que son negativas.
#--------------------------------------------------------------------------------------------------------

PFP = FP / ( VN + FP )

#--------------------------------------------------------------------------------------------------------
# Falsos Negativos (PFN): Porcentaje de casos positivos clasificados incorrectamente.
#--------------------------------------------------------------------------------------------------------

PFN = FN / ( FN + VP )

#--------------------------------------------------------------------------------------------------------
# Asertividad Positiva (AP): total de observaciones positivas determinadas como tales divididas por la
# suma entre los Verdaderos Positivos y los Falsos Positivos.
#--------------------------------------------------------------------------------------------------------

AP = VP / ( FP + VP )

#--------------------------------------------------------------------------------------------------------
# Asertividad Negativa (AN): total de observaciones negativas determinadas como tales divididas por la
# suma entre los Falsos Negativos y Verdaderos Negativos.
#--------------------------------------------------------------------------------------------------------

AN = VN / ( VN + FN )

#--------------------------------------------------------------------------------------------------------
# Resultados de las métricas.
#--------------------------------------------------------------------------------------------------------

round(data.frame(P, PP, PN, PFP, PFN, AP, AN, Error) * 100, 2)

# Resultados de las métricas.

Modelo.NV <- round(data.frame(P, PP, PN, PFP, PFN, AP, AN, Error) * 100, 2)

Modelo.NV

#-----------------------------------------------------------------------------------------------------
#
# # Clasificación con k-Nearest Neighbors (kNN).
#
#-----------------------------------------------------------------------------------------------------
# Crea probabilidades con los datos históricos y asigna predicción a los nuevos clientes
#-----------------------------------------------------------------------------------------------------
# El modelo se contruye con los datos de entrenamiento y el valor máximo optimo de K. Es importante
# tener en cuenta que el modelo debe calibrarse para obtener el mejor resultado.
#-----------------------------------------------------------------------------------------------------

train.kknn(diabetes ~ ., data = Datos.Entrenamiento, kmax = 50)

Datos.Modelo.KNN <- train.kknn(diabetes ~ ., data = Datos.Entrenamiento, kmax = 7)

Datos.Modelo.KMM.Prediccion <- predict(Datos.Modelo.KNN, Datos.Prueba[, -c(9)])

#-----------------------------------------------------------------------------------------------------
# Evaluación del Modelo.
#-----------------------------------------------------------------------------------------------------

MatrizConfusion <- table(Datos.Prueba$diabetes, Datos.Modelo.KMM.Prediccion,
                         dnn = c("Actual", "Prediccion"))

MatrizConfusion

round(prop.table(MatrizConfusion) * 100, 2) # Proporciones con relación a todos los datos.

round(prop.table(MatrizConfusion, 1) * 100, 2) # Proporciones Redondeadas, Lo hace por filas.

round(prop.table(MatrizConfusion, 2) * 100, 2) # Proporciones Redondeadas, Lo hace por columnas.

# Librería reshape2 para utilizar melt
x <- melt(MatrizConfusion)

ggplot(x, aes(Actual, Prediccion)) +
  geom_point(aes(size = value), alpha = 0.8, color="darkgreen", show.legend = FALSE) +
  geom_text(aes(label = value), color="white") +
  scale_size(range = c(15,50)) +
  theme_bw()

VN <- MatrizConfusion[1,1]
VP <- MatrizConfusion[2,2]
FN <- MatrizConfusion[2,1]
FP <- MatrizConfusion[1,2]

# Precisión Global (Exactitud)
P = ( VN + VP ) / ( VN + FP + FN + VP )

# Tasa de Error.
Error <- 1 - P

# Precisión Positiva (Sensibilidad) (PP)
PP = VP / ( FN + VP )

# Precisión Negativa (Especifidad) (PN)
PN = VN / ( VN + FP )

# Falsos Positivos (PFP)
PFP = FP / ( VN + FP )

# Falsos Negativos (PFN)
PFN = FN / ( FN + VP )

# Asertividad Positiva (AP)
AP = VP / ( FP + VP )

# Asertividad Negativa (AN)
AN = VN / ( VN + FN )

# Resultados de las métricas.

Modelo.kNN <- round(data.frame(P, PP, PN, PFP, PFN, AP, AN, Error) * 100, 2)

rbind(NV = Modelo.NV, kNN = Modelo.kNN)

#-----------------------------------------------------------------------------------------------------
#
# Clasificación Logística.
#
#-----------------------------------------------------------------------------------------------------
# Crea probabilidades con los datos históricos y asigna predicción a los nuevos clientes
#-----------------------------------------------------------------------------------------------------

Datos.Modelo.logit <- glm(diabetes ~ ., data = Datos.Entrenamiento, family = 'binomial')

# Predicción de los nuevos puntos según el modelo. La función predict() calcula la probabilidad de
# que la variable respuesta pertenezca al nivel de referencia (en este caso "Si")
Datos.Modelo.logit.Prob   <- predict(Datos.Modelo.logit, Datos.Prueba[, -c(9)])

Datos.Modelo.logit.Prediccion <- rep("Positivo", length(Datos.Modelo.logit.Prob))

Datos.Modelo.logit.Prediccion[Datos.Modelo.logit.Prob > 0.5] <- 'Negativo'

#-----------------------------------------------------------------------------------------------------
# Evaluación del Modelo.
#-----------------------------------------------------------------------------------------------------

MatrizConfusion <- table(Datos.Prueba$diabetes, Datos.Modelo.logit.Prediccion, dnn = c("Actual", "Prediccion"))

MatrizConfusion

round(prop.table(MatrizConfusion) * 100, 2) # Proporciones con relación a todos los datos.

round(prop.table(MatrizConfusion, 1) * 100, 2) # Proporciones Redondeadas, Lo hace por filas.

round(prop.table(MatrizConfusion, 2) * 100, 2) # Proporciones Redondeadas, Lo hace por columnas.

# Librería reshape2 para utilizar melt
x <- melt(MatrizConfusion)

ggplot(x, aes(Actual, Prediccion)) +
  geom_point(aes(size = value), alpha = 0.8, color="darkgreen", show.legend = FALSE) +
  geom_text(aes(label = value), color="white") +
  scale_size(range = c(15,50)) +
  theme_bw()

VN <- MatrizConfusion[1,1]
VP <- MatrizConfusion[2,2]
FN <- MatrizConfusion[2,1]
FP <- MatrizConfusion[1,2]

# Precisión Global (Exactitud)
P = ( VN + VP ) / ( VN + FP + FN + VP )

# Tasa de Error.
Error <- 1 - P

# Precisión Positiva (Sensibilidad) (PP)
PP = VP / ( FN + VP )

# Precisión Negativa (Especifidad) (PN)
PN = VN / ( VN + FP )

# Falsos Positivos (PFP)
PFP = FP / ( VN + FP )

# Falsos Negativos (PFN)
PFN = FN / ( FN + VP )

# Asertividad Positiva (AP)
AP = VP / ( FP + VP )

# Asertividad Negativa (AN)
AN = VN / ( VN + FN )

# Resultados de las métricas.

Modelo.lg <- round(data.frame(P, PP, PN, PFP, PFN, AP, AN, Error) * 100, 2)

rbind(NV = Modelo.NV, kNN = Modelo.kNN, LOG = Modelo.lg)

