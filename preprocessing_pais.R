# Se instalan los paquetes a utilizar
# install.packages(c("tidyverse", "readxl", "dplyr", "corrplot", "Amelia", "pscl", "Hmisc", "funModeling", "ggplot2", "ggtext", "ggrepel", "caret", "mlr", "stringr", "RColorBrewer", "texreg", "mltools", "keras", "fastDummies", "MLmetrics", "neuralnet", "yardstick","PCAmixdata", "party", "rpart", "rpart.plot", "RSNNS" , "randomForest", "vip", "nloptr"))

# Se llaman las librerias a utilizar 
library(tidyverse)
library(readxl)
library(dplyr)
library(corrplot)
library(Amelia)
library(pscl)
library(Hmisc)
library(funModeling)
library(ggplot2)
library(ggrepel)
library(caret)
#library(mlr)
library(stringr)
library(RColorBrewer)
library(mltools)

# Se cargan los datasets
pais <- read_excel("/Users/gastonbernheim/Desktop/Facultad/Master en Big Data/11. Tesis/Padronario por Departamento/Datasets/dataset_vfinal.xlsx",
                   sheet = 'Pais')
poblacion_2011 <- read_excel("/Users/gastonbernheim/Desktop/Facultad/Master en Big Data/11. Tesis/Padronario por Departamento/Datasets/opp_observatorio.xlsx",
                             sheet = 'poblacion')
superficie_2011 <- read_excel("/Users/gastonbernheim/Desktop/Facultad/Master en Big Data/11. Tesis/Padronario por Departamento/Datasets/opp_observatorio.xlsx", 
                              sheet = 'superficie')
ingresos <- read_excel("/Users/gastonbernheim/Desktop/Facultad/Master en Big Data/11. Tesis/Padronario por Departamento/Datasets/opp_observatorio.xlsx",
                       sheet = 'ingresos_medios_mensual')
frigorificos <- read_excel("/Users/gastonbernheim/Desktop/Facultad/Master en Big Data/11. Tesis/Padronario por Departamento/Datasets/frigorificos.xlsx",
                           sheet = 'cantidad_por_depto')

# Veo la estructura de los datasets cargados
summary(pais)
summary(poblacion_2011)
summary(superficie_2011)
summary(ingresos)

# Dataset Pais: Se hacen las transformaciones necesarias en los datasets
pais$infractor <- as.numeric(pais$infractor)
pais$localidad <- as.factor(pais$localidad)
pais$departamento <- as.factor(pais$departamento)
pais$independiente <- as.numeric(pais$independiente)
pais$supermercado <- as.numeric(pais$supermercado)
pais$polleria <- as.numeric(pais$polleria)
pais$corte <- as.numeric(pais$corte)
pais$expendio <- as.numeric(pais$expendio)
pais$vende_no_carnicos <- as.numeric(pais$vende_no_carnicos)
pais$vende_chacinados <- as.numeric(pais$vende_chacinados)
pais$elabora_prod <- as.numeric(pais$elabora_prod)
pais$hab_sec_elab <- as.numeric(pais$hab_sec_elab)
pais$realiza_coccion <- as.numeric(pais$realiza_coccion)
pais$compras_avg <- as.numeric(pais$compras_avg)
pais$ult_visita <- as.Date(as.numeric(pais$ult_visita), origin = "1899-12-30")
pais$dias_desde_30_7 <- as.integer(pais$dias_desde_30_7)

# Genero la columna tipo_carniceria y modalidad_carniceria como factores
pais <- pais %>% 
  mutate(tipo_carniceria = case_when(independiente == 1 ~ "Independiente", #condition 1
                                      supermercado == 1 ~ "Supermercado", #condition 2
                                      TRUE ~ "Polleria")) #all others
pais <- pais %>% 
  mutate(modalidad_carniceria = case_when(corte == 1 ~ "Corte", #condition 1
                                       TRUE ~ "Expendio")) #all others
# Como factores
pais$tipo_carniceria <- as.factor(pais$tipo_carniceria)
pais$modalidad_carniceria <- as.factor(pais$modalidad_carniceria)

# Fijamos la base en Independiente y Corte
pais$tipo_carniceria <- relevel(pais$tipo_carniceria, "Independiente")
pais$modalidad_carniceria <- relevel(pais$modalidad_carniceria, "Corte")

# Creamos una columna que contenga las combinaciones de tipo_carniceria y modalidad_carniceria
pais$tipo_mod_carniceria <- str_c(pais$tipo_carniceria, " ", pais$modalidad_carniceria)

# Fijamos la base en Independiente Corte
pais$tipo_mod_carniceria <- as.factor(pais$tipo_mod_carniceria)
pais$tipo_mod_carniceria <- relevel(pais$tipo_mod_carniceria, "Independiente Corte")

# Fijamos la base de departamento en Canelones
pais$departamento <- relevel(pais$departamento, "Canelones")

# Revisamos cuantos NAs hay en cada variable
colSums(is.na(pais))
missmap(pais, main = "Valores faltantes y observados", margins = c(7,4))

# Revisamos la cantidad y porcentaje de NAs en el dataset
df_status(pais, print_results = F) %>% select(variable, q_na, p_na) %>% arrange(-q_na)

# Se asigna el valor No_vende a los NAs en las variables vende_no_carnicos y vende_chacinados
pais <- pais %>% 
  mutate(vende_no_carnicos = case_when(vende_no_carnicos == 1 ~ 1, #condition 1
                                          TRUE ~ 0)) #all others
pais <- pais %>% 
  mutate(vende_chacinados = case_when(vende_chacinados == 1 ~ 1, #condition 1
                                       TRUE ~ 0)) #all others

# Revisamos la cantidad y porcentaje de NAs en el dataset
df_status(pais, print_results = F) %>% select(variable, q_na, p_na) %>% arrange(-q_na)

# Nos quedamos con las variables que tienen menos de 10% de NAs
pais <- pais[, -15]

# Eliminamos los NAs del dataset
pais <- na.omit(pais)

# Corroboramos que no existan establecimientos del tipo Polleria Expendio
pais[pais$tipo_mod_carniceria == "Polleria Expendio", ]
pais <- subset(pais, tipo_mod_carniceria!="Polleria Expendio")

# Estructura final del dataset
str(pais)

# Tabla a nivel de departamento
deptos = NULL
for (i in unique(pais$departamento)) {
  deptos = rbind(deptos, c(i, length(pais$infractor[pais$departamento == i]), 
                           sum(pais$infractor[pais$departamento == i])))
}
deptos <- data_frame(departamento = deptos[, 1], total_carnicerias = as.integer(deptos[, 2]), 
                     total_infractores = as.integer(deptos[, 3]))

deptos$porcentaje_infractores <- deptos$total_infractores / deptos$total_carnicerias

# Se agregan las variables poblacion_2011, superficie_2011, ingresos y porcentaje_infractores
# al dataset pais
pais <- merge(pais, poblacion_2011, by = "departamento")
pais <- merge(pais, superficie_2011, by = "departamento")
pais <- merge(pais, ingresos, by = "departamento")
pais <- merge(pais, frigorificos, by = "departamento")
pais <- merge(pais, deptos[ , c("departamento", "porcentaje_infractores")], by = "departamento")
