# Se instalan los paquetes a utilizar
#install.packages(c("tidyverse", "readxl", "dplyr", "Amelia", "pscl", "Hmisc", "funModeling", "ggplot2", "caret", "mlr", "keras))

# Se llaman las librerias a utilizar 
library(tidyverse)
library(readxl)
library(dplyr)
library(corrplot)
library(Amelia)
library(pscl)
library(dplyr)
library(Hmisc)
library(funModeling)
library(ggplot2)
library(ggrepel)
library(caret)
library(mlr)

# Se cargan los datasets
pais <- read_excel('/Users/gastonbernheim/Desktop/Facultad/Master en Big Data/11. Tesis/Padronario por Departamento/Datasets/dataset_v2_sin_duplicados.xlsx', sheet = 'Pais')
poblacion_2011 <- read_excel('/Users/gastonbernheim/Desktop/Facultad/Master en Big Data/11. Tesis/Padronario por Departamento/Datasets/opp_observatorio.xlsx', sheet = 'poblacion')
superficie_2011 <- read_excel('/Users/gastonbernheim/Desktop/Facultad/Master en Big Data/11. Tesis/Padronario por Departamento/Datasets/opp_observatorio.xlsx', sheet = 'superficie')
ingresos <- read_excel('/Users/gastonbernheim/Desktop/Facultad/Master en Big Data/11. Tesis/Padronario por Departamento/Datasets/opp_observatorio.xlsx', sheet = 'ingresos_medios_mensual')

# Veo la estructura de los datasets cargados
#str(pais)
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
pais$ult_visita <- as.Date(pais$ult_visita, format = '%d/%m/%Y')
pais$dias_desde_30_7 <- as.integer(pais$dias_desde_30_7)

# Genero la columna tipo_carniceria_1 y tipo_carniceria_2 como factores con: mutate + case_when
pais <- pais %>% 
  mutate(tipo_carniceria_1 = case_when(independiente == 1 ~ "Independiente", #condition 1
                                      supermercado == 1 ~ "Supermercado", #condition 2
                                      TRUE ~ "Polleria")) #all others
pais <- pais %>% 
  mutate(tipo_carniceria_2 = case_when(corte == 1 ~ "Corte", #condition 1
                                       TRUE ~ "Expendio")) #all others
# Como factores
pais$tipo_carniceria_1 <- as.factor(pais$tipo_carniceria_1)
pais$tipo_carniceria_2 <- as.factor(pais$tipo_carniceria_2)

# Fijamos la base en Independiente y Corte
pais$tipo_carniceria_1 <- relevel(pais$tipo_carniceria_1, "Independiente")
pais$tipo_carniceria_2 <- relevel(pais$tipo_carniceria_2, "Corte")

# Revisamos cuantos NAs hay en cada variable
colSums(is.na(pais))
missmap(pais, main = "Missing values vs observed")

# Revisamos la cantidad y porcentaje de NAs en el dataset
df_status(pais, print_results = F) %>% select(variable, q_na, p_na) %>% arrange(-q_na)

# Nos quedamos con las variables que tienen menos de 10% de NAs
pais <- pais[c(-9, -10, -14, -15)]

# Eliminamos los NAs del dataset
pais <- na.omit(pais)

# Estructura final del dataset
str(pais)

# Tabla con numero de infractores por departamento
inf_p_depto = NULL
for (i in unique(pais$departamento)) {
  inf_p_depto = rbind(inf_p_depto, c(i, sum(pais$infractor[pais$departamento == i])))
}
inf_p_depto <- data_frame(departamento = inf_p_depto[, 1], total_infractores = as.integer(inf_p_depto[, 2]))

# Grafico que muestre infracciones por departamento 
infractores_p_depto <- ggplot(inf_p_depto, aes(x = departamento, y = total_infractores)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label = total_infractores), hjust = -0.15, size = 3, position = position_dodge(width = 1)) +
  coord_flip()
infractores_p_depto

# Se crea la variable perc_inf_p_depto como la proporcion de infractores por departamento
perc_inf_p_depto = NULL
for (i in unique(pais$departamento)) {
  perc_inf_p_depto = rbind(perc_inf_p_depto, c(i, (sum(pais$infractor[pais$departamento == i]) / length(pais$infractor[pais$departamento == i]))))
}
perc_inf_p_depto <- data_frame(departamento = perc_inf_p_depto[, 1], porcentaje_infractores = as.double(perc_inf_p_depto[, 2]))

# Se agregan las variables poblacion_2011, superficie_2011, ingresos y perc_inf_p_depto al dataset pais
pais <- merge(pais, poblacion_2011, by = "departamento")
pais <- merge(pais, superficie_2011, by = "departamento")
pais <- merge(pais, ingresos, by = "departamento")
pais <- merge(pais, perc_inf_p_depto, by = "departamento")
