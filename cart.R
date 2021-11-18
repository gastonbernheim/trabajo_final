source("preprocessing_pais.R")

library(rpart)
library(rpart.plot)

########## COPIA DEL DATASET PAIS Y TRANSFORMACIÓN DE VARIABLES ##########
pais_tree <- data_frame(pais)

pais_tree <- pais_tree %>% 
  mutate(infractor = case_when(infractor == 1 ~ "Infractor", #condition 1
                                TRUE ~ "No Infractor")) #all others
pais_tree <- pais_tree %>% 
  mutate(vende_no_carnicos = case_when(vende_no_carnicos == 1 ~ "Vende", #condition 1
                                       TRUE ~ "No_vende")) #all others
pais_tree <- pais_tree %>% 
  mutate(vende_chacinados = case_when(vende_chacinados == 1 ~ "Vende", #condition 1
                                      TRUE ~ "No_vende")) #all others
pais_tree <- pais_tree %>% 
  mutate(elabora_prod = case_when(elabora_prod == 1 ~ "Elabora", #condition 1
                                TRUE ~ "No elabora")) #all others
pais_tree <- pais_tree %>% 
  mutate(hab_sec_elab = case_when(hab_sec_elab == 1 ~ "Habilitado", #condition 1
                                TRUE ~ "No habilitado")) #all others
pais_tree <- pais_tree %>% 
  mutate(realiza_coccion = case_when(realiza_coccion == 1 ~ "Cocina", #condition 1
                                TRUE ~ "No cocina")) #all others

# Creamos la columna elaboracion que contenga las combinaciones de hab_sec_elab y elabora_prod
pais_tree$elaboracion <- str_c(pais_tree$hab_sec_elab, " ", pais_tree$elabora_prod)

# Variables como factor
pais_tree$infractor <- as.factor(pais_tree$infractor)
pais_tree$vende_no_carnicos <- as.factor(pais_tree$vende_no_carnicos)
pais_tree$vende_chacinados <- as.factor(pais_tree$vende_chacinados)
pais_tree$elaboracion <- as.factor(pais_tree$elaboracion)
pais_tree$realiza_coccion <- as.factor(pais_tree$realiza_coccion)

# Modificamos el orden de las variables en el dataset
pais_tree <- pais_tree %>% 
  relocate(infractor, .before = departamento)

pais_tree <- pais_tree %>% 
  relocate(tipo_mod_carniceria, .after = localidad)

pais_tree <- pais_tree %>% 
  relocate(vende_no_carnicos, .after = tipo_mod_carniceria)

pais_tree <- pais_tree %>% 
  relocate(vende_chacinados, .after = vende_no_carnicos)

pais_tree <- pais_tree %>% 
  relocate(elaboracion, .after = vende_chacinados)

pais_tree <- pais_tree %>% 
  relocate(realiza_coccion, .after = elaboracion)

pais_tree <- pais_tree %>% 
  relocate(departamento, .after = realiza_coccion)

# Eliminamos las variables que no se utilizaran del dataset
pais_tree <- pais_tree[, -c(2:3, 10:20, 25)]

########## SE GENERA UNA MUESTRA DE TRAIN Y TEST ############ 
set.seed(125)
training.samples <- pais_tree$infractor %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- pais_tree[training.samples, ]
test.data <- pais_tree[-training.samples, ]

# Cuantos infractores por dataset
sum(train.data$infractor == "Infractor") / nrow(train.data)
sum(test.data$infractor == "Infractor") / nrow(test.data)

########## ARBOL CON TODAS LAS VARIABLES ##########
# Arbol de decision
# rpart por defecto hace cross validation
class_tree <- rpart(infractor ~ .,
                    data = train.data,
                    method = "class")
summary(class_tree)
rpart.plot(class_tree)
# El primer valor es la categoria predicha (Infractor / No Infractor)
# El segundo valor es la probabilidad estimada
# El tercer valor es el porcentaje de observaciones en el nodo
print(class_tree$variable.importance)
# Veo la importancia de as variables
barplot(class_tree$variable.importance, horiz = T, las=1)


# Prediccion en train.data
class_tree_pred = predict(class_tree, train.data, type = "class")

# Matriz de Confusion para train.data
table(predicted = class_tree_pred, actual = train.data$infractor)

# Funcion Accuracy
accuracy = function(actual, predicted) {
  mean(actual == predicted)
}

# Accuracy en train.data
accuracy(predicted = class_tree_pred, actual = train.data$infractor)

# Prediccion en test.data
class_tree_pred = predict(class_tree, test.data, type = "class")

# Matriz de Confusion para test.data
table(predicted = class_tree_pred, actual = test.data$infractor)

# Accuracy en test.data
accuracy(predicted = class_tree_pred, actual = test.data$infractor)

# El cp es el parametro de complejidad
plotcp(class_tree)

# Print de los cp
printcp(class_tree)

library(vip)

# Busco el mejor valor de cp
min_cp = class_tree$cptable[which.min(class_tree$cptable[,"xerror"]),"CP"]
min_cp

# Podo el arbol en el mejor valor de cp
# prunce tree using best cp
class_tree_prune = prune(class_tree, cp = min_cp)
rpart.plot(class_tree_prune)
