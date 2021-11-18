library(pROC)
library(fastDummies)
library(keras)

install_keras()

source("preprocessing_pais.R")

str(pais)
df_status(pais, print_results = F) %>% 
  select(variable, q_zeros, p_zeros, q_na) %>% 
  arrange(-q_zeros)

########## COPIA DEL DATASET PAIS Y TRANSFORMACIÓN DE VARIABLES ##########
pais_ann <- data_frame(pais)

#pais_ann$infractor <- as.integer(pais_ann$infractor)
pais_ann$realiza_coccion <- as.integer(pais_ann$realiza_coccion)
pais_ann$vende_no_carnicos <- as.integer(pais_ann$vende_no_carnicos)
pais_ann$vende_chacinados <- as.integer(pais_ann$vende_chacinados)

pais_ann <- pais_ann %>% 
  mutate(elabora_prod = case_when(elabora_prod == 1 ~ "Elabora", #condition 1
                                  TRUE ~ "No elabora")) #all others

pais_ann <- pais_ann %>% 
  mutate(hab_sec_elab = case_when(hab_sec_elab == 1 ~ "Hab", #condition 1
                                  TRUE ~ "No hab")) #all others

# Creamos la columna elaboracion que contenga las combinaciones de hab_sec_elab y elabora_prod
pais_ann$elaboracion <- str_c(pais_ann$hab_sec_elab, " ", pais_ann$elabora_prod)

# Elaboracion como factor
pais_ann$elaboracion <- as.factor(pais_ann$elaboracion)

# Creamos variables dummies por departamento y por tipo_mod_carniceria
pais_ann <- fastDummies::dummy_cols(pais_ann, select_columns = "tipo_mod_carniceria")
pais_ann <- fastDummies::dummy_cols(pais_ann, select_columns = "elaboracion")
pais_ann <- fastDummies::dummy_cols(pais_ann, select_columns = "departamento")

# Estandarizamos las variables numericas (Normalizacion Min-Max)
pais_ann$poblacion <- (pais_ann$poblacion - min(pais_ann$poblacion)) /
  (max(pais_ann$poblacion) - min(pais_ann$poblacion))

pais_ann$superficie_km2 <- (pais_ann$superficie_km2 - min(pais_ann$superficie_km2)) / 
  (max(pais_ann$superficie_km2) - min(pais_ann$superficie_km2))

pais_ann$ing_p_capita_mes_cte_2005 <- (pais_ann$ing_p_capita_mes_cte_2005 - min(pais_ann$ing_p_capita_mes_cte_2005)) /
  (max(pais_ann$ing_p_capita_mes_cte_2005) - min(pais_ann$ing_p_capita_mes_cte_2005))

# Eliminamos las variables que no se utilizaran del dataset
pais_ann <- pais_ann[, -c(1:2, 4:9, 12:13, 15:19, 23:25, 29)]

# Ordenamos las variables del dataset
pais_ann <- pais_ann %>% 
  relocate(`elaboracion_No hab No elabora`, .after = `departamento_Treinta y Tres`)

pais_ann <- pais_ann %>% 
  relocate(`elaboracion_No hab Elabora`, .after = `elaboracion_No hab No elabora`)

pais_ann <- pais_ann %>% 
  relocate(`elaboracion_Hab Elabora`, .after = `elaboracion_No hab Elabora`)

pais_ann <- pais_ann %>% 
  relocate(vende_no_carnicos, .after = `elaboracion_Hab Elabora`)

pais_ann <- pais_ann %>% 
  relocate(vende_chacinados, .after = vende_no_carnicos)

pais_ann <- pais_ann %>% 
  relocate(realiza_coccion, .after = vende_chacinados)

pais_ann <- pais_ann %>% 
  relocate(poblacion, .after = realiza_coccion)

pais_ann <- pais_ann %>% 
  relocate(superficie_km2, .after = poblacion)

pais_ann <- pais_ann %>% 
  relocate(ing_p_capita_mes_cte_2005, .after = superficie_km2)

########## SE GENERA UNA MUESTRA DE TRAIN Y TEST ############ 
set.seed(125)
training.samples <- pais_ann$infractor %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.data <- pais_ann[training.samples, ]
test.data <- pais_ann[-training.samples, ] 

# Que porcentaje de infractores hay en train y en test
sum(train.data$infractor == 1) / nrow(train.data)
sum(test.data$infractor == 1) / nrow(test.data)

x_train <- train.data[, -1] # Eliminamos la variable infractor
y_train <- train.data[, 1] # Nos quedamos solo con la variable a predecir --> infractor

x_test <- test.data[, -1]
y_test <- test.data[, 1]

str(x_train)
str(y_train)
  
########## ANN 1 OPTIMIZADOR SGD - VALIDATION SET APPROACH ########## 

# Initialize our model
ann1_sgd <- keras_model_sequential()

ann1_sgd %>%
  layer_flatten(input_shape = ncol(x_train)) %>% 
  layer_dense(units = 1, activation = "sigmoid")

summary(ann1_sgd)

ann1_sgd %>% compile(
  loss = "binary_crossentropy", #"mse", #"binary_crossentropy", #"categorical_crossentropy" 
  optimizer = "sgd", #"sgd", #"adam"
  metrics = c("accuracy")) #"mse", #accuracy"

trained_ann1_sgd <- ann1_sgd %>% fit(
  x = as.matrix(x_train),
  y = as.matrix(y_train),
  epochs = 100,
  validation_split = 0.3)

# Print del modelo
print(trained_ann1_sgd)

# Ploteamos el modelo
plot(trained_ann1_sgd)

# Evaluamos performance en train data
ann1_sgd %>% evaluate(x = as.matrix(x_train), y = as.matrix(y_train))

# Evaluamos performance en test data
ann1_sgd %>% evaluate(x = as.matrix(x_test), y = as.matrix(y_test))

# Vemos los pesos de la red
get_weights(ann1_sgd)

# Realizamos predicciones
pred_prob <- ann1_sgd %>% predict(as.matrix(x_test)) %>% as.vector() # Probabilidades predichas. Al sacar as.vector() queda una matriz
hist(pred_prob)
pred_int <- ann1_sgd %>% predict(as.matrix(x_test)) %>% `>`(0.3) %>% k_cast("int32") # 0.3 - Elijo a partir de que probabilidad se clasifica como infractor

# Guardamos resultados en forma de matrices
prediccion_metric <- as.matrix(pred_int)
y_test_metric <- as.matrix(y_test)

confusionMatrix(data = as.factor(prediccion_metric), 
                reference = as.factor(y_test_metric), 
                positive = "1", 
                mode = "prec_recall")

# El Matthews correlation index es útil cuando las observaciones entre clases varían mucho.
# Se lee igual que el coeficiene de correlación
mcc(preds = as.factor(prediccion_metric), actuals = as.factor(y_test_metric))

# Se genera un dataframe con los valores reales, predichos y probabilidades estimadas
real_pred <- data.frame(obs = as.factor(y_test_metric), 
                        pred = as.factor(prediccion_metric), 
                        Infractor = pred_prob, 
                        No_Infractor = 1 - pred_prob)

real_pred <- real_pred %>% 
  mutate(obs = case_when(obs == 1 ~ "Infractor", #condition 1
                         TRUE ~ "No Infractor")) #all others

real_pred$obs <- real_pred$obs %>%
  as.factor() %>%
  relevel("Infractor")

real_pred <- real_pred %>% 
  mutate(pred = case_when(pred == 1 ~ "Infractor", #condition 1
                          TRUE ~ "No Infractor")) #all others

real_pred$pred <- real_pred$pred %>%
  as.factor() %>%
  relevel("Infractor")

multiClassSummary(real_pred, lev = levels(real_pred$obs))

library(MLmetrics)
prSummary(real_pred, lev = levels(real_pred$obs))

########## ANN 1 OPTIMIZADOR ADAM - VALIDATION SET APPROACH ########## 

# Initialize our model
ann1_adam <- keras_model_sequential()

ann1_adam %>%
  layer_flatten(input_shape = ncol(x_train)) %>% 
  layer_dense(units = 1, activation = "sigmoid")

summary(ann1_adam)

ann1_adam %>% compile(
  loss = "binary_crossentropy", #"mse", #"binary_crossentropy", #"categorical_crossentropy" 
  optimizer = "adam", #"sgd", #"adam"
  metrics = c("accuracy")) #"mse", #accuracy"

trained_ann1_adam <- ann1_adam %>% fit(
  x = as.matrix(x_train),
  y = as.matrix(y_train),
  epochs = 100,
  validation_split = 0.3)

# Print del modelo
print(trained_ann1_adam)

# Ploteamos el modelo
plot(trained_ann1_adam)

# Evaluamos performance en train data
ann1_adam %>% evaluate(x = as.matrix(x_train), y = as.matrix(y_train))

# Evaluamos performance en test data
ann1_adam %>% evaluate(x = as.matrix(x_test), y = as.matrix(y_test))

# Vemos los pesos de la red
get_weights(ann1_adam)

# Realizamos predicciones
pred_prob <- ann1_adam %>% predict(as.matrix(x_test)) %>% as.vector() # Probabilidades predichas. Al sacar as.vector() queda una matriz
hist(pred_prob)
pred_int <- ann1_adam %>% predict(as.matrix(x_test)) %>% `>`(0.3) %>% k_cast("int32") # 0.3 - Elijo a partir de que probabilidad se clasifica como infractor

# Guardamos resultados en forma de matrices
prediccion_metric <- as.matrix(pred_int)
y_test_metric <- as.matrix(y_test)

confusionMatrix(data = as.factor(prediccion_metric), 
                reference = as.factor(y_test_metric), 
                positive = "1", 
                mode = "prec_recall")

# El Matthews correlation index es útil cuando las observaciones entre clases varían mucho.
# Se lee igual que el coeficiene de correlación
mcc(preds = as.factor(prediccion_metric), actuals = as.factor(y_test_metric))

# Se genera un dataframe con los valores reales, predichos y probabilidades estimadas
real_pred <- data.frame(obs = as.factor(y_test_metric), 
                        pred = as.factor(prediccion_metric), 
                        Infractor = pred_prob, 
                        No_Infractor = 1 - pred_prob)

real_pred <- real_pred %>% 
  mutate(obs = case_when(obs == 1 ~ "Infractor", #condition 1
                         TRUE ~ "No Infractor")) #all others

real_pred$obs <- real_pred$obs %>%
  as.factor() %>%
  relevel("Infractor")

real_pred <- real_pred %>% 
  mutate(pred = case_when(pred == 1 ~ "Infractor", #condition 1
                          TRUE ~ "No Infractor")) #all others

real_pred$pred <- real_pred$pred %>%
  as.factor() %>%
  relevel("Infractor")

multiClassSummary(real_pred, lev = levels(real_pred$obs))

library(MLmetrics)
prSummary(real_pred, lev = levels(real_pred$obs))

########## PERFORMANCE COMPARADA ANN 1 - VALIDATION SET APPROACH ##########

compare_ann1 <- data.frame(
  ann1_sgd_train = trained_ann1_sgd$metrics$loss,
  ann1_sdg_val = trained_ann1_sgd$metrics$val_loss,
  ann1_adam_train = trained_ann1_adam$metrics$loss,
  ann1_adam_val = trained_ann1_adam$metrics$val_loss
) %>%
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  gather(key = "type", value = "value", -rowname)

ggplot(compare_ann1, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("epoch") +
  ylab("loss & val_loss")

########## ANN 2 OPTIMIZADOR SGD - VALIDATION SET APPROACH ########## 

# Initialize our model
ann2_sgd <- keras_model_sequential()

ann2_sgd %>%
  layer_flatten(input_shape = ncol(x_train)) %>% 
  layer_dense(units = 100, activation = "relu") %>%
  layer_dense(units = 50, activation = "relu") %>%  
  #layer_dropout(rate = 0.1) %>%
  layer_dense(units = 5, activation = "relu") %>%  
  layer_dense(units = 1, activation = "sigmoid")

summary(ann2_sgd)

ann2_sgd %>% compile(
  loss = "binary_crossentropy", #"mse", #"binary_crossentropy", #"categorical_crossentropy" 
  optimizer = "sgd", #"sgd", #"adam"
  metrics = c("accuracy")) #"mse", #accuracy"

trained_ann2_sgd <- ann2_sgd %>% fit(
  x = as.matrix(x_train),
  y = as.matrix(y_train),
  epochs = 100,
  validation_split = 0.3)

# Print del modelo
print(trained_ann2_sgd)

# Ploteamos el modelo
plot(trained_ann2_sgd)

# Evaluamos performance en train data
ann2_sgd %>% evaluate(x = as.matrix(x_train), y = as.matrix(y_train))

# Evaluamos performance en test data
ann2_sgd %>% evaluate(x = as.matrix(x_test), y = as.matrix(y_test))

# Vemos los pesos de la red
get_weights(ann2_sgd)

# Realizamos predicciones
pred_prob <- ann2_sgd %>% predict(as.matrix(x_test)) %>% as.vector() # Probabilidades predichas. Si le saco el as.vector() me quedo con una matriz
hist(pred_prob)
pred_int <- ann2_sgd %>% predict(as.matrix(x_test)) %>% `>`(0.3) %>% k_cast("int32") # 0.3 - Elijo a partir de que probabilidad se clasifica como infractor

# Guardamos resultados en forma de matrices
prediccion_metric <- as.matrix(pred_int)
y_test_metric <- as.matrix(y_test)

confusionMatrix(data = as.factor(prediccion_metric), 
                reference = as.factor(y_test_metric), 
                positive = "1", 
                mode = "prec_recall")

# El Matthews correlation index es útil cuando las observaciones entre clases varían mucho.
# Se lee igual que el coeficiene de correlación
mcc(preds = as.factor(prediccion_metric), actuals = as.factor(y_test_metric))

# Se genera un dataframe con los valores reales, predichos y probabilidades estimadas
real_pred <- data.frame(obs = as.factor(y_test_metric), 
                        pred = as.factor(prediccion_metric), 
                        Infractor = pred_prob, 
                        No_Infractor = 1 - pred_prob)

real_pred <- real_pred %>% 
  mutate(obs = case_when(obs == 1 ~ "Infractor", #condition 1
                         TRUE ~ "No Infractor")) #all others

real_pred$obs <- real_pred$obs %>%
  as.factor() %>%
  relevel("Infractor")

real_pred <- real_pred %>% 
  mutate(pred = case_when(pred == 1 ~ "Infractor", #condition 1
                          TRUE ~ "No Infractor")) #all others

real_pred$pred <- real_pred$pred %>%
  as.factor() %>%
  relevel("Infractor")

multiClassSummary(real_pred, lev = levels(real_pred$obs))

library(MLmetrics)
prSummary(real_pred, lev = levels(real_pred$obs))

########## ANN 2 OPTIMIZADOR ADAM - VALIDATION SET APPROACH ########## 

# Initialize our model
ann2_adam <- keras_model_sequential()

ann2_adam %>%
  layer_flatten(input_shape = ncol(x_train)) %>% 
  layer_dense(units = 100, activation = "relu") %>%
  layer_dense(units = 50, activation = "relu") %>%  
  #layer_dropout(rate = 0.1) %>%
  layer_dense(units = 5, activation = "relu") %>%  
  layer_dense(units = 1, activation = "sigmoid")

summary(ann2_adam)

ann2_adam %>% compile(
  loss = "binary_crossentropy", #"mse", #"binary_crossentropy", #"categorical_crossentropy" 
  optimizer = "adam", #"sgd", #"adam"
  metrics = c("accuracy")) #"mse", #accuracy"

trained_ann2_adam <- ann2_adam %>% fit(
  x = as.matrix(x_train),
  y = as.matrix(y_train),
  epochs = 100,
  validation_split = 0.3)

# Print del modelo
print(trained_ann2_adam)

# Ploteamos el modelo
plot(trained_ann2_adam)

# Evaluamos performance en train data
ann2_adam %>% evaluate(x = as.matrix(x_train), y = as.matrix(y_train))

# Evaluamos performance en test data
ann2_adam %>% evaluate(x = as.matrix(x_test), y = as.matrix(y_test))

# Vemos los pesos de la red
get_weights(ann2_adam)

# Realizamos predicciones
pred_prob <- ann2_adam %>% predict(as.matrix(x_test)) %>% as.vector() # Probabilidades predichas. Si le saco el as.vector() me quedo con una matriz
hist(pred_prob)
pred_int <- ann2_adam %>% predict(as.matrix(x_test)) %>% `>`(0.3) %>% k_cast("int32") # 0.3 - Elijo a partir de que probabilidad se clasifica como infractor

# Guardamos resultados en forma de matrices
prediccion_metric <- as.matrix(pred_int)
y_test_metric <- as.matrix(y_test)

confusionMatrix(data = as.factor(prediccion_metric), 
                reference = as.factor(y_test_metric), 
                positive = "1", 
                mode = "prec_recall")

# El Matthews correlation index es útil cuando las observaciones entre clases varían mucho.
# Se lee igual que el coeficiene de correlación
mcc(preds = as.factor(prediccion_metric), actuals = as.factor(y_test_metric))

# Se genera un dataframe con los valores reales, predichos y probabilidades estimadas
real_pred <- data.frame(obs = as.factor(y_test_metric), 
                        pred = as.factor(prediccion_metric), 
                        Infractor = pred_prob, 
                        No_Infractor = 1 - pred_prob)

real_pred <- real_pred %>% 
  mutate(obs = case_when(obs == 1 ~ "Infractor", #condition 1
                         TRUE ~ "No Infractor")) #all others

real_pred$obs <- real_pred$obs %>%
  as.factor() %>%
  relevel("Infractor")

real_pred <- real_pred %>% 
  mutate(pred = case_when(pred == 1 ~ "Infractor", #condition 1
                          TRUE ~ "No Infractor")) #all others

real_pred$pred <- real_pred$pred %>%
  as.factor() %>%
  relevel("Infractor")

multiClassSummary(real_pred, lev = levels(real_pred$obs))

library(MLmetrics)
prSummary(real_pred, lev = levels(real_pred$obs))

########## PERFORMANCE COMPARADA ANN 2 - VALIDATION SET APPROACH ##########

compare_ann2 <- data.frame(
  ann2_sgd_train = trained_ann2_sgd$metrics$loss,
  ann2_sdg_val = trained_ann2_sgd$metrics$val_loss,
  ann2_adam_train = trained_ann2_adam$metrics$loss,
  ann2_adam_val = trained_ann2_adam$metrics$val_loss
) %>%
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  gather(key = "type", value = "value", -rowname)

ggplot(compare_ann2, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("epoch") +
  ylab("loss & val_loss")

########## ANN 3 OPTIMIZADOR SGD - VALIDATION SET APPROACH ########## 

# Initialize our model
ann3_sgd <- keras_model_sequential()

ann3_sgd %>%
  layer_flatten(input_shape = ncol(x_train)) %>% 
  layer_dense(units = 12, activation = "relu") %>%
  layer_dense(units = 6, activation = "relu") %>%  
  layer_dense(units = 1, activation = "sigmoid")

summary(ann3_sgd)

ann3_sgd %>% compile(
  loss = "binary_crossentropy", #"mse", #"binary_crossentropy", #"categorical_crossentropy" 
  optimizer = "sgd", #"sgd", #"adam"
  metrics = c("accuracy")) #"mse", #accuracy"

trained_ann3_sgd <- ann3_sgd %>% fit(
  x = as.matrix(x_train),
  y = as.matrix(y_train),
  epochs = 100,
  validation_split = 0.3)

# Print del modelo
print(trained_ann3_sgd)

# Ploteamos el modelo
plot(trained_ann3_sgd)

# Evaluamos performance en train data
ann3_sgd %>% evaluate(x = as.matrix(x_train), y = as.matrix(y_train))

# Evaluamos performance en test data
ann3_sgd %>% evaluate(x = as.matrix(x_test), y = as.matrix(y_test))

# Vemos los pesos de la red
get_weights(ann3_sgd)

# Realizamos predicciones
pred_prob <- ann3_sgd %>% predict(as.matrix(x_test)) %>% as.vector() # Probabilidades predichas. Si le saco el as.vector() me quedo con una matriz
hist(pred_prob)
pred_int <- ann3_sgd %>% predict(as.matrix(x_test)) %>% `>`(0.3) %>% k_cast("int32") # 0.3 - Elijo a partir de que probabilidad se clasifica como infractor

# Guardamos resultados en forma de matrices
prediccion_metric <- as.matrix(pred_int)
y_test_metric <- as.matrix(y_test)

confusionMatrix(data = as.factor(prediccion_metric), 
                reference = as.factor(y_test_metric), 
                positive = "1", 
                mode = "prec_recall")

# El Matthews correlation index es útil cuando las observaciones entre clases varían mucho.
# Se lee igual que el coeficiene de correlación
mcc(preds = as.factor(prediccion_metric), actuals = as.factor(y_test_metric))

# Se genera un dataframe con los valores reales, predichos y probabilidades estimadas
real_pred <- data.frame(obs = as.factor(y_test_metric), 
                        pred = as.factor(prediccion_metric), 
                        Infractor = pred_prob, 
                        No_Infractor = 1 - pred_prob)

real_pred <- real_pred %>% 
  mutate(obs = case_when(obs == 1 ~ "Infractor", #condition 1
                         TRUE ~ "No Infractor")) #all others

real_pred$obs <- real_pred$obs %>%
  as.factor() %>%
  relevel("Infractor")

real_pred <- real_pred %>% 
  mutate(pred = case_when(pred == 1 ~ "Infractor", #condition 1
                          TRUE ~ "No Infractor")) #all others

real_pred$pred <- real_pred$pred %>%
  as.factor() %>%
  relevel("Infractor")

multiClassSummary(real_pred, lev = levels(real_pred$obs))

library(MLmetrics)
prSummary(real_pred, lev = levels(real_pred$obs))

########## ANN 3 OPTIMIZADOR ADAM - VALIDATION SET APPROACH ########## 

# Initialize our model
ann3_adam <- keras_model_sequential()

ann3_adam %>%
  layer_flatten(input_shape = ncol(x_train)) %>% 
  layer_dense(units = 12, activation = "relu") %>%
  layer_dense(units = 6, activation = "relu") %>%  
  layer_dense(units = 1, activation = "sigmoid")

summary(ann3_adam)

ann3_adam %>% compile(
  loss = "binary_crossentropy", #"mse", #"binary_crossentropy", #"categorical_crossentropy" 
  optimizer = "adam", #"sgd", #"adam"
  metrics = c("accuracy")) #"mse", #accuracy"

trained_ann3_adam <- ann3_adam %>% fit(
  x = as.matrix(x_train),
  y = as.matrix(y_train),
  epochs = 100,
  validation_split = 0.3)

# Print del modelo
print(trained_ann3_adam)

# Ploteamos el modelo
plot(trained_ann3_adam)

# Evaluamos performance en train data
ann3_adam %>% evaluate(x = as.matrix(x_train), y = as.matrix(y_train))

# Evaluamos performance en test data
ann3_adam %>% evaluate(x = as.matrix(x_test), y = as.matrix(y_test))

# Vemos los pesos de la red
get_weights(ann3_adam)

# Realizamos predicciones
pred_prob <- ann3_adam %>% predict(as.matrix(x_test)) %>% as.vector() # Probabilidades predichas. Si le saco el as.vector() me quedo con una matriz
hist(pred_prob)
pred_int <- ann3_adam %>% predict(as.matrix(x_test)) %>% `>`(0.3) %>% k_cast("int32") # 0.3 - Elijo a partir de que probabilidad se clasifica como infractor

# Guardamos resultados en forma de matrices
prediccion_metric <- as.matrix(pred_int)
y_test_metric <- as.matrix(y_test)

confusionMatrix(data = as.factor(prediccion_metric), 
                reference = as.factor(y_test_metric), 
                positive = "1", 
                mode = "prec_recall")

# El Matthews correlation index es útil cuando las observaciones entre clases varían mucho.
# Se lee igual que el coeficiene de correlación
mcc(preds = as.factor(prediccion_metric), actuals = as.factor(y_test_metric))

# Se genera un dataframe con los valores reales, predichos y probabilidades estimadas
real_pred <- data.frame(obs = as.factor(y_test_metric), 
                        pred = as.factor(prediccion_metric), 
                        Infractor = pred_prob, 
                        No_Infractor = 1 - pred_prob)

real_pred <- real_pred %>% 
  mutate(obs = case_when(obs == 1 ~ "Infractor", #condition 1
                         TRUE ~ "No Infractor")) #all others

real_pred$obs <- real_pred$obs %>%
  as.factor() %>%
  relevel("Infractor")

real_pred <- real_pred %>% 
  mutate(pred = case_when(pred == 1 ~ "Infractor", #condition 1
                          TRUE ~ "No Infractor")) #all others

real_pred$pred <- real_pred$pred %>%
  as.factor() %>%
  relevel("Infractor")

multiClassSummary(real_pred, lev = levels(real_pred$obs))

library(MLmetrics)
prSummary(real_pred, lev = levels(real_pred$obs))

########## EVALUAR PERFORMANCE ########## 

# Se comparan las redes estimadas con sgd - loss
compare_ann_sgd_loss <- data.frame(
  ann1_sgd_train = trained_ann1_sgd$metrics$loss,
  ann1_sdg_val = trained_ann1_sgd$metrics$val_loss,
  ann2_sgd_train = trained_ann2_sgd$metrics$loss,
  ann2_sgd_val = trained_ann2_sgd$metrics$val_loss,
  ann3_sgd_train = trained_ann3_sgd$metrics$loss,
  ann3_sgd_val = trained_ann3_sgd$metrics$val_loss
) %>%
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  gather(key = "type", value = "value", -rowname)

ggplot(compare_ann_sgd_loss, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("epoch") +
  ylab("loss")

# Se comparan las redes estimadas con adam - loss
compare_ann_adam_loss <- data.frame(
  ann1_adam_train = trained_ann1_adam$metrics$loss,
  ann1_adam_val = trained_ann1_adam$metrics$val_loss,
  ann2_adam_train = trained_ann2_adam$metrics$loss,
  ann2_adam_val = trained_ann2_adam$metrics$val_loss,
  ann3_adam_train = trained_ann3_adam$metrics$loss,
  ann3_adam_val = trained_ann3_adam$metrics$val_loss
) %>%
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  gather(key = "type", value = "value", -rowname)

ggplot(compare_ann_adam_loss, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("epoch") +
  ylab("loss")

# Se comparan las redes estimadas con sgd - accuracy
compare_ann_sgd_acc <- data.frame(
  ann1_sgd_train = trained_ann1_sgd$metrics$accuracy,
  ann1_sdg_val = trained_ann1_sgd$metrics$val_accuracy,
  ann2_sgd_train = trained_ann2_sgd$metrics$accuracy,
  ann2_sgd_val = trained_ann2_sgd$metrics$val_accuracy,
  ann3_sgd_train = trained_ann3_sgd$metrics$accuracy,
  ann3_sgd_val = trained_ann3_sgd$metrics$val_accuracy
) %>%
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  gather(key = "type", value = "value", -rowname)

ggplot(compare_ann_sgd_acc, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("epoch") +
  ylab("accuracy")

# Se comparan las redes estimadas con adam - accuracy
compare_ann_adam_acc <- data.frame(
  ann1_adam_train = trained_ann1_adam$metrics$accuracy,
  ann1_adam_val = trained_ann1_adam$metrics$val_accuracy,
  ann2_adam_train = trained_ann2_adam$metrics$accuracy,
  ann2_adam_val = trained_ann2_adam$metrics$val_accuracy,
  ann3_adam_train = trained_ann3_adam$metrics$accuracy,
  ann3_adam_val = trained_ann3_adam$metrics$val_accuracy
) %>%
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  gather(key = "type", value = "value", -rowname)

ggplot(compare_ann_adam_acc, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("epoch") +
  ylab("accuracy")

# Evaluacion final
compare_ann_2 <- data.frame(
  ann2_sgd_train_loss = trained_ann2_sgd$metrics$loss,
  ann2_sgd_val_loss = trained_ann2_sgd$metrics$val_loss,
  ann2_adam_train_loss = trained_ann2_adam$metrics$loss,
  ann2_adam_val_loss = trained_ann2_adam$metrics$val_loss
) %>%
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  gather(key = "type", value = "value", -rowname)

ggplot(compare_ann_2, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("epoch") +
  ylab("loss")

########## ANN 2 OPTIMIZADOR SGD - OPTIMIZACION ENTRENAMIENTO ########## 

ann2_sgd_opt <- keras_model_sequential()
  
ann2_sgd_opt %>%
  layer_flatten(input_shape = ncol(x_train)) %>% 
  layer_dense(units = 100, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 50, activation = "relu") %>%  
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 5, activation = "relu") %>%  
  layer_dense(units = 1, activation = "sigmoid")

summary(ann2_sgd_opt)
  
ann2_sgd_opt %>% compile(
  loss = "binary_crossentropy", #"mse", #"binary_crossentropy", #"categorical_crossentropy" 
  optimizer = "sgd", #"sgd", #"adam"
  metrics = c("accuracy")) #"mse", #accuracy"
  
# Define class weights
class_weights <- list("0":1.0, "1":5.4)
  
# Se define un early stopping y un decay en lr
early_stop = callback_early_stopping(monitor = "val_loss", 
                                     min_delta = 1e-04, 
                                     patience = 60)
  
reduce_lr = callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.75,
                                          patience = 25, verbose = 0,
                                          mode = c("auto", "min", "max"),
                                          min_delta = 1e-04, cooldown = 0, min_lr = 0)
  
# Fit model
trained_ann2_sgd_opt <- ann2_sgd_opt %>% fit(
  x = as.matrix(x_train),
  y = as.matrix(y_train),
  epochs = 100, class_weights = class_weights,
  callbacks = c(early_stop, reduce_lr),
  validation_split = 0.3)
  
  
# Evaluamos performance en train data
ann2_sgd_opt %>% evaluate(x = as.matrix(x_train), y = as.matrix(y_train))

# Evaluamos performance en test data
ann2_sgd_opt %>% evaluate(x = as.matrix(x_test), y = as.matrix(y_test))

# Vemos los pesos de la red
get_weights(ann2_sgd_opt)

# Realizamos predicciones
pred_prob <- ann2_sgd_opt %>% predict(as.matrix(x_test)) %>% as.vector() # Probabilidades predichas. Al sacar as.vector() queda una matriz
hist(pred_prob)
pred_int <- ann2_sgd_opt %>% predict(as.matrix(x_test)) %>% `>`(0.3) %>% k_cast("int32") # 0.3 - Elijo a partir de que probabilidad se clasifica como infractor

# Guardamos resultados en forma de matrices
prediccion_metric <- as.matrix(pred_int)
y_test_metric <- as.matrix(y_test)

cm_ann2_sgd_opt <- confusionMatrix(data = as.factor(prediccion_metric), 
                               reference = as.factor(y_test_metric), 
                               positive = "1", 
                               mode = "prec_recall")
cm_ann2_sgd_opt

# El Matthews correlation index es útil cuando las observaciones entre clases varían mucho.
# Se lee igual que el coeficiene de correlación
mcc(preds = as.factor(prediccion_metric), actuals = as.factor(y_test_metric))

# Se genera un dataframe con los valores reales, predichos y probabilidades estimadas
real_pred <- data.frame(obs = as.factor(y_test_metric), 
                        pred = as.factor(prediccion_metric), 
                        Infractor = pred_prob, 
                        No_Infractor = 1 - pred_prob)

real_pred <- real_pred %>% 
  mutate(obs = case_when(obs == 1 ~ "Infractor", #condition 1
                         TRUE ~ "No Infractor")) #all others

real_pred$obs <- real_pred$obs %>%
  as.factor() %>%
  relevel("Infractor")

real_pred <- real_pred %>% 
  mutate(pred = case_when(pred == 1 ~ "Infractor", #condition 1
                          TRUE ~ "No Infractor")) #all others

real_pred$pred <- real_pred$pred %>%
  as.factor() %>%
  relevel("Infractor")

multiClassSummary(real_pred, lev = levels(real_pred$obs))

library(MLmetrics)
prSummary(real_pred, lev = levels(real_pred$obs))

########## EVALUACION FINAL DE PERFORMANCE ########## 
compare_ann_2_opt <- data.frame(
  #ann2_sgd_train_loss = trained_ann2_sgd$metrics$loss,
  #ann2_sgd_val_loss = trained_ann2_sgd$metrics$val_loss,
  #ann2_adam_train_loss = trained_ann2_adam$metrics$loss,
  #ann2_adam_val_loss = trained_ann2_adam$metrics$val_loss,
  ann2_sgd_opt_train_loss = trained_ann2_sgd_opt$metrics$loss,
  ann2_sgd_opt_val_loss = trained_ann2_sgd_opt$metrics$val_loss
) %>%
  rownames_to_column() %>%
  mutate(rowname = as.integer(rowname)) %>%
  gather(key = "type", value = "value", -rowname)

ggplot(compare_ann_2_opt, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("epoch") +
  ylab("loss")
