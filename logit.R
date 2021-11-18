source("preprocessing_pais.R")
library(pROC)

str(pais)
df_status(pais, print_results = F) %>% 
  select(variable, q_zeros, p_zeros, q_na) %>% 
  arrange(-q_zeros)

########## COPIA DEL DATASET PAIS Y TRANSFORMACION DE VARIABLES ##########
pais_logit <- data_frame(pais)

pais_logit <- pais_logit %>% 
  mutate(vende_no_carnicos = case_when(vende_no_carnicos == 1 ~ "Vende", #condition 1
                                       TRUE ~ "No_vende")) #all others

pais_logit <- pais_logit %>% 
  mutate(vende_chacinados = case_when(vende_chacinados == 1 ~ "Vende", #condition 1
                                      TRUE ~ "No_vende")) #all others

pais_logit <- pais_logit %>% 
  mutate(realiza_coccion = case_when(realiza_coccion == 1 ~ "Cocina", #condition 1
                                     TRUE ~ "No_cocina")) #all others

pais_logit <- pais_logit %>% 
  mutate(elabora_prod = case_when(elabora_prod == 1 ~ "Elabora", #condition 1
                                  TRUE ~ "No_elabora")) #all others

pais_logit <- pais_logit %>% 
  mutate(hab_sec_elab = case_when(hab_sec_elab == 1 ~ "Hab", #condition 1
                                  TRUE ~ "No_hab")) #all others

pais_logit$vende_no_carnicos <- as.factor(pais_logit$vende_no_carnicos)
pais_logit$vende_no_carnicos <- relevel(pais_logit$vende_no_carnicos, "No_vende")

pais_logit$vende_chacinados <- as.factor(pais_logit$vende_chacinados)
pais_logit$vende_chacinados <- relevel(pais_logit$vende_chacinados, "No_vende")

pais_logit$realiza_coccion <- as.factor(pais_logit$realiza_coccion)
pais_logit$realiza_coccion <- relevel(pais_logit$realiza_coccion, "No_cocina")

# Creamos la columna elaboracion que contenga las combinaciones de hab_sec_elab y elabora_prod
pais_logit$elaboracion <- str_c(pais_logit$hab_sec_elab, " ", pais_logit$elabora_prod)

# Elaboracion como factor y con base en Independiente Corte
pais_logit$elaboracion <- as.factor(pais_logit$elaboracion)
pais_logit$elaboracion <- relevel(pais_logit$elaboracion, "No_hab No_elabora")

# Tomo logs de las variables numericas
pais_logit$poblacion <- log(pais_logit$poblacion)
pais_logit$superficie_km2 <- log(pais_logit$superficie_km2)
pais_logit$ing_p_capita_mes_cte_2005 <- log(pais_logit$ing_p_capita_mes_cte_2005)

# Infractor como factor
pais_logit$infractor <- as.factor(pais_logit$infractor)

# Modificamos el orden de las variables en el dataset
pais_logit <- pais_logit %>% 
  relocate(id, .before = departamento)

pais_logit <- pais_logit %>% 
  relocate(infractor, .before = departamento)

pais_logit <- pais_logit %>% 
  relocate(tipo_mod_carniceria, .after = localidad)

pais_logit <- pais_logit %>% 
  relocate(elaboracion, .after = tipo_mod_carniceria)

pais_logit <- pais_logit %>% 
  relocate(vende_no_carnicos, .after = elaboracion)

pais_logit <- pais_logit %>% 
  relocate(vende_chacinados, .after = vende_no_carnicos)

pais_logit <- pais_logit %>% 
  relocate(realiza_coccion, .after = vende_chacinados)

# Eliminamos las variables que no se utilizaran del dataset
pais_logit <- pais_logit[, -c(1, 4, 10:17, 19:20, 25)]

########## IDENTIFICACION DE VARIABLES SIGNIFICATIVAS CON UBICACION ##########

logit <- glm(infractor ~ departamento, data = pais_logit, family = binomial)
summary(logit)

logit <- glm(infractor ~ departamento + tipo_mod_carniceria, 
                   data = pais_logit, family = binomial)
summary(logit)

logit <- glm(infractor ~ departamento + tipo_mod_carniceria + elaboracion, 
                   data = pais_logit, family = binomial)
summary(logit)

logit <- glm(infractor ~ departamento + tipo_mod_carniceria + vende_no_carnicos, 
             data = pais_logit, family = binomial)
summary(logit)

logit <- glm(infractor ~ departamento + tipo_mod_carniceria + vende_no_carnicos +
               vende_chacinados, data = pais_logit, family = binomial)
summary(logit)

logit <- glm(infractor ~ departamento + tipo_mod_carniceria + vende_chacinados +
               dias_desde_30_7, data = pais_logit, family = binomial)
summary(logit)

logit <- glm(infractor ~ departamento + tipo_mod_carniceria + vende_chacinados,
             data = pais_logit, family = binomial)
summary(logit)

########## IDENTIFICACION DE VARIABLES SIGNIFICATIVAS ASOCIADAS A CARNICERIAS ##########

logit_carniceria <- glm(infractor ~ tipo_mod_carniceria, 
                        data = pais_logit, family = binomial)
summary(logit_carniceria)

logit_carniceria <- glm(infractor ~ tipo_mod_carniceria + elaboracion,
                        data = pais_logit, family = binomial)
summary(logit_carniceria)

logit_carniceria <- glm(infractor ~ tipo_mod_carniceria + elaboracion + vende_no_carnicos,
                        data = pais_logit, family = binomial)
summary(logit_carniceria)

logit_carniceria <- glm(infractor ~ tipo_mod_carniceria + elaboracion + vende_no_carnicos +
                          vende_chacinados, data = pais_logit, family = binomial)
summary(logit_carniceria)

logit_carniceria <- glm(infractor ~ tipo_mod_carniceria + elaboracion + vende_chacinados +
                          realiza_coccion, data = pais_logit, family = binomial)
summary(logit_carniceria)

logit_carniceria <- glm(infractor ~ tipo_mod_carniceria + elaboracion + vende_chacinados +
                          realiza_coccion + dias_desde_30_7, 
                        data = pais_logit, family = binomial)
summary(logit_carniceria)

logit_carniceria <- glm(infractor ~ tipo_mod_carniceria + elaboracion + vende_chacinados +
                          dias_desde_30_7, 
                        data = pais_logit, family = binomial)
summary(logit_carniceria)

# Se decide no incluir la variable dias_desde_30_7 por tener un parametro cercano a 0
logit_carniceria <- glm(infractor ~ tipo_mod_carniceria + elaboracion + vende_chacinados, 
                        data = pais_logit, family = binomial)
summary(logit_carniceria)

########## IDENTIFICACION DE VARIABLES SIGNIFICATIVAS EXOGENAS ##########

logit_exo <- glm(infractor ~ poblacion,
                 data = pais_logit, family = binomial)
summary(logit_exo)

logit_exo <- glm(infractor ~ poblacion + superficie_km2,
                 data = pais_logit, family = binomial)
summary(logit_exo)

logit_exo <- glm(infractor ~ poblacion + superficie_km2 + ing_p_capita_mes_cte_2005,
                 data = pais_logit, family = binomial)
summary(logit_exo)

########## MODELOS SELECCIONADOS ##########

# Modelo 1 
logit_1 <- glm(infractor ~ departamento, data = pais_logit, family = binomial)
summary(logit_1)

# Modelo 2
logit_2 <- glm(infractor ~ departamento + tipo_mod_carniceria, 
               data = pais_logit, family = binomial)
summary(logit_2)

# Modelo 3
logit_3 <- glm(infractor ~ departamento + tipo_mod_carniceria + vende_chacinados,
                        data = pais_logit, family = binomial)
summary(logit_3)

########## SE GENERA UNA MUESTRA DE TRAIN Y TEST ############ 

set.seed(125)
training.samples <- pais_logit$infractor %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- pais_logit[training.samples, ]
test.data <- pais_logit[-training.samples, ]

# Que porcentaje de infractores hay en train y en test
sum(train.data$infractor == 1) / nrow(train.data)
sum(test.data$infractor == 1) / nrow(test.data)


########## RL - M1 VALIDATION SET APPROACH ############ 

# Estimo modelo
model_1 <- glm(infractor ~ departamento,
             family = binomial, data = train.data)
summary(model_1)

# Hago predicciones
probs_m1 <- model_1 %>% predict(test.data, type = "response")
hist(probs_m1,
     breaks = 6,
     main = "Histograma de probabilidades estimadas",
     xlab = "Probabilidad estimada de ser infractor",
     ylab = "Cantidad de establecimientos")
pred.classes_m1 <- ifelse(probs_m1 >= 0.3, 1, 0)

# Model accuracy
cm_m1 <- confusionMatrix(data = as.factor(pred.classes_m1), 
                reference = as.factor(test.data$infractor), 
                positive = "1", 
                mode = "prec_recall")
cm_m1

# Error rate = 1 - Accuracy
my_error <- function(confusion_matrix_table) {
  A <- confusion_matrix_table[1,1]; B <- confusion_matrix_table[1,2]
  C <- confusion_matrix_table[2,1]; D <- confusion_matrix_table[2,2]
  error_rate <- (B + C) / (A + B + C + D)
  return (error_rate)
}
my_error(cm_m1$table)

# El Matthews correlation index es útil cuando las observaciones entre clases varían mucho.
# Se lee igual que el coeficiene de correlación
mcc(preds = as.factor(pred.classes_m1), actuals = as.factor(test.data$infractor))

# Se corre un anova para ver el aporte de cada una de las variables. El NULL es un modelo solo con
# el Intercept y el objetivo es que con cada nueva variable, el Residual Deviance vaya bajando.
anova(model_1, test = "Chisq")

# El McFadden index puede utilizarse como proxy del R2 del modelo.
pR2(model_1)

# Curva ROC
roc_curve_1 <- roc(response = test.data$infractor, # true_class
                 predictor = pred.classes_m1, # predicted_class
                 levels = c("0", "1"),
                 auc = T,
                 ci=T)
roc_curve_1

########## RL - M1 K-FOLD CROSS VALIDATION ##########

# Definimos la forma de entrenamiento como k-fold cross validation
train_control <- trainControl(method = "cv", number = 10, sampling = "up")

# Entrenamos el modelo
model_1_cv <- caret::train(infractor ~ departamento,
                      data = train.data, 
                      trControl = train_control,
                      method = "glm",
                      family = "binomial",
                      metric = "Accuracy")

# Metricas de performance
summary(model_1_cv)
model_1_cv$results

# Predicciones
pred_logit_cv <- predict(model_1_cv, newdata = test.data)

# Matriz de confusion
cm_m1_cv <- confusionMatrix(data = as.factor(pred_logit_cv), 
                reference = as.factor(test.data$infractor),
                positive = "1", 
                mode = "prec_recall")
cm_m1_cv

# Error rate = 1 - Accuracy = (FP + FN / VP + FP + VN + FN)
my_error(cm_m1_cv$table)

# Curva ROC
roc_curve_1_cv <- roc(response = test.data$infractor, # true_class
                 predictor = as.numeric(pred_logit_cv), # predicted_class
                 levels = c("0", "1"),
                 auc = T,
                 ci=T)
roc_curve_1_cv

########## RL - M1 REPEATED K-FOLD CROSS VALIDATION ##########

# Definimos la forma de entrenamiento como repeated k-fold cross validation
train_control <- trainControl(method = "repeatedcv", number = 10, 
                              repeats = 5, sampling = "up")

# Entrenamos el modelo
model_1_rcv <- caret::train(infractor ~ departamento,
                      data = train.data,
                      trControl = train_control,
                      method = "glm",
                      family = "binomial",
                      metric = "Accuracy")

# Metricas de performance
summary(model_1_rcv)
model_1_rcv$results

# Predicciones
pred_logit_rcv <- predict(model_1_rcv, newdata = test.data)

# Matriz de confusion
cm_m1_rcv <- confusionMatrix(data = as.factor(pred_logit_rcv), 
                reference = as.factor(test.data$infractor), 
                positive = "1", 
                mode = "prec_recall")
cm_m1_rcv

# Error rate = 1 - Accuracy = (FP + FN / VP + FP + VN + FN)
my_error(cm_m1_rcv$table)

# Curva ROC
roc_curve_1_rcv <- roc(response = test.data$infractor, # true_class
                      predictor = as.numeric(pred_logit_rcv), # predicted_class
                      levels = c("0", "1"),
                      auc = T,
                      ci=T)
roc_curve_1_rcv

########## RL - M1 PLOT CURVAS ROC ##########

# Plot de curvas ROC obtenidas
plot(roc_curve_1, colorize = TRUE)
plot(roc_curve_1_cv, add = TRUE, col = "red")
plot(roc_curve_1_rcv, add = TRUE, col = "blue")

########## RL - M2 VALIDATION SET APPROACH ############ 

# Estimo modelo
model_2 <- glm(infractor ~ departamento + tipo_mod_carniceria,
               family = binomial, data = train.data)
summary(model_2)

# Hago predicciones
probs_m2 <- model_2 %>% predict(test.data, type = "response")
hist(probs_m2,
     breaks = 6,
     main = "Histograma de probabilidades estimadas",
     xlab = "Probabilidad estimada de ser infractor",
     ylab = "Cantidad de establecimientos")
pred.classes_m2 <- ifelse(probs_m2 >= 0.3, 1, 0)

# Model accuracy
cm_m2 <- confusionMatrix(data = as.factor(pred.classes_m2), 
                         reference = as.factor(test.data$infractor), 
                         positive = "1", 
                         mode = "prec_recall")
cm_m2

# Error rate = 1 - Accuracy
my_error(cm_m2$table)

# El Matthews correlation index es útil cuando las observaciones entre clases varían mucho.
# Se lee igual que el coeficiene de correlación
mcc(preds = as.factor(pred.classes_m2), actuals = as.factor(test.data$infractor))

# Se corre un anova para ver el aporte de cada una de las variables. El NULL es un modelo solo con
# el Intercept y el objetivo es que con cada nueva variable, el Residual Deviance vaya bajando.
anova(model_2, test = "Chisq")

# El McFadden index puede utilizarse como proxy del R2 del modelo.
pR2(model_2)

# Curva ROC
roc_curve_2 <- roc(response = test.data$infractor, # true_class
                   predictor = pred.classes_m2, # predicted_class
                   levels = c("0", "1"),
                   auc = T,
                   ci=T)
roc_curve_2

########## RL - M2 K-FOLD CROSS VALIDATION ##########

# Definimos la forma de entrenamiento como k-fold cross validation
train_control <- trainControl(method = "cv", number = 10, sampling = "up")

# Entrenamos el modelo
model_2_cv <- caret::train(infractor ~ departamento + tipo_mod_carniceria,
                           data = train.data, 
                           trControl = train_control,
                           method = "glm",
                           family = "binomial",
                           metric = "Accuracy")

# Metricas de performance
summary(model_2_cv)
model_2_cv$results

# Predicciones
pred_logit_2_cv <- predict(model_2_cv, newdata = test.data)

# Matriz de confusion
cm_m2_cv <- confusionMatrix(data = as.factor(pred_logit_2_cv), 
                      reference = as.factor(test.data$infractor),
                      positive = "1", 
                      mode = "prec_recall")
cm_m2_cv

# Error rate = 1 - Accuracy = (FP + FN / VP + FP + VN + FN)
my_error(cm_m2_cv$table)

# Curva ROC
roc_curve_2_cv <- roc(response = test.data$infractor, # true_class
                      predictor = as.numeric(pred_logit_2_cv), # predicted_class
                      levels = c("0", "1"),
                      auc = T,
                      ci=T)
roc_curve_2_cv

########## RL - M2 REPEATED K-FOLD CROSS VALIDATION ##########

# Definimos la forma de entrenamiento como repeated k-fold cross validation
train_control <- trainControl(method = "repeatedcv", number = 10, 
                              repeats = 5, sampling = "up")

# Entrenamos el modelo
model_2_rcv <- caret::train(infractor ~ departamento + tipo_mod_carniceria,
                            data = train.data,
                            trControl = train_control,
                            method = "glm",
                            family = "binomial",
                            metric = "Accuracy")

# Metricas de performance
summary(model_2_rcv)
model_2_rcv$results

# Predicciones
pred_logit_2_rcv <- predict(model_2_rcv, newdata = test.data)

# Matriz de confusion
cm_m2_rcv <- confusionMatrix(data = as.factor(pred_logit_2_rcv), 
                      reference = as.factor(test.data$infractor), 
                      positive = "1", 
                      mode = "prec_recall")
cm_m2_rcv

# Error rate = 1 - Accuracy = (FP + FN / VP + FP + VN + FN)
my_error(cm_m2_rcv$table)

# Curva ROC
roc_curve_2_rcv <- roc(response = test.data$infractor, # true_class
                       predictor = as.numeric(pred_logit_2_rcv), # predicted_class
                       levels = c("0", "1"),
                       auc = T,
                       ci=T)
roc_curve_2_rcv

########## RL - M2 PLOT CURVAS ROC ##########

# Plot de curvas ROC obtenidas
plot(roc_curve_2, colorize = TRUE)
plot(roc_curve_2_cv, add = TRUE, col = "red")
plot(roc_curve_2_rcv, add = TRUE, col = "blue")

########## RL - M3 VALIDATION SET APPROACH ############ 

# Estimo modelo
model_3 <- glm(infractor ~ departamento + tipo_mod_carniceria + vende_chacinados,
               family = binomial, data = train.data)
summary(model_3)

# Hago predicciones
probs_m3 <- model_3 %>% predict(test.data, type = "response")
hist(probs_m3,
     breaks = 6,
     main = "Histograma de probabilidades estimadas",
     xlab = "Probabilidad estimada de ser infractor",
     ylab = "Cantidad de establecimientos")
pred.classes_m3 <- ifelse(probs_m3 >= 0.3, 1, 0)

# Model accuracy
cm_m3 <- confusionMatrix(data = as.factor(pred.classes_m3), 
                         reference = as.factor(test.data$infractor), 
                         positive = "1", 
                         mode = "prec_recall")
cm_m3

# Error rate = 1 - Accuracy
my_error(cm_m3$table)

# El Matthews correlation index es útil cuando las observaciones entre clases varían mucho.
# Se lee igual que el coeficiene de correlación
mcc(preds = as.factor(pred.classes_m3), actuals = as.factor(test.data$infractor))

# Se corre un anova para ver el aporte de cada una de las variables. El NULL es un modelo solo con
# el Intercept y el objetivo es que con cada nueva variable, el Residual Deviance vaya bajando.
anova(model_3, test = "Chisq")

# El McFadden index puede utilizarse como proxy del R2 del modelo.
pR2(model_3)

# Curva ROC
roc_curve_3 <- roc(response = test.data$infractor, # true_class
                   predictor = pred.classes_m3, # predicted_class
                   levels = c("0", "1"),
                   auc = T,
                   ci=T)
roc_curve_3

########## RL - M3 K-FOLD CROSS VALIDATION ##########

# Definimos la forma de entrenamiento como k-fold cross validation
train_control <- trainControl(method = "cv", number = 10, sampling = "up")

# Entrenamos el modelo
model_3_cv <- caret::train(infractor ~ departamento + tipo_mod_carniceria + vende_chacinados,
                           data = train.data, 
                           trControl = train_control,
                           method = "glm",
                           family = "binomial",
                           metric = "Accuracy")

# Metricas de performance
summary(model_3_cv)
model_3_cv$results

# Predicciones
pred_logit_3_cv <- predict(model_3_cv, newdata = test.data)

# Matriz de confusion
cm_m3_cv <- confusionMatrix(data = as.factor(pred_logit_3_cv), 
                            reference = as.factor(test.data$infractor),
                            positive = "1", 
                            mode = "prec_recall")
cm_m3_cv

# Error rate = 1 - Accuracy = (FP + FN / VP + FP + VN + FN)
my_error(cm_m3_cv$table)

# Curva ROC
roc_curve_3_cv <- roc(response = test.data$infractor, # true_class
                      predictor = as.numeric(pred_logit_3_cv), # predicted_class
                      levels = c("0", "1"),
                      auc = T,
                      ci=T)
roc_curve_3_cv

########## RL - M3 REPEATED K-FOLD CROSS VALIDATION ##########

# Definimos la forma de entrenamiento como repeated k-fold cross validation
train_control <- trainControl(method = "repeatedcv", number = 10, 
                              repeats = 5, sampling = "up")

# Entrenamos el modelo
model_3_rcv <- caret::train(infractor ~ departamento + tipo_mod_carniceria + vende_chacinados,
                            data = train.data,
                            trControl = train_control,
                            method = "glm",
                            family = "binomial",
                            metric = "Accuracy")

# Metricas de performance
summary(model_3_rcv)
model_3_rcv$results

# Predicciones
pred_logit_3_rcv <- predict(model_3_rcv, newdata = test.data)

# Matriz de confusion
cm_m3_rcv <- confusionMatrix(data = as.factor(pred_logit_3_rcv), 
                             reference = as.factor(test.data$infractor), 
                             positive = "1", 
                             mode = "prec_recall")
cm_m3_rcv

# Error rate = 1 - Accuracy = (FP + FN / VP + FP + VN + FN)
my_error(cm_m3_rcv$table)

# Curva ROC
roc_curve_3_rcv <- roc(response = test.data$infractor, # true_class
                       predictor = as.numeric(pred_logit_3_rcv), # predicted_class
                       levels = c("0", "1"),
                       auc = T,
                       ci=T)
roc_curve_3_rcv

########## RL - M3 PLOT CURVAS ROC ##########

# Plot de curvas ROC obtenidas
plot(roc_curve_3, colorize = TRUE)
plot(roc_curve_3_cv, add = TRUE, col = "red")
plot(roc_curve_3_rcv, add = TRUE, col = "blue")

########## PLOT CURVAS ROC TODOS LOS MODELOS RL ##########

# Plot de curvas ROC obtenidas
plot(roc_curve_1, colorize = TRUE, col = "black")
plot(roc_curve_1_cv, add = TRUE, col = "black")
plot(roc_curve_1_rcv, add = TRUE, col = "black")
plot(roc_curve_2, add = TRUE, col = "red" )
plot(roc_curve_2_cv, add = TRUE, col = "red")
plot(roc_curve_2_rcv, add = TRUE, col = "red")
plot(roc_curve_3, add = TRUE, col = "blue")
plot(roc_curve_3_cv, add = TRUE, col = "blue")
plot(roc_curve_3_rcv, add = TRUE, col = "blue")


