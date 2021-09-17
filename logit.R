source('preprocessing.R')

str(pais)
df_status(pais)

# Modelo Logit
logit <- glm(infractor ~ tipo_carniceria_1 + tipo_carniceria_2 +
               elabora_prod + hab_sec_elab + realiza_coccion + dias_desde_30_7 +
               poblacion + superficie_km2 + ing_p_capita_mes_cte_2005,
             data = pais, family = binomial)
summary(logit)

# Se asignan las predicciones del Modelo Logit a glm.probs
glm.probs <-predict(logit, type = 'response')
glm.probs[1:20]

# Se hace una prediccion naive en donde si la probabilidad de ser infractor > 0.3 se considera infractor.
glm.pred <- ifelse(glm.probs > 0.3, 1, 0)
table(glm.pred, pais$infractor)
mean(glm.pred == pais$infractor)

# Se corre un anova para ver el aporte de cada una de las variables. El NULL es un modelo solo con
# el Intercept y el objetivo es que con cada nueva variable, el Residual Deviance vaya bajando.
anova(logit, test = "Chisq")

# El McFadden index puede utilizarse como proxy del R2 del modelo.
pR2(logit)

############ SE GENERA UNA MUESTRA DE TRAIN Y TEST PARA ESTIMAR MODELO ############ 

# Se separa la data en train y test
set.seed(123)
training.samples <- pais$infractor %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- pais[training.samples, ]
test.data <- pais[-training.samples, ]

# Cuantos infractores hay en train y en test
sum(train.data$infractor == 1)
sum(test.data$infractor == 1)

# Estimo modelo
model <- glm(infractor ~ tipo_carniceria_1 + tipo_carniceria_2 +
               elabora_prod + hab_sec_elab + realiza_coccion + dias_desde_30_7 +
               poblacion + superficie_km2 + ing_p_capita_mes_cte_2005,
             data = train.data, family = binomial)
summary(model)

# Hago predicciones
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.3, 1, 0)

# Model accuracy
table(predicted.classes, test.data$infractor)
mean(predicted.classes == test.data$infractor)

# El McFadden index puede utilizarse como proxy del R2 del modelo.
pR2(model)
