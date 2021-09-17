source("/Users/gastonbernheim/Desktop/Facultad/Master en Big Data/11. Tesis/Padronario por Departamento/preprocessing.R")

library(keras)
library(tensorflow)

str(pais)
df_status(pais)

# Almaceno correlacion en C
C <- cor(pais[, c(2, 4:12, 15:17)])

# Plot de la correlacion entre las variables
corrplot(C, method = "number")

# Estandarizamos las variables numericas 
pais$poblacion <- pais$poblacion / sd(pais$poblacion)
pais$superficie_km2 <- pais$superficie_km2 / sd(pais$superficie_km2)
pais$ing_p_capita_mes_cte_2005 <- pais$ing_p_capita_mes_cte_2005 / sd(pais$ing_p_capita_mes_cte_2005)
#pais$dias_desde_30_7 <- pais$dias_desde_30_7 - mean(pais$dias_desde_30_7)

# Transoformamos las variables poblacion, superficie e ing_p_capita_mes_cte_2005 a logaritmos
#pais$poblacion <- log(pais$poblacion)
#pais$superficie_km2 <- log(pais$superficie_km2)
#pais$ing_p_capita_mes_cte_2005 <- log(pais$ing_p_capita_mes_cte_2005)

# Se separa la data en train y test
set.seed(123)
training.samples <- pais$infractor %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.data <- pais[training.samples, c(2, 4:12, 15:17)] # Solo nos quedamos con las columnas numericas y la variable a predecir
test.data <- pais[-training.samples, c(2, 4:12, 15:17)] # Solo nos quedamos con las columnas numericas y la variable a predecir

str(train.data)

# Armo el "tensor" de entrenamiento
#train.data <- array(train.data)

# One Hot Encoding (debo hacerlo??)
pais.trainLabels <- to_categorical(train.data$infractor, 2)
pais.testLabels <- to_categorical(test.data$infractor, 2)

# Initialize our model
ann_model <- keras_model_sequential()

ann_model %>%
  layer_flatten(input_shape = ncol(train.data[,-1])) %>%
  #layer_dense(units = 3, activation = "relu") %>%
  layer_dense(units = 2, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

summary(ann_model)

ann_model %>% compile(
  loss = "binary_crossentropy", #mse #categorical_crossentropy #binary_crossentropy
  optimizer = optimizer_sgd(learning_rate = 0.001), #sgd #adam
  metrics = c("accuracy")) #mse

trained_model <- ann_model %>% fit(
  x = as.matrix(train.data[, -1]),
  y = train.data[, 1],
  epochs = 200,
  validation_split = 0.3)

# Print del modelo
print(trained_model)

# Ploteamos el modelo
plot(trained_model)
