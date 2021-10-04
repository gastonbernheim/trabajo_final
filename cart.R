source("preprocessing.R")

library(party)

## CTree
set.seed(123)
training.samples <- pais$infractor %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- pais[training.samples, ]
test.data <- pais[-training.samples, ]

# Create the tree.
output.tree <- ctree(
  infractor ~ tipo_carniceria_1 + tipo_carniceria_2 + elabora_prod + hab_sec_elab + 
    realiza_coccion + dias_desde_30_7 + habitantes_carniceria, 
  data = pais)

# Plot the tree.
plot(output.tree)
