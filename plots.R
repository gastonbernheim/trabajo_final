source("preprocessing.R")

# Grafico que muestre infracciones por departamento 
infractores_p_depto <- ggplot(inf_p_depto, aes(x = departamento, y = total_infractores)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label = total_infractores), hjust = -0.15, size = 3, position = position_dodge(width = 1)) +
  coord_flip()
infractores_p_depto

# Grafico que muestre porcentaje de carnicerias infractoras por departamento 
prop_infractores_p_depto <- ggplot(perc_inf_p_depto, aes(x = departamento, y = porcentaje_infractores)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label = round(porcentaje_infractores*100, digits = 1)), hjust = -0.15, size = 3, position = position_dodge(width = 1)) +
  coord_flip()
prop_infractores_p_depto

# Grafico que muestre proporcion de infractores a nivel pais
barplot(prop.table(table(pais$infractor)), 
        col = c("red", "blue"), 
        main = "Proporcion de infractores",
        ylab = "Porcentaje")

####### VARIABLE INFRACTOR 2 ESTABA EN TEXTO ###### VER CART ####
################### AGREGAR PORCENTAJES EN ULTIMO GRAFICO ###############

