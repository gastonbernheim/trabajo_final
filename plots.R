source("preprocessing_pais.R")
library(ggtext)
library(gridExtra)

# 1. Numero de carnicerias por departamento
ggplot(deptos, aes(x=reorder(departamento,total_carnicerias), y=total_carnicerias)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_carnicerias), 
            hjust = -0.15, 
            size = 3, 
            position = position_dodge(width = 1)) + 
  xlab(label = "Departamento") +
  ylab(label = "Cantidad de puntos de venta") + 
  coord_flip()

# 2. Numero de carnicerias por tipo_carniceria
carn_tipo <- ggplot(pais, aes(x = tipo_carniceria)) +
  geom_bar(stat = "count", fill = "steelblue") + 
  theme_minimal() + # theme_minimal()
  geom_text(stat = "count", aes(label =..count.., vjust = -0.3), color = "black") +
  xlab(label = "Tipo de carniceria") +
  ylab(label = "Cantidad de puntos de venta") +
  labs(fill = "Tipo de carniceria")
carn_tipo

# 3. Numero de carnicerias por modalidad_carniceria
carn_mod <- ggplot(pais, aes(x = modalidad_carniceria, fill = modalidad_carniceria)) +
  geom_bar(stat = "count") + 
  theme_minimal() +
  geom_text(stat = "count", aes(label =..count.., vjust = -0.3)) +
  xlab(label = "Modalidad de carniceria") +
  ylab(label = "Cantidad de puntos de ventas") +
  labs(fill = "Modalidad de carniceria")
carn_mod

# 4. Distribucion de carnicerias tipo_carniceria segun modalidad_carniceria
carn_tipo_mod <- ggplot(pais, aes(x = tipo_carniceria, fill = modalidad_carniceria)) +
  geom_bar(stat = "count") + 
  theme_minimal() +
  geom_text(stat = "count", aes(label =..count.., vjust = -0.95)) +
  xlab(label = "Tipo de carniceria") +
  ylab(label = "Cantidad de puntos de venta") + 
  labs(fill = "Modalidad de carniceria")
carn_tipo_mod

# 5. Numero de carnicerias por departamento y tipo de carniceria  ####### REVISAR: Eliminar leyenda del (0,0) y agregar
# numeros de cantidades #######
carn_depto_tipo <- ggplot(pais, aes(x = departamento, 
                 y = tipo_carniceria,
                 fill = tipo_carniceria)) + 
  geom_bar(stat = "identity") +
  xlab(label = "Departamento") +
  ylab(label = "Cantidad de puntos de venta") + 
  labs(fill = "Tipo de carniceria") +
  coord_flip()
carn_depto_tipo

# 6. Numero de carnicerias por departamento y modalidad de carniceria ####### REVISAR: Eliminar leyenda del (0,0) y agregar
# numeros de cantidades #######
carn_depto_mod <- ggplot(pais, aes(x = departamento, 
                 y = modalidad_carniceria,
                 fill = modalidad_carniceria)) + 
  geom_bar(stat = "identity") +
  xlab(label = "Departamento") +
  ylab(label = "Cantidad de puntos de venta") + 
  labs(fill = "Modalidad de carniceria") +
  coord_flip()
carn_depto_mod

# Me quedo solo con las columnas tipo_mod_carniceria y elabora_prod
elabora <- pais[, c("tipo_mod_carniceria", "elabora_prod")]

elabora <- elabora %>% 
  mutate(elabora_prod = case_when(elabora_prod == 1 ~ "Elabora", #condition 1
                               TRUE ~ "No elabora")) #all others

# 7. Distribución de carnicerias segun elabora_prod
carn_elabora <- ggplot(elabora, aes(x = tipo_mod_carniceria, fill = elabora_prod)) +
  geom_bar(stat = "count") + 
  xlab(label = "Tipo y modalidad de carnicería") +
  ylab(label = "Cantidad de puntos de venta") + 
  labs(fill = "Elabora productos") +
  geom_text_repel(stat = "count", position = position_stack(vjust = 0.5), size = 3.5, aes(label = ..count..)) +
  scale_x_discrete(breaks = c("Independiente Corte",
                              "Independiente Expendio",
                              "Polleria Corte",
                              "Supermercado Corte",
                              "Supermercado Expendio"),
                   labels = c("Independiente <br> Corte",
                              "Independiente <br> Expendio",
                              "Polleria <br> Corte",
                              "Supermercado <br> Corte",
                              "Supermercado <br> Expendio")) +
  theme_minimal() +
  theme(axis.text.x = element_markdown(),
        axis.text.y = element_markdown(),
        axis.title.x = element_markdown(size = 10))
carn_elabora

# Me quedo solo con las columnas tipo_mod_carniceria y hab_sec_elab
hab_sec <- pais[, c("tipo_mod_carniceria", "hab_sec_elab")]

hab_sec <- hab_sec %>% 
  mutate(hab_sec_elab = case_when(hab_sec_elab == 1 ~ "Habilitado", #condition 1
                                  TRUE ~ "No habilitado")) #all others

# 8. Distribución de carnicerias segun hab_sec_elab
carn_hab_elabora <- ggplot(hab_sec, aes(x = tipo_mod_carniceria, fill = hab_sec_elab)) +
  geom_bar(stat = "count") + 
  xlab(label = "Tipo y modalidad de carnicería") +
  ylab(label = "Cantidad de puntos de venta") + 
  labs(fill = "Sector de elaboración") +
  geom_text_repel(stat = "count", position = position_stack(vjust = 0.5), size = 3.5, aes(label = ..count..)) +
  scale_x_discrete(breaks = c("Independiente Corte",
                     "Independiente Expendio",
                     "Polleria Corte",
                     "Supermercado Corte",
                     "Supermercado Expendio"),
                   labels = c("Independiente <br> Corte",
                              "Independiente <br> Expendio",
                              "Polleria <br> Corte",
                              "Supermercado <br> Corte",
                              "Supermercado <br> Expendio")) +
  theme_minimal() +
  theme(axis.text.x = element_markdown(),
        axis.text.y = element_markdown(),
        axis.title.x = element_markdown(size = 10))
carn_hab_elabora

# Me quedo solo con las columnas tipo_mod_carniceria, realiza_coccion, vende_no_carnicos y 
# vende_chacinados
otros <- pais[, c("tipo_mod_carniceria", "realiza_coccion",
                  "vende_no_carnicos","vende_chacinados")]

otros <- otros %>% 
  mutate(realiza_coccion = case_when(realiza_coccion == 1 ~ "Cocina", #condition 1
                                  TRUE ~ "No cocina")) #all others
otros <- otros %>% 
  mutate(vende_no_carnicos = case_when(vende_no_carnicos == 1 ~ "Vende", #condition 1
                                     TRUE ~ "No vende")) #all others
otros <- otros %>% 
  mutate(vende_chacinados = case_when(vende_chacinados == 1 ~ "Vende", #condition 1
                                     TRUE ~ "No vende")) #all others

# 9. Distribución de carnicerias segun realiza_coccion, vende_no_carnicos y vende_chacinados
carn_coccion <- ggplot(otros, aes(x = tipo_mod_carniceria, fill = realiza_coccion)) +
  geom_bar(stat = "count") +
  xlab(label = "Tipo y modalidad de carnicería") +
  ylab(label = "Cantidad de puntos de venta") +
  labs(fill = "Realiza cocción") +
  geom_text_repel(stat = "count", position = position_stack(vjust = 0.5), size = 3, aes(label = ..count..)) +
  scale_x_discrete(limits = c("Independiente Corte",
                              "Independiente Expendio",
                              "Polleria Corte",
                              "Supermercado Corte",
                              "Supermercado Expendio"),
                   labels = c("Independiente <br> Corte",
                              "Independiente <br> Expendio",
                              "Polleria <br> Corte",
                              "Supermercado <br> Corte",
                              "Supermercado <br> Expendio")) +
  theme_minimal() +
  theme(axis.text.x = element_markdown(),
        axis.text.y = element_markdown(),
        axis.title.x = element_markdown(size = 10))
carn_coccion <- carn_coccion + coord_flip()

# 10. Distribución de carnicerias segun vende_no_carnicos
carn_no_carnicos <- ggplot(otros, aes(x = tipo_mod_carniceria, fill = vende_no_carnicos)) +
  geom_bar(stat = "count") +
  xlab(label = "Tipo y modalidad de carnicería") +
  ylab(label = "Cantidad de puntos de venta") +
  labs(fill = "No Cárnicos") +
  geom_text_repel(stat = "count", position = position_stack(vjust = 0.5), size = 3, aes(label = ..count..)) +
  scale_x_discrete(limits = c("Independiente Corte",
                              "Independiente Expendio",
                              "Polleria Corte",
                              "Supermercado Corte",
                              "Supermercado Expendio"),
                   labels = c("Independiente <br> Corte",
                              "Independiente <br> Expendio",
                              "Polleria <br> Corte",
                              "Supermercado <br> Corte",
                              "Supermercado <br> Expendio")) +
  theme_minimal() +
  theme(axis.text.x = element_markdown(),
        axis.text.y = element_markdown(),
        axis.title.x = element_markdown(size = 10))
carn_no_carnicos <- carn_no_carnicos + coord_flip()

# 11. Distribución de carnicerias segun vende_chacinados
carn_chacinados <- ggplot(otros, aes(x = tipo_mod_carniceria, fill = vende_chacinados)) +
  geom_bar(stat = "count") +
  xlab(label = "Tipo y modalidad de carnicería") +
  ylab(label = "Cantidad de puntos de venta") +
  labs(fill = "Chacinados") +
  geom_text_repel(stat = "count", 
                  position = position_stack(vjust = 0.5), 
                  size = 3, 
                  aes(label = ..count..)) +
  scale_x_discrete(limits = c("Independiente Corte",
                              "Independiente Expendio",
                              "Polleria Corte",
                              "Supermercado Corte",
                              "Supermercado Expendio"),
                   labels = c("Independiente <br> Corte",
                              "Independiente <br> Expendio",
                              "Polleria <br> Corte",
                              "Supermercado <br> Corte",
                              "Supermercado <br> Expendio")) +
  theme_minimal() +
  theme(axis.text.x = element_markdown(),
        axis.text.y = element_markdown(),
        axis.title.x = element_markdown(size = 10))
carn_chacinados <- carn_chacinados + coord_flip()

# 9, 10 y 11. Unimos los tres graficos en uno solo
grid.arrange(carn_coccion, carn_no_carnicos, carn_chacinados)


# Me quedo solo con la columna infractor
inf <- pais %>% 
  mutate(infractor = case_when(infractor == 1 ~ "Infractor", #condition 1
                               TRUE ~ "No_Infractor")) #all others

# 12. Proporcion de infractores a nivel pais
dist_inf <- barplot(prop.table(table(inf$infractor)),
                    names.arg = c("Infractor", "No Infractor"),
                    col = c(rgb(0.8,0.1,0.1,0.6), "steelblue"), 
                    main = "Proporción de infractores",
                    xlab = NULL,
                    ylab = "Porcentaje")

text(dist_inf, prop.table(table(inf$infractor))/2,
     labels = paste(round(prop.table(table(inf$infractor))*100, digits = 2), "%", sep = "" ))

# 13. Infracciones por departamento 
infractores_p_depto <- ggplot(deptos, aes(x = reorder(departamento, total_infractores), 
                                          y = total_infractores)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label = total_infractores), 
            hjust = -0.15, 
            size = 3, 
            position = position_dodge(width = 1)) +
  xlab(label = "Departamento") +
  ylab(label = "Cantidad de infractores") + 
  theme_minimal() +
  coord_flip()
infractores_p_depto

# 14. Porcentaje de carnicerias infractoras por departamento 
prop_infractores_p_depto <- ggplot(deptos, aes(x = reorder(departamento, porcentaje_infractores),
                                               y = porcentaje_infractores)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label = round(porcentaje_infractores*100, digits = 1)), 
            hjust = -0.15, 
            size = 3, 
            position = position_dodge(width = 1)) +
  xlab(label = "Departamento") +
  ylab(label = "Porcentaje de infractores") +
  theme_minimal() +
  coord_flip()
prop_infractores_p_depto

# 15. Carnicerias infractoras segun tipo_mod_carniceria
inf_tipomod <- pais[, c("infractor", "tipo_mod_carniceria")]
inf_tipomod_grafico <- ggplot(subset(inf_tipomod, infractor == 1), 
                              aes(x = tipo_mod_carniceria,
                                  y = infractor)) +
  geom_bar(stat = "identity") +
  xlab(label = "Tipo y modalidad") +
  ylab(label = "Cantidad de infractores") +
  scale_x_discrete(limits = c("Polleria Corte",
                              "Independiente Expendio",
                              "Supermercado Expendio",
                              "Supermercado Corte",
                              "Independiente Corte")) +
  theme_minimal() +
  coord_flip()
inf_tipomod_grafico

# 16. Carnicerias infractoras por departamento segun tipo_mod_carniceria
inf_depto_tipomod <- pais[, c("infractor", "departamento", "tipo_mod_carniceria")]
inf_tipomod_depto <- ggplot(subset(inf_depto_tipomod, infractor == 1), 
                            aes(x = departamento,
                                y = infractor,
                                fill = tipo_mod_carniceria)) +
  geom_bar(position = "fill", stat = "identity") +
  xlab(label = "Departamento") +
  ylab(label = "Proporción de infractores") +
  labs(fill = "Tipo y modalidad") +
  scale_fill_brewer(type = "qual", palette = "RdYlBu") +
  theme_minimal() +
  coord_flip()
inf_tipomod_depto
  