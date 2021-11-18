source("preprocessing_pais.R")
source("logit.R")

# Funcion generadora de probabilidades
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# Modelo estimado
logit_model <- glm(infractor ~ departamento + tipo_mod_carniceria, 
               data = pais_logit, family = binomial)

# Metricas de performance
summary(logit_model)

# Coeficiente estimada para cada combinación de departamento y tipo_mod
intercept <- coef(logit_model)[1]
artigas <- coef(logit_model)[2]
cerro_largo <- coef(logit_model)[3]
colonia <- coef(logit_model)[4]
durazno <- coef(logit_model)[5]
flores <- coef(logit_model)[6]
florida <- coef(logit_model)[7]
lavalleja <- coef(logit_model)[8]
maldonado <- coef(logit_model)[9]
montevideo <- coef(logit_model)[10]
paysandu <- coef(logit_model)[11]
rio_negro <- coef(logit_model)[12]
rivera <- coef(logit_model)[13]
rocha <- coef(logit_model)[14]
salto <- coef(logit_model)[15]
san_jose <- coef(logit_model)[16]
soriano <- coef(logit_model)[17]
tacuarembo <- coef(logit_model)[18]
treinta_tres <- coef(logit_model)[19]
independiente_expendio <- coef(logit_model)[20]
polleria_corte <- coef(logit_model)[21]
supermercado_corte <- coef(logit_model)[22]
supermercado_expendio <- coef(logit_model)[23]

########### LOGITS ###########

# Artigas - Logits estimado para cada combinación
ic_artigas <- intercept + artigas*1
ie_artigas <- intercept + artigas*1 + independiente_expendio*1
sc_artigas <- intercept + artigas*1 + supermercado_corte*1
se_artigas <- intercept + artigas*1 + supermercado_expendio*1
pc_artigas <- intercept + artigas*1 + polleria_corte*1

# Canelones - Logits estimado para cada combinación
ic_canelones <- intercept
ie_canelones <- intercept + independiente_expendio*1
sc_canelones <- intercept + supermercado_corte*1
se_canelones <- intercept + supermercado_expendio*1
pc_canelones <- intercept + polleria_corte*1

# Cerro Largo - Logits estimado para cada combinación
ic_cerro_largo <- intercept + cerro_largo*1
ie_cerro_largo <- intercept + cerro_largo*1 + independiente_expendio*1
sc_cerro_largo <- intercept + cerro_largo*1 + supermercado_corte*1
se_cerro_largo <- intercept + cerro_largo*1 + supermercado_expendio*1
pc_cerro_largo <- intercept + cerro_largo*1 + polleria_corte*1

# Colonia
ic_colonia <- intercept + colonia*1
ie_colonia <- intercept + colonia*1 + independiente_expendio*1
sc_colonia <- intercept + colonia*1 + supermercado_corte*1
se_colonia <- intercept + colonia*1 + supermercado_expendio*1
pc_colonia <- intercept + colonia*1 + polleria_corte*1

# Durazno
ic_durazno <- intercept + durazno*1
ie_durazno <- intercept + durazno*1 + independiente_expendio*1
sc_durazno <- intercept + durazno*1 + supermercado_corte*1
se_durazno <- intercept + durazno*1 + supermercado_expendio*1
pc_durazno <- intercept + durazno*1 + polleria_corte*1

# Flores
ic_flores <- intercept + flores*1
ie_flores <- intercept + flores*1 + independiente_expendio*1
sc_flores <- intercept + flores*1 + supermercado_corte*1
se_flores <- intercept + flores*1 + supermercado_expendio*1
pc_flores <- intercept + flores*1 + polleria_corte*1

# Florida
ic_florida <- intercept + florida*1
ie_florida <- intercept + florida*1 + independiente_expendio*1
sc_florida <- intercept + florida*1 + supermercado_corte*1
se_florida <- intercept + florida*1 + supermercado_expendio*1
pc_florida <- intercept + florida*1 + polleria_corte*1

# Lavalleja
ic_lavalleja <- intercept + lavalleja*1
ie_lavalleja <- intercept + lavalleja*1 + independiente_expendio*1
sc_lavalleja <- intercept + lavalleja*1 + supermercado_corte*1
se_lavalleja <- intercept + lavalleja*1 + supermercado_expendio*1
pc_lavalleja <- intercept + lavalleja*1 + polleria_corte*1

# Maldonado
ic_maldonado <- intercept + maldonado*1
ie_maldonado <- intercept + maldonado*1 + independiente_expendio*1
sc_maldonado <- intercept + maldonado*1 + supermercado_corte*1
se_maldonado <- intercept + maldonado*1 + supermercado_expendio*1
pc_maldonado <- intercept + maldonado*1 + polleria_corte*1

# Montevideo
ic_montevideo <- intercept + montevideo*1
ie_montevideo <- intercept + montevideo*1 + independiente_expendio*1
sc_montevideo <- intercept + montevideo*1 + supermercado_corte*1
se_montevideo <- intercept + montevideo*1 + supermercado_expendio*1
pc_montevideo <- intercept + montevideo*1 + polleria_corte*1

# Paysandu
ic_paysandu <- intercept + paysandu*1
ie_paysandu <- intercept + paysandu*1 + independiente_expendio*1
sc_paysandu <- intercept + paysandu*1 + supermercado_corte*1
se_paysandu <- intercept + paysandu*1 + supermercado_expendio*1
pc_paysandu <- intercept + paysandu*1 + polleria_corte*1

# Rio Negro
ic_rio_negro <- intercept + rio_negro*1
ie_rio_negro <- intercept + rio_negro*1 + independiente_expendio*1
sc_rio_negro <- intercept + rio_negro*1 + supermercado_corte*1
se_rio_negro <- intercept + rio_negro*1 + supermercado_expendio*1
pc_rio_negro <- intercept + rio_negro*1 + polleria_corte*1

# Rivera
ic_rivera <- intercept + rivera*1
ie_rivera <- intercept + rivera*1 + independiente_expendio*1
sc_rivera <- intercept + rivera*1 + supermercado_corte*1
se_rivera <- intercept + rivera*1 + supermercado_expendio*1
pc_rivera <- intercept + rivera*1 + polleria_corte*1

# Rocha
ic_rocha <- intercept + rocha*1
ie_rocha <- intercept + rocha*1 + independiente_expendio*1
sc_rocha <- intercept + rocha*1 + supermercado_corte*1
se_rocha <- intercept + rocha*1 + supermercado_expendio*1
pc_rocha <- intercept + rocha*1 + polleria_corte*1

# Salto
ic_salto <- intercept + salto*1
ie_salto <- intercept + salto*1 + independiente_expendio*1
sc_salto <- intercept + salto*1 + supermercado_corte*1
se_salto <- intercept + salto*1 + supermercado_expendio*1
pc_salto <- intercept + salto*1 + polleria_corte*1

# San Jose
ic_san_jose <- intercept + san_jose*1
ie_san_jose <- intercept + san_jose*1 + independiente_expendio*1
sc_san_jose <- intercept + san_jose*1 + supermercado_corte*1
se_san_jose <- intercept + san_jose*1 + supermercado_expendio*1
pc_san_jose <- intercept + san_jose*1 + polleria_corte*1

# Soriano
ic_soriano <- intercept + soriano*1
ie_soriano <- intercept + soriano*1 + independiente_expendio*1
sc_soriano <- intercept + soriano*1 + supermercado_corte*1
se_soriano <- intercept + soriano*1 + supermercado_expendio*1
pc_soriano <- intercept + soriano*1 + polleria_corte*1

# Tacuarembo
ic_tacuarembo <- intercept + tacuarembo*1
ie_tacuarembo <- intercept + tacuarembo*1 + independiente_expendio*1
sc_tacuarembo <- intercept + tacuarembo*1 + supermercado_corte*1
se_tacuarembo <- intercept + tacuarembo*1 + supermercado_expendio*1
pc_tacuarembo <- intercept + tacuarembo*1 + polleria_corte*1

# Treinta y Tres
ic_treinta_tres <- intercept + treinta_tres*1
ie_treinta_tres <- intercept + treinta_tres*1 + independiente_expendio*1
sc_treinta_tres <- intercept + treinta_tres*1 + supermercado_corte*1
se_treinta_tres <- intercept + treinta_tres*1 + supermercado_expendio*1
pc_treinta_tres <- intercept + treinta_tres*1 + polleria_corte*1

########### PROBABILIDADES ###########

# Artigas - Probabilidades estimadas para cada combinación
ic_artigas <- logit2prob(ic_artigas)
ie_artigas <- logit2prob(ie_artigas)
sc_artigas <- logit2prob(sc_artigas)
se_artigas <- logit2prob(se_artigas)
pc_artigas <- logit2prob(pc_artigas)

# Canelones - Probabilidades estimadas para cada combinación
ic_canelones <- logit2prob(ic_canelones)
ie_canelones <- logit2prob(ie_canelones)
sc_canelones <- logit2prob(sc_canelones)
se_canelones <- logit2prob(se_canelones)
pc_canelones <- logit2prob(pc_canelones)

# Cerro Largo
ic_cerro_largo <- logit2prob(ic_cerro_largo)
ie_cerro_largo <- logit2prob(ie_cerro_largo)
sc_cerro_largo <- logit2prob(sc_cerro_largo)
se_cerro_largo <- logit2prob(se_cerro_largo)
pc_cerro_largo <- logit2prob(pc_cerro_largo)

# Colonia
ic_colonia <- logit2prob(ic_colonia)
ie_colonia <- logit2prob(ie_colonia)
sc_colonia <- logit2prob(sc_colonia)
se_colonia <- logit2prob(se_colonia)
pc_colonia <- logit2prob(pc_colonia)

# Durazno
ic_durazno <- logit2prob(ic_durazno)
ie_durazno <- logit2prob(ie_durazno)
sc_durazno <- logit2prob(sc_durazno)
se_durazno <- logit2prob(se_durazno)
pc_durazno <- logit2prob(pc_durazno)

# Flores
ic_flores <- logit2prob(ic_flores)
ie_flores <- logit2prob(ie_flores)
sc_flores <- logit2prob(sc_flores)
se_flores <- logit2prob(se_flores)
pc_flores <- logit2prob(pc_flores)

# Florida
ic_florida <- logit2prob(ic_florida)
ie_florida <- logit2prob(ie_florida)
sc_florida <- logit2prob(sc_florida)
se_florida <- logit2prob(se_florida)
pc_florida <- logit2prob(pc_florida)

# Lavalleja
ic_lavalleja <- logit2prob(ic_lavalleja)
ie_lavalleja <- logit2prob(ie_lavalleja)
sc_lavalleja <- logit2prob(sc_lavalleja)
se_lavalleja <- logit2prob(se_lavalleja)
pc_lavalleja <- logit2prob(pc_lavalleja)

# Maldonado
ic_maldonado <- logit2prob(ic_maldonado)
ie_maldonado <- logit2prob(ie_maldonado)
sc_maldonado <- logit2prob(sc_maldonado)
se_maldonado <- logit2prob(se_maldonado)
pc_maldonado <- logit2prob(pc_maldonado)

# Montevideo
ic_montevideo <- logit2prob(ic_montevideo)
ie_montevideo <- logit2prob(ie_montevideo)
sc_montevideo <- logit2prob(sc_montevideo)
se_montevideo <- logit2prob(se_montevideo)
pc_montevideo <- logit2prob(pc_montevideo)

# Paysandu
ic_paysandu <- logit2prob(ic_paysandu)
ie_paysandu <- logit2prob(ie_paysandu)
sc_paysandu <- logit2prob(sc_paysandu)
se_paysandu <- logit2prob(se_paysandu)
pc_paysandu <- logit2prob(pc_paysandu)

# Rio Negro
ic_rio_negro <- logit2prob(ic_rio_negro)
ie_rio_negro <- logit2prob(ie_rio_negro)
sc_rio_negro <- logit2prob(sc_rio_negro)
se_rio_negro <- logit2prob(se_rio_negro)
pc_rio_negro <- logit2prob(pc_rio_negro)

# Rivera
ic_rivera <- logit2prob(ic_rivera)
ie_rivera <- logit2prob(ie_rivera)
sc_rivera <- logit2prob(sc_rivera)
se_rivera <- logit2prob(se_rivera)
pc_rivera <- logit2prob(pc_rivera)

# Rocha
ic_rocha <- logit2prob(ic_rocha)
ie_rocha <- logit2prob(ie_rocha)
sc_rocha <- logit2prob(sc_rocha)
se_rocha <- logit2prob(se_rocha)
pc_rocha <- logit2prob(pc_rocha)

# Salto
ic_salto <- logit2prob(ic_salto)
ie_salto <- logit2prob(ie_salto)
sc_salto <- logit2prob(sc_salto)
se_salto <- logit2prob(se_salto)
pc_salto <- logit2prob(pc_salto)

# San Jose
ic_san_jose <- logit2prob(ic_san_jose)
ie_san_jose <- logit2prob(ie_san_jose)
sc_san_jose <- logit2prob(sc_san_jose)
se_san_jose <- logit2prob(se_san_jose)
pc_san_jose <- logit2prob(pc_san_jose)

# Soriano
ic_soriano <- logit2prob(ic_soriano)
ie_soriano <- logit2prob(ie_soriano)
sc_soriano <- logit2prob(sc_soriano)
se_soriano <- logit2prob(se_soriano)
pc_soriano <- logit2prob(pc_soriano)

# Tacuarembo
ic_tacuarembo <- logit2prob(ic_tacuarembo)
ie_tacuarembo <- logit2prob(ie_tacuarembo)
sc_tacuarembo <- logit2prob(sc_tacuarembo)
se_tacuarembo <- logit2prob(se_tacuarembo)
pc_tacuarembo <- logit2prob(pc_tacuarembo)

# Treinta y Tres
ic_treinta_tres <- logit2prob(ic_treinta_tres)
ie_treinta_tres <- logit2prob(ie_treinta_tres)
sc_treinta_tres <- logit2prob(sc_treinta_tres)
se_treinta_tres <- logit2prob(se_treinta_tres)
pc_treinta_tres <- logit2prob(pc_treinta_tres)

########## DATAFRAME DE PROBABILIDADES ##########
prob_estimadas <- data.frame(
  "tipo.mod.depto" = c("IC Artigas", "IE Artigas", "SC Artigas", "SE Artigas", "PC Artigas",
                       "IC Canelones", "IE Canelones", "SC Canelones", "SE Canelones", "PC Canelones",
                       "IC Cerro Largo", "IE Cerro Largo", "SC Cerro Largo", "SE Cerro Largo", "PC Cerro Largo",
                       "IC Colonia", "IE Colonia", "SC Colonia", "SE Colonia", "PC Colonia",
                       "IC Durazno", "IE Durazno", "SC Durazno", "SE Durazno", "PC Durazno",
                       "IC Flores", "IE Flores", "SC Flores", "SE Flores", "PC Flores",
                       "IC Florida", "IE Florida", "SC Florida", "SE Florida", "PC Florida",
                       "IC Lavalleja", "IE Lavalleja", "SC Lavalleja", "SE Lavalleja", "PC Lavalleja",
                       "IC Maldonado", "IE Maldonado", "SC Maldonado", "SE Maldonado", "PC Maldonado",
                       "IC Montevideo", "IE Montevideo", "SC Montevideo", "SE Montevideo", "PC Montevideo",
                       "IC Paysandu", "IE Paysandu", "SC Paysandu", "SE Paysandu", "PC Paysandu",
                       "IC Rio Negro", "IE Rio Negro", "SC Rio Negro", "SE Rio Negro", "PC Rio Negro",
                       "IC Rivera", "IE Rivera", "SC Rivera", "SE Rivera", "PC Rivera",
                       "IC Rocha", "IE Rocha", "SC Rocha", "SE Rocha", "PC Rocha",
                       "IC Salto", "IE Salto", "SC Salto", "SE Salto", "PC Salto",
                       "IC San Jose", "IE San Jose", "SC San Jose", "SE San Jose", "PC San Jose",
                       "IC Soriano", "IE Soriano", "SC Soriano", "SE Soriano", "PC Soriano",
                       "IC Tacuarembo", "IE Tacuarembo", "SC Tacuarembo", "SE Tacuarembo", "PC Tacuarembo",
                       "IC Treinta y Tres", "IE Treinta y Tres", "SC Treinta y Tres", "SE Treinta y Tres", "PC Treinta y Tres"),
  "p.infractor" = as.numeric(c(ic_artigas, ie_artigas, sc_artigas, se_artigas, pc_artigas,
                               ic_canelones, ie_canelones, sc_canelones, se_canelones, pc_canelones,
                               ic_cerro_largo, ie_cerro_largo, sc_cerro_largo, se_cerro_largo, pc_cerro_largo,
                               ic_colonia, ie_colonia, sc_colonia, se_colonia, pc_colonia,
                               ic_durazno, ie_durazno, sc_durazno, se_durazno, pc_durazno,
                               ic_flores, ie_flores, sc_flores, se_flores, pc_flores,
                               ic_florida, ie_florida, sc_florida, se_florida, pc_florida,
                               ic_lavalleja, ie_lavalleja, sc_lavalleja, se_lavalleja, pc_lavalleja,
                               ic_maldonado, ie_maldonado, sc_maldonado, se_maldonado, pc_maldonado,
                               ic_montevideo, ie_montevideo, sc_montevideo, se_montevideo, pc_montevideo,
                               ic_paysandu, ie_paysandu, sc_paysandu, se_paysandu, pc_paysandu,
                               ic_rio_negro, ie_rio_negro, sc_rio_negro, se_rio_negro, pc_rio_negro,
                               ic_rivera, ie_rivera, sc_rivera, se_rivera, pc_rivera,
                               ic_rocha, ie_rocha, sc_rocha, se_rocha, pc_rocha,
                               ic_salto, ie_salto, sc_salto, se_salto, pc_salto,
                               ic_san_jose, ie_san_jose, sc_san_jose, se_san_jose, pc_san_jose,
                               ic_soriano, ie_soriano, sc_soriano, se_soriano, pc_soriano,
                               ic_tacuarembo, ie_tacuarembo, sc_tacuarembo, se_tacuarembo, pc_tacuarembo,
                               ic_treinta_tres, ie_treinta_tres, sc_treinta_tres, se_treinta_tres, pc_treinta_tres))
  )

prob_estimadas_perc <- prob_estimadas
prob_estimadas_perc$p.infractor <- as.numeric((prob_estimadas_perc$p.infractor) * 100)

########## MUESTREO DIRIGIDO - PENALIZACION MEDIANTE ENTROPIA ##########

#install.packages("nloptr")
library("nloptr")

# objective function
objective_function <- function( q, p, beta ){
  
  entropia = -sum(q*log(q))
  val = sum(p*q) + beta*entropia
  return (-val)
}

p = prob_estimadas[, 2] #c(1,1)
q = rep(1/95, 95) #c(0.5,0.5)

beta = 1

##test the function
objective_function(q,p,beta) == -1-log(2)

constr_function <- function( q, p, beta ){
  
  return (sum(q)-1)
}

# Solve using NLOPT_LN_COBYLA without gradient information
res <- nloptr(  x0 = rep(1/95, 95), #rep(1/4,4),
                eval_f = objective_function,
                lb = rep(0,95), #rep(0,4)
                eval_g_ineq = constr_function,
                opts = list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel"=1.0e-8),
                p = prob_estimadas[, 2], #c(.4,.3,.2,.1),
                beta = 10 )

res

qast = res$solution
p = prob_estimadas[, 2] #c(.4,.3,.2,.1)
prob_deteccion = sum(p*qast)
prob_deteccion

min(qast) #prob. de muestrear al que muestreo menos

########## AGREGO LA PROBABILIDAD PENALIZADA DE QUE SEAN MUESTREADOS (qi) AL DATAFRAME ##########
prob_estimadas$p.muestreo.beta_0.5 <- qast

########## GRAFICO CON PROBABILIDAD DE DETECCION PARA CADA BETA desde 0 a 1 ##########

prob_detect_beta <- data.frame(
  "beta" = seq(from = 0, to = 1, by = 0.1),
  "prob_detect" = c(0.1605504, 0.1510808, 0.1488292, 0.1441308, 0.1396503, 0.1381262, 0.134312, 0.1336709,
                    0.1297573, 0.1297573, 0.1297573))
ggplot(prob_detect_beta, aes(x = beta, y = prob_detect*100)) +
  geom_line() + 
  scale_x_continuous(limits = c(0, 1), breaks = c(prob_detect_beta$beta)) +
  scale_y_continuous(limits = c(12.5, 16.5), breaks = seq(from = 12.5, to = 16.5, by = 0.5)) +
  ylab(label = "Probabilidad de deteccion")

########## MUESTREO PROPORCIONAL ##########
# objective function
objective_function <- function( q, p ){
  
  val = sum(p*log(q))
  return (-val)
}

constr_function <- function( q, p ){
  
  return (sum(q)-1)
}

# Solve using NLOPT_LN_COBYLA without gradient information
res <- nloptr(  x0 = rep(1/95, 95), #rep(1/4,4),
                eval_f = objective_function,
                lb = rep(0, 95), #rep(0,4),
                #                ub = c(Inf,Inf),
                eval_g_ineq = constr_function,
                opts = list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel"=1.0e-8),
                p = prob_estimadas[, 2]
)

res

## En ambos casos, comparar la probabilidad de detección. 
## En el muestreo penalizado por entropía es una función de beta
## En el muestreo proporcional es simplemente un número (porque hay una única solución)

qast = res$solution
p = prob_estimadas[, 2]
prob_deteccion = sum(p*qast)
prob_deteccion

min(qast) #prob. de muestrear al que muestreo menos

########## AGREGO LA PROBABILIDAD DE QUE SEAN MUESTREADOS (qi) AL DATAFRAME ##########
prob_estimadas$p.muestreo.prop <- qast
