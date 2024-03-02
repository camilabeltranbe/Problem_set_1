#################################################
########### Punto 5 -Problem set ################
#################################################

rm(list = ls()) #se borra la memoria
library(pacman) 
p_load(tidyverse, # tidy-data (ggplot y Tidyverse)
       skimr, # summary data
       visdat, ## visualizing missing data
       corrplot, ## Correlation Plots 
       stargazer, # tables/output to TEX.
       readxl, # importar Excel
       writexl, # exportar Excel
       rio, # import/export data
       caret, # For predictive model assessment
       gridExtra, ## visualizing missing data
       data.table,
       boot) ## bootstrapping

#cambiar esta ruta por el directorio de cada uno
ifelse(getwd()=="/Users/camilabeltran",
       wd <- "/Users/camilabeltran/OneDrive/Educación/PEG - Uniandes/BDML/GitHub/problem_set/Problem_set_1",
       wd <- "/Users/aleja/Documents/Maestría Uniandes/Clases/Big Data y Machine Learning/Repositorios Git Hub/Problem_set_1/")


##NO FUNCIONA###
#IMPORTANTE: Todos los resultados, variables y gráficos se encuentran alojados en la siguiente imagen, para cargarla:
setwd(paste0(wd,"/stores"))
load("05_predicting_earnings.R")

#A continuación, encontrarán el código realizado para llegar a los resultados que se encuentran cargados en la imagen:

#cargar la base de datos a través de image 
setwd(paste0(wd,"/stores"))
load("gender_gap_earnings.R")
#La base cargada previamente contiene estimaciones correspondientes al punto anterior:
#Como la regresión lineal de log_w y female sin controles. Asimismo se tiene la estimación
# con controles como: 
#equal_pay_ols <- lm(log_w ~ female + age + maxEducLevel_factor + hoursWorkUsual + oficio_factor, data = data_tibble)
#equal_pay_ols

#data_tibble <- as_tibble(data) 
#view(head(data_tibble))

#Quitamos bases que no necesitamos
rm(age_wage_female,age_wage_male,boot_rf,boot_rm,boot_r,earnings_gap,equal_pay_fwl,equal_pay_ols,female_data_tibble,gender_plot,
   male_data_tibble,male_plot,predicted_data,female,res1,female_fn,male_fn,female_plot)


#- a | División de la muestra --------------------------------------------------------------

#En las secciones anteriores, se estimó algunas especificaciones teniendo en cuenta la inferencia. 
#En esta subsección, evaluaremos el poder predictivo de estas especificaciones.

{
#Divida la muestra en dos: una muestra de capacitación (70%) y una de prueba (30%). 
#(No olvide establecer una semilla para lograr reproducibilidad. En R, por ejemplo, puede usar set.seed(10101), 
#donde 10101 es la semilla).

#Agregamos algunas variables necesarias para las especificaciones del punto b  
data_tibble <- data_tibble %>% mutate (estrato1_factor= as.factor(estrato1))
data_tibble <- data_tibble %>% mutate (sizeFirm_factor= as.factor(sizeFirm))
data_tibble <- data_tibble %>% mutate (oficio_factor= as.factor(oficio))
data_tibble <- data_tibble %>% mutate (maxEducLevel_factor= as.factor(maxEducLevel))  
  
#Para replicabilidad seteamos la semilla
set.seed(10101)    

#Utilizamos la función createDataPartition para dividir aleatoriamente el conjunto de datos entre el entrenamiento y la prueba
#de acuerdo con la proporción

En_entrenamiento <- createDataPartition(
  y = data_tibble$log_w,  ## usamos el log del salario como outcome
  p = .70, ## The percentage of data in the
  list = FALSE
)    

training <- data_tibble[ En_entrenamiento,]
testing  <- data_tibble[-En_entrenamiento,]

#Chequeamos que este hecha la particion 70-30
nrow(training)/nrow(data_tibble)
nrow(testing)/nrow(data_tibble)
#Podemos ver que efectivamente sí está bien dividida la muestra

}

#- b | Comparación de rendimiento predictivo --------------------------------------------------------------

{
#Informar y comparar el rendimiento predictivo en términos del RMSE de todas las especificaciones anteriores con al menos 
#cinco (5) especificaciones adicionales que exploren no linealidades y complejidad.  
  
  
### Especificaciones anteriores ###  

#Modelo 1 -> primero modelo del punto 3
forma_1<- log_w ~ age + age2 
  
modelo1a <- lm(forma_1,
               data = training)

#Rendimiento fuera de muestra
predictions <- predict(modelo1a, testing)

score1a<- RMSE(predictions, testing$log_w )
score1a 

#Modelo 2 -> segundo modelo del punto 3 con controles   
forma_2<- log_w ~ age + age2 + female + informal + oficio_factor + maxEducLevel_factor + 
  hoursWorkUsual + estrato1_factor + sizeFirm_factor

modelo2a <- lm(forma_2,
               data = training)

#Rendimiento fuera de muestra
predictions <- predict(modelo2a, testing)

score2a<- RMSE(predictions, testing$log_w )
score2a 

#Modelo 3 -> primer modelo earnings gap del punto 4
forma_3<- log_w ~ female 

modelo3a <- lm(forma_3,
               data = training)

#Rendimiento fuera de muestra
predictions <- predict(modelo3a, testing)

score3a<- RMSE(predictions, testing$log_w )
score3a 

#Modelo 4 -> segundo modelo earnings gap con controles del punto 4  
forma_4<- log_w ~ female + age + maxEducLevel_factor + hoursWorkUsual + oficio_factor  

modelo4a <- lm(forma_4,
               data = training)

#Rendimiento fuera de muestra
predictions <- predict(modelo4a, testing)

score4a<- RMSE(predictions, testing$log_w )
score4a 

### Especificaciones adicionales ###

#Modelo 5    
forma_5<- log_w ~ age + age2 + female + informal + oficio_factor + maxEducLevel_factor + hoursWorkUsual + estrato1_factor + sizeFirm_factor 

modelo5a <- lm(forma_5,
               data = training)

#Rendimiento fuera de muestra
predictions <- predict(modelo5a, testing)

score5a<- RMSE(predictions, testing$log_w )
score5a 

#Modelo 6    
forma_6<- log_w ~ age + age2 + female + informal + oficio_factor + maxEducLevel_factor + 
  hoursWorkUsual + estrato1_factor + sizeFirm_factor 

modelo6a <- lm(forma_6,
               data = training)

#Rendimiento fuera de muestra
predictions <- predict(modelo6a, testing)

score6a<- RMSE(predictions, testing$log_w )
score6a 

#Modelo 7    
forma_7<- log_w ~ age + age2 + female + informal + oficio_factor + maxEducLevel_factor + 
  hoursWorkUsual + estrato1_factor + sizeFirm_factor 

modelo7a <- lm(forma_7,
               data = training)

#Rendimiento fuera de muestra
predictions <- predict(modelo7a, testing)

score7a<- RMSE(predictions, testing$log_w )
score7a 

#Modelo 8    
forma_8<- log_w ~ age + age2 + female + informal + oficio_factor + maxEducLevel_factor + 
  hoursWorkUsual + estrato1_factor + sizeFirm_factor 

modelo8a <- lm(forma_8,
               data = training)

#Rendimiento fuera de muestra
predictions <- predict(modelo8a, testing)

score8a<- RMSE(predictions, testing$log_w )
score8a 

#Modelo 9    
forma_9<- log_w ~ age + age2 + female + informal + oficio_factor + maxEducLevel_factor + 
  hoursWorkUsual + estrato1_factor + sizeFirm_factor 

modelo9a <- lm(forma_9,
               data = training)

#Rendimiento fuera de muestra
predictions <- predict(modelo9a, testing)

score9a<- RMSE(predictions, testing$log_w )
score9a 


## K-Fold Cross-Validation ##

  
}

#- c | Errores de predicción --------------------------------------------------------------

{
#Para la especificación con el error de predicción más bajo, explore aquellas observaciones que parecen "errar en el blanco". 
#Para hacerlo, calcule los errores de predicción en la muestra de prueba y examine su distribución. 
#¿Hay alguna observación en los extremos de la distribución del error de predicción? 
#¿Son estos valores atípicos personas potenciales que la DIAN debería investigar o son simplemente el producto de 
#un modelo defectuoso?
  
#revisar script andres leverage (penultima clase)  
}

#- d | LOOCV --------------------------------------------------------------

{
#Para los dos modelos con el error predictivo más bajo en la sección anterior, calcule el error predictivo utilizando 
#la validación cruzada de dejar uno fuera (LOOCV). Compare los resultados del error de prueba con los obtenidos con el 
#enfoque del conjunto de validación y explore los vínculos potenciales con la estadística de influencia.

}


save.image("05_predicting_earnings.R")
