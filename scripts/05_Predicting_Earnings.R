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
   male_data_tibble,male_plot,predicted_data,female,res1,female_fn,male_fn,female_plot,male_female_plot,peak_age_gender_plot,
   Age_wage_P_plot,Age_Wage_Profile_fn,boot_results03,box_plot,data_est_des,data_low,data_frame,data_table_missing,Dif_sex,dummy,
   For_estra,I_F_E_longer,Inf_estra,Inf_For_estr,max_educ,mean_wage,model_Age_wage,model_Age_wage_cont,des_vars,err,err_cont,
   err2,err2_cont,Matrix_summary,rmse,rmse_cont,Age_Wage_Profile_fn,Peak_age_fun)


#- a | División de la muestra --------------------------------------------------------------

#En las secciones anteriores, se estimó algunas especificaciones teniendo en cuenta la inferencia. 
#En esta subsección, evaluaremos el poder predictivo de estas especificaciones.

{
#Divida la muestra en dos: una muestra de capacitación (70%) y una de prueba (30%). 
#(No olvide establecer una semilla para lograr reproducibilidad. En R, por ejemplo, puede usar set.seed(10101), 
#donde 10101 es la semilla).

#Para replicabilidad seteamos la semilla
set.seed(10109)    

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
  
### Entrenando a los modelos ###  
  
  
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

#Modelo 5 -> agregamos experiencia, horas trabajadas al cuadrado y una interacción entre female y jefe de hogar
forma_5<- log_w ~ age + age2 + female + informal + oficio_factor + maxEducLevel_factor + hoursWorkUsual + 
  estrato1_factor + sizeFirm_factor + experiencia + hoursWorkUsual^2 + female:Jefe_h + female:experiencia + female:Autoempleado

modelo5a <- lm(forma_5,
               data = training)

#Rendimiento fuera de muestra
predictions <- predict(modelo5a, testing)

score5a<- RMSE(predictions, testing$log_w )
score5a 

#Modelo 6 -> quitamos informal, size firm factor y agregamos experiencia, experiencia al cuadrado, una interacción entre female y jefe de hogar, autoempleado y una interacción entre female y autoempleado
forma_6<- log_w ~ age + age2 + female + oficio_factor + maxEducLevel_factor + hoursWorkUsual + 
  estrato1_factor + experiencia + experiencia^2 + female:Jefe_h + Autoempleado + female:Autoempleado

modelo6a <- lm(forma_6,
               data = training)

#Rendimiento fuera de muestra
predictions <- predict(modelo6a, testing)

score6a<- RMSE(predictions, testing$log_w )
score6a 

#Modelo 7 -> agregamos experiencia, experiencia al cuadrado, experiencia al cubo, hours work usual al cuadrado y el logaritmo de la edad
forma_7<- log_w ~ age + age2 + female + informal + oficio_factor + maxEducLevel_factor + 
  hoursWorkUsual + estrato1_factor + sizeFirm_factor + experiencia + experiencia^2 + experiencia^3  + hoursWorkUsual^2 +
  log(age) + Jefe_h

modelo7a <- lm(forma_7,
               data = training)

#Rendimiento fuera de muestra
predictions <- predict(modelo7a, testing)

score7a<- RMSE(predictions, testing$log_w )
score7a 

#Modelo 8 -> agregamos experiencia al cuadrado y el logaritmo de las hoursWorkUsual
forma_8<- log_w ~ age + age2 + female + informal + oficio_factor + maxEducLevel_factor + 
  hoursWorkUsual + estrato1_factor + sizeFirm_factor + experiencia^2 + log(hoursWorkUsual)

modelo8a <- lm(forma_8,
               data = training)

#Rendimiento fuera de muestra
predictions <- predict(modelo8a, testing)

score8a<- RMSE(predictions, testing$log_w )
score8a 

#Modelo 9 ->  quitamos female, informal, hoursWorkUsual y agregamos el logaritmo de hoursWorkUsual, experiencia al cubo y el log de edad
forma_9<- log_w ~ age + age2 + oficio_factor + maxEducLevel_factor + 
  estrato1_factor + sizeFirm_factor + log(hoursWorkUsual) + experiencia^3 + log(age) + female:Jefe_h + Autoempleado

modelo9a <- lm(forma_9,
               data = training)

#Rendimiento fuera de muestra
predictions <- predict(modelo9a, testing)

score9a<- RMSE(predictions, testing$log_w )
score9a 

#mujer y estado civil o jefe de hogar

## K-Fold Cross-Validation ##

#Queremos comparar también a través de validación cruzada los modelos

control <- trainControl(
  method = "cv", ## Define the method for cross validation 
  number = 5) ## the number fof folds. 

#Modelo 1
modelo1b <- train(forma_1,  ## define the functional form (i.e the variable to predict and the features)
                  data = data_tibble,  # the data frame
                  method = 'lm',  # the method
                  trControl= control)  # input our cross validation method. 
modelo1b

#Revisemos el rendimiento en cada k-fold
modelo1b$resample

#Guardemos el RMSE
score1b<- mean(modelo1b$resample$RMSE)


#Modelo 2
modelo2b <- train(forma_2,  ## define the functional form (i.e the variable to predict and the features)
                  data = data_tibble,  # the data frame
                  method = 'lm',  # the method
                  trControl= control)  # input our cross validation method. 
modelo2b

#Revisemos el rendimiento en cada k-fold
modelo2b$resample

#Guardemos el RMSE
score2b<- mean(modelo2b$resample$RMSE)

#Modelo 3
modelo3b <- train(forma_3,  ## define the functional form (i.e the variable to predict and the features)
                  data = data_tibble,  # the data frame
                  method = 'lm',  # the method
                  trControl= control)  # input our cross validation method. 
modelo3b

#Revisemos el rendimiento en cada k-fold
modelo3b$resample

#Guardemos el RMSE
score3b<- mean(modelo3b$resample$RMSE)


#Modelo 4
modelo4b <- train(forma_4,  ## define the functional form (i.e the variable to predict and the features)
                  data = data_tibble,  # the data frame
                  method = 'lm',  # the method
                  trControl= control)  # input our cross validation method. 
modelo4b

#Revisemos el rendimiento en cada k-fold
modelo4b$resample

#Guardemos el RMSE
score4b<- mean(modelo4b$resample$RMSE)


#Modelo 5
modelo5b <- train(forma_5,  ## define the functional form (i.e the variable to predict and the features)
                  data = data_tibble,  # the data frame
                  method = 'lm',  # the method
                  trControl= control)  # input our cross validation method. 
modelo5b

#Revisemos el rendimiento en cada k-fold
modelo5b$resample

#Guardemos el RMSE
score5b<- mean(modelo5b$resample$RMSE)

#Modelo 6
modelo6b <- train(forma_6,  ## define the functional form (i.e the variable to predict and the features)
                  data = data_tibble,  # the data frame
                  method = 'lm',  # the method
                  trControl= control)  # input our cross validation method. 
modelo6b

#Revisemos el rendimiento en cada k-fold
modelo6b$resample

#Guardemos el RMSE
score6b<- mean(modelo6b$resample$RMSE)

#Modelo 7
modelo7b <- train(forma_7,  ## define the functional form (i.e the variable to predict and the features)
                  data = data_tibble,  # the data frame
                  method = 'lm',  # the method
                  trControl= control)  # input our cross validation method. 
modelo7b

#Revisemos el rendimiento en cada k-fold
modelo7b$resample

#Guardemos el RMSE
score7b<- mean(modelo7b$resample$RMSE)


#Modelo 8
modelo8b <- train(forma_8,  ## define the functional form (i.e the variable to predict and the features)
                  data = data_tibble,  # the data frame
                  method = 'lm',  # the method
                  trControl= control)  # input our cross validation method. 
modelo8b

#Revisemos el rendimiento en cada k-fold
modelo8b$resample

#Guardemos el RMSE
score8b<- mean(modelo8b$resample$RMSE)

#Modelo 9
modelo9b <- train(forma_9,  ## define the functional form (i.e the variable to predict and the features)
                  data = data_tibble,  # the data frame
                  method = 'lm',  # the method
                  trControl= control)  # input our cross validation method. 
modelo9b

#Revisemos el rendimiento en cada k-fold
modelo9b$resample

#Guardemos el RMSE
score9b<- mean(modelo9b$resample$RMSE)


### Comparación ###

## Guardamos todos los RMSE en un data set: 
scores<- data.frame( Model= c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                     RMSE_vsa= c(score1a, score2a, score3a, score4a, score5a, score6a, score7a, score8a, score9a), 
                     RMSE_kfold= c(score1b, score2b, score3b, score4b, score5b, score6b, score7b, score8b, score9b))

scores

#Guardemos la tabla
setwd(paste0(wd,"/views"))
write.table(scores, "scores_todos_los_modelos.txt", sep = "\t", row.names = FALSE)


#Grafiquemos ambos approaches
RMSE_vsa= c(score1a, score2a, score3a, score4a, score5a, score6a, score7a, score8a, score9a)
RMSE_kfold= c(score1b, score2b, score3b, score4b, score5b, score6b, score7b, score8b, score9b)

scores<- data.frame( Model= rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9),2),
                     Approach= c(rep("Validation",9),rep("K-Fold",9)),
                     RMSE= c(RMSE_vsa, RMSE_kfold))

ggplot(scores, aes(x = Model, y = RMSE, col = Approach)) +
  geom_line(size = 0.5) +
  labs(x = "Modelo",
       y = "RMSE") +
  scale_color_manual(name = "Enfoque", values = c("Validation" = "turquoise4", "K-Fold" = "lightsalmon")) +
  theme_minimal()


}

#- c | Errores de predicción --------------------------------------------------------------

{
#Para la especificación con el error de predicción más bajo, explore aquellas observaciones que parecen "errar en el blanco". 
#Para hacerlo, calcule los errores de predicción en la muestra de prueba y examine su distribución. 
#¿Hay alguna observación en los extremos de la distribución del error de predicción? 
#¿Son estos valores atípicos personas potenciales que la DIAN debería investigar o son simplemente el producto de 
#un modelo defectuoso?
  
#Hacemos un rápido chequeo para ver cuál es el modelo con el error de predicción más bajo
indice_min_k <- which.min(RMSE_kfold) 
indice_min_v <- which.min(RMSE_vsa)
indice_min_k
indice_min_v
#En ambos casos es el modelo 9

#Calculemos los errores de predicción en la muestra de prueba

#Primero volvemos a sacar las predicciones de nuestro modelo 9
predictions <- predict(modelo9a, testing)
predictions <- as.data.frame(predictions)

## Hallamos el leverage 
testing <- cbind(testing,predictions)
testing <- rename(testing,leverage = predictions)


## Errores de predicción
testing <- testing %>% mutate(errores_pred= log_w - leverage)


N <- nrow(testing)

testing$id<- seq(1 , N)

a<- ggplot(testing , aes(y = leverage , x = id , color= age, shape= as.factor(female) )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Observations",  
       y = "Leverage",
       title = "") # labels


b<- ggplot(testing , aes(y = leverage , x = errores_pred  )) +
  geom_point() + # add points
  theme_bw() + #black and white theme
  labs(x = "Errores de predicción",  
       y = "Leverage",
       title = "") # labels


# Arrange the ggplot2 plots side by side using grid.arrange()
grid.arrange(a, b, ncol = 2)

#Examinemos la distribución de los errores de predicción
##boxplot

ggplot(data= testing, 
       mapping = aes(y=errores_pred, x="")) +
  theme_bw() +
  geom_boxplot()  +
  ggtitle("")+
  ylab("Errores de predicción")+
  xlab("")

#Grafiquemos también la densidad de la distribución
ggplot(data = testing, aes(x = errores_pred)) +
  geom_density(fill = "turquoise4", alpha = 0.8, color = NA) +
  labs(title = "",
       x = "Errores de Predicción",
       y = "Densidad") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

#Identifiquemos los outliers que se encuentren en los extremos de la distribución del error de predicción

#Primera definición: cuantiles

low <- quantile(testing$errores_pred, 0.01)
up <- quantile(testing$errores_pred, 0.99)
low
up


b<-ggplot(data= testing, 
          mapping = aes(y=errores_pred, x="")) +
  theme_bw() +
  geom_boxplot()  +
  ggtitle("Outliers basados en cuantiles")+
  labs(title = "Panel A: Outliers basados en cuantiles") +
  ylab("Errores de Predicción")+
  xlab("")


b <- b + geom_hline(yintercept = low,linetype="solid",color="cyan4",linewidth=0.7) +
  geom_hline(yintercept = up,linetype="solid",color="cyan4",linewidth=0.7)


# Revisemos cuántas observaciones hay por debajo de low
observaciones_por_debajo_1 <- sum(testing$errores_pred < low)

# Revisemos cuántas observaciones hay por encima de up
observaciones_por_encima_1 <- sum(testing$errores_pred > up)

# Mostrar el número de observaciones por debajo de low y por encima de up
observaciones_por_debajo_1
observaciones_por_encima_1

#50 en ambos extremos


##Segunda definición: desviaciones estándar
c<-ggplot(data= testing, 
          mapping = aes(y=errores_pred, x="")) +
  theme_bw() +
  geom_boxplot()  +
  ggtitle("")+
  labs(title = "Panel B: Outliers basados en desviación estándar") +
  ylab("Errores de Predicción")+
  xlab("")

low <- mean(testing$errores_pred) - 2* sd(testing$errores_pred)
up <- mean(testing$errores_pred) + 2* sd(testing$errores_pred)

c <- c + geom_hline(yintercept = low,linetype="solid",color="chocolate1",size=0.7) +
  geom_hline(yintercept = up,linetype="solid",color="chocolate1",size=0.7)

#Miremos ambas definiciones juntas
grid.arrange(b, c, ncol = 2)

# Revisemos cuántas observaciones hay por debajo de low
observaciones_por_debajo_2 <- sum(testing$errores_pred < low)

# Revisemos cuántas observaciones hay por encima de up
observaciones_por_encima_2 <- sum(testing$errores_pred > up)

# Mostrar el número de observaciones por debajo de low y por encima de up
observaciones_por_debajo_2
observaciones_por_encima_2

#152 por debajo y 103 por encima


#¿Son estos valores atípicos personas potenciales que la DIAN debería investigar o son simplemente el producto de 
#un modelo defectuoso?

#Los investigados por la DIAN serían los underreporters de su ingreso del Panel A, es decir, los outliers por debajo del 1%

}

#- d | LOOCV --------------------------------------------------------------

{
#Para los dos modelos con el error predictivo más bajo en la sección anterior, calcule el error predictivo utilizando 
#la validación cruzada de dejar uno fuera (LOOCV). Compare los resultados del error de prueba con los obtenidos con el 
#enfoque del conjunto de validación y explore los vínculos potenciales con la estadística de influencia.

#Revisemos cual es el segundo menor en ambos approaches
RMSE_kfold_sin_min <- RMSE_kfold[-indice_min_k]
RMSE_vsa_sin_min <- RMSE_vsa[-indice_min_v]
  
# Encontrar el índice del segundo mínimo valor 
indice_segundo_min_k <- which.min(RMSE_kfold_sin_min)
indice_segundo_min_v <- which.min(RMSE_vsa_sin_min)
indice_segundo_min_k
indice_segundo_min_v
#En ambos casos es el modelo 5

#De esta manera, en esta subsección vamos a trabajar con los modelos 9 y 5

### Validación LOOCV ###

#Primero paralelicemos para evitar tanto tiempo
num_cores <- detectCores()  # Detecta automáticamente la cantidad de núcleos de CPU
registerDoParallel(num_cores)

control <- trainControl(
  method = "LOOCV",## input the method Leave One Out Cross Validation
  allowParallel = TRUE) #paraleliza


#Modelo 9
modelo9c <- train(forma_9,
                  data = data_tibble,
                  method = 'lm', 
                  trControl= control)
modelo9c

#Podemos obtener la predicción en cada iteración (observación) usando
head(modelo9c$pred)

#Guardamos el RMSE
score9c<-RMSE(modelo9c$pred$pred, data_tibble$log_w)


#Modelo 5
modelo5c <- train(forma_5,
                  data = data_tibble,
                  method = 'lm', 
                  trControl= control)
modelo5c

#Podemos obtener la predicción en cada iteración (observación) usando
head(modelo5c$pred)

#Guardamos el RMSE
score5c<-RMSE(modelo5c$pred$pred, data_tibble$log_w)

#Compare los resultados del error de prueba con los obtenidos con el 
#enfoque del conjunto de validación y explore los vínculos potenciales con la estadística de influencia.

### Comparación ###
scores_modelos_menor_error<- data.frame( Model= c(5, 9),
                                         RMSE_vsa= c(score5a, score9a),
                                         RMSE_kfold= c(score5b, score9b),
                                         RMSE_loocv= c(score5c, score9c))

scores_modelos_menor_error

#Grafiquemos los tres approaches
RMSE_vsa= c(score5a, score9a)
RMSE_kfold= c(score5b, score9b)
RMSE_loocv= c(score5c, score9c)

scores_modelos_menor_error<- data.frame( Model= rep(c(5, 9),3),
                     Approach= c(rep("Validation",2),rep("K-Fold",2),rep("LOOCV",2)),
                     RMSE= c(RMSE_vsa, RMSE_kfold, RMSE_loocv))

ggplot(scores_modelos_menor_error, ) + 
  geom_line(aes(x=Model,y=RMSE,col=Approach), size=0.5)+
  theme_bw() 

}
ggplot(scores_modelos_menor_error, ) + 
  geom_line(aes(x=Model,y=RMSE,col=Approach), size=0.5)+
  theme_bw() 

ggplot(scores_modelos_menor_error, aes(x = Model, y = RMSE, col = Approach)) +
  geom_line(size = 0.5) +
  labs(x = "Modelo",
       y = "RMSE") +
  scale_color_manual(name = "Enfoque", values = c("Validation" = "darkblue", "K-Fold" = "darkred", "LOOCV" = "darkgreen")) +
  theme_minimal()

setwd(paste0(wd,"/views"))
write.table(scores_modelos_menor_error, "scores_modelos_menor_error.txt", sep = "\t", row.names = FALSE)

#setwd(paste0(wd,"/stores"))
#save.image("05_predicting_earnings.RData")
