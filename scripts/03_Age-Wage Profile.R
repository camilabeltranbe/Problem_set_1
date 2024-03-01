#################################################
########### Punto 3 -Problem set 1 ################
#################################################

#### 1. Librerias y directorio ----
{
rm(list = ls()) #se borra la memoria
library(pacman) 
p_load(tidyverse, # tidy-data (ggplot y Tidyverse)
       skimr, # summary data
       visdat, ## visualizing missing data
       corrplot, ## Correlation Plots 
       stargazer, # tables/output to TEX.
       readxl, # importar Excel
       writexl, # exportar Excel
       boot,
       WVPlots) ## bootstrapping
getwd()

## Cambiar esta ruta por el directorio de cada uno
ifelse(getwd()=="/Users/camilabeltran",
       wd <- "/Users/camilabeltran/OneDrive/Educación/PEG - Uniandes/BDML/GitHub/problem_set/Problem_set_1",
       ifelse(test = getwd()=="C:/Users/Juan/Documents/Problem_set_1/scripts",
              yes = wd <-  "C:/Users/Juan/Documents/Problem_set_1",
              no = wd <- "C:/Users/juanp.rodriguez/Documents/GitHub/Problem_set_1"))

#IMPORTANTE: Todos los resultados, variables y gráficos se encuentran alojados en la siguiente imagen, para cargarla:
setwd(paste0(wd,"/stores"))
load("03_Age-Wage Profile.R")

#A continuación, encontrarán el código realizado para llegar a los resultados que se encuentran cargados en la imagen:

#cargar la base de datos a través de image 
setwd(paste0(wd,"/stores"))
load("data_GEIH.RData")

data_tibble <- as_tibble(data) 
#view(head(data_tibble))
}

#### 2. Selección de variables e imputación de Missing values ----
{
  ## Seleccion de variables con las cuales vamos a trabjar 
  data_tibble<- data_tibble %>% 
    select(directorio, secuencia_p, orden, estrato1, sex, age, ocu, oficio, orden, totalHoursWorked,
           dsi, ie , formal, informal, sizeFirm , regSalud, maxEducLevel, ingtot,
           ingtotes,ingtotob, y_salary_m, y_total_m, hoursWorkUsual) 
  #view(head(data_tibble))
  # Se eliminaron 157 variables de la muestra inicial, dejando la nueva muestra con 23 variables.
  
  ## Filto de observaciones
  # "In this problem set, we will focus only on employed individuals older than eighteen (18) years old.
  # 
  table(data$dominio) ## Solo se está trabajando con individuos de Bogota
  data_tibble <- data_tibble %>%
    filter(age>=18 & ## Mayores de edad
             ocu==1) ## Empleados
  # Se eliminaron 15.635 observaciones (aprox. el 49%) de la muestra inicial, dejando 16.542 observaciones.
  #view(data_tibble)
  
  sum(is.na(data_tibble$y_salary_m))
  sum(is.na(data_tibble$y_total_m))
  
  #Ahora deseamos imputar los datos de aquellos individuos que se encuntran ocupados,
  #pero no reportan su ingreso.
  any(is.na(data_tibble$y_salary_m)) #observemos si existen Missing values en la variable salario
  sum(is.na(data_tibble$y_salary_m)) # observemos el numero total de missing values
  
  #Para realizar la imputación utilizaremos la variable de oficio de cada individuo ocupado en la muestra
  sum(is.na(data_tibble$oficio)) #Observemos si existen missing values - Rta: No existen!
  
  #se propone para este ejercicio de imputación, remplazar los missing, por el promedio 
  #del salario mensual de la variable oficio
  data_tibble <- data_tibble %>%
    group_by(oficio) %>%
    mutate(sal_imputado = ifelse(is.na(y_salary_m), mean(y_salary_m, na.rm = TRUE), y_salary_m))
  
  #Dado que el oficio 60 contiene solo 2 individuos los cuales no reportan salario
  #no existe valor promedio para imputar, por tanto se decide eliminar las 2 observaciones.
  data_tibble <- data_tibble %>%
    filter(!is.na(sal_imputado))
  
  #Ahora bien,queremos crear la variable Female, para ello tomaremos la variable sex 
  #que se encuentra en la encuesta
  data_tibble$female <- 1 - data_tibble$sex
  
  #creamos el logaritmo del salario mensual
  data_tibble$log_w=log(data_tibble$sal_imputado)
  
  #Verificar que no tenemos problemas con la transformación a log
  sum(is.nan(data_tibble$log_w))
}

#### 3. Age - Wage Profile ----
{
  ### Modelos
  ## Creación de nueva variable y modelo 
data_tibble$age2 <- (data_tibble$age)^2 #Creamos la variable age2
#poner la variable oficio como factor porque es categorica
data_tibble <- data_tibble %>% mutate (oficio_factor= as.factor(oficio))
data_tibble <- data_tibble %>% mutate (maxEducLevel_factor= as.factor(maxEducLevel))
data_tibble$female <- 1 - data_tibble$sex
data_tibble <- data_tibble %>% mutate (estrato1_factor= as.factor(estrato1))
data_tibble <- data_tibble %>% mutate (sizeFirm_factor= as.factor(sizeFirm))

  ## Modelos (Solo edad y con controles)
model_Age_wage <- lm(log_w ~ age + age2, data = data_tibble) #Realizamos la regresión
model_Age_wage_cont <- lm(log_w ~ age + age2 + female + informal + oficio_factor + maxEducLevel_factor + hoursWorkUsual + estrato1_factor + sizeFirm_factor, data = data_tibble) #Realizamos la regresión
summary(model_Age_wage_cont)
  
  # Visualizacion de modelos 
stargazer(model_Age_wage, type = "text") # Modelo simple
stargazer(model_Age_wage_cont, type = "text") #Modelo completo
stargazer(model_Age_wage_cont, type = "text", keep = c("age", "age2", "female", "informal", "hoursWorkUsual") ) #Modelo completo
stargazer(model_Age_wage, model_Age_wage_cont, type = "text", keep = c("constant","age", "age2", "female", "informal", "hoursWorkUsual") ) #Modelo completo

# Peak-age
Peak_age_fun <- function(age_1, age_2) {
   Edad_P<- -(age_1/(2*age_2))
  return (Edad_P)
                                      }

Peak_age_fun(age_1 = 0.046,age_2 = -0.0005) #Modelo 1 (46 años)
Peak_age_fun(age_1 = 0.024,age_2 = -0.0002) #Modelo 2 (60 años)


  # Realiza predicciones con el modelo
data_tibble$predicted <- predict(model_Age_wage, newdata = data_tibble)
data_tibble$predicted_cont <- predict(model_Age_wage_cont, newdata = data_tibble)

  #Modelo predicted vs salario 
ggplot(data_tibble, aes(x = predicted_cont, y = log_w))+
  geom_point(color = "#00AFBB")+
  geom_abline(color ="darkblue")+
  xlab("Predicción")+
  ylab("Log(Salario)")+
  theme_minimal()


# Calculate error Modelo simple
err <- data_tibble$predicted - data_tibble$log_w
# Square the error vector
err2 <- err^2
# Take the mean, and sqrt it
(rmse <-sqrt(mean(err2))) # 0.683

# Calculate error Modelo Completo
err_cont <- data_tibble$predicted_cont - data_tibble$log_w
# Square the error vector
err2_cont <- err_cont^2
# Take the mean, and sqrt it
(rmse_cont <-sqrt(mean(err2_cont, na.rm = TRUE))) # 0.413
}

#### 4. Bootstrap estimation ----
{
#Intervalos de confianza con bootstrap
Age_Wage_Profile_fn<-function(data,index){
  f <- lm(log_w ~ age + age2, data = data_tibble, subset= index)
  b1 <- f$coefficients[2]
  b2 <- f$coefficients[3]
  max_edad <- -(b1)/(2*b2)
  return(max_edad)
    }

#Verifiquemos que la función... funciona!
Age_Wage_Profile_fn(data_tibble,1:nrow(data_tibble))

#Se utiliza la funcion boot para estimar la regresion con bootstrap
set.seed(5382)
boot_results03 <- boot(data_tibble, Age_Wage_Profile_fn, R = 100)
boot_results03 <- as.data.frame(boot_results03$t)
hist(boot_results03$V1) #distribucion del valor maximo de la edad con bootstrap
quantile(boot_results03$V1,0.025) #percentil 2.5 (47.16848)
quantile(boot_results03$V1,0.975) #percentil 97.5 (49.35956)


Age_wage_P_plot <- ggplot(data_tibble, aes(x = age, y = log_w)) +
  geom_point(aes(color = "Real"), alpha = 0.5) +  # Puntos para valores reales
  geom_line(aes(y = predicted, color = "Predicho"), linewidth = 1) +  # Línea para valores predichos
  scale_color_manual(values = c("Real" = "gray", "Predicho" = "darkblue"),name="") +  # Colores de puntos y líneas
  labs(title = "Perfil edad-ingreso: Bogotá",
       x = "Edad",
       y = "Ln(salario)") +
  theme_minimal()
print(Age_wage_P_plot)
}

save.image("03_Age-Wage Profile.R")
