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
       WVPlots,
       patchwork) ## bootstrapping
getwd()

## Cambiar esta ruta por el directorio de cada uno
ifelse(getwd()=="/Users/camilabeltran",
       wd <- "/Users/camilabeltran/OneDrive/Educación/PEG - Uniandes/BDML/GitHub/problem_set/Problem_set_1",
       ifelse(getwd()=="C:/Users/Juan/Documents/Problem_set_1/scripts",
              wd <-  "C:/Users/Juan/Documents/Problem_set_1",
                ifelse(test = getwd()=="C:/Users/juanp.rodriguez/Documents/GitHub/Problem_set_1",
                       wd <- "C:/Users/juanp.rodriguez/Documents/GitHub/Problem_set_1",
                       wd <- "/Users/aleja/Documents/Maestría Uniandes/Clases/Big Data y Machine Learning/Repositorios Git Hub/Problem_set_1")))

wd <- "C:/Users/User/OneDrive - Universidad de los andes/Big Data y Machine Learning/Problem_set_1/Problem_set_1"

#IMPORTANTE: Todos los resultados, variables y gráficos se encuentran alojados en la siguiente imagen, para cargarla:
setwd(paste0(wd,"/stores"))
load("03_Age-Wage Profile.R")

#A continuación, encontrarán el código realizado para llegar a los resultados que se encuentran cargados en la imagen:

#cargar la base de datos a través de image 
load("data.R")
}


#### 2. Age - Wage Profile ----
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

Peak_age_mod_simple <- Peak_age_fun(age_1 = model_Age_wage$coefficients[2],age_2 = model_Age_wage$coefficients[3]) #Modelo 1 (46 años)
Peak_age_mod_cont <- Peak_age_fun(age_1 = model_Age_wage_cont$coefficients[2],age_2 = model_Age_wage_cont$coefficients[3]) #Modelo 2 (60 años)


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

#### 3. Bootstrap estimation ----
## Modelo Simple
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
boot_results_mod_simple <- boot(data_tibble, Age_Wage_Profile_fn, R = 1000)
Dist_Peak_age_mod_simple <- as.data.frame(boot_results_mod_simple$t)
hist(Dist_Peak_age_mod_simple$V1) #distribucion del valor maximo de la edad con bootstrap

## Histograma - Modelo Simple
Hist_mod_simp <- ggplot(Dist_Peak_age_mod_simple, aes(x = V1)) +
  geom_histogram(bins = 50, color = "white", fill = "#528B8B") +
  labs(x ='Edad', y='Frecuencia', title = "Panel A: Modelo simple")+
  geom_vline(aes(xintercept = Peak_age_mod_simple), color = "mistyrose", linewidth = 1)+
  theme_minimal()
Hist_mod_simp

quantile(Dist_Peak_age_mod_simple$V1,0.025) #percentil 2.5 (47.16848)
quantile(Dist_Peak_age_mod_simple$V1,0.975) #percentil 97.5 (49.35956)


Age_wage_P_plot <- ggplot(data_tibble, aes(x = age, y = log_w)) +
  geom_point(aes(color = "Real"), alpha = 0.5) +  # Puntos para valores reales
  geom_line(aes(y = predicted, color = "Predicho"), linewidth = 1) +  # Línea para valores predichos
  scale_color_manual(values = c("Real" = "mistyrose", "Predicho" = "#528B8B"),name="") +  # Colores de puntos y líneas
  labs(title = "Panel A: Modelo simple",
       x = "Edad",
       y = "Ln(Salario)") +
  theme_minimal()
print(Age_wage_P_plot)

}

## Modelo Complejo
{
  #Intervalos de confianza con bootstrap
  Age_Wage_Profile_mod_comp<-function(data,index){
    f <- lm(log_w ~ age + age2 + female + informal + oficio_factor + maxEducLevel_factor + hoursWorkUsual + estrato1_factor + sizeFirm_factor, data = data_tibble, subset = index)
    b1 <- f$coefficients[2]
    b2 <- f$coefficients[3]
    max_edad <- -(b1)/(2*b2)
    return(max_edad)
  }
  
  #Verifiquemos que la función... funciona!
  Age_Wage_Profile_mod_comp(data_tibble,1:nrow(data_tibble))
  
  #Se utiliza la funcion boot para estimar la regresion con bootstrap
  set.seed(5382)
  boot_results_mod_cont <- boot(data_tibble, Age_Wage_Profile_mod_comp, R = 1000)
  Dist_Peak_age_mod_cont <- as.data.frame(boot_results_mod_cont$t)
  hist(Dist_Peak_age_mod_cont$V1) #distribucion del valor maximo de la edad con bootstrap
  
  ## Histograma - Modelo Simple
  Hist_mod_cont <- ggplot(Dist_Peak_age_mod_cont, aes(x = V1)) +
    geom_histogram(bins = 50, color = "white", fill = "steelblue") +
    labs(x ='Edad', y='Frecuencia', title = "Panel B: Modelo con controles")+
    geom_vline(aes(xintercept = Peak_age_mod_cont), color = "seashell3", linewidth = 1)+
    theme_minimal()
  Hist_mod_cont
  
  quantile(Dist_Peak_age_mod_cont$V1,0.025) #percentil 2.5 (47.16848)
  quantile(Dist_Peak_age_mod_cont$V1,0.975) #percentil 97.5 (49.35956)
  
  
  Age_wage_P_plot_cont <- ggplot(data_tibble, aes(x = age, y = log_w)) +
    geom_point(aes(color = "Real"), alpha = 0.5) +  # Puntos para valores reales
    geom_line(aes(y = predicted_cont, color = "Predicho"), linewidth = 0.8) +  # Línea para valores predichos
    scale_color_manual(values = c("Real" = "seashell3", "Predicho" = "steelblue"),name="") +  # Colores de puntos y líneas
    labs(title = "Panel B: Modelo con controles",
         x = "Edad",
         y = "Ln(Salario)") +
    theme_minimal()
  print(Age_wage_P_plot_cont)
}

### Panel 1
Hist_mod_simp + Hist_mod_cont

### Panel 2
Age_wage_P_plot + Age_wage_P_plot_cont
# Volvemos al estado original
par(mfrow = c(1, 1))


save.image("03_Age-Wage Profile.RData")
