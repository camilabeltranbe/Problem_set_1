#################################################
########### Punto 4 -Problem set ################
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
       boot) ## bootstrapping

#cambiar esta ruta por el directorio de cada uno 
#ruta julian
#wd <-"C:/Users/User/OneDrive - Universidad de los andes/Big Data y Machine Learning/Problem_set_1"
#ruta camila
wd <- "/Users/camilabeltran/OneDrive/Educación/PEG - Uniandes/BDML/GitHub/problem_set/Problem_set_1"
#cargar la base de datos a través de image 
setwd(paste0(wd,"/stores"))
load("data_GEIH.RData")

data_tibble <- as_tibble(data) 
view(head(data_tibble))

########## Selección de variables e imputación de Missing values ##########
{
## Seleccion de variables con las cuales vamos a trabjar -(si no sirve, hacer con dataframe)
data_tibble<- data_tibble %>% 
  select(directorio, secuencia_p, orden, estrato1, sex, age, ocu, oficio, orden, totalHoursWorked,
         dsi, ie , formal, informal, sizeFirm , regSalud, maxEducLevel, ingtot,
         ingtotes,ingtotob, y_salary_m, y_total_m, hoursWorkUsual) 
view(head(data_tibble))
# Se eliminaron 157 variables de la muestra inicial, dejando la nueva muestra con 21 variables.

## Filto de observaciones
# BORRAR: solo guia - "In this problem set, we will focus only on employed individuals older than eighteen (18) years old.
# ¿Preguntar si el filtro está bien?
table(data$dominio) ## Solo se está trabajando con individuos de Bogota
data_tibble <- data_tibble %>%
  filter(age>=18 & ## Mayores de edad
           ocu==1) ## Empleados
# Se eliminaron 15.635 observaciones (aprox. el 49%) de la muestra inicial, dejando 16.542 observaciones.
view(data_tibble)

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
#todos_numeros <- all(is.numeric(data_tibble$log_w))
#print(todos_numeros)
#rm(todos_numeros)
}
####################### a) Regresión ######################################
{
earnings_gap <- lm(log_w ~ female, data = data_tibble)

# Mostrar resultados con stargazer
stargazer(earnings_gap, type = "text") #quitar el "text" si se quiere en LATEX
}
####################### b) Equal Pay for Equal Work? ######################
{
#poner la variable oficio como factor porque es categorica
data_tibble<- data_tibble %>% mutate (oficio_factor= as.factor(oficio)) 

# usando OLS  
equal_pay_ols <- lm(log_w ~ female + age + maxEducLevel + hoursWorkUsual + oficio_factor, data = data_tibble)
equal_pay_ols$coefficients[2] #extrae el coeficiente de interes
#Donde:
#maxEducLevel	= max. education level attained
#hoursWorkUsual = usual weekly hours worked - principal occ.
#oficio
#age=edad

# usando FWL
res1 <- residuals(lm(log_w ~ age + maxEducLevel + hoursWorkUsual + oficio_factor, data = data_tibble)) 
res2 <- residuals(lm(female ~ age + maxEducLevel + hoursWorkUsual + oficio_factor, data = data_tibble))
equal_pay_fwl <- lm(res1 ~ res2)
coefficients(equal_pay_fwl)[2]

# Mostrar resultados con stargazer
stargazer(equal_pay_fwl,type = "text",omit = "Constant",dep.var.labels = "Ln(salario)",covariate.labels = "mujer")

# ii) FWL- Bootstrap

###########Opcion 1 - sin control ofico #############
# funcion para realizar bootstrap
female_fn<-function(data,index){
  res1 <- residuals(lm(log_w ~ age + maxEducLevel + hoursWorkUsual + oficio_factor, data = data_tibble, subset=index)) 
  res2 <- residuals(lm(female ~ age + maxEducLevel + hoursWorkUsual + oficio_factor, data = data_tibble, subset=index))
  coef(lm(res1 ~ res2, subset=index))[2] #retorna el segundo coeficiente de la regresion
}
#Verifiquemos que la función... funciona!
female_fn(data_tibble,1:nrow(data_tibble))

#Se utiliza la funcion boot para estimar la regresion con bootstrap
set.seed(5382)
boot_r <- boot(data_tibble, female_fn, R = 1000)
boot_r
}

####### c) plot age-wage profile / estimate the implied “peak ages”########
{
#Creamos variable de edad^2
data_tibble$age2 <- (data_tibble$age)^2

#Realizamos el age-wage profile para mujeres
age_wage_female <- lm(log_w ~ age + age2, data = data_tibble, subset= female==1) 
stargazer(age_wage_female, type = "text",omit = "Constant")

# Crear una nueva base de datos solo con observaciones de mujeres
female_data_tibble <- subset(data_tibble, sex == 0)

# Realiza predicciones con el modelo e intervalos de confianza
female_data_tibble$predicted <- predict(age_wage_female, newdata = female_data_tibble, interval = "confidence", level = 0.95)[,"fit"]
female_data_tibble$conf.low <- predict(age_wage_female, newdata = female_data_tibble, interval = "confidence", level = 0.95)[,"lwr"]
female_data_tibble$conf.high <- predict(age_wage_female, newdata = female_data_tibble, interval = "confidence", level = 0.95)[,"upr"]

female_plot <- ggplot(female_data_tibble, aes(x = age, y = log_w)) +
                geom_point(aes(color = "Real"), alpha = 0.5) +  # Puntos para valores reales
                geom_line(aes(y = predicted, color = "Predicho"), size = 1) +  # Línea para valores predichos
                geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = "Intervalo de Confianza"), 
                            fill = "red", alpha = 0.3) +  # Intervalos de confianza
                scale_color_manual(values = c("Real" = "gray", "Predicho" = "darkblue"),name="") +  # Colores de puntos y líneas
                labs(title = "Perfil edad-ingreso: mujeres",
                     x = "Edad",
                     y = "Ln(salario)") +
                theme_minimal() +
                theme(plot.background = element_rect(fill = "white")) # Establecer el fondo del gráfico como blanco
print(female_plot)

# Realizamos el age-wage profile para hombres
age_wage_male <- lm(log_w ~ age + age2, data = data_tibble, subset= sex==1) 
stargazer(age_wage_male, type = "text",omit = "Constant")

# Crear una nueva base de datos solo con observaciones de hombres
male_data_tibble <- subset(data_tibble, sex == 1)

# Realiza predicciones con el modelo e intervalos de confianza
male_data_tibble$predicted <- predict(age_wage_male, newdata = male_data_tibble, interval = "confidence", level = 0.95)[,"fit"]
male_data_tibble$conf.low <- predict(age_wage_male, newdata = male_data_tibble, interval = "confidence", level = 0.95)[,"lwr"]
male_data_tibble$conf.high <- predict(age_wage_male, newdata = male_data_tibble, interval = "confidence", level = 0.95)[,"upr"]

male_plot <- ggplot(male_data_tibble, aes(x = age, y = log_w)) +
             geom_point(aes(color = "Real"), alpha = 0.5) +  # Puntos para valores reales
             geom_line(aes(y = predicted, color = "Predicho"), size = 1) +  # Línea para valores predichos
             geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = "Intervalo de Confianza"), 
             fill = "red", alpha = 0.3) +  # Intervalos de confianza
             scale_color_manual(values = c("Real" = "gray", "Predicho" = "darkblue"),name="") +  # Colores de puntos y líneas
             labs(title = "Perfil edad-ingreso: hombres",
               x = "Edad",
               y = "Ln(salario)") +
             theme_minimal() +
             theme(plot.background = element_rect(fill = "white")) # Establecer el fondo del gráfico como blanco

print(male_plot)

# Edad maximizadora mujeres
female_age_max <- -(age_wage_female$coefficients["age"])/(2*age_wage_female$coefficients["age2"])
female_age_max # Resulta ser a los 44 años

# Edad maximizadora hombres
male_age_max <- -(age_wage_male$coefficients["age"])/(2*age_wage_male$coefficients["age2"])
male_age_max # Resulta ser a los 51 años

predicted_data <- rbind(female_data_tibble,male_data_tibble)
predicted_data$sex <- factor(predicted_data$sex, levels = c(0, 1), labels = c("Mujeres", "Hombres"))

# Plot de prediccion por edad y sexo
gender_plot <- ggplot(predicted_data, aes(x = age, y = predicted, color = sex)) +
                geom_line() +  # Líneas continuas
                geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = sex),show.legend = F, alpha = 0.3) +  # Banda de intervalo de confianza
                labs(title = "Ingreso por edad y sexo",
                   x = "Edad",
                   y = "Ingreso") +
                theme_minimal() +
                scale_color_manual(values = c("Mujeres" = "darkblue", "Hombres" = "darkred"),name="") + # Cambiar colores de las líneas
                scale_fill_manual(values = c("Mujeres" = "lightblue", "Hombres" = "pink")) + # Cambiar colores del relleno
                theme(plot.background = element_rect(fill = "white")) # Establecer el fondo del gráfico como blanco

print(gender_plot)

#Guardar los graficos
setwd(paste0(wd,"/Views"))
ggsave("age_wage_profile_hombres.png", male_plot, width = 10, height = 6, units = "in")
ggsave("age_wage_profile_mujeres.png", female_plot, width = 10, height = 6, units = "in")
ggsave("age_wage_profile_gender.png", gender_plot, width = 10, height = 6, units = "in")
}
