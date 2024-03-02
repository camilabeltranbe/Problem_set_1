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
       boot, # bootstrapping
       gridExtra,#para unir graficos
       ggplot2) #graficos 

#cambiar esta ruta por el directorio de cada uno
ifelse(getwd()=="/Users/camilabeltran",
       wd <- "/Users/camilabeltran/OneDrive/Educación/PEG - Uniandes/BDML/GitHub/problem_set/Problem_set_1",
       ifelse(getwd()=="C:/Users/Juan/Documents/Problem_set_1/scripts",
              wd <-  "C:/Users/Juan/Documents/Problem_set_1",
              ifelse(test = getwd()=="C:/Users/juanp.rodriguez/Documents/GitHub/Problem_set_1",
                     wd <- "C:/Users/juanp.rodriguez/Documents/GitHub/Problem_set_1",
                     wd <- "/Users/aleja/Documents/Maestría Uniandes/Clases/Big Data y Machine Learning/Repositorios Git Hub/Problem_set_1")))

#IMPORTANTE: Todos los resultados, variables y gráficos se encuentran alojados en la siguiente imagen, para cargarla:
setwd(paste0(wd,"/stores"))
load("gender_gap_earnings.R")

#A continuación, encontrarán el código realizado para llegar a los resultados que se encuentran cargados en la imagen:

#cargar la base de datos a través de image 
load("03_Age-Wage Profile.R")

####################### a) Regresión ######################################
{
earnings_gap <- lm(log_w ~ female, data = data_tibble)
# Mostrar resultados con stargazer
stargazer(earnings_gap, type = "text") #quitar el "text" si se quiere en LATEX
}
####################### b) Equal Pay for Equal Work? ######################
{
#poner la variable oficio como factor porque es categorica
data_tibble <- data_tibble %>% mutate (oficio_factor= as.factor(oficio))
data_tibble <- data_tibble %>% mutate (maxEducLevel_factor= as.factor(maxEducLevel))

# usando OLS  
equal_pay_ols <- lm(log_w ~ female + age + maxEducLevel_factor + hoursWorkUsual + oficio_factor, data = data_tibble)
equal_pay_ols$coefficients[2] #extrae el coeficiente de interes
#Donde:
#maxEducLevel_factor	= max. education level attained
#hoursWorkUsual = usual weekly hours worked - principal occ.
#oficio
#age=edad

# usando FWL
res1 <- residuals(lm(log_w ~ age + maxEducLevel_factor + hoursWorkUsual + oficio_factor, data = data_tibble)) 
female <- residuals(lm(female ~ age + maxEducLevel_factor + hoursWorkUsual + oficio_factor, data = data_tibble))
equal_pay_fwl <- lm(res1 ~ female)
coefficients(equal_pay_fwl)[2]

# Mostrar resultados con stargazer
stargazer(equal_pay_fwl,type = "text",omit = "Constant",dep.var.labels = "Ln(salario)",covariate.labels = "mujer")
stargazer(equal_pay_fwl,earnings_gap,type = "text",dep.var.labels = c("Ln(salario) con controles","Ln(salario) sin controles"),covariate.labels = "mujer")

# ii) FWL- Bootstrap
# funcion para realizar bootstrap
female_fn<-function(data,index){
  res1 <- residuals(lm(log_w ~ age + maxEducLevel_factor + hoursWorkUsual + oficio_factor, data = data_tibble, subset=index)) 
  res2 <- residuals(lm(female ~ age + maxEducLevel_factor + hoursWorkUsual + oficio_factor, data = data_tibble, subset=index))
  coef(lm(res1 ~ res2, subset=index))[2] #retorna el segundo coeficiente de la regresion
}
#Verifiquemos que la función... funciona!
female_fn(data_tibble,1:nrow(data_tibble))

#Se utiliza la funcion boot para estimar la regresion con bootstrap
set.seed(5382)
boot_r <- boot(data_tibble, female_fn, R = 1000)
boot_r
}
######## c) plot age-wage profile / estimate the implied “peak ages"#######
{
#Creamos variable de edad^2
data_tibble$age2 <- (data_tibble$age)^2

#Realizamos el age-wage profile para mujeres
#Crear una nueva base de datos solo con observaciones de mujeres
female_data_tibble <- subset(data_tibble, sex == 0)
age_wage_female <- lm(log_w ~ age + age2, data = female_data_tibble) 
stargazer(age_wage_female, type = "text",omit = "Constant")

#Realiza predicciones con el modelo
female_data_tibble$predicted <- predict(age_wage_female, newdata = female_data_tibble)

#Intervalos de confianza con bootstrap
female_fn<-function(data,index){
  f <- lm(log_w ~ age + age2, data = female_data_tibble, subset= index)
  b1 <- f$coefficients[2]
  b2 <- f$coefficients[3]
  max_edad <- -(b1)/(2*b2)
  return(max_edad)
}

#Verifiquemos que la función... funciona!
female_fn(female_data_tibble,1:nrow(female_data_tibble))

#Se utiliza la funcion boot para estimar la regresion con bootstrap
set.seed(5382)
boot_rf <- boot(female_data_tibble, female_fn, R = 1000)
boot_rf <- as.data.frame(boot_rf$t)
hist(boot_rf$V1) #distribucion del valor maximo de la edad con bootstrap
quantile(boot_rf$V1,0.025) #percentil 2.5 (42.53374)
quantile(boot_rf$V1,0.975) #percentil 97.5 (44.71811)

female_plot <- ggplot(female_data_tibble, aes(x = age, y = log_w)) +
                geom_point(aes(color = "Real"), alpha = 0.5) +  # Puntos para valores reales
                geom_line(aes(y = predicted, color = "Predicho"), linewidth = 1) +  # Línea para valores predichos
                scale_color_manual(values = c("Real" = "gray", "Predicho" = "darkblue"),name="") +  # Colores de puntos y líneas
                labs(title = "Panel B: Mujeres",
                     x = "Edad",
                     y = "Ln(salario)") +
                theme_minimal()
print(female_plot)

#Realizamos el age-wage profile para hombres
#Crear una nueva base de datos solo con observaciones de hombres
male_data_tibble <- subset(data_tibble, sex == 1)
age_wage_male <- lm(log_w ~ age + age2, data = male_data_tibble) 
stargazer(age_wage_male, type = "text",omit = "Constant")

#Realiza predicciones con el modelo
male_data_tibble$predicted <- predict(age_wage_male, newdata = male_data_tibble)

#Intervalos de confianza con bootstrap
male_fn<-function(data,index){
  f <- lm(log_w ~ age + age2, data = male_data_tibble, subset= index)
  b1 <- f$coefficients[2]
  b2 <- f$coefficients[3]
  max_edad <- -(b1)/(2*b2)
  return(max_edad)
}
#Verifiquemos que la función... funciona!
male_fn(male_data_tibble,1:nrow(male_data_tibble))

#Se utiliza la funcion boot para estimar la regresion con bootstrap
set.seed(5382)
boot_rm <- boot(male_data_tibble, male_fn, R = 1000)
boot_rm <- as.data.frame(boot_rm$t)
hist(boot_rm$V1) #distribucion del valor maximo de la edad con bootstrap
quantile(boot_rm$V1,0.025) #percentil 2.5 (49.91383)
quantile(boot_rm$V1,0.975) #percentil 97.5 (53.45359)

male_plot <- ggplot(male_data_tibble, aes(x = age, y = log_w)) +
              geom_point(aes(color = "Real"), alpha = 0.5) +  # Puntos para valores reales
              geom_line(aes(y = predicted, color = "Predicho"), linewidth = 1) +  # Línea para valores predichos
              scale_color_manual(values = c("Real" = "gray", "Predicho" = "darkred"),name="") +  # Colores de puntos y líneas
              labs(title = "Panel A: Hombres",
                   x = "Edad",
                   y = "Ln(salario)") +
              theme_minimal()
print(male_plot)
male_female_plot <- grid.arrange(male_plot, female_plot, ncol = 2)

print(male_female_plot)
#plot de perfil edad - ingreso por sexo
predicted_data <- rbind(female_data_tibble,male_data_tibble)
predicted_data$sex <- factor(predicted_data$sex, levels = c(0, 1), labels = c("Mujeres", "Hombres"))

# Plot de prediccion por edad y sexo
gender_plot <- ggplot(predicted_data, aes(x = age, y = predicted, color = sex)) +
                geom_line() +  # Líneas continuas
                labs(x = "Edad",
                     y = "Ln(salario)") +
                theme_test()+
                theme(legend.position = "top")+  # Mueve la leyenda arriba
                scale_color_manual(values = c("Mujeres" = "darkblue", "Hombres" = "darkred"),name="") # Cambiar colores de las líneas
print(gender_plot)

#Plot de distribucion de edad de ingreso maximo con bootstrap
par(mfrow = c(1, 2))  # Divide el área de trazado en 1 fila y 2 columnas

# Histograma para boot_rf$V1 (Panel A)
hist(boot_rm$V1, main = "", ylab = "Frecuencia", xlab = "Edad de ingreso máximo")
abline(v = 51.56067, col = "darkred", lty = 2)
mtext("Panel A: Hombres", side = 3, line = 2)  # Agrega etiqueta para Panel A

# Histograma para boot_rm$V1 (Panel B)
hist(boot_rf$V1, main = "", ylab = "Frecuencia", xlab = "Edad de ingreso máximo")
abline(v = 43.59954, col = "darkblue", lty = 2)
mtext("Panel B: Mujeres", side = 3, line = 2)  # Agrega etiqueta para Panel B

# Guardar el gráfico combinado en un objeto
peak_age_gender_plot <- recordPlot()

# Restaurar el diseño de la cuadrícula original
par(mfrow = c(1, 1))

#Guardar los graficos
setwd(paste0(wd,"/Views"))
ggsave("age_wage_profile_hombres.png", male_plot, width = 10, height = 6, units = "in")
ggsave("age_wage_profile_mujeres.png", female_plot, width = 10, height = 6, units = "in")
ggsave("age_wage_profile_gender.png", gender_plot, width = 10, height = 6, units = "in")
}
setwd(paste0(wd,"/Stores"))

save.image("gender_gap_earnings.R")
