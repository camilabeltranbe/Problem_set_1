#################################################
########### Punto 2 -Problem set 1 ################
#################################################

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
       ggpubr) ## bootstrapping



#cambiar esta ruta por el directorio de cada uno

ifelse(getwd()=="/Users/camilabeltran",
       wd <- "/Users/camilabeltran/OneDrive/Educación/PEG - Uniandes/BDML/GitHub/problem_set/Problem_set_1",
       ifelse(getwd()=="/User/OneDrive - Universidad de los andes",
       wd <- "C:/Users/User/OneDrive - Universidad de los andes/Big Data y Machine Learning/Problem_set_1/Problem_set_1",
       ifelse(getwd()=="C:/Users/Juan/",
       wd <- "C:/Users/Juan/Documents/Problem_set_1",
       wd <- "/Users/aleja/Documents/Maestría Uniandes/Clases/Big Data y Machine Learning/Repositorios Git Hub/Problem_set_1")))

setwd(paste0(wd,"/stores"))
#Cargar datos a partir de 0_1_web_scraping###
load("data_GEIH.RData")

## Cambio de formato a Tibble   
data_tibble <- as_tibble(data) 
view(head(data_tibble))
}
########## Selección de variables e imputación de Missing values ##########
{
  ## a. Seleccion de variables
  ## Seleccion de variables con las cuales vamos a trabjar 
  data_tibble<- data_tibble %>% 
    select(directorio, secuencia_p, orden, estrato1, sex, age, ocu, oficio, orden, totalHoursWorked,
           ie , formal, informal, sizeFirm , regSalud, maxEducLevel, ingtot,
           ingtotes,ingtotob, y_salary_m, y_total_m, hoursWorkUsual, experiencia=p6426) 
  #view(head(data_tibble))
  # Se eliminaron 157 variables de la muestra inicial, dejando la nueva muestra con 21 variables.
}

{
  ## b. Filto de observaciones
  # "In this problem set, we will focus only on employed individuals older than eighteen (18) years old.
  # 
  table(data$dominio) ## Solo se está trabajando con individuos de Bogota
  data_tibble <- data_tibble %>%
    filter(age>=18 & ## Mayores de edad
             ocu==1) ## Empleados
  # Se eliminaron 15.635 observaciones (aprox. el 49%) de la muestra inicial, dejando 16.542 observaciones.
  #view(data_tibble)
}

{
  ## c. Missing values
  # Se renombraron las variables más relevantes para presentar la gráfica de missing
  data_table_missing <- data_tibble %>% 
    rename(ID_Hogar = directorio) %>% 
    rename(Genero = sex) %>% 
    rename(Estrato = estrato1) %>% 
    rename(Edad = age) %>% 
    rename(Empleado = ocu) %>% 
    rename(Hor_trabajadas = totalHoursWorked) %>% 
    rename(Informal = informal) %>% 
    rename(Tam_Firma = sizeFirm) %>% 
    rename(Nivel_Educ = maxEducLevel) %>% 
    rename(Ing_tot = ingtot) %>% 
    rename(Salario_m =y_salary_m)
  
  data_table_missing <- data_table_missing %>% 
    select(ID_Hogar, Genero, Estrato, Edad, Empleado, Hor_trabajadas, 
           Informal, Tam_Firma, Nivel_Educ, Ing_tot, Salario_m, experiencia)
  
  ## Grafica general
  png("grafica_missing.png") # Formato grafica
  vis_miss(data_table_missing) # tabla missing
  dev.off() # Cierra la grafica
  #vis_dat(data_table_missing) # Opcion 2
  #vis_miss(data_table_missing ,sort_miss = TRUE, cluster = TRUE) # Opcion 3
}

{  
  ## d. Imputacion de Variable salario 
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
  
  #creamos el logaritmo del salario mensual
  data_tibble$log_w=log(data_tibble$sal_imputado)
  
  #Verificar que no tenemos problemas con la transformación a log
  sum(is.nan(data_tibble$log_w))
  
  ## Eliminar el missing de Educacion maxima
  any(is.na(data_tibble$maxEducLevel)) #observemos si existen Missing values en la variable salario
  data_tibble <- data_tibble %>%
    filter(!is.na(maxEducLevel))
  
}

{
## e. Nuevas variables
  #Ahora bien,queremos crear la variable Female, para ello tomaremos la variable sex 
  #que se encuentra en la encuesta
  data_tibble$female <- 1 - data_tibble$sex
  # Nuevas variables esta sección
  data_tibble <- data_tibble %>%
    mutate(Ln_wage_tot = log(y_total_m),
           age2 = age^2,
           Ln_wage_sal = log(y_salary_m))
}

{
  ## f. Renombrar variables continuas 
  data_est_des <- data_tibble %>% 
    rename(Edad = age) %>% 
    rename(Horas_Trabajadas=totalHoursWorked) %>% 
    rename(Salario_mensual = sal_imputado)
}

####################### Estadisticas descriptivas ###########################
{
## Pasar los datos a dataframe

data_est_des <- as.data.frame(data_est_des)

## a. Variables cuantitativas importantes
des_vars= c("Edad", "Horas_Trabajadas", "Salario_mensual")
stargazer(data_est_des[des_vars], type = "text", title="Estadísticas Descriptivas", digits=1, out="Tabla_Est_descriptivas.txt")
stargazer(data_est_des[des_vars], digits=1)

## b. Variables Cuantitativas importantes
# i. Edad y sexo
  # Igual promedio de edad en hombres y mujeres - 39 años 
data_tibble %>% 
  group_by(sex) %>% 
  summarise(mean(age, na.rm = TRUE))

# ii. Horas trabajadas y sexo
  # Las mujeres trabajan menos tiempo 
data_tibble %>% 
  group_by(sex) %>% 
  summarise(mean(totalHoursWorked, na.rm = TRUE))

# iii. Max educación 
  # No se adjunto
  # No es claro las categorias de educación
Matrix_summary <- summary(as.factor(data_tibble$maxEducLevel))
#dotchart(Matrix_summary)

max_educ <- as.data.frame(Matrix_summary)
max_educ$Educ <- c("A", "B", "C", "D", "E", "F")

ggdotchart(
  max_educ, x = "Educ", y = "Matrix_summary",
  add = "segments",
  ylab = "Número de individuos",
  xlab = "Nivel de educación",
  rotate = TRUE)

# iv. Informalidad - Formalidad y Estrato
table(data_tibble$estrato1)
table(data_tibble$informal)
table(data_tibble$formal)
  #Prop. en estrato 2 y 3
100*(table(data_tibble$estrato1)[2]+table(data_tibble$estrato1)[3])/nrow(data_tibble)
  #Prop. Informales
100*(table(data_tibble$informal)[2]/nrow(data_tibble))

{
table(data_tibble$informal)

Inf_estra <- data_tibble %>% 
              group_by(estrato1) %>% 
              summarise(sum(informal, na.rm = TRUE))
Inf_estra <- Inf_estra %>% 
              rename("Estrato"="estrato1",
                     "Informales"="sum(informal, na.rm = TRUE)")

For_estra <- data_tibble %>% 
                group_by(estrato1) %>% 
                summarise(sum(formal, na.rm = TRUE))
For_estra <- For_estra %>% 
              rename("Estrato"="estrato1",
                      "Formales"="sum(formal, na.rm = TRUE)")
Inf_For_estr <- Inf_estra
Inf_For_estr$Formales <-  For_estra$Formales

Inf_For_estr$Estrato <- as.factor(Inf_For_estr$Estrato)

I_F_E_longer <- Inf_For_estr %>%
  pivot_longer(
    cols = c(Informales, Formales),
    names_to = "Formalidad",
    values_to = "Individuos"
  )
}

  # Grafico Estrato y formalidad
  # La mayoria de la muestra se concentra en estratos 2 y 3
ggdotchart(
  I_F_E_longer, x = "Estrato", y = "Individuos", 
  group = "Formalidad", color = "Formalidad", palette = "jco",
  add = "segment", position = position_dodge(0.3),
  sorting = "none",
  facet.by = "Formalidad",
  rotate = TRUE, legend = "none")

## v. Histograma del salario por genero
{
# proceso previo
# Salario medio por genero (datos brutos) 
data_tibble$female <- 1 - data_tibble$sex
data_tibble %>% 
  group_by(female) %>% 
  summarise(median(y_salary_m, na.rm =TRUE))

# Salario medio por genero (datos tratados) 
mean_wage <- data_tibble %>% group_by(female) %>% summarise(median(sal_imputado))
Dif_sex <- mean_wage[2,2]*100/mean_wage[1,2] # Brecha salarial en Colombia 84%

# Histograma
data_low <- data_tibble %>% 
  filter(sal_imputado < 2000000) %>% 
  mutate(sal_mill=sal_imputado/1000000)
13012/16539 # 80% 
}
# Histograma por genero
dummy <- data_low %>%
  group_by(female) %>%
  summarize(mean = mean(sal_mill))


ggplot(data_low, aes(x=sal_mill, group=as.factor(female), fill=as.factor(female))) +
  geom_density(adjust=1.5, alpha=.5, color=NA) +
  labs(title = "",
       x = "Salario ($ Millones)",
       y = "Densidad") +
  scale_fill_manual(name = "Género", labels = c("Hombre", "Mujer"), values=c("wheat","turquoise4"))+
  geom_vline(data = dummy, aes(xintercept = mean, color = c("darkgreen", "wheat4")), show.legend = FALSE)+
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) # Establecer el fondo del gráfico como blanco
}


#### 6. Estimacion del perfil edad-salarios
## No sirve
{
# Convertir datos a data.frame  
  data_frame <- as.data.frame(data_tibble)
#### Wage-age profile
# Nuevas variables esta sección
  data_frame <- data_frame %>%
  mutate(Ln_wage_tot = log(y_total_m),
         age2 = age^2,
         Ln_wage_sal = log(y_salary_m))

## box_plot: edad vs salario
# Borrar: no se si es con Ln_wage_sal o con el total
box_plot <- ggplot(data=data_frame , mapping = aes(as.factor(age) , Ln_wage_sal)) + 
  geom_boxplot()+
  geom_vline(xintercept = 50,
             linetype = 2,
             color = 1)
box_plot


## add another geometry 1
# Borrar: es necesario hacer la distincipon? 
box_plot <- box_plot +
  geom_point(aes(colour=as.factor(sex))) +
  scale_color_manual(values = c("0"="red" , "1"="blue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")
box_plot

}



