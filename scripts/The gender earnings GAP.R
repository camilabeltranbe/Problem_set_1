#################################################
########### Punto 4 -Problem set ################
#################################################

rm(list = ls()) #se borra la memoria
require(pacman) 

p_load(rio, # import/export data
       tidyverse, # tidy-data (ggplot y Tidyverse
       skimr, # summary data
       visdat, ## visualizing missing data
       corrplot, ## Correlation Plots 
       stargazer, # tables/output to TEX.
       rvest,## web-scraping
       readxl, ## Importar Excel
       writexl) ## Exportar Excel


wd <-"C:/Users/User/OneDrive - Universidad de los andes/Big Data y Machine Learning/Problem_set_1"
  #cambiar esta ruta por el directorio de cada uno 
data <- read_excel("C:/Users/User/OneDrive - Universidad de los andes/Big Data y Machine Learning/Problem_set_1/Problem_set_1/stores/data_GEIH.xlsx")
View(data)

data_tibble <- as_tibble(data) 
view(head(data_tibble))

## Primer vistazo 
skim(data_tibble) %>% 
  head()

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

#creamos el logatirmo del salario mensual
data_tibble$log_w=log(data_tibble$sal_imputado)

#Verificar que no tenemos problemas con la transformación a log
todos_numeros <- all(is.numeric(data_tibble$log_w))
print(todos_numeros)
rm(todos_numeros)

####################### a) Regresión ######################################

earnings_gap <- lm(log_w ~ female, data = data_tibble)

# Mostrar resultados con stargazer
stargazer(earnings_gap, type = "text")#quitar el "text" si se quiere en LATEX

####################### b) Equal Pay for Equal Work? ######################

equal_pay <- lm(log_w ~ female + age + maxEducLevel + hoursWorkUsual + oficio, data= data_tibble)

#Donde:
#maxEducLevel	= max. education level attained
#hoursWorkUsual = usual weekly hours worked - principal occ.
#oficio
#age=edad

# Mostrar resultados con stargazer
stargazer(equal_pay, type = "text")

#Resultados para comparación
stargazer(earnings_gap,equal_pay,type="text",digits=4)
