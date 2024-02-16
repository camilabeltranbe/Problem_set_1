#################################################
############## Problem set ######################
#################################################

#### 1. Paquetes ----

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

#### 2. Fijar el directorio ----

## Camila
# wd <- "/Users/camilabeltran/OneDrive/Educación/PEG - Uniandes/BDML/GitHub/problem_set/Problem_set_1" #cambiar esta ruta por el directorio de cada uno 

## Juan 1
# wd <- "C:/Users/juanp.rodriguez/Documents/GitHub/Problem_set_1"

## Juan 2
wd <- "C:/Users/Juan/Documents/Problem_set_1"
  
setwd(paste0(wd,"/stores")) #se establace la ruta para guardar los resultados

## Alejandra
setwd("/Users/aleja/Documents/Maestría Uniandes/Clases/Big Data y Machine Learning/Repositorios Git Hub/Problem_set_1/stores")


#### 3. Importar base de datos (web-scraping) ----

## URL de las 10 paginas
url <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",1:10,".html") #crea una lista de todas las URL

data <- as.data.frame(NULL) #crea un data vacio en donde se ira agregando cada tabla de las URL 

## Union 10 bases 
for (i in 1:length(url)){ #hace un loop para leer cada una de las url (data chunk) y unirlas en una tabla
  my_html <- read_html(url[i])
  tabla <- my_html %>% 
    html_table()
  tabla <- as.data.frame(tabla)
  data <- rbind(data, tabla)
  }

rm(list = (c("tabla","my_html","i","url","wd"))) #se borran los elementos que no quiero guardar
save.image("data_GEIH.RData") #guardar los datos en .RData para que cargarlos sea mas sencillo 

write_xlsx(data, "data_GEIH.xlsx")

data <- read_excel("~/Problem_set_1/stores/data_GEIH.xlsx")
View(data)


#### 4. Inspeccion de los datos y manejo de missing ----

## Cambio de formato a Tibble   
data_tibble <- as_tibble(data) 
view(head(data_tibble))

## Primer vistazo 
skim(data_tibble) %>% 
       head()

## Seleccion de variables con las cuales vamos a trabjar -(si no sirve, hacer con dataframe)
data_tibble<- data_tibble %>% 
       select(directorio, secuencia_p, orden, estrato1, sex, age, ocu, oficio, orden, totalHoursWorked,
                    dsi, ie , formal, informal, sizeFirm , regSalud, maxEducLevel, ingtot,
                    ingtotes,ingtotob, y_salary_m, y_total_m)
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


## Missing 

data_tibble_miss <- skim(data_tibble) %>% 
       select(skim_variable, n_missing)

Nobs= nrow(data_tibble) 
Nobs

data_miss<- data_tibble_miss %>% 
       mutate(p_missing= n_missing/Nobs)
view(head(data_miss))

vis_miss(data_tibble)


#### 5. Estadisticas descriptivas ----

## Pasar los datos a dataframe
data_frame <- as.data.frame(data_tibble)

# Borrar: Poner variables mas importantes
des_vars= c("totalHoursWorked")
stargazer(data_frame[des_vars], type = "text", title="Estadísticas Descriptivas", digits=1, out="Tabla_Est_descriptivas.txt")


#### 6. Estimacion del perfil edad-salarios

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

## Modelo
model_Age_wage <- lm(Ln_wage ~ age + age2, data = data_frame)
summary(model_Age_wage)

# al derivar age, con respecto a y, e igualar a cero queda Age=-B_1/(2B_2)
# Edad maximizadora 
Age_max <- -(model_Age_wage$coefficients["age"])/(2*model_Age_wage$coefficients["age2"])
Age_max # Resulta ser a los 41 años. 

box_plot <- ggplot(data=data_frame , mapping = aes(as.factor(age) , Ln_wage_sal)) + 
  geom_boxplot()+
  geom_vline(xintercept = (40 - 16),
             linetype = 1,
             color = 2)
box_plot


