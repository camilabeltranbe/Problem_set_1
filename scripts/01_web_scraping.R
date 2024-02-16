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
      writexl) ## Exportar Excel

#### 2. Fijar el directorio ----

## Camila
# wd <- "/Users/camilabeltran/OneDrive/Educación/PEG - Uniandes/BDML/GitHub/problem_set/Problem_set_1" #cambiar esta ruta por el directorio de cada uno 

## Juan 1
# wd <- "C:/Users/juanp.rodriguez/Documents/GitHub/Problem_set_1"

## Juan 2
wd <- "C:/Users/Juan/Documents/Problem_set_1"

setwd("/Users/aleja/Documents/Maestría Uniandes/Clases/Big Data y Machine Learning/Repositorios Git Hub/Problem_set_1/stores")
  
setwd(paste0(wd,"/stores")) #se establace la ruta para guardar los resultados


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
data_frame <- as_tibble(data_tibble)

des_vars= c("var_1", "var_2", "age")
stargazer(df[des_vars], type="text")

#### 6. Estimacion del perfil edad-salarios

## box_plot: edad vs salario
box_plot <- ggplot(data=data_frame , mapping = aes(as.factor(age) , y_salary_m)) + 
            geom_boxplot() 
box_plot

## add another geometry 1
box_plot <- box_plot +
            geom_point(aes(colour=as.factor(sex))) +
            scale_color_manual(values = c("0"="red" , "1"="blue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")
box_plot

## add another geometry 2
box_plot <- box_plot +
            geom_point(aes(colour=as.factor(ocu))) +
            scale_color_manual(values = c("0"="purple" , "1"="green") , label = c("0"="Desempleado" , "1"="Empleado") , name = "Sexo")
box_plot
