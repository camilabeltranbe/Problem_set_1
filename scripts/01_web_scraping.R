#################################################
############## Problem set ######################
#################################################

#### 1. Paquetes ----

rm(list = ls()) #se borra la memoria
require(pacman) 

p_load(tidyverse, # contiene las librerias ggplot, dplyr...
       rvest) # web-scraping

#### 2. Fijar el directorio ----

## Camila
# wd <- "/Users/camilabeltran/OneDrive/Educación/PEG - Uniandes/BDML/GitHub/problem_set/Problem_set_1" #cambiar esta ruta por el directorio de cada uno 

## Juan 
wd <- "C:/Users/juanp.rodriguez/Documents/GitHub/Problem_set_1"
  
setwd(paste0(wd,"/stores")) #se establace la ruta para guardar los resultados


#### 3. Importar base de datos ----

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
