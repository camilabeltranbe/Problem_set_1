#main script
#este código contiene todos los scripts utilizados para el problem set 1

#se borra la memoria
rm(list = ls())

#se define la ruta de trabajo
ifelse(getwd()=="/Users/camilabeltran",
       wd <- "/Users/camilabeltran/OneDrive/Educación/PEG - Uniandes/BDML/GitHub/problem_set/Problem_set_1",
       ifelse(getwd()=="C:/Users/Juan/Documents/Problem_set_1/scripts",
       wd <-  "C:/Users/Juan/Documents/Problem_set_1",
       ifelse(test = getwd()=="C:/Users/juanp.rodriguez/Documents/GitHub/Problem_set_1",
       wd <- "C:/Users/juanp.rodriguez/Documents/GitHub/Problem_set_1",
       wd <- "/Users/aleja/Documents/Maestría Uniandes/Clases/Big Data y Machine Learning/Repositorios Git Hub/Problem_set_1")))

#se establece la ruta de la carpeta de "scripts"
setwd(paste0(wd,"/scripts"))

#Script: "01_web_scraping.R". Realiza el proceso de web scraping para conseguir los datos
source("01_web_scraping.R")
#Script: "02_Data.R". Realiza la limpieza de la base de datos y estadísticas descriptivas
source("02_Data.R")
#Script: "03_Data.R". Realiza estimaciones del perfil edad-ingreso
source("03_Age-Wage Profile.R")
#Script: "04_The gender earnings GAP.R". Realiza estimaciones de la brecha salarial por género
source("04_The gender earnings GAP.R")
#Script: "05_Predicting_Earnings.R". Realiza predicciones y estimaciones de cross-validation
source("05_Predicting_Earnings.R")
