# Problem Set 1: Big Data y Machine Learning para Economía Aplicada
***03 de marzo de 2024***  
***Universidad de los Andes***  
***Camila Beltran (cod. 202213110)***  
***Julian Pulido (cod. 201814833)***  
***Alejandra Guevara (cod. 202324570)***  
***Juan Pablo Rodríguez (cod. 201716771)***  

Este repositorio almacena toda la información utilizada para la solución del problem set 1 de la materia de Big Data y Machine Learning para Economía Aplicada. La información se encuentra separada en cuatro carpetas:

* `document`: En esta carpeta está el documento final en  _.pdf_, dado que el trabajo se realizó en grupo, decidimos utilizar el compilador de texto en línea _overleaf_. Por lo tanto, este repositorio no permite la reproducibilidad del documento final. 
* `scripts`: En esta carpeta están los _scripts_ utilizados para generar los resultados. 
  * `00_main_script`: Contiene el código principal, se determina el directorio de trabajo y llama a los otros scripts. Para su reproducibilidad es importante determinar la ruta de trabajo. Por ejemplo: 
    ```
    wd <- mi_ruta/Problem_set_1
    ```
    Luego de determinar la ruta, se puede correr el código desde la linea 17.
  * `01_web_scraping`: Este código realiza _web scraping_ de la [página web](https://ignaciomsarmiento.github.io/GEIH2018_sample/) del profesor Ignacio Sarmiento, para la adquisición de los datos. Además, guarda la base de datos en formato _.RData_ en la carpeta de `stores`.
  * `02_data`: Este código realiza la limpieza de la base de datos, mantiene las variables de interés, realiza imputación de datos y estadísticas descriptivas. Además, guarda los resultados en formato _.RData_ en la carpeta de `stores`.
  * `03_age_wage_profile`: Este código realiza las estimaciones de la sección 3 del problem set (teoría edad-salario). Además, guarda los resultados en formato _.RData_ en la carpeta de `stores`.
  * `04_gender_earnings_gap`: Este código realiza las estimaciones de la sección 4 del problem set (brecha salarial por género). Además, guarda los resultados en formato _.RData_ en la carpeta de `stores`.
  * `05_predicting_earnings`: Este código realiza las estimaciones de la sección 5 del problem set (predicciones de ingreso). Además, guarda los resultados en formato _.RData_ en la carpeta de `stores`.
* `stores`: En esta carpeta están la base de datos completa de la página web y los resultados de cada script en formato _.RData_. Los archivos _.RData_ tienen la facilidad de cargar los resultados sin tener que correr cada código, lo cual puede tardarse al implementar las metologias de _bootstrap_ y _loocv_.
  * 01_data_GEIH.RData
  * 02_data.RData
  * 03_age_wage_profile.RData
  * 04_gender_gap_earnings.RData
  * 05_predicting_earnings.RData  
* `views`: En esta carpeta están las tablas y figuras del documento final.

  * `views/tables`
    
    1. Estadísticas descriptivas de las variables cuantitativas más relevantes
    2. Estimación Salario - Edad
    3. Estimación de la brecha salarial por género
    4. Estimación del perfil edad-ingreso por género   
    
  * `views/figures`
  
    1. Proporción de missing values de las variables más representativas
    2. Distribución de Salarios por Género
    3. Frecuencia de individuos por estrato, segmentado por el grado de formalidad laboral
    4. Dispersión entre los valores observados del salario en su transformación logarítmica vs los valores predichos por el modelo con controles
    5. Histograma del método de remuestreo (Bootstrap) del pico máximo de ingresos explicado por la edad
    6. Relación entre la edad y el logaritmo del salario de los valores observados y predichos por un modelo simple y uno complejo (con controles)
    7. Predicción del perfil edad-ingreso por género
    8. Brecha salarial de género por edad
    9. Distribución de la edad de máximo ingreso por género (estimación por bootstrap)
