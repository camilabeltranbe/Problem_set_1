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
  * `01_web_scraping`: Este código realiza _web scraping_ de la [página web](https://ignaciomsarmiento.github.io/GEIH2018_sample/) del profesor Ignacio Sarmiento, para la adquisición de los datos. Además, guarda la base de datos en formato _.xlsx_ y _.RData_ en la carpeta de `stores`.
  
* `stores`: contains all the data sets used. If files are "too big" for GitHub, include a document describing where people can access the data.
4. `views`: contains all figures and tables

## Some general reminders: 

- It is essential how you write up the document. Be sure to be organized and consistent in explaining your equations and findings. Make sure that there are no compilation errors.
- Write understandable code, separating and commenting on each section. Coding, like in writing, style is critical for readability. If the code is well written, it should be self-contained. There is no need to write everything you did. I encourage you to follow the [tidyverse style guide](https://style.tidyverse.org/)

