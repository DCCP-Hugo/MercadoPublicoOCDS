# Aplicación para almacenar y visualizar compras públicas usando el estándar OCDS.

## Tabla de contenido
  * [Demo](#demo)
  * [Resumen](#resumen)
  * [Motivación](#motivación)
  * [Alcance](#alcance)
  * [Instalación](#instalación)
  * [Utilización de carga de datos](#Utilizacióndecargadedatos)
  * [Modelo de datos](#modelodedatos)
  * [Agradecimientos](#agradecimientos)
  * [Benchmarking](#benchmarking)

## Demo
[<img target="_blank" src="https://github.com/DCCP-Hugo/MercadoPublicoOCDS/blob/master/Im%C3%A1genes/EjemploViz.png" width=400>](https://github.com/DCCP-Hugo/MercadoPublicoOCDS/blob/master/Im%C3%A1genes/EjemploViz.png)

## Resumen
Se desarrolla una aplicación en R, que bajo el ingreso de unos pocos parámetros, descarga automáticamente datos masivos de licitaciones públicas en formato OCDS, los almacena en una base de datos Postgress, y permite la visualización en un panel usando Shiny.

## Motivación
Actualmente existe gran interés por parte de distintos actores por información en las compras públicas en Chile. Éstos pueden ser la sociedad civil monitoreando el correcto uso de los fondos públicos, proveedores buscando oportunidades de negocio, entre otros.

A su vez, existen esfuerzos tanto de organizaciones no gubernamentales (ONG) tales como la [Alianza del gobierno abierto](https://www.opengovpartnership.org/)  y [Open Contracting Partnership](https://www.open-contracting.org/es/) (OCP) entre otros, que establecen lineamientos y recomendaciones para democratizar y abrir los datos públicos hacia la ciudadanía, con fines de transparencia y aumentar la participación de éstos en el monitoreo de las acciones gubernamentales.

En esta línea, OCP genera un estándar de modelo de datos a nivel global, que permite generar análisis comparativos de compras públicas en todos los países que lo posean, independiente de la estructura de datos, procesos de compra o nomenclatura de sus datos. La tecnología base recomendada para esto, es a través de archivos JSON, cuya conexión con el usuario es usualmente a través de APIs, por la gran cantidad de datos. 

En favor de lo anterior, [ChileCompra](https://www.chilecompra.cl/) adopta los lineamientos de datos abiertos implementando una [API](https://desarrolladores.mercadopublico.cl/) con estándar de datos [OCDS](https://standard.open-contracting.org/latest/es/) (Open Contracting Data Standard) propuesto por OCP, que permite a cualquier usuario descargar datos de compras públicas tanto en tiempo real, como a través de descargas masivas de datos, teniendo todas las ventajas que el estándar conlleva.

El problema de los datos provenientes de esta API en particular es la gran barrera de entrada en conocimientos tecnológicos que presenta para los usuarios: El entendimiento del negocio de compras públicas, manipulación de listas anidadas en formato JSON, manejo de softwares especializados en datos, el idioma inglés y la manipulación de archivos pesados (1 gigabyte app por archivo mensual.) 

Ante esta problemática, se desarrolla un aplicativo en R cuyo objetivo es servir como “molde” para disminuir algunas de las barreras de entrada en utilización de datos de compras públicas a la ciudadanía, facilitando la extracción, manipulación y visualización de los datos. 

Describiéndolo de manera más específica, el aplicativo desarrollado en R permite la extracción de los archivos de licitaciones en formato OCDS por el usuario, generar un modelo de datos relacionales en una base de datos en PostgreSQL, alimentar tablas relaciones a partir de archivos JSON, utilizar estas mismas tablas para realizar consultas y alimentar un panel con visualizaciones utilizando Shiny, incluyendo métricas de ejemplo que pueden ser de interés tanto para la sociedad civil como para proveedores.

## Alcance y consideraciones
Solo Licitaciones (públicas y privadas), que hayan sido publicadas desde el 2009 en adelante. 

Se espera tener descargas masivads de Convenios Marco y Tratos Directos a inicios del año 2021.

Considera solo las fases de Tender y Award de OCDS.

Los campos almacenados y visualizados no son la totalidad de campos presentes en los archivos JSON.

## Instalación
La instalación se divide en distintas etapas:
1. Instalación de R y Rstudio. 
Recomiendo seguir las indicaciones de este video:
[Video explicativo de instalación](https://www.youtube.com/watch?v=rxsE3Uc_bnU)
	- Link de descarga R: [Link](https://www.r-project.org/)
	- Link de descarga RStudio: [Link](https://rstudio.com/products/rstudio/download/#download)
    - La versión utilizada para R base es la 3.5.2 .
    - La versión utilizada para RStudio es la 1.1.463

2. Instalación y configuración de Postgress
Recomiendo seguir las indicaciones del siguiente video hasta el minuto 6:15 :
[Video explicativo de instalación](https://www.youtube.com/watch?v=cHGaDfzJyY4)
    - Link de descarga: [Link](https://www.postgresql.org/download/)
    - Versión utilizada: 10.10-1 .
    - Muy importante recordar la constraseña y puerto ingresado.
    - La instalación estándar es suficiente, no es necesario instalar StackBuilder.
	- Configuración: Abrir la aplicación pgAdmin4, ingresar contraseña indicada en la instalación de PostgreSQL, presionar sobre el ícono de base de datos y seleccionar "Crear nueva base de datos", llamar a esta base de datos "OCDS". Para tener una noción de cómo realizar este paso, recomiendo el siguiente [Video](https://www.youtube.com/watch?v=BW8Sr_ueSJI) .

3. Configuración de aplicativo:
    - Clonar o descargar este repositorio en su computador.
	- Ejecutar el archivo: __InstalarPackages.R__
	- Abrir el archivo __Extraccion.R__ y modificar los campos requeridos de conexión a postgress, carpetas locales y rango de fecha a actualizar.
	- Agregar en el archivo app.R dentro de la carpeta app, los datos de la conexión a PostgreSQL requeridos al inicio del archivo.
	
## Utilización de carga de datos
1. Indicar en archivo __ExtraccionDMV5.R__ los meses a extraer en sección inicial del código.
2. Ejecutar el archivo  __Extraccion.R__.
      - Con una muy buena conexión a internet, toma aproximadamente 15 minutos cargar 1 mes de datos.
	  - Una vez terminada la ejecución de __Extraccion.R__ , es posible realizar consultas en la base de datos PostgreSQL.
	  - Se recomienda encarecidamente revisar la [metadata](https://github.com/DCCP-Hugo/MercadoPublicoOCDS/blob/master/Metadata.csv).
		

## Modelo de datos
[<img target="_blank" src="https://github.com/DCCP-Hugo/MercadoPublicoOCDS/blob/master/Im%C3%A1genes/Modelo%20datos.png" width=400>](https://github.com/DCCP-Hugo/MercadoPublicoOCDS/blob/master/Im%C3%A1genes/Modelo%20datos.png)


## Utilización de visualización shiny
1. Indicar en archivo __Extraccion.R__ los meses a extraer en sección inicial del código.	  
	- Agregar en el archivo app.R dentro de la carpeta app, los datos de la conexión a PostgreSQL requeridos al inicio del archivo.
	- Ejecutar visualización.
	- Cliquear en la pequeña tuerca azul cercana a la esquina superior izquierda.
	- Seleccionar el rango de fechas a visualizar y si se desea ver licitaciones adjudicadas o publicadas.
	- Cliquear en "Ingreso Fechas Precarga".
	- Modificar filtros deseados y cliquear en el botón "Consultar".
	- Es posible filtrar con los gráficos de burbuja.

## Benchmarking:
1. [OCDS Kingfisher](https://ocdsdeploy.readthedocs.io/en/latest/use/kingfisher.html)
2. [USA Spending](https://www.usaspending.gov/)

## Agradecimientos:
1. [Open Contracting Partnership](https://www.open-contracting.org/es)
2. [Mi Indicador](https://mindicador.cl)

## Tecnologías utilizadas

[<img target="_blank" src="https://www.r-project.org/logo/Rlogo.svg" width=100>](https://r-project.org/) [<img target="_blank" src="https://wiki.postgresql.org/images/a/a4/PostgreSQL_logo.3colors.svg" width=100>](https://postgresql.org/) 





    