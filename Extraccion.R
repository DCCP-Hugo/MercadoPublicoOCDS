##################################################
## Proyecto: Extraccion de datos para panel licitaciones
## Descripción: Extraer datos desde descargas masivas en formato OCDS para ser usadas en visualizacion
## Fecha:27-02-2020
## Autor: Hugo Gallardo
## Contacto: hugo.gallardo@chilecompra.cl
## Versión:5.0
################################################## 

#Paquetes usados
library(tidyr)
library(dplyr)
library(DT)
library(RODBC)
library(sqldf)
library(lubridate)
library(tibble)
library(miceadds)
library(readxl)
library(httr)
library(jsonlite)
library(RPostgreSQL)
library(stringi)
library(stringr)
library(writexl)



#INPUTS DE USUARIO: El usuario debe llenar los siguientes campos.
################################################################

#Conexión a postgress
# Utilizar la contraseña designada de la instalación de postgress.
#Llenar entre los " "
pw <- {
  " "
}
# Carga un objeto postgress (NO CAMBIAR)
drv <- dbDriver("PostgreSQL")

# en dbname = Agregar la base de datos creada  para cargar las tablas
#port = .. El port designado en la instalación. Por defecto viene 5432
#user = , por defecto el superuser se define como postgres
con <- dbConnect(drv, dbname = "OCDS",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw) # remueve la contraseña para 


# Ingresar Path de carpeta local en la que se crearán las carpetas a guardar archivos RAR y JSON
urlTodo <- '..../MercadoPublicoOCDS'

#URL de carpetas específicas donde se guarden los zipFiles y Jsons
urlZipFile <- '..../MercadoPublicoOCDS/zipFiles' 
urlJson <- '.../MercadoPublicoOCDS/Jsons'


#Fecha inicial y final a extrar el en el formato: " ". En caso de querer siempre el último mes, no modificar "AnioFinal"
#Ingresar fecha inicial y final en formato yyyy-mm-01 (Dado que la carga es por mes, el día no se modifica). 
#La aplicación agregara a la base de datos todos los meses entre fechaInicial y fechaFinal.
#Obs la fecha inicial de los datos en OCDS es el año 2009

fechaInicial <- as.Date('2020-01-01')

#En caso de querer siempre el último mes, utilizar como fecha final el siguiente código -> fechaFinal <- as.Date(today())
fechaFinal <- as.Date('2020-07-01')







#USUARIO NO EXPERIMENTADO EN R: NO MODIFICAR NADA DE AQUÍ EN ADELANTE
#USUARIO NO EXPERIMENTADO EN R: NO MODIFICAR NADA DE AQUÍ EN ADELANTE
#USUARIO NO EXPERIMENTADO EN R: NO MODIFICAR NADA DE AQUÍ EN ADELANTE
#USUARIO NO EXPERIMENTADO EN R: NO MODIFICAR NADA DE AQUÍ EN ADELANTE
#USUARIO NO EXPERIMENTADO EN R: NO MODIFICAR NADA DE AQUÍ EN ADELANTE
########################################################################################################################
########################################################################################################################
########################################################################################################################

############################################################
#Funciones
############################################################

#Permite modificar formatos Date a formato para URL en OCDS
FormatoFechaURL <- function(fecha){
  fecha <- as.character(fecha)
  fecha <- str_remove_all(fecha,'-')
  fecha <- gsub('.{2}$', '', fecha)
} 

#Funcion para transformar una lista en un vector para query
Filtro_Query_texto <- function(lista){
  #Agregando comas al final de IDs
  # library(stringr)
  # library(stringi)
  Filtro<- paste0("'",lista,"'",  ",")
  #Removiendo coma del último valor
  Filtro[length(Filtro)]<- str_remove(tail(Filtro, n=1), fixed(","))
  #Uniendo lista en solo una variable string
  Filtro <- paste(Filtro, collapse = '', sep= '')
}

#Función para transformar Nulls a NA en caso de texto
TransNullNA <- function(texto){
  if(is.null(texto)){
    texto <- NA
  }
  texto
}
#Transforma nulls a NAN en caso de numerics
TransNullNAN <- function(numero){
  if(is.null(numero)){
    numero <- NaN
  }
  numero
}

#Transforma nulls de fechas a fecha mínima 1980
TransNullFecha <- function(fecha){
  if(is.null(fecha)){
    fecha <- '1980-01-01T01:01:01Z'
  }
  fecha
}

#Obtener linea de strings antes de | para compradores y proveedores. Si seccion = 1, toma la primera seccion, si es 2, la segunda
SepTexto <- function(texto,seccion){
  # texto <- " organismo | Uncompra "
  if(seccion == 1){
    texto <- strsplit(texto, "[|]")[[1]][1]
    texto <-trimws(texto)
  }else if(seccion == 2){
    texto <- strsplit(texto, "[|]")[[1]][2]
    texto <-trimws(texto)
  }else{
    texto
  }
}

# Traducción estado licitacion OCDS a nomenclatura Chile
EstadoLicTrad <- function(texto){
  if(texto == "active"){"activo"}
  else if(texto =="unsuccessful"){"desierta"}
  else if(texto == "complete"){"adjudicada"}
  else if(texto== "cancelled"){"revocada"}
  else if(texto== "pending"){"suspendida"}
  else{NA}
}

############################################################
#Conexion a Postgres
############################################################

#Setear encoding a UTF8
postgresqlpqExec(con, "SET client_encoding = 'UTF8'") 

############################################################
#Creacion Tabla postgress licitaciones
############################################################

if(dbExistsTable(con, "licitaciones") == FALSE){

  #Se le agregan doble quotes para que mantenga la mayuscula en el nombre de la columna
  sql_command <- 'CREATE TABLE "licitaciones"
  (
  "ocid" character varying NOT NULL,
  "id_lic" character varying,
  "nombre_lic" character varying,
  "desc_lic" character varying,
  "nombre_institucion" character varying,
  "nombre_uncompra" character varying,
  "id_uncompra" character varying,
  "region_uncompra" character varying,
  "fecha_apertura" timestamp,
  "fecha_cierre" timestamp,
  "fecha_adj" timestamp,
  "estado_lic" character varying,
  "tipo_lic" character varying,
  "etapa_lic" character varying,
  "monto_est" numeric,
  "monto_adj" numeric,
  "moneda" character varying,
  "oferentes" numeric,
  
  CONSTRAINT OCID_pkey PRIMARY KEY ("ocid")
  
  )
  WITH (
  OIDS=FALSE
  );
  '
  #Ejecutar código en postgress
  dbGetQuery(con, sql_command)
}

############################################################
#Creacion Tabla postgress Proveedores
############################################################
if(dbExistsTable(con, "proveedores") == FALSE){
  #Se le agregan doble quotes para que mantenga la mayuscula en el nombre de la columna
  sql_command <- 'CREATE TABLE "proveedores"
  (
  "id_lic" character varying,
  "id_sucursal" character varying,
  "rut_sucursal" character varying,
  "nombre_empresa" character varying,
  "nombre_sucursal" character varying,
  "nombrelegal_sucursal" character varying,
  "pais_sucursal" character varying,
  "region_sucursal" character varying
  )
  WITH (
  OIDS=FALSE
  );
  '
  #Ejecutar código en postgress
  dbGetQuery(con, sql_command)
}

############################################################
#Creacion Tabla postgress Items
############################################################
if(dbExistsTable(con, "items") == FALSE){
  #Se le agregan doble quotes para que mantenga la mayuscula en el nombre de la columna
  sql_command <- 'CREATE TABLE "items"
  (
  "id_lic" character varying,
  "id_item" character varying,
  "nombre_item" character varying,
  "cantidad" integer,
  "unidad" character varying,
  "costo_u" numeric,
  "moneda" character varying,
  "idproducto_onu" character varying
  )
  WITH (
  OIDS=FALSE
  );
  '
  #Ejecutar código en postgress
  dbGetQuery(con, sql_command)
}


############################################################
#Creacion Tabla Rubro produtos
############################################################

if(dbExistsTable(con, "rubros") == FALSE){
  #Se le agregan doble quotes para que mantenga la mayuscula en el nombre de la columna
  sql_command <- 'CREATE TABLE "rubros"
  (
  "idproducto_onu" character varying,
  "nombreproducto_onu" character varying,
  "id_nivel3" character varying,
  "nombre_nivel3" character varying,
  "nombre_nivel2" character varying,
  "nombre_nivel1" character varying

  )
  WITH (
  OIDS=FALSE
  );
  '
  #Ejecutar código en postgress
  dbGetQuery(con, sql_command)
}

#Insertar valores a tabla rubros
nrubros <- dbGetQuery(con, 'select count(*) from rubros')
if(nrubros$count ==0){
  #Importar csv a dataframe y subir a tabla rubros. (Se debe sacar de local)
  DFRubros<- read.csv(file = paste0(urlTodo,'/Rubros.csv'), header = TRUE, stringsAsFactors = FALSE, sep=";", encoding ="UTF-8")
  #en este caso por encoding me modifica valor de primera columna
  names(DFRubros)[1] <- "idproducto_onu"
  
  #Insertar en tabla rubros en Postgress
  if(nrow(DFRubros) != 0){
    dbWriteTable(con, 'rubros', DFRubros,row.names=FALSE,append=TRUE)
  }
  
}
############################################################
#Creacion Tabla Cambios de moneda a nivel mensual
############################################################

if(dbExistsTable(con, "cambio_moneda") == FALSE){
  # specifies the details of the table
  #Se le agregan doble quotes para que mantenga la mayuscula en el nombre de la columna
  sql_command <- 'CREATE TABLE "cambio_moneda"
  (
  "moneda" character varying,
  "cambioclp" numeric,
  "fecha" timestamp,
  "anio" integer,
  "mes" integer,
  "dia" integer
  )
  WITH (
  OIDS=FALSE
  );
  '
  #Ejecutar código en postgress
  dbGetQuery(con, sql_command)
}

########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################

############################################################
#Extracción Conversiones de moneda mensual
############################################################

#Paso previo: Identificación de años a llenar en base de datos

#Primera casuística: Base de datos se encuentra vacía
AnioInicioAPI<-2009
AnioFinalAPI <- year(Sys.Date())
TodosAniosMoneda <- AnioInicioAPI:AnioFinalAPI

#Query para tener el número de filas de tabla cambio moneda
nFilaMoneda <- (dbGetQuery(con, "select count(*) from cambio_moneda"))

#Caso 1, tabla está vacía
if(nFilaMoneda$count[1] ==0 ){
  ListAnio <- TodosAniosMoneda
} else{
  #Caso 2: Tabla posee solo algunos años
  p<-1
  ListAnioAux <- vector()
  for(k in  1:length(TodosAniosMoneda)) { 
    AnioMonedaAux <- (dbGetQuery(con, paste0("select count(*) from cambio_moneda where anio = ",TodosAniosMoneda[k])))
    if(AnioMonedaAux$count[1] == 0){
      ListAnioAux[p] <- TodosAniosMoneda[k]
      p<-p+1
    }
  }
  #Agregar último año en caso que no lo posea. Con esto me aseguro que el último año siempre esté actualizado y posea un elemento
  if(AnioFinalAPI %in% ListAnioAux){} else { ListAnioAux <- c(ListAnioAux, AnioFinalAPI)}
  ListAnio <- ListAnioAux
}

#Generar listados de monedas
ListaMonedas<- c("uf","utm","euro","dolar") 

##################################################################################################################
#Extraer todos los cambios de moneda diarios por moneda desde consulta API anual, y guardarlos como mensual.
#Se utiliza API https://mindicador.cl/api/ dado que no requiere registro copmo en el banco central
#Carga inicial de tabla

#UF tiene datos todos los días
#UTM está a nivel mensual con el primer día de cada mes
#Dolar está solo agunos datos (248 del anio)
#Euro está solo agunos datos (248 del anio)

#Dado lo anterior, se genera un proceso distinto para cada unidad y después se combinan.

#Ingresando UTM

for( i in 1: length(ListaMonedas)){
  AnioIter <-AnioInicioAPI

  #Primer caso: UF
  if(i ==1){
    for(k in  1:length(ListAnio)) {
      if(k == 1){
        #Extraigo el json anual
        urlAPIMonedaAnio<- paste0('https://mindicador.cl/api/',ListaMonedas[i],'/',ListAnio[k])
        GETAPIMonedaAnio <- GET(urlAPIMonedaAnio)
        MonedaJsonAnio <- fromJSON(content(GETAPIMonedaAnio, "text"), simplifyVector = FALSE)
        #Itero sobre json para transformar a DF
        for(j in 1:length(MonedaJsonAnio$serie)){
          if(j==1){
            DFMonedaAnioDia <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
            DFMonedaAnioDia <-setNames(DFMonedaAnioDia,  c("moneda", "cambioclp", "fecha"))
          }else{
            DFMonedaAnioDiaAux <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
            DFMonedaAnioDiaAux <-setNames(DFMonedaAnioDiaAux,  c("moneda", "cambioclp", "fecha"))
            DFMonedaAnioDia<- bind_rows(DFMonedaAnioDia,DFMonedaAnioDiaAux)
          }
        }
        #Llevo cambio de moneda diaria a mensual filtrando por el primer día de cada mes:
        DFMonedaAnioMes <- DFMonedaAnioDia %>% filter(., day(fecha) ==01)
        

      } else{
        
          urlAPIMonedaAnio<- paste0('https://mindicador.cl/api/',ListaMonedas[i],'/',ListAnio[k])
          GETAPIMonedaAnio <- GET(urlAPIMonedaAnio)
          MonedaJsonAnio <- fromJSON(content(GETAPIMonedaAnio, "text"), simplifyVector = FALSE)
          #Itero sobre json para transformar a DF
          for(j in 1:length(MonedaJsonAnio$serie)){
            if(j==1){
              DFMonedaAnioDia <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
              DFMonedaAnioDia <-setNames(DFMonedaAnioDia,  c("moneda", "cambioclp", "fecha"))
            }else{
              DFMonedaAnioDiaAux <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
              DFMonedaAnioDiaAux <-setNames(DFMonedaAnioDiaAux,  c("moneda", "cambioclp", "fecha"))
              DFMonedaAnioDia<- bind_rows(DFMonedaAnioDia,DFMonedaAnioDiaAux)
            }
          }
          #Llevo cambio de moneda diaria a mensual:
          DFMonedaAnioMesAux <- DFMonedaAnioDia %>% filter(., day(fecha) ==01)
          DFMonedaAnioMes<- bind_rows(DFMonedaAnioMes,DFMonedaAnioMesAux)
          
 
      }
      
    }
    #Caso UTM
  }else if(i==2){
    
    for(k in  1:length(ListAnio)) {
      if(k == 1){
        #Extraigo el json anual
        urlAPIMonedaAnio<- paste0('https://mindicador.cl/api/',ListaMonedas[i],'/',ListAnio[k])
        GETAPIMonedaAnio <- GET(urlAPIMonedaAnio)
        MonedaJsonAnio <- fromJSON(content(GETAPIMonedaAnio, "text"), simplifyVector = FALSE)
        #Itero sobre json para transformar a DF
        for(j in 1:length(MonedaJsonAnio$serie)){
          if(j==1){
            DFMonedaAnioDia <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
            DFMonedaAnioDia <-setNames(DFMonedaAnioDia,  c("moneda", "cambioclp", "fecha"))
          }else{
            DFMonedaAnioDiaAux <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
            DFMonedaAnioDiaAux <-setNames(DFMonedaAnioDiaAux,  c("moneda", "cambioclp", "fecha"))
            DFMonedaAnioDia<- bind_rows(DFMonedaAnioDia,DFMonedaAnioDiaAux)
          }

        }
        #Llevo cambio de moneda diaria a mensual filtrando por el primer día de cada mes:
        DFMonedaAnioMesAux <- DFMonedaAnioDia
        DFMonedaAnioMes<- bind_rows(DFMonedaAnioMes,DFMonedaAnioMesAux)

      } else{
      
        urlAPIMonedaAnio<- paste0('https://mindicador.cl/api/',ListaMonedas[i],'/',ListAnio[k])
        GETAPIMonedaAnio <- GET(urlAPIMonedaAnio)
        MonedaJsonAnio <- fromJSON(content(GETAPIMonedaAnio, "text"), simplifyVector = FALSE)
        #Itero sobre json para transformar a DF
        for(j in 1:length(MonedaJsonAnio$serie)){
          if(j==1){
            DFMonedaAnioDia <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
            DFMonedaAnioDia <-setNames(DFMonedaAnioDia,  c("moneda", "cambioclp", "fecha"))
          }else{
            DFMonedaAnioDiaAux <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
            DFMonedaAnioDiaAux <-setNames(DFMonedaAnioDiaAux,  c("moneda", "cambioclp", "fecha"))
            DFMonedaAnioDia<- bind_rows(DFMonedaAnioDia,DFMonedaAnioDiaAux)
          }
        }
        #Llevo cambio de moneda diaria a mensual:
        DFMonedaAnioMesAux <- DFMonedaAnioDia
        DFMonedaAnioMes<- bind_rows(DFMonedaAnioMes,DFMonedaAnioMesAux)
        
      }
      
    }
    
  }else if(i >=3){
    for(k in  1:length(ListAnio)) {
      if(k == 1){
        #Extraigo el json anual
        urlAPIMonedaAnio<- paste0('https://mindicador.cl/api/',ListaMonedas[i],'/',ListAnio[k])
        GETAPIMonedaAnio <- GET(urlAPIMonedaAnio)
        MonedaJsonAnio <- fromJSON(content(GETAPIMonedaAnio, "text"), simplifyVector = FALSE)
        #Itero sobre json para transformar a DF
        for(j in 1:length(MonedaJsonAnio$serie)){
          if(j==1){
            DFMonedaAnioDia <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
            DFMonedaAnioDia <-setNames(DFMonedaAnioDia,  c("moneda", "cambioclp", "fecha"))
          }else{
            DFMonedaAnioDiaAux <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
            DFMonedaAnioDiaAux <-setNames(DFMonedaAnioDiaAux,  c("moneda", "cambioclp", "fecha"))
            DFMonedaAnioDia<- bind_rows(DFMonedaAnioDia,DFMonedaAnioDiaAux)
          }
        }
        #Llevo cambio de moneda diaria a mensual filtrando por el primer día de cada mes:
        DFMonedaAnioDia <- DFMonedaAnioDia %>% mutate(., anio = year(fecha)) %>% mutate(., mes = month(fecha))%>%
          arrange(fecha)
        DFMonedaAnioDia <- DFMonedaAnioDia[!duplicated(DFMonedaAnioDia[,c('anio', 'mes')]),]
        DFMonedaAnioDia$anio <- NULL
        DFMonedaAnioDia$mes <- NULL
        
        DFMonedaAnioMesAux <- DFMonedaAnioDia
        DFMonedaAnioMes<- bind_rows(DFMonedaAnioMes,DFMonedaAnioMesAux)
        
      } else{
        

        urlAPIMonedaAnio<- paste0('https://mindicador.cl/api/',ListaMonedas[i],'/',ListAnio[k])
        GETAPIMonedaAnio <- GET(urlAPIMonedaAnio)
        MonedaJsonAnio <- fromJSON(content(GETAPIMonedaAnio, "text"), simplifyVector = FALSE)
        #Itero sobre json para transformar a DF
        for(j in 1:length(MonedaJsonAnio$serie)){
          if(j==1){
            DFMonedaAnioDia <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
            DFMonedaAnioDia <-setNames(DFMonedaAnioDia,  c("moneda", "cambioclp", "fecha"))
          }else{
            DFMonedaAnioDiaAux <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
            DFMonedaAnioDiaAux <-setNames(DFMonedaAnioDiaAux,  c("moneda", "cambioclp", "fecha"))
            DFMonedaAnioDia<- bind_rows(DFMonedaAnioDia,DFMonedaAnioDiaAux)
          }
        }
        #Llevo cambio de moneda diaria a mensual:
        DFMonedaAnioDia <- DFMonedaAnioDia %>% mutate(., anio = year(fecha)) %>% mutate(., mes = month(fecha))%>%
          arrange(fecha)
        DFMonedaAnioDia <- DFMonedaAnioDia[!duplicated(DFMonedaAnioDia[,c('anio', 'mes')]),]
        DFMonedaAnioDia$anio <- NULL
        DFMonedaAnioDia$mes <- NULL
        DFMonedaAnioMesAux <- DFMonedaAnioDia
        #Se agrega a DF final
        DFMonedaAnioMes<- bind_rows(DFMonedaAnioMes,DFMonedaAnioMesAux)
      }
      
    }
    
  }
  
}

#Separando fecha en anio, mes y dia
DFMonedaAnioMes<- DFMonedaAnioMes %>% mutate(., anio = year(fecha), mes = month(fecha), dia=day(fecha))
#Transformando a nomenclatura de licitacion
for(p in 1: nrow(DFMonedaAnioMes)){
  if(DFMonedaAnioMes$moneda[p] == 'uf'){
    DFMonedaAnioMes$moneda[p] <- 'CLF'
  }else if(DFMonedaAnioMes$moneda[p] == 'utm'){
    DFMonedaAnioMes$moneda[p] <- 'UTM'
  }else if(DFMonedaAnioMes$moneda[p] == 'dolar'){
    DFMonedaAnioMes$moneda[p] <- 'USD'
  }else if(DFMonedaAnioMes$moneda[p] == 'euro'){
    DFMonedaAnioMes$moneda[p] <- 'EUR'
  }
}

if(nFilaMoneda$count ==0){
  dbWriteTable(con, 'cambio_moneda', DFMonedaAnioMes,row.names=FALSE,append=TRUE)
}else{

  dbSendQuery(con, paste0('delete from cambio_moneda where anio =',AnioFinalAPI ))
  dbCommit(con)
  dbWriteTable(con, 'cambio_moneda', DFMonedaAnioMes,row.names=FALSE,append=TRUE)
}

########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################


############################################################
#Extracción de datos por Descargas masivas OCDS
###########################################################

#Extracción de histórico
#Creación de carpeta para guardar zip files en caso que no se haya creado
#Se debe redirigir las direcciones de las carpetas

######################################################################
#Iteración por archivo mensual
#######################################################################

#Rango de todos los meses entre las fechas
listaFechas <- seq.Date(fechaInicial, fechaFinal, by="month")
#Pasar a formato legible por API
listaFechas <- FormatoFechaURL(listaFechas)


for(j in 1:length(listaFechas)){
  fechaIter <- listaFechas[j]
  urlDM <- paste0('https://ocds.blob.core.windows.net/ocds/',fechaIter,'.zip')
  download.file(urlDM, paste0(urlZipFile,'/',fechaIter,'.zip'))
  
  
  #Generar un listado de licitaciones e iterar una por una para ingresarlas a bases de datos en postgress
  ##Logica:
  ##-Si es una licitación nueva, se genera un nuevo record en la base de datos
  ##-Si existe, elimina todos los records de esa licitación, y la vuelve a cargar
  
  #Genero lista de archivos a extraer para iteración
  ListArchivosDF <- unzip(paste0(urlZipFile,'/',fechaIter,'.zip'), list = TRUE)
  
  #Error encontrado: ARchivos Json sin datos. Pueden ser detectados utilizando la columna Length al ser menor o igual a 500.
  # Se guarda en un csv los casos con error por formato.
  ErrorListArchivos <- filter(ListArchivosDF, ListArchivosDF$Length <=300 )
  write.csv(ErrorListArchivos,paste0(urlTodo, '/','errores',fechaIter,'.csv'),row.names = FALSE )
  
  #Se actualiza ListArchivosDF quitando las con length 0
  ListArchivosDF <- filter(ListArchivosDF, ListArchivosDF$Length>=300)
  
  #Extraer archivos a carpeta JSON
  
  unzip(paste0(urlZipFile,'/',fechaIter,'.zip'), list = FALSE, overwrite = TRUE, exdir =urlJson )
  
  
  ######################################################################
  #Lógica para identificar si existe una licitación en la base de datos.
  #######################################################################
  #La lógica es: Si la licitación existe en la BBDD, entonces se eliminan sus registros antes de pasar a agregarlos nuevamente.
  
  #Extraer listado inicial de todas las licitaciones que se encuentran en BBDD postgres
  TodasLicsPG <- dbGetQuery(con, 'SELECT licitaciones."id_lic" FROM licitaciones')
  
  if(nrow(TodasLicsPG)!=0){
    #Extraer todas las licitaciones que vienen en un zip de mes
    ListLicbatch <- ListArchivosDF %>% select( ., Name)
    ListLicbatch$Name <- gsub('.{5}$', '', ListLicbatch$Name)
    ListLicbatch$Name <- trimws(ListLicbatch$Name)
    
    #Asignar flag a las que coincidan
    LicsFlag <- inner_join(ListLicbatch,TodasLicsPG, by=c("Name" = "id_lic"))
    
    #Formatear a vector
    
    LicsFlagVec <-Filtro_Query_texto(LicsFlag$Name)
    #Eliminando filas de tablas en caso de existir id_lic en BD
    
    if(nrow(LicsFlag) != 0){
      
      #Eliminar de tabla items
      dbSendQuery(con, paste0('delete from items where items."id_lic" IN (',LicsFlagVec,')'))
      dbCommit(con)
      #Eliminar de tabla proveedores
      dbSendQuery(con, paste0('delete from proveedores where proveedores."id_lic" IN (',LicsFlagVec,')'))
      dbCommit(con)
      #Eliminar de tabla licitaciones
      dbSendQuery(con, paste0('delete from licitaciones where licitaciones."id_lic" IN (',LicsFlagVec,')'))
      dbCommit(con)
      
    }
  
      #Extraer todas las licitaciones que vienen en un zip de mes
      ListLicbatch <- ListArchivosDF %>% select( ., Name)
      ListLicbatch$Name <- gsub('.{5}$', '', ListLicbatch$Name)
      ListLicbatch$Name <- trimws(ListLicbatch$Name)
      
      #Asignar flag a las que coincidan
      LicsFlag <- inner_join(ListLicbatch,TodasLicsPG, by=c("Name" = "id_lic"))
      
      #Formatear a vector
      
      LicsFlagVec <-Filtro_Query_texto(LicsFlag$Name)
      #Eliminando filas de tablas en caso de existir id_lic en BD
      
      if(nrow(LicsFlag) != 0){
        
        #Eliminar de tabla items
        dbSendQuery(con, paste0('delete from items where items."id_lic" IN (',LicsFlagVec,')'))
        dbCommit(con)
        #Eliminar de tabla proveedores
        dbSendQuery(con, paste0('delete from proveedores where proveedores."id_lic" IN (',LicsFlagVec,')'))
        dbCommit(con)
        #Eliminar de tabla licitaciones
        dbSendQuery(con, paste0('delete from licitaciones where licitaciones."id_lic" IN (',LicsFlagVec,')'))
        dbCommit(con)
        
        }
  }
  #################################################################################################
  #################################################################################################
  #Iterando por todos los JSONS de un mes
  #################################################################################################
  
  if(nrow(ListArchivosDF) !=0){
    for(k in 1:nrow(ListArchivosDF)){
      
      #al estar mal construido un Json, genera un error. Se debe manejar este error con un next.
      LicJson <- read_json(paste0(urlJson,'/', ListArchivosDF$Name[k]), simplifyVector = FALSE)
      ##Hipotesis: Utilizar el compiled release para todo como ultima version de estado.
      
      ############################################################
      #Inicializar DF para JSON
      ############################################################
      DFLic <- NULL
      DFProv <- NULL
      DFItem <- NULL
      DFLic <- as.data.frame(DFLic)
      DFProv <- as.data.frame(DFProv)
      DFItem <- as.data.frame(DFItem)
      
      ############################################################
      #Traduciendo datos de Json a DF
      ############################################################
      if(length(LicJson$records[[1]]$compiledRelease$awards) !=0){
        
        ocid <- LicJson$records[[1]]$ocid
        id_lic <- LicJson$records[[1]]$compiledRelease$tender$id
        nombre_lic <- TransNullNA(LicJson$records[[1]]$compiledRelease$awards[[1]]$title)
        desc_lic <-  TransNullNA(LicJson$records[[1]]$compiledRelease$awards[[1]]$description)
        nombre_institucion<- TransNullNA(SepTexto(LicJson$records[[1]]$compiledRelease$tender$procuringEntity$name,1))
        nombre_uncompra<- TransNullNA(SepTexto(LicJson$records[[1]]$compiledRelease$tender$procuringEntity$name,2))
        id_uncompra <- TransNullNA(LicJson$records[[1]]$compiledRelease$tender$procuringEntity$id)
        region_uncompra <-TransNullNA(LicJson[["records"]][[1]][["compiledRelease"]][["parties"]][[1]][["address"]][["region"]])
        fecha_apertura <- TransNullFecha(LicJson$records[[1]]$compiledRelease$tender$tenderPeriod$startDate)
        fecha_cierre <- TransNullFecha(LicJson$records[[1]]$compiledRelease$tender$tenderPeriod$endDate)
        fecha_adj <- TransNullFecha(LicJson$records[[1]]$compiledRelease$awards[[1]]$date)
        estado_lic <- TransNullNA(EstadoLicTrad(LicJson$records[[1]]$compiledRelease$awards[[1]]$status))
        tipo_lic <- TransNullNA(LicJson$records[[1]]$compiledRelease$tender$procurementMethodDetails)
        etapa_lic <- "adjudicacion"
        monto_est <-TransNullNAN(LicJson$records[[1]]$compiledRelease$tender$value$amount)
        monto_adj <-TransNullNAN(LicJson$records[[1]]$compiledRelease$awards[[1]]$value$amount) 
        moneda <- TransNullNA(LicJson$records[[1]]$compiledRelease$awards[[1]]$value$currency)
        #Se le resta 1 porque el primer party es el comprador
        oferentes <- TransNullNAN((as.integer(length(LicJson[["records"]][[1]][["compiledRelease"]][["parties"]])) - 1))
        
        DFLic <- data.frame(ocid = ocid, id_lic = id_lic , nombre_lic = nombre_lic,
                            desc_lic = desc_lic, nombre_institucion = nombre_institucion, 
                            nombre_uncompra = nombre_uncompra, id_uncompra = id_uncompra, region_uncompra =region_uncompra,
                            fecha_apertura = fecha_apertura, fecha_cierre = fecha_cierre, fecha_adj = fecha_adj,
                            estado_lic = estado_lic , tipo_lic = tipo_lic, etapa_lic = etapa_lic, monto_est =monto_est,
                            monto_adj = monto_adj, moneda = moneda, oferentes = oferentes)
        
      } else{
        
        #Se agregan los casos en que no sean adjudicadas y estén en fase tender (Todas las licitaciones parten desde tender)
        ocid <- LicJson$records[[1]]$ocid
        id_lic <- LicJson$records[[1]]$compiledRelease$tender$id
        nombre_lic <- LicJson$records[[1]]$compiledRelease$tender$title
        desc_lic <-  LicJson$records[[1]]$compiledRelease$tender$description
        nombre_institucion<- SepTexto(LicJson$records[[1]]$compiledRelease$tender$procuringEntity$name,1)
        nombre_uncompra<- SepTexto(LicJson$records[[1]]$compiledRelease$tender$procuringEntity$name,2)
        id_uncompra <- TransNullNA(LicJson$records[[1]]$compiledRelease$tender$procuringEntity$id)
        region_uncompra <- TransNullNA(LicJson[["records"]][[1]][["compiledRelease"]][["parties"]][[1]][["address"]][["region"]])
        fecha_apertura <- LicJson$records[[1]]$compiledRelease$tender$tenderPeriod$startDate
        fecha_cierre <- TransNullFecha(LicJson$records[[1]]$compiledRelease$tender$tenderPeriod$endDate)
        fecha_adj <- TransNullFecha(LicJson$records[[1]]$compiledRelease$awards[[1]]$date)
        estado_lic <- TransNullNA(EstadoLicTrad(LicJson$records[[1]]$compiledRelease$tender$status))
        tipo_lic <- TransNullNA(LicJson$records[[1]]$compiledRelease$tender$procurementMethodDetails)
        etapa_lic <- "publicacion"
        monto_est <-TransNullNAN(LicJson$records[[1]]$compiledRelease$tender$value$amount)
        monto_adj <- NaN
        moneda <- TransNullNA(LicJson$records[[1]]$compiledRelease$tender$value$currency)  
        #Se le resta 1 porque el primer party es el comprador
        oferentes <- TransNullNAN((as.integer(length(LicJson[["records"]][[1]][["compiledRelease"]][["parties"]])) - 1))
        
        DFLic <- data.frame(ocid = ocid, id_lic = id_lic , nombre_lic = nombre_lic,
                            desc_lic = desc_lic, nombre_institucion = nombre_institucion, 
                            nombre_uncompra = nombre_uncompra, id_uncompra = id_uncompra,region_uncompra = region_uncompra,
                            fecha_apertura = fecha_apertura, fecha_cierre = fecha_cierre, fecha_adj = fecha_adj,
                            estado_lic = estado_lic , tipo_lic = tipo_lic, etapa_lic = etapa_lic,monto_est=monto_est,
                            monto_adj = monto_adj,  moneda = moneda,  oferentes = oferentes)
      }
      
      #
      ############################################################
      #DF proveedores adjudicados
      ###########################################################
      if(length(LicJson$records[[1]]$compiledRelease$awards) !=0 &&
         LicJson$records[[1]]$compiledRelease$awards[[1]]$status != "cancelled" &&
         LicJson$records[[1]]$compiledRelease$awards[[1]]$status != "unsuccessful"&&
         LicJson$records[[1]]$compiledRelease$awards[[1]]$status != "pending" &&
         length(LicJson$records[[1]]$compiledRelease$awards[[1]]$suppliers)!=0){
        
        #Se genera iteración para encontrar dentro de la lista parties el/los supplier
        Aux1 <-vector()
        Aux2 <-vector()
        Aux3 <-vector()
        Aux4 <-vector()
        Aux5 <-vector()
        Aux6 <-vector()
        Aux7 <-vector()
        Aux8 <-vector()
        p<-1
        
        for(r in 1:length(LicJson[["records"]][[1]][["compiledRelease"]][["parties"]])){
          #Como regla de negocio, asumo que si posee más de un rol, es un proveedor adjudicado (Debería poseer Tender y Supplier)

          if(length(LicJson[["records"]][[1]][["compiledRelease"]][["parties"]][[r]][["roles"]]) >1 &&
             LicJson[["records"]][[1]][["compiledRelease"]][["parties"]][[r]][["roles"]][[1]] != 'procuringEntity' &&
             LicJson[["records"]][[1]][["compiledRelease"]][["parties"]][[r]][["roles"]][[1]] != 'buyer'){
            Aux1[p] <- TransNullNA(LicJson$records[[1]]$compiledRelease$tender$id) #ID lic
            Aux2[p] <- TransNullNA(LicJson[["records"]][[1]][["compiledRelease"]][["parties"]][[r]][["id"]]) #ID sucursal
            Aux3[p] <- TransNullNA(LicJson[["records"]][[1]][["compiledRelease"]][["parties"]][[r]][["identifier"]][["id"]]) #Rut
            Aux4[p] <- TransNullNA(SepTexto(LicJson[["records"]][[1]][["compiledRelease"]][["parties"]][[r]][["name"]],1)) #Nombre empresa
            Aux5[p] <- TransNullNA(SepTexto(LicJson[["records"]][[1]][["compiledRelease"]][["parties"]][[r]][["name"]],2))#Nomre sucursal
            Aux6[p] <- TransNullNA(LicJson[["records"]][[1]][["compiledRelease"]][["parties"]][[r]][["identifier"]][["legalName"]])#Nombrelegal sucursal
            Aux7[p] <- TransNullNA(LicJson[["records"]][[1]][["compiledRelease"]][["parties"]][[r]][["address"]][["countryName"]]) #Pais
            Aux8[p] <- TransNullNA(LicJson[["records"]][[1]][["compiledRelease"]][["parties"]][[r]][["address"]][["region"]]) #Region
            p<-p+1
          }
        }
        
        DFProv <- as.data.frame(matrix(c(Aux1,Aux2,Aux3,Aux4,Aux5,Aux6,Aux7,Aux8),ncol=8))
        DFProv <- rename(DFProv,id_lic = V1,id_sucursal = V2, rut_sucursal=V3, nombre_empresa=V4, 
                         nombre_sucursal=V5 , nombrelegal_sucursal= V6 ,pais_sucursal =V7,region_sucursal=V8 )
        
      }
      ############################################################
      #DF nivel producto en licitaciones adjudicadas
      ############################################################
      
      #Observación: Se realiza un llenado por columna a través de vectores, dado que no en todas las licitaciones existen todos los campos. 
      if(length(LicJson$records[[1]]$compiledRelease$awards) !=0 &&
         LicJson$records[[1]]$compiledRelease$awards[[1]]$status != "cancelled" &&
         LicJson$records[[1]]$compiledRelease$awards[[1]]$status != "unsuccessful" &&
         LicJson$records[[1]]$compiledRelease$awards[[1]]$status != "pending"&&
         length(LicJson$records[[1]]$compiledRelease$awards[[1]]$items)!=0 ) {
        
          Aux1 <-vector()
          Aux2 <-vector()
          Aux3 <-vector()
          Aux4 <-vector()
          Aux5 <-vector()
          Aux6 <-vector()
          Aux7 <-vector()
          Aux8 <-vector()
        
        for(m in 1:length(LicJson$records[[1]]$compiledRelease$awards[[1]]$items)){

            Aux1[m] <- TransNullNA(LicJson$records[[1]]$compiledRelease$tender$id)
            Aux2[m] <- TransNullNA(LicJson$records[[1]]$compiledRelease$awards[[1]]$items[[m]]$id)
            Aux3[m] <- TransNullNA(LicJson$records[[1]]$compiledRelease$awards[[1]]$items[[m]]$description)
            Aux4[m] <-TransNullNAN(LicJson$records[[1]]$compiledRelease$awards[[1]]$items[[m]]$quantity)
            Aux5[m] <-TransNullNA(LicJson$records[[1]]$compiledRelease$awards[[1]]$items[[m]]$unit$name)
            Aux6[m] <-TransNullNAN(LicJson$records[[1]]$compiledRelease$awards[[1]]$items[[m]]$unit$value$amount)
            Aux7[m] <- TransNullNA(LicJson$records[[1]]$compiledRelease$awards[[1]]$items[[m]]$unit$value$currency)
            Aux8[m] <- TransNullNA(LicJson$records[[1]]$compiledRelease$awards[[1]]$items[[m]]$classification$id)
        }
         DFItem <- as.data.frame(matrix(c(Aux1,Aux2,Aux3,Aux4,Aux5,Aux6,Aux7,Aux8),ncol=8))
         DFItem <- rename(DFItem,id_lic = V1,id_item = V2, nombre_item=V3, cantidad=V4,
                          unidad=V5 , costo_u= V6 ,moneda =V7,idproducto_onu=V8 )
         #Cambiando cantidades a integer en caso que R me los transforme en notación exponencial.
         DFItem$cantidad <- as.integer(DFItem$cantidad )
         
      } else if( length(LicJson$records[[1]]$compiledRelease$awards) ==0 &&
                 length(LicJson[["records"]][[1]][["compiledRelease"]][["tender"]][["items"]])!=0){
        #Agregar items cuando son licitaciones no adjudicadas
        
        Aux1 <-vector()
        Aux2 <-vector()
        Aux3 <-vector()
        Aux4 <-vector()
        Aux5 <-vector()
        Aux6 <-vector()
        Aux7 <-vector()
        Aux8 <-vector()
        
        for(m in 1:length(LicJson[["records"]][[1]][["compiledRelease"]][["tender"]][["items"]])){
          
          Aux1[m] <- TransNullNA(LicJson$records[[1]]$compiledRelease$tender$id)
          Aux2[m] <- TransNullNA(LicJson[["records"]][[1]][["compiledRelease"]][["tender"]][["items"]][[m]][["id"]])
          Aux3[m] <- TransNullNA(LicJson[["records"]][[1]][["compiledRelease"]][["tender"]][["items"]][[m]][["description"]])
          Aux4[m] <-TransNullNAN(LicJson[["records"]][[1]][["compiledRelease"]][["tender"]][["items"]][[m]][["quantity"]])
          Aux5[m] <-TransNullNA(LicJson[["records"]][[1]][["compiledRelease"]][["tender"]][["items"]][[m]][["unit"]][["name"]])
          Aux6[m] <-TransNullNAN(LicJson[["records"]][[1]][["compiledRelease"]][["tender"]][["items"]][[m]][["unit"]][["amount"]])
          Aux7[m] <- TransNullNA(LicJson[["records"]][[1]][["compiledRelease"]][["tender"]][["items"]][[m]][["unit"]][["currency"]])
          Aux8[m] <- TransNullNA(LicJson[["records"]][[1]][["compiledRelease"]][["tender"]][["items"]][[m]][["classification"]][["id"]])
        }
        DFItem <- as.data.frame(matrix(c(Aux1,Aux2,Aux3,Aux4,Aux5,Aux6,Aux7,Aux8),ncol=8))
        DFItem <- rename(DFItem,id_lic = V1,id_item = V2, nombre_item=V3, cantidad=V4,
                         unidad=V5 , costo_u= V6 ,moneda =V7,idproducto_onu=V8 )
        #Cambiando cantidades a integer en caso que R me los transforme en notación exponencial.
        DFItem$cantidad <- as.integer(DFItem$cantidad )
        
      }
      
      #Agregar datos a tabla licitaciones
      if(nrow(DFLic) != 0){
        dbWriteTable(con, 'licitaciones', DFLic,row.names=FALSE,append=TRUE)
      }
      #Agregar datos a tabla proveedores
      if(nrow(DFProv) != 0){
        dbWriteTable(con, 'proveedores', DFProv,row.names=FALSE,append=TRUE)
      }
      #Agregar datos a tabla items
      if(nrow(DFItem) != 0){
        dbWriteTable(con, 'items', DFItem,row.names=FALSE,append=TRUE)
      }
      #Agregar datos a tabla transacciones
      
      #Dropear una tabla. SendQuery envía comandos sin tener outputs
      # dbSendQuery(con, "drop table licitaciones")
      # dbCommit(con)
      # dbSendQuery(con, "drop table proveedores")
      # dbCommit(con)
      # dbSendQuery(con, "drop table items")
      # dbCommit(con)
      # dbSendQuery(con, "drop table cambio_moneda")
      # dbCommit(con)
       print(k)
    }
  } #Fin loop carpeta json
  
  #Eliminar todos los archivos Json utilizados
  jsonRemove <-do.call(file.remove, list(list.files(urlJson, full.names = TRUE)))

  #Eliminar el archivo zip utilizado
  zipRemove <-do.call(file.remove, list(list.files(urlZipFile, full.names = TRUE)))
  
} #Fin loop archivo

###############################Fin código###############################################
