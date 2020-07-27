##################################################
## Proyecto: PAnel licitaciones OCDS
## Descripción: Sección de aplicación en shiny para visualizar datos de descargas masivas OCDS
## Fecha: 07-05-2020
## Autor: Hugo Gallardo
## Contacto: hugo.gallardo@chilecompra.cl
## Versión: 1.0
##################################################  

#Paquetes utilizados
library(dplyr)
library(DT)
library(RODBC)
library(sqldf)
library(lubridate)
library(miceadds)
library(readxl)
library(httr)
library(jsonlite)
library(RPostgreSQL)
library(stringi)
library(stringr)
library(shiny)
library(shinydashboard)
library(writexl)
library(shinyWidgets)
library(plotly)
# shinycssloaders


################################INPUTS DE USUARIO######################################################################

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
  "dccp"
}
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname = "OCDS",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw
                 
)
rm(pw) # remueve el pasword para que no aparezca en el cuadro de variables


#Se intentará corregir el siguiente problema en el futuro:
#En caso de que aparezca un error Cannot allocate a new connection: 16 connections already opened, ejecutar el siguiente código:
#Cerrar todas las conexiones de postgress
# lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})


#USUARIO NO EXPERIMENTADO EN R: NO MODIFICAR NADA DE AQUÍ EN ADELANTE
########################################################################################################################
########################################################################################################################
########################################################################################################################


############################################################
#Funciones
############################################################

############################################################
#Conexion a Postgres
############################################################
#Pasos necesarios para utilizar postgreSQL:
#1) Instalar postgress
#2) Setear el nombre de la base de datos. Por defecto es postgres, pero manualmente creé una llamada OCDS.
##Intentaré generar esta creación con create database
#3) 


#Cerrar todas las conexiones de postgress
# lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})

# initdata <- sqlQuery(dbconnection,paste("select * from MyTable;")) odbcClose(channel) 
############################################################
#Consultas en Postgres para consultar datos
############################################################

#########################################################################################
postgresqlpqExec(con, "SET client_encoding = 'UTF8'") 
#########################################################################################

#########################################################################################
#########################################################################################
#Funciones
#Link Licitacion
createLinkLic <- function(val) {
  sprintf('<a href="http://www.mercadopublico.cl/fichaLicitacion.html?idLicitacion=%s" class="btn btn-primary" target="_blank">Link Lic</a>',val)
}

#Link a chileproveedores
createLinkProv <- function(val) {
  sprintf('<a href="http://www.chileproveedores.cl/Modulos/resultadobusquedapublica.aspx?r=%s&e=0" 
          target="_blank" class="btn btn-primary" 
          >LinkProv</a>',val)
}

###Función para formatear números en gráficos###
comma <- function(x) format(x, digits = 0, big.mark = ".", decimal.mark = ",")
comma2 <- function(x) format(x, digits = 2, big.mark = ".", decimal.mark = ",")
comma3 <- function(x) format(x,  big.mark = ".", decimal.mark = ",", scientific = FALSE)

#Dado el ambiente de windows, esta función debe ser aplicada en todas las queries a postgress
set_utf8 <- function(x) {
  # Declare UTF-8 encoding on all character columns:
  chr <- sapply(x, is.character)
  x[, chr] <- lapply(x[, chr, drop = FALSE], `Encoding<-`, "UTF-8")
  # Same on column names:
  Encoding(names(x)) <- "UTF-8"
  x
}

#Extrae último dígito
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Agregar puntuación y guión a rut sin formato
Mod_Rut2 <- function(rut){
  dig<- substrRight(rut,1)
  num <- gsub('.{1}$', '', rut)
  num <- sub("\\-.*", "", num)%>% as.integer() %>% format(., digits = 0, big.mark = ".", decimal.mark = ",")
  Rut<-trimws(paste0(num,"-",dig)) %>%toupper()
  Rut
}
#########################################################################################
#########################################################################################
#Todas las consultas quedan en función de fecha calendario

#Obtener todos los organismos que licitaron para añadir a filtro
ListOrgLic<- set_utf8(dbGetQuery(con,  "select distinct nombre_institucion from licitaciones"))
ListOrgLic <- ListOrgLic[,1]
ListOrgLic<- c("Todo",ListOrgLic)


#Obtener todos las unidades de compra que licitaron para añadir a filtro
ListUnCompraLic<- set_utf8((dbGetQuery(con,  "select distinct nombre_uncompra from licitaciones")))

ListUnCompraLic <- ListUnCompraLic[,1]
ListUnCompraLic<- c("Todo",ListUnCompraLic)


#Obtener todos los proveedores que participaron en una licitación
ListEmprLic <- set_utf8((dbGetQuery(con, "select distinct nombre_empresa from proveedores")))
ListEmprLic <- ListEmprLic[,1]
ListEmprLic<- c("Todo",ListEmprLic) 

#Obtener todos las sucursales que participaron en una licitación
ListSucLic <- set_utf8(dbGetQuery(con, "select distinct nombre_sucursal from proveedores"))
ListSucLic <- ListSucLic[,1]
ListSucLic<- c("Todo",ListSucLic) 

#Obtener todos los tipos de licitación ingresados
ListTipoLic <- set_utf8(dbGetQuery(con, "select distinct tipo_lic from licitaciones"))
ListTipoLic <- ListTipoLic[,1]
ListTipoLic<- c("Todo",ListTipoLic)

#Obtener todos los estados de licitación ingresados

ListEstadoLic <- set_utf8(dbGetQuery(con, "select distinct estado_lic from licitaciones"))
ListEstadoLic <- ListEstadoLic[,1]
ListEstadoLic<- c("Todo",ListEstadoLic)

#Obtener categoria productos
ListCatRub <- set_utf8(dbGetQuery(con, "select distinct nombre_nivel3 from rubros"))
ListCatRub <- ListCatRub[,1]
ListCatRub<- c("Todo",ListCatRub)

#Obtener nombre productos
ListProdOnu <-set_utf8( dbGetQuery(con, "select distinct nombreproducto_onu from rubros"))
ListProdOnu <- ListProdOnu[,1]
ListProdOnu<- c("Todo",ListProdOnu)

#Obtener Listado de regiones comprador
ListRegionComp <- set_utf8(dbGetQuery(con, "select distinct region_uncompra from licitaciones"))
ListRegionComp <- ListRegionComp[,1]
ListRegionComp<- c("Todo",ListRegionComp)

#Obtener listado de paises proveedor
# ListPaisSuc <- set_utf8(dbGetQuery(con, "select distinct pais_sucursal from proveedores"))
# ListPaisSuc <- ListPaisSuc[,1]
# ListPaisSuc<- c("Todo",ListPaisSuc)

#Obtener listado de regiones proveedor
# ListRegionSuc <- set_utf8(dbGetQuery(con, "select distinct region_sucursal from proveedores"))
# ListRegionSuc <- ListRegionSuc[,1]
# ListRegionSuc<- c("Todo",ListRegionSuc)

#Obtener tabla con rubros
DFRubros <- set_utf8(dbGetQuery(con,"select  * from rubros" ))

#Obtener monto máximo de licitación
MaxMonto <- set_utf8(dbGetQuery(con, "select max(monto_adj)  
                        from licitaciones"))
MaxMonto <- round(MaxMonto$max[1])


#############################################################################################

#Código para sidebar en shiny
sidebar <- dashboardSidebar(
  id="",
  collapsed =FALSE,
  tags$head(
    tags$script(
      HTML(
        "
        $(document).ready(function(){
        // Bind classes to menu items, easiet to fill in manually
        var ids = ['Panel_Licitaciones','Rubros'];
        for(i=0; i<ids.length; i++){
        $('a[data-value='+ids[i]+']').addClass('my_subitem_class');
        }
        
        // Register click handeler
        $('.my_subitem_class').on('click',function(){
        // Unactive menuSubItems
        $('.my_subitem_class').parent().removeClass('active');
        })
        })
        "
      )
    )
  ),
  width = 190,
  sidebarMenu(
    id = 'SidebarLic',
    menuItem('Panel_Licitaciones', tabName = 'Panel_Licitaciones', icon = icon("bar-chart")),
    menuItem('Rubros', tabName = 'Rubros', icon = icon("bar-chart"))
  )
)


############################################################
#User interface
############################################################
ui <- dashboardPage(
  
  dashboardHeader(title ="Panel Licitaciones OCDS",
                  tags$li(class = "dropdown", tags$a(HTML(paste(uiOutput("Refresh1")))))
  ),
  sidebar,
  dashboardBody(
    #Modificando colores de título
    tags$head(tags$style(HTML('
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #191a1c;
                              }
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #191a1c;
                              }
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #191a1c;
                              }
                              /*background body   */  
                              .content-wrapper {background-color: #191a1c;}
                              '))),
    tabItems(
      tabItem(
        tabName ="Panel_Licitaciones",
        fluidRow(
          fluidRow(column(width=1,
                          dropdown(
                            style="pill",icon=icon("cog"),status="primary",tooltip=tooltipOptions(title = "Filtros"),
                            inputId="ButtonLic",animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                                         exit = animations$fading_exits$fadeOutLeft),width = "300px",
                            actionBttn(inputId="BotonFiltFechaLic", label = "Ingreso Fechas Precarga", icon = NULL, style = "float",color = "success",size="xs"),
                            dateRangeInput(inputId="TiempoLic",label="Fecha",start=Sys.Date()-30,end=Sys.Date(),
                                           #min=Sys.Date() - 366,max=Sys.Date()-2,
                                           separator="hasta",weekstart=1,autoclose=TRUE,language="es", startview = "month"),
                            radioButtons(inputId = "RadioLic" , label = "Ingreso etapa licitación" , 
                                         choices = list('Adjudicación' = 'adjudicacion', 'Publicación' = 'publicacion'), 
                                         selected = 'adjudicacion', inline= TRUE),
                            
                            actionBttn(inputId="BotonFiltLic", label = "Consultar", icon = NULL, style = "float",color = "success",size="xs"),
                            
                            # pickerInput(inputId="RegFilterLic",label="Región (Unidad de Compra)",choices = ListRegionComp, selected = "Todo",
                            #             options=list(liveSearch=TRUE, liveSearchStyle="contains",
                            #                          showTick=TRUE,size=4,noneSelectedText="Elija opción",
                            #                          selectedTextFormat="count",style="btn-primary",`actions-box` = TRUE,
                            #                          `deselect-all-text` = "Quitar Todo",
                            #                          `select-all-text` = "Seleccionar Todo"),multiple=TRUE
                            # ),
                            
                            selectizeInput( inputId="RegFilterLic", label = "Región (Unidad de Compra)", choices=ListRegionComp,  multiple=TRUE),
                            
                            # pickerInput(inputId="InstFilterLic",label="Institución",choices = ListOrgLic, selected = "Todo",
                            #             options=list(liveSearch=TRUE, liveSearchStyle="contains",
                            #                          showTick=TRUE,size=4,noneSelectedText="Elija opción",
                            #                          selectedTextFormat="count",style="btn-primary",`actions-box` = TRUE,
                            #                          `deselect-all-text` = "Quitar Todo",
                            #                          `select-all-text` = "Seleccionar Todo"),multiple=TRUE
                            # ),
                            
                            selectizeInput( inputId="InstFilterLic", label = "Institución", choices=ListOrgLic,  multiple=TRUE),
                            
                            
                            # pickerInput(inputId="UcompFilterLic",label="Unidad de compra",choices = ListUnCompraLic, selected = "Todo",
                            #             options=list(liveSearch=TRUE, liveSearchStyle="contains",
                            #                          showTick=TRUE,size=4,noneSelectedText="Elija opción",
                            #                          selectedTextFormat="count",style="btn-primary",`actions-box` = TRUE,
                            #                          `deselect-all-text` = "Quitar Todo",
                            #                          `select-all-text` = "Seleccionar Todo"),multiple=TRUE
                            # ),
                            
                            selectizeInput( inputId="UcompFilterLic", label = "Unidad de compra", choices=ListUnCompraLic,  multiple=TRUE),
                            
                            pickerInput(inputId="TipoFilterLic",label="Tipo de Licitación",choices = ListTipoLic, selected = "Todo",
                                        options=list(liveSearch=TRUE, liveSearchStyle="contains",
                                                     showTick=TRUE,size=4,noneSelectedText="Elija opción",
                                                     selectedTextFormat="count",style="btn-primary",`actions-box` = TRUE,
                                                     `deselect-all-text` = "Quitar Todo",
                                                     `select-all-text` = "Seleccionar Todo"),multiple=TRUE
                            ),
                            pickerInput(inputId="EstadoFilterLic",label="Estado Licitación",choices = ListEstadoLic, selected = "Todo",
                                        options=list(liveSearch=TRUE, liveSearchStyle="contains",
                                                     showTick=TRUE,size=4,noneSelectedText="Elija opción",
                                                     selectedTextFormat="count",style="btn-primary",`actions-box` = TRUE,
                                                     `deselect-all-text` = "Quitar Todo",
                                                     `select-all-text` = "Seleccionar Todo"),multiple=TRUE
                            ),
                            
                            # pickerInput(inputId="SucursalFilterLic",label="Empresa",choices = ListEmprLic, selected = "Todo",
                            #             options=pickerOptions(liveSearch=TRUE, liveSearchStyle="contains",
                            #                                   showTick=TRUE,size=4,noneSelectedText="Elija opción",
                            #                                   selectedTextFormat="count",style="btn-primary"),multiple=TRUE
                            # ),
                            
                            # pickerInput(inputId="EmpresaFilterLic",label="Empresa",choices = ListSucLic, selected = "Todo",
                            #             options=pickerOptions(liveSearch=TRUE, liveSearchStyle="contains",
                            #                                   showTick=TRUE,size=4,noneSelectedText="Elija opción",
                            #                                   selectedTextFormat="count",style="btn-primary"),multiple=TRUE
                            # ),
                            #
                            
                            selectizeInput( inputId="EmpresaFilterLic", label = "Empresa", choices=ListEmprLic,  multiple=TRUE),
                            
                            
                            numericRangeInput(
                              inputId = "MontoFiltLic", label = "Rango de monto en Pesos:",
                              value = c(0, MaxMonto), separator = "hasta"
                            ),
                            
                            # pickerInput(inputId="CatProdFilterLic",label="Categoria",choices = ListCatRub, selected = "Todo",
                            #             options=list(liveSearch=TRUE, liveSearchStyle="contains",
                            #                          showTick=TRUE,size=4,noneSelectedText="Elija opción",
                            #                          selectedTextFormat="count",style="btn-primary",`actions-box` = TRUE,
                            #                          `deselect-all-text` = "Quitar Todo",
                            #                          `select-all-text` = "Seleccionar Todo"),multiple=TRUE
                            # ),
                            
                            
                            selectizeInput( inputId="CatProdFilterLic", label = "Categoria", choices=ListCatRub,  multiple=TRUE),
                            
                            # pickerInput(inputId="ProdOnuFilterLic",label="Productos",choices = ListProdOnu, selected = "Todo",
                            #             options=list(liveSearch=TRUE, liveSearchStyle="contains",
                            #                          showTick=TRUE,size=4,noneSelectedText="Elija opción",
                            #                          selectedTextFormat="count",style="btn-primary",`actions-box` = TRUE,
                            #                          `deselect-all-text` = "Quitar Todo",
                            #                          `select-all-text` = "Seleccionar Todo"),multiple=TRUE
                            # )
                            
                            selectizeInput( inputId="ProdOnuFilterLic", label = "Productos", choices=ListProdOnu,  multiple=TRUE)
                            
                            
                            # ,
                            # 
                            # 
                            # radioButtons("FiltrosLic","Banderas Rojas:", choices = list("Todo" = "Todo" , "Preguntas" = "Preguntas", "Reclamos" = "Reclamos", "Fecha_Horario"="Fecha_Horario",
                            #                                                             "No competencia" = "No competencia"), selected ="Todo", inline=TRUE
                            # )
                            
                          )
            )
          ),
          # ,
          # #Para separar filtros de gráficos
          # 
           box(plotlyOutput("PlotCompLic", width = '100%')),
           box(plotlyOutput("PlotProvLic", width = '100%')),
           #box(plotlyOutput("PlotEstadoLic", width = '100%')),
           box(plotlyOutput("PlotItemsLic", width = '100%')),
           box(plotlyOutput("PlotOferentesLic", width = '100%')),
           # box(wordcloud2Output("PlotWordLic", width = '100%')),
           box((DTOutput("DataTabLic")),width = 12, title = span(strong("Detalle de Licitaciones"),
                                                                style = "color: black; text-align:center")),
           box((DTOutput("DataTabItem")),width = 12, title = span(strong("Detalle de Items"),
                                                                style = "color: black; text-align:center")),
           box((DTOutput("DataTabProv")),width = 8, title = span(strong("Detalle de Proveedores"),
                                                                 style = "color: black; text-align:center")),
          #,
           fluidRow(div(style = "height:15px")),
          # downloadButton("DescargarTodoLic","Descargar Tabla Completa" , class = "test1"),
           tags$head(tags$style(".test1{background-color:black;} .test1{color: white;} ")),
           downloadButton("DescargarFiltLic","Descargar Licitaciones", class = "test1" ),
           downloadButton("DescargarFiltItem","Descargar Ítems", class = "test1"),
          downloadButton("DescargarFiltProv","Descargar Proveedores", class = "test1")
          
        )
      ), #Fin tab 1
      tabItem(
        tabName ="Rubros",
        #fluidRow(
        box((DTOutput("DataRubros")),width = 6, title = span(strong("Tabla de rubros"),
                                                              style = "color: black; text-align:center"))
        #)
      )
      
    )  
  )
)

############################################################
#Server
############################################################


server <- function(session,input, output) {
  
  #Obs de filtros: Todos dependerán de la fecha
  
  #Realizar la consulta a partir de filtro temporal

  #Consultas iniciales por fecha
  #Filtro Fecha tabla Licitaciones
  dataLicFecha <- eventReactive(
    input$BotonFiltFechaLic, {
      
      if(input$RadioLic == 'adjudicacion'){
        DetalleLic <- set_utf8(dbGetQuery(con, paste0(
          "select
          lic.*,
        case when lic.moneda = 'CLP' then lic.monto_adj else
        lic.monto_adj * cm.cambioclp end as monto_clp
        from licitaciones as lic
        left join cambio_moneda as cm
        on cast(extract(year from lic.fecha_apertura) as integer) = cm.anio and
        cast(extract(month from lic.fecha_apertura) as integer) = cm.mes
        and lic.moneda = cm.moneda

        where etapa_lic = '", input$RadioLic,"'

        and
        fecha_apertura between '",input$TiempoLic[1], "' and '",input$TiempoLic[2],"'")))
        
      }else{
        DetalleLic <- set_utf8(dbGetQuery(con, paste0(
          "select
          lic.*,
          case when lic.moneda = 'CLP' then lic.monto_est else
          lic.monto_adj * cm.cambioclp end as monto_clp
          from licitaciones as lic
          left join cambio_moneda as cm
          on cast(extract(year from lic.fecha_apertura) as integer) = cm.anio and
          cast(extract(month from lic.fecha_apertura) as integer) = cm.mes
          and lic.moneda = cm.moneda
          
          where etapa_lic = '", input$RadioLic,"'
          and
          fecha_apertura between '",input$TiempoLic[1], "' and '",input$TiempoLic[2],"'")))
        
      }

      #Setear fechas 1980 a NA. Al intentar con sapply me modifica los posixct a numerics
      for(j in 1:nrow(DetalleLic)){
        if(year(DetalleLic$fecha_adj[j]) == 1980)
        {DetalleLic$fecha_adj[j]<- NA} 
      }
      
      DetalleLic$fecha_apertura <- as.Date(DetalleLic$fecha_apertura )
      DetalleLic$fecha_cierre <- as.Date(DetalleLic$fecha_cierre )
      DetalleLic$fecha_adj <- as.Date(DetalleLic$fecha_adj )
      DetalleLic
    }
  )
  
  #Filtro Fecha tabla items
  
  dataLicItem <- eventReactive(
    input$BotonFiltFechaLic, {
      DetalleItem <- set_utf8(dbGetQuery(con, paste0(
        "select  items.*,   
        case when items.moneda = 'CLP' then items.costo_u else
        items.costo_u * cm.cambioclp end as costo_u_clp,
        rubros.nombreproducto_onu, id_nivel3, nombre_nivel3,nombre_nivel2, nombre_nivel1
        from items 
        inner join licitaciones as lic on lic.id_lic = items.id_lic
        left join rubros on rubros.idproducto_onu = items.idproducto_onu
        left join cambio_moneda as cm
        on cast(extract(year from lic.fecha_apertura) as integer) = cm.anio 
        and cast(extract(month from lic.fecha_apertura) as integer) = cm.mes
        and lic.moneda = cm.moneda
        where  etapa_lic = '", input$RadioLic,"' and
        fecha_apertura between '",input$TiempoLic[1], "' and '",input$TiempoLic[2],"'")))
      
      DetalleItem
    }
  )

  #Filtro Fecha tabla proveedores
  dataLicProv <- eventReactive(
    input$BotonFiltFechaLic, {
      DetalleProv <- set_utf8(dbGetQuery(con, paste0(
        "select  proveedores.* 
        from proveedores 
        inner join licitaciones on licitaciones.id_lic = proveedores.id_lic
        where  etapa_lic = '", input$RadioLic,"' and
        licitaciones.fecha_apertura between '",input$TiempoLic[1], "' and '",input$TiempoLic[2],"'")))
      
      DetalleProv$rut_sucursal <- Mod_Rut2(DetalleProv$rut_sucursal)
      DetalleProv
    }
  )
  

  #Datos para tabla de rubros
  dataRubros<- reactive(DFRubros <- set_utf8(dbGetQuery(con,"select  * from rubros" )
  ))

  #Filtro Region comprador
  dataLicAdj1 <- eventReactive(
    input$BotonFiltLic, {
      if(!is.null(input$RegFilterLic)){
        
        if(input$RegFilterLic =="Todo"){
          dataLicFecha()
        }else{
          subset(dataLicFecha(),dataLicFecha()$region_uncompra %in% input$RegFilterLic)
        }
      }else{
        dataLicFecha()
      }
    }  
  )
  
  #Filtro organismo
  dataLicAdj2 <- eventReactive(
    input$BotonFiltLic, {
      if(!is.null(input$InstFilterLic)){
        
        if(input$InstFilterLic =="Todo"){
          dataLicAdj1()
        }else{
          subset(dataLicAdj1(),dataLicAdj1()$nombre_institucion %in% input$InstFilterLic)
        }
      }else{
        dataLicAdj1()
      }
    }  
  )
  
  #Filtro tipo uncompra
  dataLicAdj3 <- eventReactive(
    input$BotonFiltLic, {
      if(!is.null(input$UcompFilterLic)){
        
        if(input$UcompFilterLic =="Todo"){
          dataLicAdj2()
        }else{
          subset(dataLicAdj2(),dataLicAdj2()$nombre_uncompra %in% input$UcompFilterLic)
        }
      }else{
        dataLicAdj2()
      }
    }  
  )
  
  #Filtro tipo licitacion
  dataLicAdj4 <- eventReactive(
    input$BotonFiltLic, {
      if(!is.null(input$TipoFilterLic)){
        
        if(input$TipoFilterLic =="Todo"){
          dataLicAdj3()
        }else{
          subset(dataLicAdj3(),dataLicAdj3()$tipo_lic %in% input$TipoFilterLic)
        }
      }else{
        dataLicAdj3()
      }
    }  
  )
  
  #Filtro estado lic
  dataLicAdj5 <- eventReactive(
    input$BotonFiltLic, {
      if(!is.null(input$EstadoFilterLic)){
        
        if(input$EstadoFilterLic =="Todo"){
          dataLicAdj4()
        }else{
          subset(dataLicAdj4(),dataLicAdj4()$estado_lic %in% input$EstadoFilterLic)
        }
      }else{
        dataLicAdj4()
      }
    }  
  )
  
  #Filtro proveedor
  dataLicAdj6<- eventReactive(
    input$BotonFiltLic, {
      if(!is.null(input$EmpresaFilterLic)){
        
        if(input$EmpresaFilterLic =="Todo"){
          dataLicAdj5()
        }else{
          
          #Generar una tabla de items solo con las licitaciones filtradas hasta este punto
          AuxProv<- subset(dataLicProv(),dataLicProv()$nombre_empresa %in% input$EmpresaFilterLic)
          semi_join(dataLicAdj5(),AuxProv, by='id_lic')
        }
      }else{
        dataLicAdj5()
      }
    }  
  )
  
  
  #Filtro CLP
  
  dataLicAdj7<- eventReactive(
    input$BotonFiltLic, {
      if(input$MontoFiltLic[1] ==0 && input$MontoFiltLic[2] == MaxMonto){
        dataLicAdj6()

      } else{
        subset(dataLicAdj6(),dataLicAdj6()$monto_adj >= input$MontoFiltLic[1] & 
                 dataLicAdj6()$monto_adj <= input$MontoFiltLic[2] | is.na(dataLicAdj6()$monto_adj))
      }
    }
  )

  #Filtro Cat productos
  dataLicAdj8<- eventReactive(
    input$BotonFiltLic, {
      if(!is.null(input$CatProdFilterLic)){
        
        if(input$CatProdFilterLic =="Todo"){
          dataLicAdj7()
        }else{
          
          #Generar una tabla de items solo con las licitaciones filtradas hasta este punto
          AuxItem<- subset(dataLicItem(),dataLicItem()$nombre_nivel3 %in% input$CatProdFilterLic)
          semi_join(dataLicAdj7(),AuxItem, by='id_lic')
        }
      }else{
        dataLicAdj7()
      }
    }  
  )
  
  #Filtro  productos onu
  dataLicAdj9<- eventReactive(
    input$BotonFiltLic, {
      if(!is.null(input$ProdOnuFilterLic)){
        
        if(input$ProdOnuFilterLic =="Todo"){
          dataLicAdj8()
        }else{
          AuxItem2<- subset(dataLicItem(),dataLicItem()$nombreproducto_onu %in% input$ProdOnuFilterLic)
          semi_join(dataLicAdj8(),AuxItem2, by='id_lic')
        }
      }else{
        dataLicAdj8()
      }
    }  
  )
  
  dataLicAdj10<- reactive({
    d <- event_data("plotly_click", source ="PlotCompLic" )
    f <- event_data("plotly_click", source ="PlotProvLic" )
    T1 <- if(is.null(d) & is.null(f)){
      dataLicAdj9()
    }else if(!is.null(d) & is.null(f)){
      subset(dataLicAdj9(), dataLicAdj9()$nombre_institucion == as.character(d$key))
    } else if (is.null(d) & !is.null(f)){
      prov1 <- subset(dataLicProv(), dataLicProv()$nombre_empresa == as.character(f$key))
      semi_join(dataLicAdj9(), prov1, by="id_lic") 
      
    } else if (!is.null(d) & !is.null(f)){
      dataLicAdj9()
    }
    
  })
  
  
  ############################################################
  #Filtros con observe
  ############################################################
  
  #Observe de unidad de compra
  observe({
    if(!is.null(input$InstFilterLic)){
      FInstLic <- dataLicAdj1() %>% filter(nombre_institucion %in% input$InstFilterLic) %>% select(.,nombre_uncompra) %>% arrange(.,nombre_uncompra) %>%unique()
      updateSelectInput(session,"UcompFilterLic", choices = c("Todo",FInstLic,selected = "Todo"))
    }else{
      updateSelectInput(session,"UcompFilterLic", choices ="Todo",selected = "Todo") 
    }
  })
  
  #Observe de productos por categoría
  observe({
    if(!is.null(input$CatProdFilterLic) ){
      ProdOnuList <- dataRubros() %>% filter(nombre_nivel3 %in% input$CatProdFilterLic) %>% select(.,nombreproducto_onu) %>% arrange(.,nombreproducto_onu) %>%unique()
      updateSelectInput(session,"ProdOnuFilterLic", choices = c("Todo",ProdOnuList,selected = "Todo"))
    }else{
      updateSelectInput(session,"ProdOnuFilterLic", choices ="Todo",selected = "Todo")
    }
  })
  
  
  ############################################################
  #Graficos
  ############################################################
  
  #Gráfico de burbuja compradores.
  output$PlotCompLic<- renderPlotly({
    GrafCompLic1 <- dataLicAdj9() %>% group_by(nombre_institucion) %>% summarise(., Cantidad = n()) 
    GrafCompLic2 <- dataLicAdj9() %>% group_by(nombre_institucion) %>% summarise(., Monto = sum(monto_clp, na.rm=TRUE)) 
    GrafCompLic <- inner_join(GrafCompLic1,GrafCompLic2, by="nombre_institucion") %>% arrange(desc(Monto,Cantidad)) 
    GrafCompLic <- GrafCompLic %>% filter(., !is.na(Monto))
    GrafCompLic <- head(GrafCompLic,30)
    #GrafCompLic$Monto <- as.integer(GrafCompLic$Monto
    #customdata <- GrafCompLic$NombreOrganismo
    key <- GrafCompLic$nombre_institucion
    p<- plot_ly(GrafCompLic, 
                x = ~Cantidad, 
                y = ~Monto, type = 'scatter', mode = 'markers', color = ~nombre_institucion, colors = 'Paired',
                #variables para unir con filtros
                source = "PlotCompLic",
                key = ~key,
                #customdata = ~customdata,
                size = ~Monto,sizes = c(10, 50),
                marker = list( opacity = 0.5,sizemode= 'diameter'),
                hoverinfo = 'text',
                text = ~paste('<b>Institución:</b>', nombre_institucion, '<br><b>Cantidad Lic:</b>', comma3(Cantidad),'<br><b>Monto Lic:</b>', trimws(comma3(Monto)) )) %>%
      layout(title = '<b>Montos(CLP) vs cantidad de Lic por Comprador</b>',
             xaxis = list(showgrid = FALSE),
             yaxis = list(showgrid = FALSE),
             showlegend = FALSE)
    ggplotly(p)
  })
  
  
  #Gráfico de burbuja Proveedores.
    output$PlotProvLic<- renderPlotly({
      if(input$RadioLic == "publicacion"){
    }else{
      #Identificar licitaciones que solo tengan 1 proveedor adjudicado
      #Abarca aproximadamente el 82% de todas las licitaciones en los últimos 3 meses
      DetalleProvPlot <- dataLicProv() %>% group_by(.,id_lic) %>% summarise(., suma = n()) %>% filter(., suma ==1) %>% select(.,id_lic)
      #Join con tabla licitaciones para obtener todos los campos de licitación y montos
      DetalleProvPlot2 <- inner_join(dataLicAdj9(),DetalleProvPlot, by ='id_lic')
      #Ahora, juntarlo con licitaciones para obtener monto

      DetalleProvPlot3 <- inner_join(DetalleProvPlot2,dataLicProv(),by ='id_lic' ) %>% group_by(nombre_empresa ) %>% 
        summarise(.,Transado = sum(monto_clp))

      DetalleProvPlot4 <- inner_join(DetalleProvPlot2,dataLicProv(),by ='id_lic' ) %>% group_by(nombre_empresa ) %>% 
        summarise(.,Cant_Lic= n())
      DetalleProvPlot5 <- inner_join(DetalleProvPlot3,DetalleProvPlot4, by='nombre_empresa') %>% arrange(., desc(Transado))
    
      DetalleProvPlot5 <-head(DetalleProvPlot5,30)
      #gráfico
      key <- DetalleProvPlot5$nombre_empresa
      p<- plot_ly(DetalleProvPlot5, 
                  x = ~Cant_Lic, 
                  y = ~Transado, type = 'scatter', mode = 'markers', color = ~nombre_empresa, colors = 'Paired',
                  #variables para unir con filtros
                  source = "PlotProvLic",
                  key = ~key,
                  #customdata = ~customdata,
                  size = ~Transado,sizes = c(10, 50),
                  marker = list( opacity = 0.5,sizemode= 'diameter'),
                  hoverinfo = 'text',
                  text = ~paste('<b>Proveedor:</b>', nombre_empresa, '<br><b>Cantidad Lic:</b>', comma3(Cant_Lic),'<br><b>Monto Lic:</b>', trimws(comma3(Transado)) )) %>%
        layout(title = '<b>Montos(CLP) vs cantidad de Licitaciones adjudicadas por 1 proveedor</b>',
               xaxis = list(showgrid = FALSE),
               yaxis = list(showgrid = FALSE),
               showlegend = FALSE)
      
      
      ggplotly(p)
    }
    })
    
  

  #   
  #   
  
  ####Grafico Estados licitaciones
  # output$PlotEstadoLic<- renderPlotly({
  #   GrafEstadoLic1 <- dataLicAdj9() %>% group_by(nombre_institucion, estado_lic) %>% summarise(., CantidadN = n()) 
  #   GrafEstadoLic2 <- GrafEstadoLic1 %>% group_by(estado_lic) %>% summarise(.,Cantidad = sum(CantidadN))
  #   GrafEstadoLic3 <- GrafEstadoLic1 %>% group_by(estado_lic) %>% summarise(.,Cantidad = sum(CantidadN)) %>% summarize(Total = sum(Cantidad))
  #   GrafEstadoLic2 <- GrafEstadoLic2 %>% mutate( ., Total =GrafEstadoLic3$Total[1]) %>% mutate(., Porcentaje= as.integer((Cantidad/Total)*100))
  #   key <- GrafEstadoLic2$estado_lic
  #   
  #   p <- plot_ly(GrafEstadoLic2,
  #                x = ~estado_lic,
  #                y = ~Cantidad,
  #                color = ~estado_lic, colors = 'Paired',
  #                name = "Licitaciones por estado",
  #                type = "bar",
  #                hoverinfo = 'text',
  #                key = ~key,
  #                source = "PlotEstadoLic",
  #                text = ~paste0('<b>Estado: </b>', estado_lic, '<br><b>Cantidad: </b>', comma3(Cantidad), '<br><b>Porcentaje: </b>',comma3(Porcentaje),"%")) %>%
  #     layout(title = '<b>Frecuencia de Estados de Licitaciones</b>',
  #            #xaxis = list(showgrid = FALSE),
  #            yaxis = list(showgrid = FALSE),
  #            showlegend = FALSE)
  #   
  #   ggplotly(p)
  # })
  
  ####Grafico Oferentes por licitación
  output$PlotOferentesLic<- renderPlotly({
    GrafOferentes <- dataLicAdj10() %>% group_by(., oferentes) %>% mutate(Freq = n()) %>% select(oferentes, Freq) %>% arrange(oferentes) %>% unique()
    # barraColor <- GrafOferentes %>% filter( ., oferentes == 1)
    
    key <- GrafOferentes$oferentes
    
    p <- plot_ly(GrafOferentes,
                 x = ~oferentes,
                 y = ~Freq,
                 #color = ~estado_lic, colors = 'Paired',
                 name = "Cantidad de oferentes por licitación",
                 type = "bar",
                 hoverinfo = 'text',
                 key = ~key,
                 source = "PlotEstadoLic",
                 text = ~paste0('<b>Oferentes: </b>', oferentes, '<br><b>Licitaciones: </b>', comma3(Freq))) %>%
      layout(title = '<b>Cantidad de oferentes por licitación</b>',
             #xaxis = list(showgrid = FALSE),
             yaxis = list(showgrid = FALSE),
             showlegend = FALSE) 

    ggplotly(p)
  })
  
  ##Bar chart con productos con mayor monto adjudicado
  
  output$PlotItemsLic<- renderPlotly({
    
    ProdItemBar <- semi_join(dataLicItem(), dataLicAdj10(), by="id_lic"  )
    ProdItemBar <- ProdItemBar %>% group_by(idproducto_onu, nombreproducto_onu) %>% summarise(MontoProd = sum(costo_u_clp* cantidad)) %>%
         arrange(desc(MontoProd)) %>% head(10)
    #PAra ordenarlos en gráfico por monto
    ProdItemBar$nombreproducto_onu <- factor(ProdItemBar$nombreproducto_onu , levels = unique(ProdItemBar$nombreproducto_onu)[order(ProdItemBar$MontoProd, decreasing = FALSE)])
    key <- ProdItemBar$nombreproducto_onu
    
    p <- plot_ly(ProdItemBar,
                 x = ~MontoProd,
                 y = ~nombreproducto_onu,
                 color = ~nombreproducto_onu, colors = 'Paired',
                 name = "Top 10 productos transados por monto",
                 type = "bar",
                 orientation = 'h',
                 hoverinfo = 'text',
                 key = ~key,
                 source = "PlotEstadoLic",
                 text = ~paste0('<b>Producto: </b>', nombreproducto_onu,'<br><b>ID Producto: </b>', idproducto_onu ,'<br><b>Monto: </b>', comma3(MontoProd))) %>%
      layout(title = '<b>Top 10 productos transados por monto</b>',
             #xaxis = list(showgrid = FALSE),
             yaxis = list(showgrid = FALSE),
             showlegend = FALSE)
    
    ggplotly(p)
    
  })
  
  #TABLA Licitaciones adjudicadas
  output$DataTabLic <- renderDT({
    datatable( asd <- dataLicAdj10() %>% mutate(Enlace = createLinkLic(id_lic)) %>% select(Enlace, everything(), -ocid)
               ,
               escape = F,
               rownames = F,
               caption=tags$caption("Detalle Ordenes de Compra",style="color:#fff !important; font-weight:600 !important;"),
               class="compact hover",
               selection="multiple",
               extensions = list("FixedHeader","KeyTable"),
               options = list(
                 initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"),
                 pageLength = 5, 
                 autoWidth = T,
                 searching = T,
                 lengthChange = TRUE,
                 scrollX = TRUE,
                 #extensions = 'Scroller',
                 fixedHeader =TRUE,
                 escape =FALSE,
                 #paging=FALSE,
                 scrollCollapse=TRUE,
                 keys=TRUE,
                 # dom = 'Bfrtip', 
                 # buttons = I('colvis'),
                 #processing=FALSE,
                 #columnDefs = list(list(visible=FALSE, targets=c(3,6,12,13))),
                 #tableTools=list(sSwfPath = copySWF('www'),aButtons=c('copy','csv','print')),
                 fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}')
               )
    )%>% formatCurrency(c("monto_est","monto_adj","monto_clp"),currency = "",digits = 0, interval = 3, mark = ".") %>% formatStyle(names(asd),fontSize = '11px',fontWeight='600')
  })
  
  
  ##Generando tablas a partir de click en tabla licitación
  
  #Generando la tabla drill down de Items
  drilldataItem <- reactive({
    shiny::validate(
      need(length(input$DataTabLic_rows_selected) > 0, "")
    )    
    LicSelect <- dataLicAdj10()[as.integer(input$DataTabLic_rows_selected), ]$id_lic
    subset(dataLicItem(),dataLicItem()$id_lic %in% LicSelect)
    
  })
  
  #TABLA items adjudicadas
  output$DataTabItem <- renderDT({
    datatable( asd <- drilldataItem()
               ,
               escape = F,
               rownames = F,
               caption=tags$caption("Detalle Ordenes de Compra",style="color:#fff !important; font-weight:600 !important;"),
               class="compact hover",
               selection="multiple",
               extensions = list("FixedHeader","KeyTable"),
               options = list(
                 initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"),
                 pageLength = 5, 
                 autoWidth = T,
                 searching = T,
                 lengthChange = TRUE,
                 scrollX = TRUE,
                 #extensions = 'Scroller',
                 fixedHeader =TRUE,
                 escape =FALSE,
                 #paging=FALSE,
                 scrollCollapse=TRUE,
                 keys=TRUE,
                 # dom = 'Bfrtip', 
                 # buttons = I('colvis'),
                 #processing=FALSE,
                 #columnDefs = list(list(visible=FALSE, targets=c(3,6,12,13))),
                 #tableTools=list(sSwfPath = copySWF('www'),aButtons=c('copy','csv','print')),
                 fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}')
               )
    )%>% formatCurrency(c("costo_u","costo_u_clp"),currency = "",digits = 0, interval = 3, mark = ".") %>% formatStyle(names(asd),fontSize = '11px',fontWeight='600')
  })
  
  
  #Generando la tabla drill down de Proveedores

  #Interacción
  drilldataProv <- reactive({
    shiny::validate(
      need(length(input$DataTabLic_rows_selected) > 0, "")
    )    
    
    LicSelect <- dataLicAdj10()[as.integer(input$DataTabLic_rows_selected), ]$id_lic
    subset(dataLicProv(),dataLicProv()$id_lic %in% LicSelect)
    
  })
  
  
  #Generación de tabla
  output$DataTabProv <- renderDT({
    if(input$RadioLic == "publicacion"){
    }else{
    datatable( asd <- drilldataProv() %>% mutate(Enlace = createLinkProv(rut_sucursal)) %>% select(Enlace, everything())
               ,
               escape = F,
               rownames = F,
               caption=tags$caption("Detalle Ordenes de Compra",style="color:#fff !important; font-weight:600 !important;"),
               class="compact hover",
               selection="multiple",
               extensions = list("FixedHeader","KeyTable"),
               options = list(
                 initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"),
                 pageLength = 5, 
                 autoWidth = T,
                 searching = T,
                 lengthChange = TRUE,
                 scrollX = TRUE,
                 #extensions = 'Scroller',
                 fixedHeader =TRUE,
                 escape =FALSE,
                 #paging=FALSE,
                 scrollCollapse=TRUE,
                 keys=TRUE,
                 # dom = 'Bfrtip', 
                 # buttons = I('colvis'),
                 #processing=FALSE,
                 #columnDefs = list(list(visible=FALSE, targets=c(3,6,12,13))),
                 #tableTools=list(sSwfPath = copySWF('www'),aButtons=c('copy','csv','print')),
                 fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}')
               )
    )
   } %>% formatStyle(names(asd),fontSize = '11px',fontWeight='600')
  })
  
  #TABLA rubros
  output$DataRubros <- renderDT({
    datatable( asd<-DFRubros
               ,
               escape = F,
               rownames = F,
               caption=tags$caption("Detalle Ordenes de Compra",style="color:#fff !important; font-weight:600 !important;"),
               class="compact hover",
               selection="multiple",
               extensions = list("FixedHeader","KeyTable"),
               options = list(
                 initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"),
                 pageLength = 5, 
                 autoWidth = T,
                 searching = T,
                 lengthChange = TRUE,
                 scrollX = TRUE,
                 #extensions = 'Scroller',
                 fixedHeader =TRUE,
                 escape =FALSE,
                 #paging=FALSE,
                 scrollCollapse=TRUE,
                 keys=TRUE,
                 # dom = 'Bfrtip', 
                 # buttons = I('colvis'),
                 #processing=FALSE,
                 #columnDefs = list(list(visible=FALSE, targets=c(3,6,12,13))),
                 #tableTools=list(sSwfPath = copySWF('www'),aButtons=c('copy','csv','print')),
                 fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}')
               )
    ) %>% formatStyle(names(asd),fontSize = '11px',fontWeight='600')
  })
  
  #Observe filtro dinámico de unidad de compra
  # observe({
  #   if(!is.null(input$InstFilterLic)){
  #     FInstLic <- DetalleLic %>% filter(Institucion == input$InstFilterLic) %>% select(.,UnCompra) %>% arrange(.,UnCompra) %>%unique()
  #     updateSelectInput(session,"UcompFilterLic", choices = c("Todo",FInstLic,selected = "Todo"))
  #   }else{
  #     updateSelectInput(session,"UcompFilterLic", choices ="Todo",selected = "Todo") 
  #   }
  # })
  
  #Cerrar todas las conexiones de postgress una vez termine de usarse la app
  # session$onSessionEnded(function(){
  #   lapply(dbListConnections(drv = dbDriver("PostgreSQL")), function(x) {dbDisconnect(conn = x)})
  #   
  # })

  ############################################################
  #Botones de descarga
  ############################################################
  
  #Descarga tabla licitaciones
  output$DescargarFiltLic <- downloadHandler(
    filename = function() {
      paste("DetalleLicFiltrado", "xlsx", sep=".")
    },
    content = function(filename) {
      dataLicAdj10()
      #write.csv(dataC(),filename, row.names = F, sep= ";")
      write_xlsx(dataLicAdj10(), path = filename, col_names = TRUE, format_headers = TRUE)
    }
  )
  
  
  #Descarga tabla items
  output$DescargarFiltItem <- downloadHandler(
    filename = function() {
      paste("DetalleItemFiltrado", "xlsx", sep=".")
    },
    content = function(filename) {
      drilldataItem()
      #write.csv(dataC(),filename, row.names = F, sep= ";")
      write_xlsx(drilldataItem(), path = filename, col_names = TRUE, format_headers = TRUE)
    }
  )
  
  #Descarga tabla proveedores
  
  output$DescargarFiltProv <- downloadHandler(
    filename = function() {
      paste("DetalleProvFiltrado", "xlsx", sep=".")
    },
    content = function(filename) {
      drilldataProv()
      #write.csv(dataC(),filename, row.names = F, sep= ";")
      write_xlsx(drilldataProv(), path = filename, col_names = TRUE, format_headers = TRUE)
    }
  )
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
