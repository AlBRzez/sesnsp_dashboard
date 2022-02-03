library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(janitor)
library(scales) 
library(httr)
library(rvest)
library(reactable)


###### Datos ui ----------------------------------------------------------------
# Crime catalogue for selectors
catalogo <- read_csv("catalogo_delitos.csv")
estados <- read_csv("catalogo_estados.csv")
# Date adjustment 
hoy <- today()
if (day(hoy) >= 20) {
        fecha_datos_ini <- floor_date(as_date(hoy), "month") - months(1)
        fecha_datos_1 <- floor_date(as_date(hoy), "month") - months(2)
        fecha_datos <- rollback(hoy)
} else {
        fecha_datos_ini <- floor_date(as_date(hoy), "month") - months(2)
        fecha_datos_1 <- floor_date(as_date(hoy), "month") - months(3)
        fecha_datos <- rollback(hoy - months(1))
        
}

##### Header -------------------------------------------------------------------
header <- dashboardHeader(
        title = "Seguimiento de incidencia delictiva en México",
        titleWidth = "35%"
)

##### Side bar - filters -------------------------------------------------------
sidebar <- dashboardSidebar(
        minified = FALSE,
        width = "200px",
        
        ## Time selection
        awesomeRadio(
                inputId = "tiempo",
                label = "Elige un periodo",
                choices = c(
                        "Mes único" = "mes",
                        "Rango" = "rango"
                ),
                selected = "mes",
        ),
        
        conditionalPanel(
                condition = "input.tiempo == 'mes'",
                airDatepickerInput(
                        "mes_sel",
                        label = "Mes",
                        value = fecha_datos,
                        maxDate = fecha_datos,
                        minDate = "2015-01-01",
                        view = "months", #editing what the popup calendar shows when it opens
                        minView = "months", #making it not possible to go down to a "days" view and pick the wrong date
                        dateFormat = "mm-yyyy",
                        language = "es"
                )
        ),
        
        conditionalPanel(
                condition = "input.tiempo == 'rango'",
                airDatepickerInput(
                        "mes_rango",
                        label = "Rango de fechas",
                        range = TRUE,
                        value = c(fecha_datos_ini, fecha_datos),
                        maxDate = fecha_datos,
                        minDate = "2015-01-30",
                        view = "months", 
                        minView = "months", 
                        dateFormat = "mm-yyyy",
                        language = "es"
                        
                )
        ),
        
        ## Date selection
        pickerInput(
                inputId = "estado",
                label = "Selecciona una entidad",
                choices = estados$entidad,
                selected = "Ciudad de México",
                options = list(
                        style = "btn-primary"
                )
                # )
        ),
        
        sidebarMenu(
                
                menuItem(
                        text = tags$p("Resumen por estado"),
                        tabName = "resumen",
                        icon = icon("duplicate", lib = "glyphicon")
                ),
                
                menuItem(
                        text = tags$p("Desglose por delito"),
                        tabName = "desglose",
                        icon = icon("duplicate", lib = "glyphicon")
                ),
                
                menuItem(
                        text = tags$p("Filtros (para delitos)"),
                        tabName = "filtros",
                        icon = icon("filter"),
                        # icon = icon("filter-cogwheel", lib = "glyphicon"),
                        
                        #### Seleccion de delito ####
                        awesomeRadio(
                                inputId = "desagregacion",
                                label = "Elige el nivel de desagregación",
                                choices = c("Tipo de delito" = "tipo",
                                            "Subtipo de delito" = "subtipo",
                                            "Modalidad del delito" = "modalidad")
                        ),
                        pickerInput(
                                "tipo", "Tipo",
                                choices = unique(catalogo$tipo_de_delito),
                                options = list(
                                        `actions-box` = TRUE,
                                        size = 10,
                                        `live-search` = TRUE
                                ),
                                multiple = F
                        ),
                        
                        conditionalPanel(
                                condition = "input.desagregacion == 'subtipo' | input.desagregacion == 'modalidad'",
                                uiOutput("lista_subtipo")
                        ),
                        
                        conditionalPanel(
                                condition = "input.desagregacion == 'modalidad'",
                                uiOutput("lista_modalidad")
                        ),
                        
                        ## Plot viz selection 
                        awesomeRadio(
                                inputId = "orden",
                                label = "Orden de la gráfica",
                                choices = c(
                                        "Ranking" = "ranking",
                                        "Alfabético" = "alfabetico"
                                ),
                                selected = "ranking"
                        )
                )
        )
)



##### Body  --------------------------------------------------------------------
body <- dashboardBody(
        tags$head(
                tags$style(HTML('.info-box {min-height: 50px;} .info-box-icon {height: 50px; line-height: 50px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
        
        tabItems(
                tabItem(tabName = "resumen",
                        
                        infoBoxOutput(
                                width = 6,
                                "show_estado"
                        ),
                        infoBoxOutput(
                                width = 6,
                                "fechas"
                        ),
                        box(
                                title = "Delitos en el estado",
                                width = 12,
                                reactableOutput("tabla_rank")
                        )
                        
                ),
                
                tabItem(tabName = "desglose",
                        infoBoxOutput(
                                width = 6,
                                "delito_final"),
                        
                        infoBoxOutput(
                                width = 6,
                                "fechas_2"
                        ),
                        box(
                                title = "Ranking de estados",
                                reactableOutput("tabla_del")
                        ),
                        tabBox(id  = "grafica",
                               tabPanel(
                                       title = "Total de CI",
                                       plotOutput("plotabs")
                               ),
                               tabPanel(
                                       title = "CI por cada 100,000 habitantes",
                                       plotOutput("plotpob")
                               )
                        )
                )
                
        )
)


#### UI function ---------------------------------------------------------------
ui <- dashboardPage(
        header,
        sidebar,
        body,
        footer = dashboardFooter(
                left = tags$a(
                  href = "https://github.com/AlBRzez/sesnsp_dashboard",      
                        "@AlBRzez",
                ),
                right = tags$a(
                        href = "https://www.gob.mx/sesnsp/acciones-y-programas/datos-abiertos-de-incidencia-delictiva",
                        "Datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública"
                )
                
        )
)