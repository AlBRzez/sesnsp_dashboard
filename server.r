## Data ------------------------------------------------------------------------

# Get google file id
URL <- "https://www.gob.mx/sesnsp/acciones-y-programas/datos-abiertos-de-incidencia-delictiva"
page <- GET(URL)
page <- read_html(page)
a <- html_nodes(page, "a")
a <- as.character(a)
link <- a[grepl("Cifras de Incidencia Delictiva Estatal, 2015", a)]

id <- gsub('^.*\\/d\\s*|\\s*view.*$', '', link)
id <- gsub("\\/", "", id)

# Get data
secretariado <-
        read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id),
                 check.names = F, encoding = "latin1")

## Cataloge
catalogo <- read_csv("catalogo_delitos.csv")

dels <- 
        catalogo %>% 
        select(Delito = tipo_de_delito) %>% 
        distinct()

## Datos de población
poblacion <- read_csv("poblacion_pais.csv")


# wrangle data
meses <- tibble(nom_mes = c("enero", "febrero", "marzo", "abril",
                            "mayo", "junio", "julio", "agosto",
                            "septiembre", "octubre", "noviembre", "diciembre"),
                mes = c(1:12))


english_months <- 
        c("january", "february", "march", "april", "may", "june", 
          "july", "august", "september", "october", "november", "december")
spanish_months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

to_spanish_dict <- spanish_months
names(to_spanish_dict) <- english_months

translate_date <- function(date, output_lang = "es"){
        if (output_lang == "es") {
                str_replace_all(tolower(date), to_spanish_dict)
        }
}


secretariado <- 
        secretariado %>% 
        tibble() %>% 
        clean_names() %>% 
        mutate(
                across(c(ano, clave_ent, enero:diciembre), as.numeric)
        ) %>% 
        pivot_longer(
                enero:diciembre, names_to = "nom_mes", values_to = "total"
        ) %>% 
        left_join(meses) %>% 
        left_join(poblacion %>% 
                          select(clave_ent, poblacion = Total) %>% 
                          mutate(
                                  poblacion = extract_numeric(poblacion)
                          )) %>% 
        mutate(
                fecha = as_date(paste(ano, mes, "01", sep = "-")),
                entidad = case_when(
                        entidad == "Coahuila de Zaragoza" ~ "Coahuila",
                        entidad == "Michoacán de Ocampo" ~ "Michoacán",
                        entidad == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
                        T ~ entidad
                )
                
        ) %>% 
        drop_na(total)



## Server function -------------------------------------------------------------
server <- function(input, output, session) {
        
        # Conditional selectors ---------
        ## Conditional crime
        output$lista_subtipo <- 
                renderUI({
                        req(input$tipo)
                        
                        pickerInput(
                                "subtipo", "Subtipo",
                                choices = catalogo %>% 
                                        filter(tipo_de_delito == input$tipo) %>% 
                                        pull(subtipo_de_delito) %>% 
                                        unique(),
                                options = list(
                                        `actions-box` = TRUE,
                                        size = 10,
                                        `live-search` = TRUE
                                ),
                                multiple = F
                        )
                })
        
        output$lista_modalidad <- 
                renderUI({
                        req(input$subtipo)
                        pickerInput(
                                "modalidad", "Modalidad",
                                choices = catalogo %>% 
                                        filter(tipo_de_delito == input$tipo & 
                                                       subtipo_de_delito == input$subtipo) %>% 
                                        pull(modalidad) %>% 
                                        unique(),
                                options = list(
                                        `actions-box` = TRUE,
                                        size = 10,
                                        `live-search` = TRUE
                                ),
                                multiple = F
                        )
                })
        
        
        # Conditional date selector
        datos_fecha <- reactive({
                if (input$tiempo == "mes") {
                        
                        prov <- 
                                secretariado %>%
                                filter(year(fecha) == year(input$mes_sel) & 
                                               month(fecha) == month(input$mes_sel))# %>%
                } else {
                        
                        
                        prov <- 
                                secretariado %>%
                                filter(
                                        year(fecha) >= year(input$mes_rango[1]) & 
                                                month(fecha) >= month(input$mes_rango[1]) &
                                                year(fecha) <= year(input$mes_rango[2]) &
                                                month(fecha) <= month(input$mes_rango[2])
                                )
                        
                }
                prov 
        })
        
        # Reactive data -----------------
        ## used DF
        datos <- reactive({
                
                if (input$desagregacion == "tipo") {
                        prov2 <-
                                datos_fecha() %>%
                                mutate(
                                        delito_final = tipo_de_delito
                                )
                        delito_final_sel <-  input$tipo
                } else if (input$desagregacion == "subtipo") {
                        prov2 <-
                                datos_fecha() %>%
                                mutate(
                                        delito_final = paste(tipo_de_delito, 
                                                             subtipo_de_delito)
                                )
                        delito_final_sel <- paste(input$tipo, input$subtipo)
                } else if (input$desagregacion == "modalidad") {
                        prov2 <-
                                datos_fecha() %>%
                                mutate(
                                        delito_final = paste(tipo_de_delito, 
                                                             subtipo_de_delito, 
                                                             modalidad)
                                )
                        delito_final_sel <- paste(input$tipo, input$subtipo, input$modalidad)
                }
                
                prov2 %>% 
                        filter(delito_final == delito_final_sel) %>% 
                        group_by(
                                entidad,
                                delito_final,
                                poblacion
                        ) %>%
                        summarise(
                                total = sum(total),
                                .groups = "drop"
                        ) %>%
                        mutate(
                                p100k = (total/poblacion) * 100000,
                                color = ifelse(entidad == input$estado, "#34BA53", "gray75")
                        ) 
        })
        
        # Infoboxes ------------------
        ## Infobox to indicate crime
        output$delito_final <- renderInfoBox({
                if (input$desagregacion == "tipo") {
                        delito_final_sel <-  input$tipo
                } else if (input$desagregacion == "subtipo") {
                        delito_final_sel <- paste(input$tipo, input$subtipo, sep = " | ")
                } else if (input$desagregacion == "modalidad") {
                        delito_final_sel <- paste(input$tipo, input$subtipo, 
                                                  input$modalidad,
                                                  sep = " | ")
                }
                
                infoBox(title =  tags$p(
                        delito_final_sel,
                        style = "font-size: 1.2em"
                ),
                color = "orange", fill = TRUE,
                icon = icon("exclamation-circle"))
        })
        
        ## Infobox to show selected dates
        output$fechas <- output$fechas_2 <- renderInfoBox({
                if (input$tiempo == "mes") {
                        texto <- paste("Mes:", format(input$mes_sel, "%B %Y"))
                } else {
                        texto <- paste("De", format(input$mes_rango[1], "%B %Y"),
                                       "a", format(input$mes_rango[2], "%B %Y"))
                }
                
                texto <- translate_date(texto, "es")
                
                infoBox(tags$p(texto,
                               style = "font-size: 1.2em"
                ),
                color = "yellow", fill = TRUE,
                icon = icon("calendar-alt"))
        })
        
        # Infobox to show state
        output$show_estado <- renderInfoBox({
                texto <- paste("Delitos en", input$estado)
                
                infoBox(tags$p(texto,
                               style = "font-size: 1.2em"),
                        color = "red", fill = TRUE,
                        icon = icon("map-marker-alt"))
        })
        
        # Plots -----------------------------
        ## Plot: absolute CI
        output$plotabs <- renderPlot({
                
                tabla <- datos()
                
                
                if (input$orden == "ranking") {
                        p <- ggplot(tabla, aes(x = reorder(entidad, -total), y = total,
                                               fill = color))
                } else {
                        p <- ggplot(tabla, aes(x = entidad, y = total,
                                               fill = color))
                }
                
                p +
                        geom_bar(stat = "identity", position = "dodge", alpha = .8) +
                        geom_hline(yintercept = mean(tabla$total),
                                   linetype = "dashed", color = "#e74c3c") +
                        annotate(geom = "text", 
                                 label = paste0("Prom. nac. ", 
                                                comma(round(mean(tabla$total),2))),
                                 x = 32, y = mean(tabla$total),
                                 color = "#e74c3c", size = 4.5,
                                 vjust = -1, hjust = 1, fontface = "bold",) +
                        scale_y_continuous(labels = comma_format(accuracy = 1)) +
                        scale_fill_identity() +
                        theme_minimal() +
                        theme(
                                axis.text.x = element_text(angle = 90, 
                                                           hjust  = 1, vjust = .5),
                                axis.title = element_blank()
                        )
        })
        
        # Plot: CI relative to population
        output$plotpob <- renderPlot({
                
                tabla <- datos() 
                
                
                if (input$orden == "ranking") {
                        p <- ggplot(tabla, aes(x = reorder(entidad, -p100k), y = p100k,
                                               fill = color))
                } else {
                        p <- ggplot(tabla, aes(x = entidad, y = p100k,
                                               fill = color))
                }
                
                p +
                        geom_bar(stat = "identity", position = "dodge", alpha = .8) +
                        geom_hline(yintercept = mean(tabla$p100k),
                                   linetype = "dashed", color = "#e74c3c") +
                        annotate(geom = "text", 
                                 label = paste0("Prom. nac. ", 
                                                comma(round(mean(tabla$p100k),2))),
                                 x = 32, y = mean(tabla$p100k),
                                 color = "#e74c3c", size = 4.5,
                                 vjust = -1, hjust = 1, fontface = "bold",) +
                        scale_y_continuous(labels = comma_format(accuracy = 1)) +
                        scale_fill_identity() +
                        theme_minimal() +
                        theme(
                                axis.text.x = element_text(angle = 90, hjust  = 1, vjust = .5),
                                axis.title = element_blank()
                        )
        })
        
        # Crime table -----------------------------
        output$tabla_del <- renderReactable({
                tab <-  
                        datos() %>% 
                        arrange(desc(p100k)) %>% 
                        rowid_to_column(var = "Rank. 100k") %>% 
                        arrange(desc(total)) %>% 
                        rowid_to_column(var = "Rank. tot") %>% 
                        mutate(
                                p100k = round(p100k, 2)
                                
                        ) %>%
                        select(Entidad = entidad,
                               Total = total,
                               `Rank. tot`,
                               "Tot. 100k" = p100k,
                               `Rank. 100k`
                        ) %>% 
                        mutate(
                                across(-Entidad, as.numeric)
                        ) 
                
                reactable(tab,
                          height = "400px",
                          defaultColDef = colDef(
                                  format = colFormat(
                                          separators = TRUE, 
                                          locales = "es-MX"
                                  ),
                                  align = "left",
                                  minWidth = 50,
                                  style = list(fontSize = 10),
                                  #              fontWeight = "bold"),
                                  headerClass = "bar-sort-header"
                          ),
                          columns = list(
                                  Entidad = colDef(
                                          minWidth = 60,
                                  )
                          ),
                          rowStyle = function(index) {
                                  if (tab[index, "Entidad"] == input$estado) {
                                          list(fontWeight = "bold",
                                               background = "#54ba53")
                                  }
                          },
                          defaultPageSize = 32, 
                          minRows = 32, 
                          searchable = TRUE)
        })
        
        # Ranking table
        output$tabla_rank <- renderReactable({
                
                tabla <- 
                        datos_fecha() %>% 
                        mutate(
                                p100k = (total/poblacion) * 100000
                        )  %>% 
                        group_by(subtipo_de_delito, modalidad) %>% 
                        mutate(
                                prom = mean(total),
                                prom_100 = mean(p100k)
                        ) %>% 
                        arrange(desc(total)) %>% 
                        group_split() %>% 
                        map(., rowid_to_column) %>% 
                        bind_rows() %>% 
                        rename("Rank. abs" = rowid) %>% 
                        group_by(subtipo_de_delito, modalidad) %>% 
                        arrange(desc(p100k)) %>% 
                        group_split() %>% 
                        map(., rowid_to_column) %>% 
                        bind_rows() %>% 
                        rename("Rank. 100k" = rowid) %>% 
                        filter(entidad == input$estado) %>% 
                        mutate(
                                p100k = round(p100k, 2),
                                prom = round(prom, 2),
                                prom_100 = round(prom_100, 2)
                        ) %>% 
                        select(bien_juridico_afectado, tipo_de_delito, subtipo_de_delito, 
                               modalidad, total, prom, `Rank. abs`, 
                               p100k, prom_100, `Rank. 100k`) %>% 
                        adorn_totals() %>%
                        mutate(
                                across(c(tipo_de_delito, subtipo_de_delito, modalidad),
                                       function(v) {ifelse(bien_juridico_afectado == "Total",
                                                           "Total", v)}),
                                across(c(`Rank. abs`, `Rank. 100k`),
                                       function(v) {ifelse(bien_juridico_afectado == "Total",
                                                           "-", v)})
                        )
                
                reactable(tabla,
                          resizable = TRUE,
                          height = "500px",
                          # filterable = TRUE,
                          striped = TRUE, 
                          highlight = TRUE,
                          groupBy = c("bien_juridico_afectado", "tipo_de_delito", 
                                      "subtipo_de_delito"),
                          defaultColGroup = colGroup(headerClass = "group-header"),
                          defaultColDef = colDef(
                                  format = colFormat(
                                          separators = TRUE, 
                                          locales = "es-MX"
                                  ),
                                  align = "left",
                                  minWidth = 50,
                                  style = list(fontSize = 10),
                                  headerClass = "bar-sort-header"
                          ),
                          columns = list(
                                  bien_juridico_afectado = colDef(
                                          name = "Bien jurídico"
                                  ),
                                  tipo_de_delito = colDef(
                                          name = "Tipo delito"#,
                                  ),
                                  subtipo_de_delito = colDef(
                                          name = "Subtipo delito"#,
                                  ),
                                  modalidad = colDef(
                                          name = "Modalidad"#,
                                  ),
                                  total = colDef(
                                          name = "Total",
                                          style = JS("function(rowInfo) {
                                                   var value = rowInfo.row['total'] 
                                                   if (value > rowInfo.row['prom']) {
                                                   var color = '#f94144'
                                                   } else {
                                                   var color = '#90be6d'
                                                   } 
                                                   return { color: color, fontWeight: 'bold', fontSize: 12 }
                                                   }") ,
                                          headerStyle = list(borderLeft = "1px solid #FFFF"),
                                          aggregate = "sum"
                                  ),
                                  prom = colDef(
                                          name = "Promedio nac.",
                                          aggregate = "sum",
                                          format = colFormat(digits = 2,
                                                             separators = TRUE)
                                  ),
                                  "Rank. abs",
                                  p100k = colDef(
                                          name = "Por 100k habs.",
                                          style = JS("function(rowInfo) {
                                                   var value = rowInfo.row['p100k'] 
                                                   if (value > rowInfo.row['prom_100']) {
                                                   var color = '#f94144'
                                                   } else {
                                                   var color = '#90be6d'
                                                   } 
                                                   return { color: color, fontWeight: 'bold', fontSize: 12 }
                                                   }") ,
                                          headerStyle = list(borderLeft = "1px solid #FFFF"),
                                          aggregate = "sum"
                                  ),
                                  prom_100 = colDef(
                                          name = "Prom. por 100k habs.",
                                          aggregate = "sum",
                                          format = colFormat(digits = 2)
                                  ),
                                  "Rank.100 k"),
                          rowStyle = function(index) {
                                  if (tabla[index, "bien_juridico_afectado"] == "Total") {
                                          list(fontWeight = "bold",
                                               background = "#84DAF0")
                                  }
                          }
                          )
                
                
        })
}
