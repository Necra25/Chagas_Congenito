library(shiny)
library(tidyverse)
library(tidyr)
library(DT)
library(lubridate)
library(shinyjs)
library(shinyWidgets)
library(highcharter)
library(bslib)
library(rsconnect)
if (!require('data.table')) install.packages('data.table'); library('data.table')

base<- fread("Datos/base_chagas_shiny.csv", sep=",",na.strings=c("NA","*sin dato*",""),encoding="Latin-1")

TABLA1<-base %>% filter(ANIO_NAC >2018 & CLASIF_RESUMEN=="Confirmado") %>%  group_by(ANIO_NAC,SEPI_NORM,CLASIF_RESUMEN) %>% summarise(Total = n()) %>% group_by(ANIO_NAC,CLASIF_RESUMEN) %>% complete(SEPI_NORM = seq(1,52,by=1),Total = 0)

TABLA<-base %>% filter(ANIO_NAC >2018 & CLASIF_RESUMEN=="Confirmado") %>% group_by(PROV_RESI_CARGA,ANIO_NAC) %>% summarise(Total = n()) %>% pivot_wider(names_from = ANIO_NAC, values_from = Total)

TABLA <-
  TABLA %>% ungroup() %>% add_row(
    PROV_RESI_CARGA = "Catamarca",
    '2019' = NA,
    '2020' = NA,
    '2021' = NA,
    '2022' = NA,
    '2023' = NA,
    .before = 3
  ) %>% add_row(
    PROV_RESI_CARGA = "Argentina",
    '2019' = sum(TABLA$`2019`, na.rm = T),
    '2020' = sum(TABLA$`2020`, na.rm = T),
    '2021' = sum(TABLA$`2021`, na.rm = T),
    '2022' = sum(TABLA$`2022`, na.rm = T),
    '2023' = sum(TABLA$`2023`, na.rm = T)
  )

TABLA <- TABLA %>% rename("Jurisdiccion de Residencia" = PROV_RESI_CARGA)


tema_chagas = bs_theme(bootswatch = "spacelab")
#tema_chagas = bs_theme_update(tema_chagas, bg = "#fff",
#                              fg = "#000")

# Defino UI para mi aplicación
ui <- fluidPage(useShinyjs(),
                theme = tema_chagas,
                # Titulo
                #titlePanel("Chagas congénito en Argentina"),
                fluidRow(column(
                  width = 12,
                  h1(strong("Chagas congénito en Argentina")),
                  align = "center"
                )),
                
                # primera fila de la ui
                fluidRow(
                  column(
                    width = 3,
                    selectInput(
                      inputId = "selectAnio",
                      label = "Seleccionar año: ",
                      choices = unique(TABLA1$ANIO_NAC),
                      selected = "2023",
                      multiple = FALSE
                    ),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    checkboxInput(
                      inputId = "Mostrar_Tabla",
                      label = "Ver/Ocultar Tabla",
                      value = FALSE
                    ),
                    downloadButton(
                      "descargar",
                      "Descargar"
                    ),
                    
                  ),
                  column(
                    width = 9,
                    highcharter::highchartOutput("grafico2"),
                    h1(strong(
                      "Tabla: Casos de chagas congénito por jurisdicción de residencia y año de nacimiento"
                    )),
                    DT::DTOutput("MyTable")
                  )
                ))


# Defino server
server <- function(input, output) {
  
  
 
  observeEvent(input$Mostrar_Tabla, {
    
    if (input$Mostrar_Tabla == TRUE) {
      show("MyTable")
    } else {
      hide("MyTable")
    }
      })  
  
  output$MyTable = DT::renderDataTable(TABLA)
  
  output$grafico2= renderHighchart({
    anio=input$selectAnio
    
    niveles_anio <- unique(anio)
    niveles_sem <- seq(0,53,by=1)
    # armo el grafico con highchart
    hc2 <- highchart() %>% 
      hc_chart(type = "column")  %>% 
      hc_title(text = "Casos de Chagás congénito por SE, Argentina, periodo 2019-2023") %>%
      hc_xAxis(categories = niveles_sem,title = list(text = "Semana Epidemiológica")) %>%
      hc_yAxis(title = list(text = "Casos notificados ")) %>%
      hc_exporting(enabled = TRUE) # enable exporting option
    
    # Agrega una serie de datos para cada nivel de "prov"
    for (nivel in niveles_anio) {
      data_serie <- TABLA1[TABLA1$ANIO_NAC == nivel,]
      hc2 <- hc2 %>%
        hc_add_series(
          data_serie,
          "column",
          hcaes(x = SEPI_NORM, y = Total),
          name = nivel,
          marker = list(radius = 4),
          color = "darkred"
        )
    }
    print(hc2)
  })
  output$descargar = downloadHandler(
    
    # nombre del archivo a descargar
    
    filename = function() {
      paste('Chagas_congenito', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      
      # procesamiento de los datos para descargar
      datosParaDescargar = TABLA
      
      # descarga de los datos
      write.csv(datosParaDescargar, file, row.names = F, na = "")
    }
  )
}

# Corro la application
shinyApp(ui = ui, server = server)
