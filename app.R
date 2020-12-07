# SHINY APP INTELIGENCIA COMPETITIVA

############################################
# LIBRERÍAS
############################################

library(shiny)
library(shinyjs)  # Para eecutar comandos JS
library(DT)
library(htmltools)
library(httr)
library(timeDate)
library(anytime)
library(jsonlite)
library(stringr)
library(dplyr)
library(ggplot2)
library(rjson)
library(jsonlite)
library(leaflet)  #Libreía mapa
library(RCurl)
library(utils)
library(shinybusy)
library(lubridate)

#Scripts R apoyo externos
source("llamada_api_thb_borme.R")
source("llamada_api_thb_borme_fechas.R")
source("llamada_api_thb_noticias.R")
#source("llamada_api_thb_fichas.R")
source("llamada_api_thb_twitter.R")
source("llamada_api_thb_contratacionesEstado.R")


#==================
# CENSO EMPRESAS
#==================
# CENSO
df_censo <- read.csv("censo_Ermua.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8", dec = ",")

#====================
# REFERENCIA CNAES
#====================
df_cnae <- read.csv("referencia_CNAEs.csv", header = TRUE, sep = ";")
df_cnae <- df_cnae[,c(1,3)]
df_cnae$completo <- paste(df_cnae[,1],df_cnae[,2], sep = " ")


######################################################
# INTERFAZ DE USUARIO
######################################################
ui <- fluidPage(style = "width: 100%; height: 100%;",
    
    # Inicialización shinyjs
    useShinyjs(),  
        # Título panel
    #titlePanel(title=div(img(src="https://s2.static-clubeo.com/uploads/eee-ermua/sponsors/ayuntamiento-de-ermua__nrw5sl.jpg",style = 'width: 70px; high: 140px;'),
                         #"INTELIGENCIA COMPETITIVA ERMUA")),
    
    titlePanel("PANEL DE INTELIGENCIA COMPETITIVA"),
    
    navbarPage(id ="menu", "Menú Inteligencia Competitiva",
               
               tabPanel("Lectura BORME",
                        sidebarLayout(
                            
                            # Menú de datos
                            sidebarPanel(
                                
                                # Información general para el usuario
                                helpText(style = "font-size: 16px; text-align:center; font-weight: bold; padding-bottom: 10px;",
                                         "INFORMACIÓN GENERAL", ),
                                
                                helpText(style = "font-size: 14px; padding-bottom: 5px;",
                                         "Selecciona:",
                                         br(), HTML('&nbsp;'), HTML('&nbsp;'),
                                         "1) Intervalo de fechas.",
                                         br(), HTML('&nbsp;'), HTML('&nbsp;'),
                                         "2) Variables a mostrar.",
                                         br(), HTML('&nbsp;'), HTML('&nbsp;'),
                                         "3) (opcional) Variables por las que hacer el filtrado."),
                                
                                HTML('<hr style="size="30";">'),
                                
                                # FILTRADO POR INTERVALO DE FECHAS
                                fluidRow(style='padding-bottom: 14px;text-align:center;',
                                         
                                         helpText(style = "font-size: 16px; text-align:center; font-weight: bold; padding-bottom: 10px;",
                                                  "INSERCIÓN DE DATOS", ),
                                         
                                         helpText(style = "font-size: 14px; text-align:center;",
                                                  "1) Selección de intervalo de fechas", ),
                                         # RANGO DE FECHAS
                                         dateRangeInput("fechas_borme",
                                                        "Rango de fechas",
                                                        start = format(Sys.time()),
                                                        end = format(Sys.time())),
                                ),
                                
                                # VARIABLES A TRABAJAR/MOSTRAR
                                fluidRow(style='padding-bottom: 14px;',
                                         helpText(style = "font-size: 14px; text-align:center;",
                                                  "2) Selección de variables a mostrar"),
                                         
                                         column(6,
                                                
                                                sliderInput("radio_num","Radio en km",min = 10, max = 150, 50, step = 1),
                                                
                                                #Empresa dentro del radio de referencia
                                                checkboxInput("radio_ref", "Activar control visualización empresas por radio de referencia.",
                                                              value = FALSE),
                                                
                                                # Filtrado por domicilio de empresa dentro del radio de referencia
                                                radioButtons("radio_ref_bool", "Dentro del radio de referencia",
                                                             c("SÍ" = "Dentro",
                                                               "NO" = "Fuera",
                                                               "TODOS" = "todos"),
                                                             selected = "todos")
                                         ),
                                         
                                         column(6,
                                                checkboxGroupInput("checkGroup", label = h4("Selección de variables"), 
                                                                   choices = list("Constitución objeto social" = "Constitución.objeto.social",
                                                                                  "Cambio objeto social" = "Cambio.objeto.social",
                                                                                  "Ceses" = "Ceses",
                                                                                  "Nombramientos" = "Nombr",
                                                                                  "Ampliaciones" = "Ampl",
                                                                                  "Disolución" = "Disolución",
                                                                                  "Extinción" = "Extinción")
                                                                   ),
                                         )
                                ),
                                
                                # VARIABLES POR LAS QUE FILTRAR
                                fluidRow(style='padding-bottom: 16px; text-align:center;',
                                         helpText(style = "font-size: 14px;",
                                                  "3) Selección de filtros"),
                                         
                                         # Búsqueda por objeto social
                                         textInput("f_objeto_social", "Búsqueda por objeto social"),
                                         
                                         # Búsqueda por nombre de empresa
                                         textInput("f_empresa", "Búsqueda por empresa"),
                                         
                                         # Boton descarga borme
                                         downloadButton("descarga_borme", "Descarga datos Borme en .csv"),
                                ),
                                fluidRow(style='padding-bottom: 16px; text-align:center;',
                                         tags$div(title="Para cargar datos históricos del Borme, pulsa en \"Carga de datos históricos\". En el panel de fechas emergente, selecciona el rango de fechas deseado y pulsa \"Cargar datos\" para confrimar la operación",
                                            actionButton("carga_DHB","Carga de datos históricos",selected = FALSE),
                                         ),
                                ),
                                fluidRow(style='padding-bottom: 16px; text-align:center;',
                                        
                                         dateRangeInput("fechas_borme_hist",
                                                        "Rango de fechas",
                                                        start = format(Sys.time()),
                                                        end = format(Sys.time())),
                                         
                                         tags$head(tags$script(src = "mensajes.js")),
                                         actionButton("inicio_carga_DHB", "Cargar datos")
                                ),width = 3,
                            ),
                            
                            mainPanel(
                                
                                tabsetPanel(id = "tabs_borme",
                                            
                                            tabPanel(id = "tab_general",
                                                     span("General",title="Contiene todos los datos del Borme"),   #Con span genero un popup de ayuda.
                                                     
                                                     # Panel DATAFRAME
                                                     fluidRow(style='padding-top: 18px;',
                                                         dataTableOutput("tabla_borme_general"),
                                                     ),
                                            ),
                                            
                                            tabPanel(id = "tab_constitucion",
                                                     span("Constitución empresas",title="Contiene los datos nuevas empresas constituidas"),
                                                    # Panel MAPA
                                                    fluidRow(style='padding-bottom: 25px;',
                                                     leafletOutput("mapa_borme_constitucion"),
                                                    ),
                                                    
                                                    # Panel DATAFRAME
                                                    fluidRow(
                                                    dataTableOutput("tabla_borme_const"),
                                                    ),
                                                    
                                                    # Panel GRÁFICO DE BARRAS RECUENTO EMPRESAS CONSTITUIDAS
                                                    fluidRow(style='padding-bottom: 25px;',
                                                     plotOutput("grafico_borme"),
                                                    )
                                            ),
                                            
                                            tabPanel(id = "tab_cds",
                                                     span("Cambio domicilio social",title="Contiene los datos de empresas con cambio de domicilio social"),   #Con span genero un popup de ayuda.
                                                     
                                                     # Panel MAPA
                                                     fluidRow(style='padding-bottom: 25px;',
                                                              leafletOutput("mapa_borme_cambio_ds"),
                                                     ),
                                                     # Panel DATAFRAME
                                                     fluidRow(style='padding-bottom: 25px;',
                                                              dataTableOutput("tabla_borme_cambio_ds"),
                                                     ),
                                            ),
                                            tabPanel(id = "tab_disolución",
                                                     span("Disolución",title="Contiene los datos de empresas en disolución"),   #Con span genero un popup de ayuda.
                                                     
                                                     # Panel DATAFRAME
                                                     fluidRow(style='padding-bottom: 25px;',
                                                              dataTableOutput("tabla_borme_disolucion"),
                                                     ),
                                            )
                                )
                                
                            )
                        )
               ),
               
               tabPanel("Lectura Noticias",
                        sidebarLayout(
                            
                            # Menú de datos
                            sidebarPanel(
                                
                                # Información general para el usuario
                                helpText(style = "font-size: 16px; text-align:center; font-weight: bold; padding-bottom: 10px;",
                                         "INFORMACIÓN GENERAL", ),
                                
                                helpText(style = "font-size: 14px; padding-bottom: 10px;",
                                         "Selecciona:",
                                         br(), HTML('&nbsp;'), HTML('&nbsp;'),
                                         "1) Intervalo de fechas.",
                                         br(), HTML('&nbsp;'), HTML('&nbsp;'),
                                         "2) (opcional) Variables por las que hacer el filtrado."),
                                
                                HTML('<hr style="size="30";">'),
                                
                                # FILTRADO POR INTERVALO DE FECHAS
                                fluidRow(style='padding-bottom: 18px; text-align:center;',
                                         
                                         helpText(style = "font-size: 16px; text-align:center; font-weight: bold; padding-bottom: 10px;",
                                                  "INSERCIÓN DE DATOS", ),
                                         
                                         helpText(style = "font-size: 14px; text-align:center;",
                                                  "1) Selección de intervalo de fechas", ),
                                         # RANGO DE FECHAS
                                         dateRangeInput("fechas_noticias",
                                                        "Rango de fechas",
                                                        start = format(Sys.time()),
                                                        end = format(Sys.time())),
                                ),
                                
                                # VARIABLES POR LAS QUE FILTRAR
                                fluidRow(style='padding-bottom: 18px; text-align:center;',
                                         helpText(style = "font-size: 14px;",
                                                  "3) Selección de filtros"),
                                         
                                         # Búsqueda por objeto social
                                         textInput("palabra_clave", "Búsqueda por palabra clave"),
                                         
                                         # Botón descarga noticias
                                         downloadButton("descarga_noticias", "Descarga datos Noticias en .csv")
                                ),width = 3,
                            ),
                            
                            # Panel de resultados
                            mainPanel(
                                # Panel DATAFRAME
                                fluidRow(
                                    dataTableOutput("tabla_noticias"),
                                ),
                                
                                # Panel GRÁFICO DE BARRAS RECUENTO NOTICIAS
                                fluidRow(style='padding-bottom: 25px;',
                                         plotOutput("grafico_noticias"),
                                )
                            )
                        )
               ),
               
               tabPanel("Censo de empresas",
                        sidebarLayout(
                            sidebarPanel(
                                dateRangeInput("fechas","Filtro por intervalo de fechas",start = "1960-01-01", end = Sys.Date()),
                                textInput("palabra_clave", "Búsqueda por palabra clave"),
                                selectInput("calle", "Filtro por ubicación",
                                            c("Todas",gsub("[(].*","",gsub(",.*","",unique(df_censo$`Domicilio_social`)))[order(gsub(",.*","",unique(df_censo$`Domicilio_social`)))])),
                                sliderInput("empleados", "Filtro por rango de empleados",0,max(na.omit(as.numeric(unique(gsub("[ (].*","",df_censo$Empleados))))),c(0,100),step = 1
                                ),
                                selectInput("div_cnae", "Filtro por CNAE",
                                            #c("Todos",substring(unique(df_censo$CNAE),1,2)[order(substring(unique(df_censo$CNAE),1,2))][-1])
                                            c("Todos",df_cnae$completo), multiple = TRUE, selected = "Todos"
                                ),
                                #div(style = "color: black; font-size:14px; font-weight: bold;","Filtro redes sociales"),
                                radioButtons("RRSS", "Filtro por redes sociales",
                                             choices = list("Con RRSS" = 1, "Sin RRSS" = 2,
                                                            "Todas" = 3), selected = 3),
                                div(style = "color: black; font-size:14px; font-weight: bold;","Filtro empresas extintas"),
                                checkboxInput("extinguidas", "Ocultar empresas extintas", value = TRUE, width = NULL),
                                downloadButton("downloadDatacenso_csv", "Descargar CSV"),
                                width = 3,
                            ),
                            
                            mainPanel(
                                tabsetPanel(id = "tabs_censo",
                                            tabPanel(id = "censo","Censo de empresas",
                                                     fluidRow(
                                                         leafletOutput("mapa_censo", height = 500)
                                                     ),
                                                     br(),
                                                     fluidRow(
                                                         dataTableOutput("tabla_censo")
                                                     )
                                            )
                                )
                            )
                        )
               ), #Cierre panel censo
               
               tabPanel("Redes sociales",
                        sidebarLayout(
                            
                            # Menú de datos
                            sidebarPanel(
                                
                                # Información general para el usuario
                                helpText(style = "font-size: 16px; text-align:center; font-weight: bold; padding-bottom: 10px;",
                                         "INFORMACIÓN GENERAL", ),
                                
                                helpText(style = "font-size: 14px; padding-bottom: 10px;",
                                         "Selecciona:",
                                         br(), HTML('&nbsp;'), HTML('&nbsp;'),
                                         "1) Intervalo de fechas.",
                                         br(), HTML('&nbsp;'), HTML('&nbsp;'),
                                         "2) (opcional) Variables por las que hacer el filtrado."),
                                
                                HTML('<hr style="size="30";">'),
                                
                                # FILTRADO POR INTERVALO DE FECHAS
                                fluidRow(style='padding-bottom: 18px; text-align:center;',
                                         
                                         helpText(style = "font-size: 16px; text-align:center; font-weight: bold; padding-bottom: 10px;",
                                                  "INSERCIÓN DE DATOS", ),
                                         
                                         helpText(style = "font-size: 14px; text-align:center;",
                                                  "1) Selección de intervalo de fechas", ),
                                         # RANGO DE FECHAS
                                         dateRangeInput("fechas_redes",
                                                        "Rango de fechas",
                                                        start = format(Sys.time()),
                                                        end = format(Sys.time())),
                                ),
                                
                                # VARIABLES POR LAS QUE FILTRAR
                                fluidRow(style='padding-bottom: 18px; text-align:center;',
                                         helpText(style = "font-size: 14px;",
                                                  "3) Selección de filtros"),
                                         
                                         # Búsqueda por palabra_clave
                                         textInput("palabra_clave_redes", "Búsqueda por palabra clave"),
                                         
                                         # Botón descarga noticias
                                         downloadButton("descarga_redes", "Descarga datos análisis redes sociales en .csv")
                                ),width = 3,
                            ),
                            
                            # Panel de resultados
                            mainPanel(
                                tabsetPanel(id = "tabs_redes",
                                            tabPanel(id = "analisis_twitter",
                                                     span("Análisis descriptivo por palabras clave",title="Contiene la descripción (tweets, recuentos, usuarios...) de los diferentes tweets publicados"),   #Con span genero un popup de ayuda.
                                                     # Panel DATAFRAME
                                                     fluidRow(
                                                         dataTableOutput("tabla_redes"),
                                                     )
                                            ),
                                            tabPanel(id = "localiz_usuarios",
                                                     span("Localización tweets",title="Contiene la descripción (tweets, recuentos, usuarios...) de los diferentes tweets publicados"),   #Con span genero un popup de ayuda.
                                                     # Panel MAPA
                                                     fluidRow(style='padding-bottom: 25px;',
                                                              leafletOutput("mapa_redes"),
                                                     ),
                                                     # Panel DATAFRAME
                                                     fluidRow(
                                                         dataTableOutput("tabla_redes_localiz"),
                                                     )
                                            ),
                                            tabPanel(id = "analisis_twitter_cuentas",
                                                     span("Análisis descriptivo por cuentas",title="Contiene la descripción (tweets, recuentos, usuarios...) de los diferentes tweets publicados"),   #Con span genero un popup de ayuda.
                                                     # Panel DATAFRAME
                                                     fluidRow(
                                                         dataTableOutput("tabla_redes_cuentas")
                                                     )
                                            )
                                )
                                            
                            )
                        )
               ),
               tabPanel("Contrataciones Estado",
                        sidebarLayout(
                            
                            # Menú de datos
                            sidebarPanel(width= 3,
                                
                                # Información general para el usuario
                                helpText(style = "font-size: 16px; text-align:center; font-weight: bold; padding-bottom: 10px;",
                                         "INFORMACIÓN GENERAL", ),
                                
                                helpText(style = "font-size: 14px; padding-bottom: 10px;",
                                         "Selecciona:",
                                         br(), HTML('&nbsp;'), HTML('&nbsp;'),
                                         "1) Intervalo de fechas.",
                                         br(), HTML('&nbsp;'), HTML('&nbsp;'),
                                         "2) (opcional) Variables por las que hacer el filtrado."),
                                
                                HTML('<hr style="size="30";">'),
                                
                                # FILTRADO POR INTERVALO DE FECHAS
                                fluidRow(style='padding-bottom: 18px; text-align:center;',
                                         
                                         helpText(style = "font-size: 16px; text-align:center; font-weight: bold; padding-bottom: 10px;",
                                                  "INSERCIÓN DE DATOS", ),
                                         
                                         helpText(style = "font-size: 14px; text-align:center;",
                                                  "1) Selección de intervalo de fechas", ),
                                         # RANGO DE FECHAS
                                         dateRangeInput("fechas_contrataciones",
                                                        "Rango de fechas",
                                                        start = format(Sys.time()),
                                                        end = format(Sys.time())),
                                ),
                                
                                fluidRow(style='padding-bottom: 10px; text-align:center;',
                                         helpText(style = "font-size: 14px;",
                                                  "3) Selección de filtros"),
                                ),
                                
                                # VARIABLES POR LAS QUE FILTRAR
                                fluidRow(style='padding-left: 18px; text-align:left;',
                                         
                                         helpText(style = "font-size: 14px; font-weight: bold; color: black;",
                                                  "Plataforma de contrtación"),
                                         
                                         #Contrataciones Euskadi
                                         checkboxInput('contr_euskadi', 'Contrataciones Euskadi',value = F),
                                         
                                         #Estado licitación
                                         checkboxGroupInput("check_estado", label="Estado contratación", 
                                                            choices = c("Publicada" = "Publicada",
                                                                        "Adjudicada" = "Adjudicada",
                                                                        "Parcialmente adjudicada" = "Parcialmente adjudicada",
                                                                        "Resuelta" = "Resuelta",
                                                                        "Parcialmente Resuelta" = "Parcialmente Resuelta",
                                                                        "Resolución Provisional" = "Resolución Provisional",
                                                                        "Anuncio Previo" = "Anuncio Previo",
                                                                        "Anulada" = "Anulada",
                                                                        "Evaluación" = "Evaluación",
                                                                        "Evaluación Previa" = "Evaluación Previa"),
                                                      
                                                            selected = "Publicada",)
                                ),
                                
                                fluidRow(style='padding-bottom: 18px; text-align:center;',

                                         # Búsqueda por objeto social
                                         textInput("palabra_clave_contratacionesEstado", "Búsqueda por palabra clave"),
                                         
                                         # Botón descarga noticias
                                         downloadButton("descarga_contrataciones_Estado", "Descarga datos en .csv")
                                ),
                            ),
                            
                            # Panel de resultados
                            mainPanel(
                                # Panel DATAFRAME
                                fluidRow(
                                    dataTableOutput("tabla_contrataciones")
                                )
                            )
                        )
               ),
               tabPanel("Panel general",
                        sidebarLayout(
                            
                            # Menú de datos
                            sidebarPanel(
                                
                                # Información general para el usuario
                                helpText(style = "font-size: 16px; text-align:center; font-weight: bold; padding-bottom: 10px;",
                                         "INFORMACIÓN GENERAL", ),
                                
                                helpText(style = "font-size: 14px; padding-bottom: 10px;",
                                         "Selecciona:",
                                         br(), HTML('&nbsp;'), HTML('&nbsp;'),
                                         "1) Intervalo de fechas.",
                                         br(), HTML('&nbsp;'), HTML('&nbsp;'),
                                         "2) (opcional) Variables por las que hacer el filtrado."),
                                
                                HTML('<hr style="size="30";">'),
                                
                                # FILTRADO POR INTERVALO DE FECHAS
                                fluidRow(style='padding-bottom: 18px; text-align:center;',
                                         
                                         helpText(style = "font-size: 16px; text-align:center; font-weight: bold; padding-bottom: 10px;",
                                                  "INSERCIÓN DE DATOS", ),
                                         
                                         helpText(style = "font-size: 14px; text-align:center;",
                                                  "1) Selección de intervalo de fechas", ),
                                         # RANGO DE FECHAS
                                         dateRangeInput("fechas_general",
                                                        "Rango de fechas",
                                                        start = format(Sys.time()),
                                                        end = format(Sys.time())),
                                ),
                                
                                # VARIABLES POR LAS QUE FILTRAR
                                fluidRow(style='padding-bottom: 18px; text-align:center;',
                                         helpText(style = "font-size: 14px;",
                                                  "3) Selección de filtros"),
                                         
                                         # Búsqueda por objeto social
                                         textInput("palabra_clave_general", "Búsqueda por palabra clave")
                                         
                                         # Botón descarga noticias
                                         #downloadButton("descarga_general", "Descarga datos Panel general en .csv")
                                ),width = 3,
                            ),
                            
                            # Panel de resultados
                            mainPanel(
                                column(
                                    # Panel DATAFRAME
                                    fluidRow(
                                        helpText(style = "font-size: 16px; text-align:center; padding-bottom: 10px; font-weight: bold; color: black;",
                                                 "TABLA LECTURA BORME"),
                                        dataTableOutput("tabla_general_borme")
                                    ),
                                    
                                    fluidRow(
                                        helpText(style = "font-size: 16px; text-align:center; padding-bottom: 10px; padding-top: 10px; font-weight: bold; color: black;",
                                                 "TABLA LECTURA NOTICIAS"),
                                        dataTableOutput("tabla_general_noticias")
                                    ),
                                    
                                    #fluidRow(
                                    #    helpText(style = "font-size: 16px; text-align:center; padding-bottom: 10px; padding-top: 10px; font-weight: bold; color: black;",
                                    #             "TABLA FICHAS EMPRESAS"),
                                    #    dataTableOutput("tabla_general_fichas")
                                    #),
                                    
                                    fluidRow(
                                        helpText(style = "font-size: 16px; text-align:center; padding-bottom: 10px; padding-top: 10px; font-weight: bold; color: black;",
                                                 "TABLA REDES SOCIALES"),
                                        dataTableOutput("tabla_general_twitter")
                                    ),
                                    fluidRow(
                                        helpText(style = "font-size: 16px; text-align:center; padding-bottom: 10px; padding-top: 10px; font-weight: bold; color: black;",
                                                 "TABLA CONTRATACIONES DEL ESTADO"),
                                        dataTableOutput("tabla_general_contratacionesEstado")
                                    ), width = 12
                                )
                            )
                        )
               ),
               tags$style(type = 'text/css',
                          '.dataTables_scrollBody {transform:rotateX(180deg);}',
                          '.dataTables_scrollBody table {transform:rotateX(180deg);}'
               )
    )
)
               

######################################################
# LÓGICA DE SERVIDOR
######################################################

server <- function(input, output, session) {
    
    ###############################################
    # INICIALIZACIÓN LÓGICA DE VISUALIZACIÓN OBJETOS SHINY
    
    # Lógica de visualización ocultación botones radio de referencia
    observeEvent(input$radio_ref, {
        
        if(input$radio_ref){
            shinyjs::show("radio_ref_bool")
        }else{
            shinyjs::hide("radio_ref_bool")
        }
    })
    
    # Lógica de visualización ocultación gráfica empresas constituidas
    observeEvent(gsub("á|é|í|ó|ú","",gsub(".*\">\\s*|</span>.*", "", input$tabs_borme)) == "Constitucin empresas", {
        
        if(gsub("á|é|í|ó|ú","",gsub(".*\">\\s*|</span>.*", "", input$tabs_borme)) == "Constitucin empresas"){
            shinyjs::show("grafico_borme")
        }else{
            shinyjs::hide("grafico_borme")
        }
    })
    
    
    #Lógica carga de datos históricos BORME
    #Lógica visualización
    observe(shinyjs::toggle("fechas_borme_hist", condition = input$carga_DHB > 0))
    observe(shinyjs::toggle("inicio_carga_DHB", condition = input$carga_DHB > 0))
    
    #Inicio proceso
    observeEvent(input$inicio_carga_DHB, {
        
        tiempo_estimado <- as.numeric(format(round(((3.5 * (as.numeric(input$fechas_borme_hist[2] - input$fechas_borme_hist[1]) + 1) * 0.7)/60), 2), nsmall = 2))
        fecha_finalizacion <- Sys.time() + tiempo_estimado*3600
        
        mensaje <- paste('Proceso de carga de datos históricos BORME iniciado. Cálculo estimado para finalización: ',
                         tiempo_estimado, " horas, por lo que no se recomienda volver a iniciar el mismo proceso hasta el: ",
                         fecha_finalizacion, sep = "")
        session$sendCustomMessage(type = 'testmessage',
                                  message = mensaje)
        
        #Lllamada a función lectura Borme por fechas
        status_borme_fechas <- llamada_api_fechas(as.character(input$fechas_borme_hist[1]), as.character(input$fechas_borme_hist[2]))
    })
    
    
    ###############################################
    # FUNCIONES DE APOYO
    
    #Obtención coordenadas municipio referencia
    coordenadas_municipio <- reactive({
        
        #df_atributos <- atributos_Borme()
        #municipio_ref <- df_atributos$value[df_atributos$key == "Municipio"]
        
        #Coordenadas de referencia del municipio con geocoder API
        #Endpoint geocoder API
        #geocoder_endpoint <- "https://geocoder.api.here.com/6.2/geocode.json?app_id=HRwFz9rfbtRq63qGH4ZQ&app_code=aMRd84WGRs4h1591F-g82w&searchtext="
        
        #coordenadas_ref_municipio <- jsonlite::fromJSON(paste(geocoder_endpoint,municipio_ref,"%20(Espa%C3%B1a)",sep = ""))
        #coordenadas_ref_municipio <- coordenadas_ref_municipio$Response$View$Result %>% as.data.frame()
        #longitud_ref_municipio <- coordenadas_ref_municipio$Location$DisplayPosition$Longitude
        #latitud_ref_municipio <- coordenadas_ref_municipio$Location$DisplayPosition$Latitude
        #coor_referencia <- c(longitud_ref_municipio, latitud_ref_municipio)
        
        coor_referencia <- c(-2.504553, 43.187484)
        
        return(coor_referencia)
    })
    
    ################################################
    # LLAMADAS API THINGSBOARD
    
    # Llamada API atributos activo Borme
    atributos_Borme <- reactive({
        #Llamada a la API
        atributos_df <- llamada_atributos()
        
        return(atributos_df)
    })
    
    # Llamada a API datos Thingsboard BORME reactivo
    datos_estructurados_borme <- reactive({
        
        if(input$menu == "Panel general"){
            fecha_inicial <- input$fechas_general[1]
            fecha_final <- input$fechas_general[2]
        }else{
            fecha_inicial <- input$fechas_borme[1]
            fecha_final <- input$fechas_borme[2]
        }
        
        tiempo_estimado <- as.numeric(fecha_final - fecha_inicial) * 0.4 #en segundos
        tiempo_estimado <- seconds_to_period(tiempo_estimado)
        
        show_modal_spinner(
            spin = "double-bounce",
            color = "#3c6e94",
            text = paste("Cargando datos.
            Tiempo estimado de la operación: ",tiempo_estimado,
                         " Por favor, espere.",sep = ""),
            session = session
        )
        
        #Llamada a la API
        datos_borme <- llamada_api(as.character(fecha_inicial), as.character(fecha_final))
        
        remove_modal_spinner(session = getDefaultReactiveDomain())
        
        nombres <- c("Empresa","Fusión sociedades abosrbidas", "Modificaciones estatutarias",
                     "Cambio denominación social", "Cambio domicilio social", "Cambio objeto social",
                     "Ceses liquiSoli", "Ceses apoderado", "Ceses Adm. Único",
                     "Ceses liquidador", "Ceses liquidador mancomunado", "Ceses adminSolid",
                     "Ceses Adm. Mancomunado", "Ceses Soc. Prof", "Ceses depositorio",
                     "Ceses entid. Deposit.", "Ceses entid. Promo.", "Ceses consejero",
                     "Ceses vicepresidente", "Ceses presidente", "Ceses secretario",
                     "Nombramiento liquiSoli", "Nombramiento apoderado", "Nombramiento Adm. Único",
                     "Nombramiento liquidador", "Nombramiento liquidador mancomunado", "Nombramiento Adm. Solid",
                     "Nombramiento Soc. Prof", "Nombramiento auditor","Nombramiento Adm. Mancomunado",
                     "Nombramiento Entid. Deposit.", "Nombramiento Entid. Promo.", "Nombramiento consejero",
                     "Nombramiento vicepresidente","Nombramiento presidente", "Nombramiento secretario",
                     "Ampliación capital suscrito", "Ampliación capital resultante suscrito", "Ampliación capital desembolsado",
                     "Ampliación capital resultante desembolsado", "Ampliación capital", "Declaración unipersonalidad socio único",
                     "Reducción capital importe reducción","Reducción capital resultante suscrito", "Reelecciones Adm. Único",
                     "Reelecciones auditor", "Reelecciones auditor suplente", "Revocaciones auditor",
                     "Revocaciones apoderado", "Revocaciones apoderado mancomunado", "Revocaciones apoderadoSol",
                     "Situación Concursal Procedimiento", "Situación Concursal Resolución firme","Situación Concursal Fecha Resolución",
                     "Situación Concursal Proceso", "Situación Concursal Juzgado", "Situación Concursal Juez",
                     "Situación Concursal Resoluciones","Disolución", "Extinción",
                     "Constitución comienzo operaciones", "Constitución objeto social","Constitución domicilio social",
                     "Constitución capital", "Otros conceptos","Datos registrales", 
                     "Latitud", "Longitud","Municipio","Distancia respecto municipio km", "Provincia","Fecha"
                     )
        
        
        provincias <- tolower(atributos_Borme()$value[atributos_Borme()$key == "Provincias"])
        
        # Manejo de error: "inexistencia de datos para el intervalo de fechas seleccionado" de cara al usuario
        shiny::validate(
            need(datos_borme != 0, "¡Aviso!\nNo existen datos disponibles para el intervalo de fechas seleccionado.\nSelecciona otro intervalo si lo deseas.")
        )

        datos_borme <- datos_borme[str_detect(provincias, gsub("/.*","",tolower(datos_borme$Provincia))), ]
        
        colnames(datos_borme) <- nombres
        
        return(datos_borme)
    })
    
    # Llamada a API datos Thingsboard NOTICIAS
    datos_estructurados_noticias <- reactive({
        
        if(input$menu == "Panel general"){
            fecha_inicial <- input$fechas_general[1]
            fecha_final <- input$fechas_general[2]
        }else{
            fecha_inicial <- input$fechas_noticias[1]
            fecha_final <- input$fechas_noticias[2]
        }
        
        tiempo_estimado <- as.numeric(fecha_final - fecha_inicial) * 0.4 #en segundos
        tiempo_estimado <- seconds_to_period(tiempo_estimado)
        
        show_modal_spinner(
            spin = "double-bounce",
            color = "#3c6e94",
            text = paste("Cargando datos.
            Tiempo estimado de la operación: ",tiempo_estimado,
                         " Por favor, espere.",sep = ""),
            session = session
        )
        
        #Llamada a la API
        datos_noticias <- llamada_api_noticias(as.character(fecha_inicial), as.character(fecha_final))
        
        remove_modal_spinner(session = getDefaultReactiveDomain())
        
        # Manejo de error: "inexistencia de datos para el intervalo de fechas seleccionado" de cara al usuario
        shiny::validate(
            need(datos_noticias != 0, "¡Aviso!\nNo existen datos disponibles para el intervalo de fechas seleccionado.\nSelecciona otro intervalo si lo deseas.")
        )
        
        
        ##############################***************************************
        
        # ******************** CONFIGURADO PARA CANTABRIA ****************************
        
        datos_noticias <- datos_noticias[!str_detect(tolower(datos_noticias$`Periódico Título`),"cantabria"), ]
        
        return(datos_noticias)
    })
    
    # Llamada a API datos Thingsboard Contrataciones del Estado
    datos_estructurados_contratacionesEstado <- reactive({
        
        if(input$menu == "Panel general"){
            fecha_inicial <- input$fechas_general[1]
            fecha_final <- input$fechas_general[2]
        }else{
            fecha_inicial <- input$fechas_contrataciones[1]
            fecha_final <- input$fechas_contrataciones[2]
        }
        
        tiempo_estimado <- as.numeric(fecha_final - fecha_inicial) * 0.4 #en segundos
        tiempo_estimado <- seconds_to_period(tiempo_estimado)
        
        show_modal_spinner(
            spin = "double-bounce",
            color = "#3c6e94",
            text = paste("Cargando datos.
            Tiempo estimado de la operación: ",tiempo_estimado,
                         " Por favor, espere.",sep = ""),
            session = session
        )
        
        #Llamada a la API
        datos_contrataciones <- llamada_api_contratacionesEstado(as.character(fecha_inicial), as.character(fecha_final))
        
        remove_modal_spinner(session = getDefaultReactiveDomain())
        
        # Manejo de error: "inexistencia de datos para el intervalo de fechas seleccionado" de cara al usuario
        shiny::validate(
            need(datos_contrataciones != 0, "¡Aviso!\nNo existen datos disponibles para el intervalo de fechas seleccionado.\nSelecciona otro intervalo si lo deseas.")
        )
        
        return(datos_contrataciones)
        
    })
    
    ##################################################
    # FILTRADOS
    
    # Filtrado datos
    datos_filtrados_borme <- reactive({
        
        df <- datos_estructurados_borme()  #Llamada a API
        
        # Búsqueda objeto social
        # Si detecta dato introducido por el usuario en f_objeto_social filtra la búsqueda y devuelve los valores en una lista para generar el mapa.
        if(!input$f_objeto_social == ""){
            #Filtrado por búsqueda de palabras clave
            objeto_social <- input$f_objeto_social
            tab_seleccionado <- gsub(".*\">\\s*|</span>.*", "", input$tabs_borme)
            if(tab_seleccionado == "Cambio domicilio social"){
                variable_donde_buscar <- df$`Cambio objeto social`
            }else{
                variable_donde_buscar <- df$`Constitución objeto social`
            }
            filtrado_objeto_social <- grepl(objeto_social, variable_donde_buscar, ignore.case = T)
        }else{
            filtrado_objeto_social <- !(df$Empresa %in% "-")
        }
        
        # Búsqueda empresa
        # Si detecta dato introducido por el usuario en f_empresa filtra la búsqueda y devuelve los valores en una lista para generar el mapa.
        if(!input$f_empresa == ""){
            #Filtrado por bÃºsqueda de palabras clave
            empresa <- input$f_empresa
            filtrado_empresa <- grepl(empresa, df$Empresa, ignore.case = T)
        }else{
            filtrado_empresa <- !(df$Empresa %in% "-")
        }
        
        #Filtro por radio de referencia
        radio_ref <- as.numeric(input$radio_num)
        df$`Distancia respecto municipio km` <- format(round(as.numeric(df$`Distancia respecto municipio km`), 3), nsmall = 3)
        df$`Distancia respecto municipio km`[grepl("NA",df$`Distancia respecto municipio km`)] <- "-"
        
        #Cálculo distancias
        for(i in 1:nrow(df)){
            if(!grepl("-",df$`Distancia respecto municipio km`[i])){
                if(as.numeric(df$`Distancia respecto municipio km`[i]) < radio_ref){
                    df$Dentro[i] <- "Dentro"
                }else{
                    df$Dentro[i] <- "Fuera"
                }
            }else{
                df$Dentro[i] <- NA
            }
        }
        
        
        #Si el usuario selecciona la visulaizaciÃ³n del radio de referencia, se le da la opción de filtrar por radio sí, no o todos.
        if(input$radio_ref & input$radio_ref_bool == "todos"){
            filtrado_radio <- !(df$Empresa %in% "-")
        }else if(input$radio_ref){
            #filtrado_radio <- df$`Dentro del radio de referencia km` %in% input$radio_ref_bool
            filtrado_radio <- df$Dentro %in% input$radio_ref_bool
        }else{
            filtrado_radio <- !(df$Empresa %in% "-")  #SelecciÃ³n de todos menos NA
        }
        
        df_filtrado <- subset(df, (filtrado_objeto_social & filtrado_radio & filtrado_empresa))
        
        
        ############
        #Filtrado en funcón de tabs
        tab_seleccionado <- gsub(".*\">\\s*|</span>.*", "", input$tabs_borme)
        tab_seleccionado <- gsub("á|é|í|ó|ú","",tab_seleccionado)
        
        switch (tab_seleccionado,
                "General"={
                    df_filtrado <- df_filtrado
                    tab <- 1
                },
                "Constitucin empresas"={
                    df_filtrado <- df_filtrado[!grepl("-",df_filtrado$`Constitución objeto social`), ]
                    tab <- 2
                },
                "Cambio domicilio social"={
                    df_filtrado <- df_filtrado[!grepl("-",df_filtrado$`Cambio domicilio social`), ]
                    tab <- 3
                },
                "Disolucin"={
                    df_filtrado <- df_filtrado[!grepl("-",df_filtrado$`Disolución`), ]
                    tab <- 4
                }
        )
        
        df_filtrado$Fecha <- gsub("-","/",df_filtrado$Fecha)
        
        df <- df_filtrado
        
        # Transformación de visualización. Se eliminan las columnas con todas las filas = NA.
        df_filtrado <- data.frame(lapply(df_filtrado, function(x) {
            gsub("-", NA, x)
        }), stringsAsFactors = F)
        
        #Comprobación de existencia de datos para el manejo de "-", NA y visualización variables
        if(nrow(df_filtrado) != 0){
            df_filtrado <- df_filtrado[, colSums(is.na(df_filtrado)) != nrow(df_filtrado)]  
            df_filtrado[is.na(df_filtrado)] <- "-"
            
            #Crear columna coordeandas empresa y no visualizar columna Latitud
            df_filtrado$`Coordenadas empresa` <- paste(df$Latitud,df$Longitud,sep = ", ")
            df_filtrado[df_filtrado=="-, -"]<-"-"
            df_filtrado <- df_filtrado[ , !(names(df_filtrado) %in% "Latitud")]
        }
        
        df_filtrado <- df_filtrado[!grepl("-",df_filtrado$Empresa), ]
        
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
            need(nrow(df_filtrado) != 0,
                 "¡Aviso!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo deseas.")
        )
        
        return(df_filtrado)
    })
    
    # Filtrado datos
    datos_filtrados_noticias <- reactive({
        
        df <- datos_estructurados_noticias()  #Llamada a API
        
        #Selección palabra clave en función de selección tab menú
        if(input$menu == "Panel general"){
            palabra_clave <- input$palabra_clave_general
        }else{
            palabra_clave <- input$palabra_clave
        }
        
        # FILTRADO POR PALABRA CLAVE
        # Si detecta dato introducido por el usuario en palabra clave filtra la búsqueda y devuelve los valores.
        if(palabra_clave != ""){
            
            #Filtrado por búsqueda de palabras clave. Se realiza máscara OR con resultados booleanos.
            filtrado_palabra_clave <- apply(as.data.frame(mapply(grepl, palabra_clave, df, ignore.case = T)),1,any)
        }else{
            filtrado_palabra_clave <- !(df$`Noticia link` %in% NA)
        }
        
        df_filtrado_noticias <- na.omit(subset(df, (filtrado_palabra_clave)))

        
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
            need(nrow(df_filtrado_noticias) != 0,
                 "¡Aviso!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo deseas.")
        )
        
        return(df_filtrado_noticias)
    })
    
    
    # Filtrado datos TWITTER PALABRA CLAVE + llamada API
    datos_filtrados_twitter <- reactive({
        
        if(input$menu == "Panel general"){
            fecha_inicial <- input$fechas_general[1]
            fecha_final <- input$fechas_general[2]
        }else{
            fecha_inicial <- input$fechas_redes[1]
            fecha_final <- input$fechas_redes[2]
        }
        
        df <- llamada_api_twitter(as.character(fecha_inicial), as.character(fecha_final))  #Llamada a API
 
        # Manejo de error: "inexistencia de datos para el intervalo de fechas seleccionado" de cara al usuario
        shiny::validate(
            need(df != 0, "¡Aviso!\nNo existen datos disponibles para el intervalo de fechas seleccionado.\nSelecciona otro intervalo si lo deseas.")
        )
        
        #Extracción palabra objetivo de búsqueda
        df$`Palabra asociada` <- paste(df$`Cadena de búsqueda x`, df$`Cadena de búsqueda y`, sep = ", ")
        
        for(i in 1:length(df$`Palabra asociada`)){
            df$`Palabra asociada`[i] <- paste(unique(str_split(df$`Palabra asociada`[i], ", ")[[1]]), collapse = ", ")
        }
        
        # Eliminación columnas cadena de búsqueda
        df <- df[ , -which(names(df) %in% c("Cadena de búsqueda y", "Cadena de búsqueda x"))]
        #Cambio de posición columna "Palabra asociada" a primera posición
        df2 <- subset(df, select = "Palabra asociada")
        df <- df[ , -which(names(df) %in% "Palabra asociada")]
        df <- data.frame(df2,df)
        names(df) <- str_replace_all(names(df),"[.]", " ")
        
        # Eliminación sufijos x (tweet) e y (usuario)
        names(df) <- str_replace_all(names(df)," x", "")
        names(df) <- str_replace_all(names(df)," y", "")
        
        
        
        # Manejo de error: "inexistencia de datos para el intervalo de fechas seleccionado" de cara al usuario
        shiny::validate(
            need(df != 0, "¡Aviso!\nNo existen datos disponibles para el intervalo de fechas seleccionado.\nSelecciona otro intervalo si lo deseas.")
        )
        
        #Selección palabra clave en función de selección tab menú
        if(input$menu == "Panel general"){
            palabra_clave <- input$palabra_clave_general
        }else{
            palabra_clave <- input$palabra_clave_redes
        }
        
        # FILTRADO POR PALABRA CLAVE
        # Si detecta dato introducido por el usuario en palabra clave filtra la búsqueda y devuelve los valores.
        if(!palabra_clave == ""){
            
            #Filtrado por búsqueda de palabras clave. Se realiza máscara OR con resultados booleanos.
            filtrado_palabra_clave <- apply(as.data.frame(mapply(grepl, palabra_clave, df, ignore.case = T)),1,any)
        }else{
            filtrado_palabra_clave <- !(df$Texto %in% NA)
        }
        
        df_filtrado_twitter <- na.omit(subset(df, (filtrado_palabra_clave)))
        
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
            need(nrow(df_filtrado_twitter) != 0,
                 "¡Aviso!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo deseas.")
        )
        
        return(df_filtrado_twitter)
    })
    
    # Filtrado datos TWITTER CENTROS EMPRESA + llamada API
    datos_filtrados_twitter_cuentas <- reactive({
        
        if(input$menu == "Panel general"){
            fecha_inicial <- input$fechas_general[1]
            fecha_final <- input$fechas_general[2]
        }else{
            fecha_inicial <- input$fechas_redes[1]
            fecha_final <- input$fechas_redes[2]
        }
        
        df <- llamada_api_twitter_cuentas(as.character(fecha_inicial), as.character(fecha_final))  #Llamada a API
        
        # Manejo de error: "inexistencia de datos para el intervalo de fechas seleccionado" de cara al usuario
        shiny::validate(
            need(df != 0, "¡Aviso!\nNo existen datos disponibles para el intervalo de fechas seleccionado.\nSelecciona otro intervalo si lo deseas.")
        )
        
        # FILTRADO POR PALABRA CLAVE
        #Selección palabra clave en función de selección tab menú
        if(input$menu == "Panel general"){
            palabra_clave <- input$palabra_clave_general
        }else{
            palabra_clave <- input$palabra_clave_redes
        }
        
        # FILTRADO POR PALABRA CLAVE
        # Si detecta dato introducido por el usuario en palabra clave filtra la búsqueda y devuelve los valores.
        if(!palabra_clave == ""){
            
            #Filtrado por búsqueda de palabras clave. Se realiza máscara OR con resultados booleanos.
            filtrado_palabra_clave <- apply(as.data.frame(mapply(grepl, palabra_clave, df, ignore.case = T)),1,any)
        }else{
            filtrado_palabra_clave <- !(df$Texto %in% NA)
        }
        
        df_filtrado_twitter <- na.omit(subset(df, (filtrado_palabra_clave)))
        
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
            need(nrow(df_filtrado_twitter) != 0,
                 "¡Aviso!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo deseas.")
        )
        
        return(df_filtrado_twitter)
    })
    
    # Filtrado contrataciones del estado por palabra clave
    datos_filtrados_contratacionesEstado <- reactive({
        
        datos_filtrados <- datos_estructurados_contratacionesEstado()
        
        # Filtrado por checkboxgroup
        log_filtro <- (datos_filtrados$Estado.de.la.Licitación %in% input$check_estado)
        pos_filtro <- grep(TRUE,log_filtro)
        
        if(length(pos_filtro)){
            datos_filtrados <- datos_filtrados[pos_filtro, ]
        }
        
        # Manejo de error: "inexistencia de datos para el intervalo de fechas seleccionado" de cara al usuario
        shiny::validate(
            need(nrow(datos_filtrados) > 1, "¡Aviso!\nNo existen datos disponibles para los filtros seleccionados.")
        )
        
        #Filtrado por rasio button
        if(input$contr_euskadi){
            datos_filtrados <- datos_filtrados[grep("euskadi", datos_filtrados$Enlace.licitación),]
        }
        
        # Manejo de error: "inexistencia de datos para el intervalo de fechas seleccionado" de cara al usuario
        shiny::validate(
            need(nrow(datos_filtrados) > 1, "¡Aviso!\nNo existen datos disponibles para los filtros seleccionados.")
        )
        
        datos_filtrados$Enlace.licitación <- paste0("<a href='", as.character(datos_filtrados$Enlace.licitación),"' >", as.character(datos_filtrados$Enlace.licitación),"</a>")
        datos_filtrados$Documento..Formalización <- paste0("<a href='", as.character(datos_filtrados$Documento..Formalización),"' >", as.character(datos_filtrados$Documento..Formalización),"</a>")
        datos_filtrados$Documento..Anuncio.de.Licitación <- paste0("<a href='", as.character(datos_filtrados$Documento..Anuncio.de.Licitación),"' >", as.character(datos_filtrados$Documento..Anuncio.de.Licitación),"</a>")
        datos_filtrados$Documento..Pliego <- paste0("<a href='", as.character(datos_filtrados$Documento..Pliego),"' >", as.character(datos_filtrados$Documento..Pliego),"</a>")
        datos_filtrados$Documento..Adjudicación <- paste0("<a href='", as.character(datos_filtrados$Documento..Adjudicación),"' >", as.character(datos_filtrados$Documento..Adjudicación),"</a>")
        
        # FILTRADO POR PALABRA CLAVE
        #Selección palabra clave en función de selección tab menú
        if(input$menu == "Panel general"){
            palabra_clave <- input$palabra_clave_general
        }else{
            palabra_clave <- input$palabra_clave_contratacionesEstado
        }
        
        # FILTRADO POR PALABRA CLAVE
        # Si detecta dato introducido por el usuario en palabra clave filtra la búsqueda y devuelve los valores.
        if(!palabra_clave == ""){
            
            #Filtrado por búsqueda de palabras clave. Se realiza máscara OR con resultados booleanos.
            filtrado_palabra_clave <- apply(as.data.frame(mapply(grepl, palabra_clave, datos_filtrados, ignore.case = T)),1,any)
        }else{
            filtrado_palabra_clave <- !(datos_filtrados$Expediente %in% NA)
        }
        
        df_filtrado <- na.omit(subset(datos_filtrados, (filtrado_palabra_clave)))
        
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
            need(nrow(df_filtrado) != 0,
                 "¡Aviso!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo deseas.")
        )
        
        return(df_filtrado)
        
    })
    
    #####################################################
    # GENERACIÓN MAPA
    
    # Generación mapa Leaflet CONSITUCIÓN BORME
    output$mapa_borme_constitucion <- renderLeaflet({
        
        df_filtrados <- datos_filtrados_borme()
        
        # Filtrado por selección de registro en la tabla
        filtrado_tabla <- input$tabla_borme_const_rows_selected
        if(length(filtrado_tabla)){
            df_filtrados <-  df_filtrados[filtrado_tabla, , drop = F]
        }
        
        coordenadas <- unlist(df_filtrados$`Coordenadas empresa`)
        
        #Eliminación de NAs en data frame para obtener únicamente las empresas geolocalizadas
        for(i in 1:length(unlist(coordenadas))){
            if(!str_detect(coordenadas[[i]],"[0-9]")){
                coordenadas[[i]] <- NA
            }
        }
        
        df_filtrados$`Coordenadas empresa` <- coordenadas
        
        coordenadas <- na.omit(coordenadas)
        latitud <- as.numeric(gsub("\\,.*","",coordenadas))
        longitud <- as.numeric(gsub(".*\\, ","",coordenadas))
        
        #Inicialización popup
        empresas_popup <- df_filtrados$Empresa[!is.na(df_filtrados$`Coordenadas empresa`)]
        const_objeto_social_popup <- df_filtrados$Constitución.objeto.social[!is.na(df_filtrados$`Coordenadas empresa`)]
        popup <- paste(empresas_popup, const_objeto_social_popup, sep = "\n")
        
        #Obtención atributo radio
        radio_ref <- as.numeric(input$radio_num)*1000
        
        #Creación mapa
        leaflet() %>% addTiles() %>% addMarkers(lng = longitud, 
                                                lat = latitud,
                                                popup = popup) %>% addCircles(lng = coordenadas_municipio()[1], lat = coordenadas_municipio()[2], weight = 2, color = "red",
                                                                              radius = radio_ref)
    })
    
    
    # Generación mapa Leaflet CAMBIO DOMICILIO SOCIAL BORME
    output$mapa_borme_cambio_ds <- renderLeaflet({
        
        df_filtrados <- datos_filtrados_borme()
        
        # Filtrado por selección de registro en la tabla
        filtrado_tabla <- input$tabla_borme_cambio_ds_rows_selected
        if(length(filtrado_tabla)){
            df_filtrados <-  df_filtrados[filtrado_tabla, , drop = F]
        }
        
        coordenadas <- unlist(df_filtrados$`Coordenadas empresa`)
        
        #Eliminación de NAs en data frame para obtener únicamente las empresas geolocalizadas
        for(i in 1:length(unlist(coordenadas))){
            if(!str_detect(coordenadas[[i]],"[0-9]")){
                coordenadas[[i]] <- NA
            }
        }
        
        df_filtrados$`Coordenadas empresa` <- coordenadas
        
        coordenadas <- na.omit(coordenadas)
        latitud <- as.numeric(gsub("\\,.*","",coordenadas))
        longitud <- as.numeric(gsub(".*\\, ","",coordenadas))
        
        #Inicialización popup
        empresas_popup <- df_filtrados$Empresa[!is.na(df_filtrados$`Coordenadas empresa`)]
        cambio_objeto_social_popup <- df_filtrados$Cambio.domicilio.social[!is.na(df_filtrados$`Coordenadas empresa`)]
        popup <- paste(empresas_popup, cambio_objeto_social_popup, sep = "\n")
        
        #Obtención atributo radio
        radio_ref <- as.numeric(input$radio_num)*1000
        
        #Creación mapa
        leaflet() %>% addTiles() %>% addMarkers(lng = longitud, 
                                                lat = latitud,
                                                popup = popup) %>% addCircles(lng = coordenadas_municipio()[1], lat = coordenadas_municipio()[2], weight = 2, color = "red",
                                                                              radius = radio_ref)
    })
    
    # Generación mapa Leaflet USUARIOS TWITTER
    output$mapa_redes <- renderLeaflet({
        
        df_filtrados <- datos_filtrados_twitter()
        
        df_filtrados <- subset(df_filtrados, !(df_filtrados$`Latitud usuario` %in% "-"))
        
        # Filtrado por selección de registro en la tabla
        filtrado_tabla <- input$tabla_redes_localiz_rows_selected
        if(length(filtrado_tabla)){
            df_filtrados <-  df_filtrados[filtrado_tabla, , drop = F]
        }
        
        latitud <- as.numeric(df_filtrados$`Latitud usuario`)
        longitud <- as.numeric(df_filtrados$`Longitud usuario`)
        
        #Inicialización popup
        popup <- paste(df_filtrados$`Nombre x`,df_filtrados$Texto, sep = ";\n")
        
        #Creación mapa
        leaflet() %>% addTiles() %>% addMarkers(lng = longitud, 
                                                lat = latitud,
                                                popup = popup)
    })
    
    #####################################################
    # GENERACIÓN DATAFRAME
    
    #####
    # PESTAÑA BORME
    #####
    
    #Manejo de tablas presentes en los tabs BORME
    manejo_tablas_borme <- reactive({
        
        df_tabla <- datos_filtrados_borme()
        
        ###########
        #Filtrado en función de checkBox
        if(!is.null(input$checkGroup)){
            posicion_en_nombres <- c()
            for(i in 1:length(input$checkGroup)){
                posicion_en_nombres <- append(posicion_en_nombres, grep(input$checkGroup[i],names(df_tabla)))
            }
            columnas <- c(1, posicion_en_nombres)
            df_tabla <- df_tabla[ , columnas]
            
            # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
            shiny::validate(
                need(!is.null(ncol(df_tabla)),
                     "¡Aviso!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo deseas.")
            )
            
            #No mostrar filas que contengan solo "-"
            filas_unicas <- !(duplicated(df_tabla[ , seq(2,ncol(df_tabla))]) | duplicated(df_tabla[ , seq(2,ncol(df_tabla))], fromLast = TRUE))
            df_tabla <- df_tabla[filas_unicas , ]
        }
        
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
            need(nrow(df_tabla) != 0,
                 "¡Aviso!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifica el valor de los filtros si lo deseas.")
        )
        
        # En caso de solo visualizar la variable empresa, se convierte a objeto tipo entero y hay que volver a reconvertirlo a data.frame
        if(typeof(df_tabla) == "integer"){
            df_tabla <- as.data.frame(df_tabla)
            names(df_tabla) <- "Empresa"
        }
        
        #Nombres de filas igual a ID entero ascendente
        #Sí no hay datos no modificar el nombre de las filas
        if(nrow(df_tabla) > 0){
            row.names(df_tabla) <- seq(1,nrow(df_tabla))
        }
        
        return(df_tabla)
        
    })
    

    #Objeto reactivo (permite acceder a el por todas las funciones). 
    #Inicialización valor 0 de variable en la que se vuelca el valor de la página anterior para implementar la lógica de visualización de la tabla
    pag_anterior_1 <- reactiveValues(valor = 0)
    
    #Generación dataframe como tabla empresas constituidas
    output$tabla_borme_const <- renderDataTable({
        
        df_tabla <- manejo_tablas_borme()
        
        # Orden columnas en función de longitud de valores dataframe de la fila seleccionada 
        filtrado_tabla <- input$tabla_borme_const_rows_selected
        if(length(filtrado_tabla)){
            n_palabras_valores_fila <- sapply(df_tabla[filtrado_tabla[length(filtrado_tabla)],],nchar)
            vector_num_palabras_sin_empresa <- order(n_palabras_valores_fila[2:length(n_palabras_valores_fila)], decreasing = T) + 1
            df_tabla <- df_tabla[ , c(1,vector_num_palabras_sin_empresa)]
        }
        
        #Lógica de visualización de la tabla. Debe permitir ordenar las columnas de la fila seleccionada manteniendo la tabla en la página en la que se encuentre.
        #El parámetro displayStart de las opciones de la datatable me indican en que página debe empezar la tabla. Si hay 5 registros por pag, un valor de 20 lleva a la página 3.
        # Hay que jugar con el valor a restar al número de la fila/registro en función de su posición respecto la primera fila de esa pagina. Como esta en JS, el array empieza por 0 (de la fila 15 a la 11 hay que restar 5 posiciones)
        if(length(filtrado_tabla)){
            valor_a_restar <- filtrado_tabla[length(filtrado_tabla)] - 5*(filtrado_tabla[length(filtrado_tabla)] - (filtrado_tabla[length(filtrado_tabla)]-floor(filtrado_tabla[length(filtrado_tabla)]/5)))
            
            if(valor_a_restar == 0){
                valor_a_restar <- 5
            }else{
                valor_a_restar <- filtrado_tabla[length(filtrado_tabla)] - 5*(filtrado_tabla[length(filtrado_tabla)] - (filtrado_tabla[length(filtrado_tabla)]-floor(filtrado_tabla[length(filtrado_tabla)]/5)))
            }
            
            pag_anterior <- filtrado_tabla[length(filtrado_tabla)]-valor_a_restar
            pag_anterior_1$valor <- pag_anterior
        }else{
            if(exists("pag_anterior_1")){
                pag_anterior <- pag_anterior_1$valor
            }else{
                pag_anterior <- 0
            }
        }
        
        #Límite visualización registros tabla
        tabla <- datatable(df_tabla, extensions = c('FixedHeader','Buttons'), options = list(pageLength = 5,
                                                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                    scrollX=TRUE, 
                                                    scrollCollapse=TRUE,
                                                    displayStart = pag_anterior,
                                                    fixedHeader = TRUE,
                                                    dom = 'lBfrtip', 
                                                    buttons = c('copy', 'csv', 'excel', 'pdf')),
                           selection = list(mode = "multiple",
                                            selected = filtrado_tabla,   # Selección de las filas de la tabla seleccionadas por el usuario.
                                            target = "row"),
                           escape = FALSE)
        
        return(tabla)

    })
    
    #Generación dataframe como tabla borme general
    output$tabla_borme_general <- renderDataTable({
        
        df_tabla <- manejo_tablas_borme()
        
        # Orden columnas en función de longitud de valores dataframe de la fila seleccionada 
        filtrado_tabla <- input$tabla_borme_general_rows_selected
        if(length(filtrado_tabla)){
            n_palabras_valores_fila <- sapply(df_tabla[filtrado_tabla[length(filtrado_tabla)],],nchar)
            vector_num_palabras_sin_empresa <- order(n_palabras_valores_fila[2:length(n_palabras_valores_fila)], decreasing = T) + 1
            df_tabla <- df_tabla[ , c(1,vector_num_palabras_sin_empresa)]
        }
        
        #Lógica de visualización de la tabla. Debe permitir ordenar las columnas de la fila seleccionada manteniendo la tabla en la página en la que se encuentre.
        #El parámetro displayStart de las opciones de la datatable me indican en que página debe empezar la tabla. Si hay 5 registros por pag, un valor de 20 lleva a la página 3.
        # Hay que jugar con el valor a restar al número de la fila/registro en función de su posición respecto la primera fila de esa pagina. Como esta en JS, el array empieza por 0 (de la fila 15 a la 11 hay que restar 5 posiciones)
        if(length(filtrado_tabla)){
            valor_a_restar <- filtrado_tabla[length(filtrado_tabla)] - 5*(filtrado_tabla[length(filtrado_tabla)] - (filtrado_tabla[length(filtrado_tabla)]-floor(filtrado_tabla[length(filtrado_tabla)]/5)))
            
            if(valor_a_restar == 0){
                valor_a_restar <- 5
            }else{
                valor_a_restar <- filtrado_tabla[length(filtrado_tabla)] - 5*(filtrado_tabla[length(filtrado_tabla)] - (filtrado_tabla[length(filtrado_tabla)]-floor(filtrado_tabla[length(filtrado_tabla)]/5)))
            }
            
            pag_anterior <- filtrado_tabla[length(filtrado_tabla)]-valor_a_restar
            pag_anterior_1$valor <- pag_anterior
        }else{
            if(exists("pag_anterior_1")){
                pag_anterior <- pag_anterior_1$valor
            }else{
                pag_anterior <- 0
            }
        }
        
        #Límite visualización registros tabla
        tabla <- datatable(df_tabla, extensions = c('FixedHeader','Buttons'), options = list(pageLength = 5,
                                                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                    scrollX=TRUE, 
                                                    scrollCollapse=TRUE,
                                                    displayStart = pag_anterior,
                                                    fixedHeader = TRUE,
                                                    dom = 'lBfrtip', 
                                                    buttons = c('copy', 'csv', 'excel', 'pdf')),
                           selection = list(mode = "multiple",
                                            selected = filtrado_tabla,   # Selección de las filas de la tabla seleccionadas por el usuario.
                                            target = "row"),
                           escape = FALSE)
        
        return(tabla)
        
    },options = list(scrollX = T))
    
    
    #Generación dataframe como tabla empresas con cambio domicilio social
    output$tabla_borme_cambio_ds <- renderDataTable({
        
        df_tabla <- manejo_tablas_borme()
        
        # Orden columnas en función de longitud de valores dataframe de la fila seleccionada 
        filtrado_tabla <- input$tabla_borme_cambio_ds_rows_selected
        if(length(filtrado_tabla)){
            n_palabras_valores_fila <- sapply(df_tabla[filtrado_tabla[length(filtrado_tabla)],],nchar)
            vector_num_palabras_sin_empresa <- order(n_palabras_valores_fila[2:length(n_palabras_valores_fila)], decreasing = T) + 1
            df_tabla <- df_tabla[ , c(1,vector_num_palabras_sin_empresa)]
        }
        
        #Lógica de visualización de la tabla. Debe permitir ordenar las columnas de la fila seleccionada manteniendo la tabla en la página en la que se encuentre.
        #El parámetro displayStart de las opciones de la datatable me indican en que página debe empezar la tabla. Si hay 5 registros por pag, un valor de 20 lleva a la página 3.
        # Hay que jugar con el valor a restar al número de la fila/registro en función de su posición respecto la primera fila de esa pagina. Como esta en JS, el array empieza por 0 (de la fila 15 a la 11 hay que restar 5 posiciones)
        if(length(filtrado_tabla)){
            valor_a_restar <- filtrado_tabla[length(filtrado_tabla)] - 5*(filtrado_tabla[length(filtrado_tabla)] - (filtrado_tabla[length(filtrado_tabla)]-floor(filtrado_tabla[length(filtrado_tabla)]/5)))
            
            if(valor_a_restar == 0){
                valor_a_restar <- 5
            }else{
                valor_a_restar <- filtrado_tabla[length(filtrado_tabla)] - 5*(filtrado_tabla[length(filtrado_tabla)] - (filtrado_tabla[length(filtrado_tabla)]-floor(filtrado_tabla[length(filtrado_tabla)]/5)))
            }
            
            pag_anterior <- filtrado_tabla[length(filtrado_tabla)]-valor_a_restar
            pag_anterior_1$valor <- pag_anterior
        }else{
            if(exists("pag_anterior_1")){
                pag_anterior <- pag_anterior_1$valor
            }else{
                pag_anterior <- 0
            }
        }
        
        #Límite visualización registros tabla
        tabla <- datatable(df_tabla, extensions = c('FixedHeader','Buttons'), options = list(pageLength = 5,
                                                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                    scrollX=TRUE, 
                                                    scrollCollapse=TRUE,
                                                    displayStart = pag_anterior,
                                                    fixedHeader = TRUE,
                                                    dom = 'lBfrtip', 
                                                    buttons = c('copy', 'csv', 'excel', 'pdf')),
                           selection = list(mode = "multiple",
                                            selected = filtrado_tabla,   # Selección de las filas de la tabla seleccionadas por el usuario.
                                            target = "row"),
                           escape = FALSE)
        
        return(tabla)
        
    })
    
    #Generación dataframe como tabla empresas con cambio domicilio social
    output$tabla_borme_disolucion <- renderDataTable({
        
        df_tabla <- manejo_tablas_borme()
        
        # Orden columnas en función de longitud de valores dataframe de la fila seleccionada 
        filtrado_tabla <- input$tabla_borme_cambio_ds_rows_selected
        if(length(filtrado_tabla)){
            n_palabras_valores_fila <- sapply(df_tabla[filtrado_tabla[length(filtrado_tabla)],],nchar)
            vector_num_palabras_sin_empresa <- order(n_palabras_valores_fila[2:length(n_palabras_valores_fila)], decreasing = T) + 1
            df_tabla <- df_tabla[ , c(1,vector_num_palabras_sin_empresa)]
        }
        
        #Lógica de visualización de la tabla. Debe permitir ordenar las columnas de la fila seleccionada manteniendo la tabla en la página en la que se encuentre.
        #El parámetro displayStart de las opciones de la datatable me indican en que página debe empezar la tabla. Si hay 5 registros por pag, un valor de 20 lleva a la página 3.
        # Hay que jugar con el valor a restar al número de la fila/registro en función de su posición respecto la primera fila de esa pagina. Como esta en JS, el array empieza por 0 (de la fila 15 a la 11 hay que restar 5 posiciones)
        if(length(filtrado_tabla)){
            valor_a_restar <- filtrado_tabla[length(filtrado_tabla)] - 5*(filtrado_tabla[length(filtrado_tabla)] - (filtrado_tabla[length(filtrado_tabla)]-floor(filtrado_tabla[length(filtrado_tabla)]/5)))
            
            if(valor_a_restar == 0){
                valor_a_restar <- 5
            }else{
                valor_a_restar <- filtrado_tabla[length(filtrado_tabla)] - 5*(filtrado_tabla[length(filtrado_tabla)] - (filtrado_tabla[length(filtrado_tabla)]-floor(filtrado_tabla[length(filtrado_tabla)]/5)))
            }
            
            pag_anterior <- filtrado_tabla[length(filtrado_tabla)]-valor_a_restar
            pag_anterior_1$valor <- pag_anterior
        }else{
            if(exists("pag_anterior_1")){
                pag_anterior <- pag_anterior_1$valor
            }else{
                pag_anterior <- 0
            }
        }
        
        #Límite visualización registros tabla
        tabla <- datatable(df_tabla, extensions = c('FixedHeader','Buttons'), options = list(pageLength = 5,
                                                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                    scrollX=TRUE, 
                                                    scrollCollapse=TRUE,
                                                    displayStart = pag_anterior,
                                                    fixedHeader = TRUE,
                                                    dom = 'lBfrtip', 
                                                    buttons = c('copy', 'csv', 'excel', 'pdf')),
                           selection = list(mode = "multiple",
                                            selected = filtrado_tabla,   # Selección de las filas de la tabla seleccionadas por el usuario.
                                            target = "row"),
                           escape = FALSE)
        
        return(tabla)
        
    })
    
    ####
    # PESTAÑA GENERAL
    ####
    
    #Tabla panel general Borme
    output$tabla_general_borme <- renderDataTable({
        
        df_tabla <- datos_estructurados_borme()
        
        #Nombres de filas igual a ID entero ascendente
        #Sí no hay datos no modificar el nombre de las filas
        if(nrow(df_tabla) > 0){
            row.names(df_tabla) <- seq(1,nrow(df_tabla))
        }
        
        # Orden columnas en función de longitud de valores dataframe de la fila seleccionada 
        filtrado_tabla <- input$tabla_general_borme_rows_selected
        if(length(filtrado_tabla)){
            n_palabras_valores_fila <- sapply(df_tabla[filtrado_tabla[length(filtrado_tabla)],],nchar)
            vector_num_palabras_sin_empresa <- order(n_palabras_valores_fila[2:length(n_palabras_valores_fila)], decreasing = T) + 1
            df_tabla <- df_tabla[ , c(1,vector_num_palabras_sin_empresa)]
        }
        
        #Lógica de visualización de la tabla. Debe permitir ordenar las columnas de la fila seleccionada manteniendo la tabla en la página en la que se encuentre.
        #El parámetro displayStart de las opciones de la datatable me indican en que página debe empezar la tabla. Si hay 5 registros por pag, un valor de 20 lleva a la página 3.
        # Hay que jugar con el valor a restar al número de la fila/registro en función de su posición respecto la primera fila de esa pagina. Como esta en JS, el array empieza por 0 (de la fila 15 a la 11 hay que restar 5 posiciones)
        if(length(filtrado_tabla)){
            valor_a_restar <- filtrado_tabla[length(filtrado_tabla)] - 5*(filtrado_tabla[length(filtrado_tabla)] - (filtrado_tabla[length(filtrado_tabla)]-floor(filtrado_tabla[length(filtrado_tabla)]/5)))
            
            if(valor_a_restar == 0){
                valor_a_restar <- 5
            }else{
                valor_a_restar <- filtrado_tabla[length(filtrado_tabla)] - 5*(filtrado_tabla[length(filtrado_tabla)] - (filtrado_tabla[length(filtrado_tabla)]-floor(filtrado_tabla[length(filtrado_tabla)]/5)))
            }
            
            pag_anterior <- filtrado_tabla[length(filtrado_tabla)]-valor_a_restar
            pag_anterior_1$valor <- pag_anterior
        }else{
            if(exists("pag_anterior_1")){
                pag_anterior <- pag_anterior_1$valor
            }else{
                pag_anterior <- 0
            }
        }
        
        #Límite visualización registros tabla
        tabla <- datatable(df_tabla, extensions = c('FixedHeader','Buttons'), options = list(pageLength = 5,
                                                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                    scrollX=TRUE, 
                                                    scrollCollapse=TRUE,
                                                    displayStart = pag_anterior,
                                                    fixedHeader = TRUE,
                                                    dom = 'lBfrtip', 
                                                    buttons = c('copy', 'csv', 'excel', 'pdf')),
                           selection = list(mode = "multiple",
                                            selected = filtrado_tabla,   # Selección de las filas de la tabla seleccionadas por el usuario.
                                            target = "row"),
                           escape = FALSE)
        
        return(tabla)
        
    })
    
    #Generación dataframe tabla noticias 
    output$tabla_noticias <- renderDataTable({
        
        noticias <- datos_filtrados_noticias()[] %>% 
            mutate(`Noticia link` = paste0("<a href='", `Noticia link`,"' target='_blank'>", `Noticia link`,"</a>"))
        
        #Nombres de filas igual a ID entero ascendente
        #Sí no hay datos no modificar el nombre de las filas
        if(nrow(noticias) > 0){
            row.names(noticias) <- seq(1,nrow(noticias))
        }
        
        #Límite visualización registros tabla
        tabla <- datatable(noticias, extensions = c('FixedHeader','Buttons'), options = list(pageLength = 5,
                                                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                    scrollX=TRUE, 
                                                    scrollCollapse=TRUE,
                                                    fixedHeader = TRUE,
                                                    dom = 'lBfrtip', 
                                                    buttons = c('copy', 'csv', 'excel', 'pdf')),
                           escape = FALSE)
        
        return(tabla)
        
    })
    
    #Generación dataframe tabla noticias GENERAL
    output$tabla_general_noticias <- renderDataTable({
        
        noticias_general <- datos_filtrados_noticias()[] %>% 
            mutate(`Noticia link` = paste0("<a href='", `Noticia link`,"' target='_blank'>", `Noticia link`,"</a>"))
        
        #Nombres de filas igual a ID entero ascendente
        #Sí no hay datos no modificar el nombre de las filas
        if(nrow(noticias_general) > 0){
            row.names(noticias_general) <- seq(1,nrow(noticias_general))
        }
        
        tabla_noticias_general <- datatable(noticias_general, extensions = c('FixedHeader','Buttons'), options = list(pageLength = 5,
                                                                             columnDefs = list(list(className = 'dt-center' , targets = "_all")),
                                                                             scrollx = TRUE,
                                                                             scrollCollapse = T,
                                                                             fixedHeader = TRUE,
                                                                             dom = 'lBfrtip', 
                                                                             buttons = c('copy', 'csv', 'excel', 'pdf')),
                                            escape = F)
        return(tabla_noticias_general)
        
    })
    
    output$tabla_general_fichas <- renderDataTable({
        
        fichas <- datos_filtrados_fichas()
        
        #Filtrado por palabra clave
        if(!input$palabra_clave_general == ""){
            fichas <- subset(fichas, apply(mapply(grepl, input$palabra_clave_general, fichas, ignore.case = T),1,any))
        }
        
        #Nombres de filas igual a ID entero ascendente
        #Sí no hay datos no modificar el nombre de las filas
        if(nrow(fichas) > 0){
            row.names(fichas) <- seq(1,nrow(fichas))
        }
        
        #Límite visualización registros tabla
        tabla <- datatable(fichas, extensions = c('FixedHeader','Buttons'), options = list(pageLength = 5,
                                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                  scrollX=TRUE, 
                                                  scrollCollapse=TRUE,
                                                  fixedHeader = TRUE,
                                                  dom = 'lBfrtip', 
                                                  buttons = c('copy', 'csv', 'excel', 'pdf')),
                           escape = FALSE)
        
        return(tabla)
        
    })
    
    #Generación dataframe tabla FICHAS GENERAL
    output$tabla_general_twitter <- renderDataTable({
        
        twitter <- datos_filtrados_twitter()
        
        #Creación de links e imagenes en formato html para verlos en el datatable.
        #Creación de links e imagenes en formato html para verlos en el datatable.
        if(!is.null(twitter$`Imagen perfil`)) {twitter$`Imagen perfil` <- paste0("<img src='", twitter$`Imagen perfil`,"' width='90' ", "height='90' ","</img>")}
        if(!is.null(twitter$`Banner perfil`)) {twitter$`Banner perfil`[!grepl("-",twitter$`Banner perfil`)] <- paste0("<img src='", twitter$`Banner perfil`[twitter$`Banner perfil` != "-"],"' width='100' ", "height='70' ",">","</img>")}
        if(!is.null(twitter$`Link tweet`)) {twitter$`Link tweet` <- paste0("<a href='", twitter$`Link tweet`,"' >", twitter$`Link tweet`,"</a>")}
        #twitter$`Medio tweet`[!grepl("-",twitter$`Medio tweet`)] <- paste0("<img src='", twitter$`Medio tweet`[twitter$`Medio tweet` != "-"],"' width='100' ", "height='70' ",">","</img>")
        #twitter$`URL perfil expandido y` <- paste0("<a href='", twitter$`URL perfil expandido y`,"' >", twitter$`URL perfil expandido y`,"</a>")
        if(!is.null(twitter$`URL medio expandido`)) {twitter$`URL medio expandido` <- paste0("<a href='", twitter$`URL medio expandido`,"' >", twitter$`URL medio expandido`,"</a>")}
        if(!is.null(twitter$`Link URL texto tweet`)) {twitter$`Link URL texto tweet` <- paste0("<a href='", twitter$`Link URL texto tweet`,"' >", twitter$`Link URL texto tweet`,"</a>")}
        if(!is.null(twitter$`URL perfil`)) {twitter$`URL perfil` <- paste0("<a href='", twitter$`URL perfil`,"' >", twitter$`URL perfil`,"</a>")}
        
        
        #Lógica para encadenar N fotos del mismo tweet
        if(!is.null(twitter$`URL medio exterior`)){
            a<- str_split(twitter$`URL medio exterior`, " ")
            for(i in 1:length(twitter$`URL medio exterior`)){
                if(twitter$`URL medio exterior`[i] == "-"){
                    twitter$`URL medio exterior`[i] <- twitter$`URL medio exterior`[i]
                }else{
                    b <- c()
                    for(j in 1:length(a[[i]])){
                        b <- append(b,paste0("<img src='",a[[i]][j],"' width='100' ", "height='70' ",">","</img>"))
                    }
                    b <- paste(b,collapse = " ")
                    
                    twitter$`URL medio exterior`[i] <- b
                }
            }
        }
        
        
        #Filtrado por palabra clave
        if(!input$palabra_clave_general == ""){
            twitter <- subset(twitter, apply(mapply(grepl, input$palabra_clave_general, twitter, ignore.case = T),1,any))
        }
        
        #Nombres de filas igual a ID entero ascendente
        #Sí no hay datos no modificar el nombre de las filas
        if(nrow(twitter) > 0){
            row.names(twitter) <- seq(1,nrow(twitter))
        }
        
        #Límite visualización registros tabla
        tabla <- datatable(twitter, extensions = c('FixedHeader','Buttons'), options = list(pageLength = 5,
                                                   columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                   scrollX=TRUE, 
                                                   scrollCollapse=TRUE,
                                                   fixedHeader = TRUE,
                                                   dom = 'lBfrtip', 
                                                   buttons = c('copy', 'csv', 'excel', 'pdf')),
                           escape = FALSE)
        
        return(tabla)
        
    })
    
    # Tabla contrataciones del estado general 
    output$tabla_general_contratacionesEstado <- renderDataTable({
        
        tabla <- datos_filtrados_contratacionesEstado()
        
        #Nombres de filas igual a ID entero ascendente
        #Sí no hay datos no modificar el nombre de las filas
        if(nrow(tabla) > 0){
            row.names(tabla) <- seq(1,nrow(tabla))
        }
        
        #Límite visualización registros tabla
        tabla <- datatable(tabla, extensions = c('FixedHeader','Buttons'), options = list(pageLength = 5,
                                                 columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                 scrollX=TRUE, 
                                                 scrollCollapse=TRUE,
                                                 fixedHeader = TRUE,
                                                 dom = 'lBfrtip', 
                                                 buttons = c('copy', 'csv', 'excel', 'pdf')),
                           escape = FALSE)
        
        return(tabla)
        
    })
    
    
    #Generación dataframe tabla FICHAS EMPRESAS GEOLOCALIZADAS
    output$tabla_redes <- renderDataTable({
        
        twitter <- subset(datos_filtrados_twitter(), !(datos_filtrados_twitter()$Texto %in% "-"))
        
        #Creación de links e imagenes en formato html para verlos en el datatable.
        if(!is.null(twitter$`Imagen perfil`)) {twitter$`Imagen perfil` <- paste0("<img src='", twitter$`Imagen perfil`,"' width='90' ", "height='90' ","</img>")}
        if(!is.null(twitter$`Banner perfil`)) {twitter$`Banner perfil`[!grepl("-",twitter$`Banner perfil`)] <- paste0("<img src='", twitter$`Banner perfil`[twitter$`Banner perfil` != "-"],"' width='100' ", "height='70' ",">","</img>")}
        if(!is.null(twitter$`Link tweet`)) {twitter$`Link tweet` <- paste0("<a href='", twitter$`Link tweet`,"' >", twitter$`Link tweet`,"</a>")}
        #twitter$`Medio tweet`[!grepl("-",twitter$`Medio tweet`)] <- paste0("<img src='", twitter$`Medio tweet`[twitter$`Medio tweet` != "-"],"' width='100' ", "height='70' ",">","</img>")
        #twitter$`URL perfil expandido y` <- paste0("<a href='", twitter$`URL perfil expandido y`,"' >", twitter$`URL perfil expandido y`,"</a>")
        if(!is.null(twitter$`URL medio expandido`)) {twitter$`URL medio expandido` <- paste0("<a href='", twitter$`URL medio expandido`,"' >", twitter$`URL medio expandido`,"</a>")}
        if(!is.null(twitter$`Link URL texto tweet`)) {twitter$`Link URL texto tweet` <- paste0("<a href='", twitter$`Link URL texto tweet`,"' >", twitter$`Link URL texto tweet`,"</a>")}
        if(!is.null(twitter$`URL perfil`)) {twitter$`URL perfil` <- paste0("<a href='", twitter$`URL perfil`,"' >", twitter$`URL perfil`,"</a>")}
        
        
        #Lógica para encadenar N fotos del mismo tweet
        if(!is.null(twitter$`URL medio exterior`)){
            a<- str_split(twitter$`URL medio exterior`, " ")
            for(i in 1:length(twitter$`URL medio exterior`)){
               if(twitter$`URL medio exterior`[i] == "-"){
                   twitter$`URL medio exterior`[i] <- twitter$`URL medio exterior`[i]
               }else{
                   b <- c()
                   for(j in 1:length(a[[i]])){
                        b <- append(b,paste0("<img src='",a[[i]][j],"' width='100' ", "height='70' ",">","</img>"))
                    }
                   b <- paste(b,collapse = " ")
            
                   twitter$`URL medio exterior`[i] <- b
               }
            }
        }
        
        
        #Nombres de filas igual a ID entero ascendente
        #Sí no hay datos no modificar el nombre de las filas
        if(nrow(twitter) > 0){
            row.names(twitter) <- seq(1,nrow(twitter))
        }
        
        #Límite visualización registros tabla
        tabla <- datatable(twitter, extensions = c('FixedHeader','Buttons'), options = list(pageLength = 5,
                                                  columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                  scrollX=TRUE, 
                                                  scrollCollapse=TRUE,
                                                  fixedHeader = TRUE,
                                                  dom = 'lBfrtip', 
                                                  buttons = c('copy', 'csv', 'excel', 'pdf')),
                           escape = FALSE)
        
        return(tabla)
        
    })
    
    #### 
    # PANEL REDES
    ####
    
    #Generación dataframe tabla FICHAS EMPRESAS GEOLOCALIZADAS
    output$tabla_redes <- renderDataTable({
        
        twitter <- subset(datos_filtrados_twitter(), !(datos_filtrados_twitter()$Texto %in% "-"))
        
        #Creación de links e imagenes en formato html para verlos en el datatable.
        if(!is.null(twitter$`Imagen perfil`)) {twitter$`Imagen perfil` <- paste0("<img src='", twitter$`Imagen perfil`,"' width='90' ", "height='90' ","</img>")}
        if(!is.null(twitter$`Banner perfil`)) {twitter$`Banner perfil`[!grepl("-",twitter$`Banner perfil`)] <- paste0("<img src='", twitter$`Banner perfil`[twitter$`Banner perfil` != "-"],"' width='100' ", "height='70' ",">","</img>")}
        if(!is.null(twitter$`Link tweet`)) {twitter$`Link tweet` <- paste0("<a href='", twitter$`Link tweet`,"' >", twitter$`Link tweet`,"</a>")}
        #twitter$`Medio tweet`[!grepl("-",twitter$`Medio tweet`)] <- paste0("<img src='", twitter$`Medio tweet`[twitter$`Medio tweet` != "-"],"' width='100' ", "height='70' ",">","</img>")
        #twitter$`URL perfil expandido y` <- paste0("<a href='", twitter$`URL perfil expandido y`,"' >", twitter$`URL perfil expandido y`,"</a>")
        if(!is.null(twitter$`URL medio expandido`)) {twitter$`URL medio expandido` <- paste0("<a href='", twitter$`URL medio expandido`,"' >", twitter$`URL medio expandido`,"</a>")}
        if(!is.null(twitter$`Link URL texto tweet`)) {twitter$`Link URL texto tweet` <- paste0("<a href='", twitter$`Link URL texto tweet`,"' >", twitter$`Link URL texto tweet`,"</a>")}
        if(!is.null(twitter$`URL perfil`)) {twitter$`URL perfil` <- paste0("<a href='", twitter$`URL perfil`,"' >", twitter$`URL perfil`,"</a>")}
        
        
        #Lógica para encadenar N fotos del mismo tweet
        if(!is.null(twitter$`URL medio exterior`)){
            a<- str_split(twitter$`URL medio exterior`, " ")
            for(i in 1:length(twitter$`URL medio exterior`)){
                if(twitter$`URL medio exterior`[i] == "-"){
                    twitter$`URL medio exterior`[i] <- twitter$`URL medio exterior`[i]
                }else{
                    b <- c()
                    for(j in 1:length(a[[i]])){
                        b <- append(b,paste0("<img src='",a[[i]][j],"' width='100' ", "height='70' ",">","</img>"))
                    }
                    b <- paste(b,collapse = " ")
                    
                    twitter$`URL medio exterior`[i] <- b
                }
            }
        }
        
        #Nombres de filas igual a ID entero ascendente
        #Sí no hay datos no modificar el nombre de las filas
        if(nrow(twitter) > 0){
            row.names(twitter) <- seq(1,nrow(twitter))
        }
        
        #Límite visualización registros tabla
        tabla <- datatable(twitter, extensions = c('FixedHeader','Buttons'), options = list(pageLength = 5,
                                                   columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                   scrollX=TRUE, 
                                                   scrollCollapse=TRUE,
                                                   fixedHeader = TRUE,
                                                   dom = 'lBfrtip', 
                                                   buttons = c('copy', 'csv', 'excel', 'pdf')),
                           escape = FALSE)
        
        return(tabla)
        
    })
    
    #Generación dataframe tabla FICHAS EMPRESAS GEOLOCALIZADAS
    output$tabla_redes_cuentas <- renderDataTable({
        
        twitter <- subset(datos_filtrados_twitter_cuentas(), !(datos_filtrados_twitter_cuentas()$Texto %in% "-"))
        
        #Creación de links e imagenes en formato html para verlos en el datatable.
        if(!is.null(twitter$`Imagen perfil`)) {twitter$`Imagen perfil` <- paste0("<img src='", twitter$`Imagen perfil`,"' width='90' ", "height='90' ","</img>")}
        if(!is.null(twitter$`Banner perfil`)) {twitter$`Banner perfil`[!grepl("-",twitter$`Banner perfil`)] <- paste0("<img src='", twitter$`Banner perfil`[twitter$`Banner perfil` != "-"],"' width='100' ", "height='70' ",">","</img>")}
        if(!is.null(twitter$`Link tweet`)) {twitter$`Link tweet` <- paste0("<a href='", twitter$`Link tweet`,"' >", twitter$`Link tweet`,"</a>")}
        #twitter$`Medio tweet`[!grepl("-",twitter$`Medio tweet`)] <- paste0("<img src='", twitter$`Medio tweet`[twitter$`Medio tweet` != "-"],"' width='100' ", "height='70' ",">","</img>")
        #twitter$`URL perfil expandido y` <- paste0("<a href='", twitter$`URL perfil expandido y`,"' >", twitter$`URL perfil expandido y`,"</a>")
        if(!is.null(twitter$`URL medio expandido`)) {twitter$`URL medio expandido` <- paste0("<a href='", twitter$`URL medio expandido`,"' >", twitter$`URL medio expandido`,"</a>")}
        if(!is.null(twitter$`Link URL texto tweet`)) {twitter$`Link URL texto tweet` <- paste0("<a href='", twitter$`Link URL texto tweet`,"' >", twitter$`Link URL texto tweet`,"</a>")}
        if(!is.null(twitter$`URL perfil`)) {twitter$`URL perfil` <- paste0("<a href='", twitter$`URL perfil`,"' >", twitter$`URL perfil`,"</a>")}
        
        
        #Lógica para encadenar N fotos del mismo tweet
        if(!is.null(twitter$`URL medio exterior`)){
            a<- str_split(twitter$`URL medio exterior`, " ")
            for(i in 1:length(twitter$`URL medio exterior`)){
                if(twitter$`URL medio exterior`[i] == "-"){
                    twitter$`URL medio exterior`[i] <- twitter$`URL medio exterior`[i]
                }else{
                    b <- c()
                    for(j in 1:length(a[[i]])){
                        b <- append(b,paste0("<img src='",a[[i]][j],"' width='100' ", "height='70' ",">","</img>"))
                    }
                    b <- paste(b,collapse = " ")
                    
                    twitter$`URL medio exterior`[i] <- b
                }
            }
        }
        
        #Nombres de filas igual a ID entero ascendente
        #Sí no hay datos no modificar el nombre de las filas
        if(nrow(twitter) > 0){
            row.names(twitter) <- seq(1,nrow(twitter))
        }
        
        #Límite visualización registros tabla
        tabla <- datatable(twitter, extensions = c('FixedHeader','Buttons'), options = list(pageLength = 5,
                                                   columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                   scrollX=TRUE, 
                                                   scrollCollapse=TRUE,
                                                   fixedHeader = TRUE,
                                                   dom = 'lBfrtip', 
                                                   buttons = c('copy', 'csv', 'excel', 'pdf')),
                           escape = FALSE)
        
        return(tabla)
        
    })
    
    #Generación dataframe tabla FICHAS EMPRESAS GEOLOCALIZADAS
    output$tabla_redes_localiz <- renderDataTable({
        
        twitter <- datos_filtrados_twitter()
        twitter <- subset(twitter, !(twitter$`Latitud usuario` %in% "-"))
        
        
        twitter$`Imagen perfil y` <- paste0("<img src='", twitter$`Imagen perfil y`,"' width='90' ", "height='90' ","</img>")
        twitter$`Banner perfil`[!grepl("-",twitter$`Banner perfil`)] <- paste0("<img src='", twitter$`Banner perfil`[twitter$`Banner perfil` != "-"],"' width='100' ", "height='70' ",">","</img>")
        twitter$`Link tweet` <- paste0("<a href='", twitter$`Link tweet`,"' >", twitter$`Link tweet`,"</a>")
        #twitter$`Medio tweet`[!grepl("-",twitter$`Medio tweet`)] <- paste0("<img src='", twitter$`Medio tweet`[twitter$`Medio tweet` != "-"],"' width='100' ", "height='70' ",">","</img>")
        twitter$`URL perfil expandido y` <- paste0("<a href='", twitter$`URL perfil expandido y`,"' >", twitter$`URL perfil expandido y`,"</a>")
        twitter$`URL medio expandido` <- paste0("<a href='", twitter$`URL medio expandido`,"' >", twitter$`URL medio expandido`,"</a>")
        #twitter$`Link URL texto tweet` <- paste0("<a href='", twitter$`Link URL texto tweet`,"' >", twitter$`Link URL texto tweet`,"</a>")
        
        
        #Lógica para encadenar N fotos del mismo tweet
        a<- str_split(twitter$`URL medio exterior`, " ")
        for(i in 1:length(twitter$`URL medio exterior`)){
            if(twitter$`URL medio exterior`[i] == "-"){
                twitter$`URL medio exterior`[i] <- twitter$`URL medio exterior`[i]
            }else{
                b <- c()
                for(j in 1:length(a[[i]])){
                    b <- append(b,paste0("<img src='",a[[i]][j],"' width='100' ", "height='70' ",">","</img>"))
                }
                b <- paste(b,collapse = " ")
                
                twitter$`URL medio exterior`[i] <- b
            }
        }
        
        
        #Nombres de filas igual a ID entero ascendente
        #Sí no hay datos no modificar el nombre de las filas
        if(nrow(twitter) > 0){
            row.names(twitter) <- seq(1,nrow(twitter))
        }
        
        #Límite visualización registros tabla
        tabla <- datatable(twitter, extensions = c('FixedHeader','Buttons'), options = list(pageLength = 5,
                                                   columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                   scrollX=TRUE, 
                                                   scrollCollapse=TRUE,
                                                   fixedHeader = TRUE,
                                                   dom = 'lBfrtip', 
                                                   buttons = c('copy', 'csv', 'excel', 'pdf')),
                           escape = FALSE)
        
        return(tabla)
        
    })
    
    ####
    # PANEL CONTRATACIONES ESTADO
    ####
    
    output$tabla_contrataciones <- renderDataTable({
        
        contrataciones <- datos_filtrados_contratacionesEstado()
        
        #Nombres de filas igual a ID entero ascendente
        #Sí no hay datos no modificar el nombre de las filas
        if(nrow(contrataciones) > 0){
            row.names(contrataciones) <- seq(1,nrow(contrataciones))
        }
        
        #Límite visualización registros tabla
        tabla <- datatable(contrataciones, extensions = c('FixedHeader','Buttons'), options = list(pageLength = 5,
                                                          columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                          scrollX=TRUE, 
                                                          scrollCollapse=TRUE,
                                                          fixedHeader = TRUE,
                                                          dom = 'lBfrtip', 
                                                          buttons = c('copy', 'csv', 'excel', 'pdf')),
                           escape = FALSE)
        
        return(tabla)
        
    })
    
    #####################################################
    # GENERACIÓN GRÁFICO RECUENTO EMPRESAS CONSTITUIDAS
    
    #Generación gráfico recuento empresas constituidas por fecha
    output$grafico_borme <- renderPlot({
        
        df_filtrados <- datos_filtrados_borme()
        
        #Comprobación de existencia de datos
        if(nrow(df_filtrados) > 0){
            #Recuento fechas
            fechas_uniq <- unique(df_filtrados$Fecha)
            recuento_fechas <- c()
            for(i in 1:length(fechas_uniq)){
                recuento_fechas[i] <- nrow(subset(df_filtrados, df_filtrados$Fecha == fechas_uniq[i]))
            }
            
            df_recuento_fechas <- data.frame("Fecha" = fechas_uniq, "Recuento" = recuento_fechas)
            
            #Grafico de líneas
            ggplot(data=df_recuento_fechas, aes(x=Fecha, y=Recuento, group=1)) + 
                geom_bar(stat="identity", fill="steelblue", width= 0.3) + 
                geom_text(aes(label=Recuento), vjust=1.5, color="white", size=7) + 
                ggtitle("Recuento de empresas constituidas por fecha") + 
                theme(
                    plot.title = element_text(size=17, face="bold.italic"),
                    axis.title.x = element_text(size=15, face="bold"),
                    axis.title.y = element_text(size=15, face="bold"),
                    axis.text.x = element_text(size=14),
                    axis.text.y = element_text(size=14)
                )
        }
    })
    
    #Generación gráfico recuento noticias por fecha
    output$grafico_noticias <- renderPlot({
        
        df_filtrados_noticias <- datos_filtrados_noticias()
        
        #Comprobación de existencia de datos
        if(nrow(df_filtrados_noticias) > 0){
            #Recuento fechas
            fechas_uniq <- unique(df_filtrados_noticias$Fecha)
            recuento_fechas <- c()
            for(i in 1:length(fechas_uniq)){
                recuento_fechas[i] <- nrow(subset(df_filtrados_noticias, df_filtrados_noticias$Fecha == fechas_uniq[i]))
            }
            
            df_recuento_fechas <- data.frame("Fecha" = fechas_uniq, "Recuento" = recuento_fechas)
            
            #Grafico de líneas
            g_recuento_fechas <-
                ggplot(data=df_recuento_fechas, aes(x=Fecha, y=Recuento, group=1)) + 
                geom_bar(stat="identity", fill="steelblue", width=0.3) + 
                geom_text(aes(label=Recuento), vjust=1.5, color="white", size=7) + 
                ggtitle("Recuento noticias por fecha") + 
                theme(
                    plot.title = element_text(size=17, face="bold.italic"),
                    axis.title.x = element_text(size=15, face="bold"),
                    axis.title.y = element_text(size=15, face="bold"),
                    axis.text.x = element_text(size=14),
                    axis.text.y = element_text(size=14)
                )
            
            g_recuento_fechas
        }
    })
    
    ###################################################
    # DESCARGA DE DATOS
    ###################################################
    
    # DESCARGA DATOS BORME
    output$descarga_borme <- downloadHandler(
        
        filename = function() {
            paste("descarga_borme_", as.character(input$fechas_borme[1]),"_", as.character(input$fechas_borme[2]),".csv", sep="")
        },
        content = function(file) {
            write.csv(datos_filtrados_borme(), file, eol="\n", sep = ",")  # eol="\n" es para el encoding de caracteres en .csv
        }
    )
    
    # DESCARGA DATOS NOTICIAS
    output$descarga_noticias <- downloadHandler(
        
        filename = function() {
            paste("descarga_noticias_",as.character(input$fechas_noticias[1]),"_",as.character(input$fechas_noticias[2]),".csv", sep="")
        },
        content = function(file) {
            write.csv(datos_filtrados_noticias(), file, eol="\n", sep = ",")
        }
    )
    
    # DESCARGA DATOS TWITTER
    output$descarga_redes <- downloadHandler(
        
        filename = function() {
            paste("descarga_Twitter_",as.character(input$fechas_redes[1]),"_",as.character(input$fechas_redes[2]),".csv", sep="")
        },
        content = function(file) {
            write.csv(datos_filtrados_twitter(), file, eol="\n", sep = ",")
        }
    )
    
    # DESCARGA DATOS CONTRATACIONES ESTADO
    output$descarga_contrataciones_Estado <- downloadHandler(
        
        filename = function() {
            paste("descarga_ContratacionesEstado_",as.character(input$fechas_contrataciones[1]),"_",as.character(input$fechas_contrataciones[2]),".csv", sep="")
        },
        content = function(file) {
            write.csv(datos_filtrados_contratacionesEstado(), file, eol="\n", sep = ",")
        }
    )
    
    
    
    # =================================================================
    # =================================================================
    # =================================================================
    # CENSO DE EMRPESAS
    # =================================================================
    # =================================================================
    # =================================================================
    
    datos_filtrado <- reactive({
        
        df <- df_censo
        
        # 1) Filtro por intervalo de fechas
        df <- df[as.Date(as.character(df$`Fecha_constitución`), "%d/%m/%Y") >= as.Date(input$fechas[1]) &
                     as.Date(as.character(df$`Fecha_constitución`)) <= as.Date(input$fechas[2]) |
                     df$`Fecha_constitución` == "-",]
        
        # 2) Filtro por num empleados
        # Lógica selección empleados == 2- cuando input empleados == 0
        inicial_0 <- "-"
        df_sin_info_empleados <- df[df$Empleados == inicial_0,] 
        df_con_info_empleados <- df[df$Empleados != inicial_0,]
        df_con_info_empleados  <- df_con_info_empleados[as.numeric(gsub("[ (].*","",df_con_info_empleados$Empleados)) >= input$empleados[1] & as.numeric(gsub("[ (].*","",df_con_info_empleados$Empleados)) <= input$empleados[2],]
        df <- rbind(df_con_info_empleados,df_sin_info_empleados)

        
        #df <- na.omit(df)
        #print(nrow(df))
        # 3) Filtro por división CNAE
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
            need(!is.null(input$div_cnae),
                 "Atención!\nNo se ha seleccionado ningún CNAE.\nSeleccione un CNAE por favor.")
        )
        
        if(input$div_cnae == "Todos"){  
            df <- df
        }else if(length(input$div_cnae) > 1){
            codigos_a_extraer <- c()
            for(i in 1:length(input$div_cnae)){
                if(grepl("[A-Z]",substring(input$div_cnae[i],1,1))){
                    
                    pos_letra_demandada <- grep(substring(input$div_cnae[i],1,1),letters,ignore.case = TRUE)
                    codigos_2 <- gsub("([0-9]+).*$", "\\1",
                                      df_cnae$completo[c((grep(letters[pos_letra_demandada],df_cnae$COD_CNAE2009,ignore.case = TRUE) + 1):
                                                             (grep(letters[pos_letra_demandada + 1],df_cnae$COD_CNAE2009,ignore.case = TRUE) - 1)
                                      )]
                    )
                }else{
                    codigo_seleccionado <- gsub("([0-9]+).*$", "\\1", input$div_cnae[i])
                    numero_de_carct <- nchar(codigo_seleccionado)
                    codigos_2 <- gsub("([0-9]+).*$", "\\1",
                                      df_cnae$completo[grep(codigo_seleccionado,substring(df_cnae$COD_CNAE2009,1,numero_de_carct))]
                    )
                }
                codigos_a_extraer <- c(codigos_a_extraer, codigos_2)
            }
            df <- df[which(gsub("([0-9]+).*$", "\\1",df$CNAE) %in% codigos_a_extraer),]
            
        }else{
            if(grepl("[A-Z]",substring(input$div_cnae,1,1))){
                
                pos_letra_demandada <- grep(substring(input$div_cnae,1,1),letters,ignore.case = TRUE)
                codigos_a_extraer <- gsub("([0-9]+).*$", "\\1",
                                          df_cnae$completo[c((grep(letters[pos_letra_demandada],df_cnae$COD_CNAE2009,ignore.case = TRUE) + 1):
                                                                 (grep(letters[pos_letra_demandada + 1],df_cnae$COD_CNAE2009,ignore.case = TRUE) - 1)
                                          )]
                )
            }else{
                codigo_seleccionado <- gsub("([0-9]+).*$", "\\1", input$div_cnae)
                numero_de_carct <- nchar(codigo_seleccionado)
                codigos_a_extraer <- gsub("([0-9]+).*$", "\\1",
                                          df_cnae$completo[grep(codigo_seleccionado,substring(df_cnae$COD_CNAE2009,1,numero_de_carct))]
                )
            }
            df <- df[which(gsub("([0-9]+).*$", "\\1",df$CNAE) %in% codigos_a_extraer),]
        }
        
        # 4) Filtro por ubicación
        if(input$calle == "Todas"){
            df <- df
        }else{
            df <- df[grep(input$calle,gsub(",.*","",df$`Domicilio_social`)),]
        }
        
        # 5) Filtro extinguidas
        if(input$extinguidas == TRUE){
            df <- df[df$Estado == "Activa",]
        }else{
            df <- df
        }
        
        df[df == ""] <- "-"
        
        # 6) Filtrad por existencia RRSS
        if(input$RRSS == 1){
            df <- df[df$RRSS != "-" ,]
        }else if(input$RRSS == 2){
            df <- df[df$RRSS == "-",]
        }else{
            df <- df
        }
        
        # 7) Filtrad por palabra clave
        if(input$palabra_clave != ""){
            #Filtrado por búsqueda de palabras clave. Se realiza máscara OR con resultados booleanos.
            filtrado_palabra_clave <- apply(as.data.frame(mapply(grepl, input$palabra_clave, df, ignore.case = T)),1,any)
            filtrado_palabra_clave <- apply(as.data.frame(mapply(grepl, input$palabra_clave, df[,c(1:4)], ignore.case = T)),1,any)
            df <- subset(df, filtrado_palabra_clave)
        }else{
            df <- df
        }
        
        df <- df[,c(1,2,5,6,7,8,10,9,21,11,12,20,23,19,18,16,15,17,13,14,3,4)]
        
        df
        
    })
    
    datos_filtrado_mapa <- reactive({
        df <- datos_filtrado()
        
        # Filtrado por selección de registro en la tabla
        filtrado_tabla <- input$tabla_censo_rows_selected
        if(length(filtrado_tabla)){
            df <-  df[filtrado_tabla, , drop = F]
        }
        
        return(df)
    })
    
    
    # MAPA
    output$mapa_censo <- renderLeaflet({
        
        df <- datos_filtrado_mapa()
        
        df <- df[as.numeric(df$Latitud) > 41,]
        
        latitud <- as.numeric(df$Latitud)
        longitud <- as.numeric(df$Longitud)
        
        popup <- paste(
            "Empresa: ", df$`Denominación_social`,"<br/>", 
            "CNAE: ", df$CNAE, "<br/>", 
            "Empleados ", df$Tamaño_empresa_por_empleados, "<br/>",
            "Facturación: ", df$Tamaño_empresa_por_facturación, "<br/>",
            sep="") %>%
            lapply(htmltools::HTML)
        
        leaflet() %>% addTiles() %>% addMarkers(lng = longitud, lat = latitud, popup = popup)
    })
    
    # TABLA
    output$tabla_censo <- renderDataTable({
        
        df <- datos_filtrado()
        
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
            need(nrow(df) != 0,
                 "Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifique el valor de los filtros si lo desea.")
        )
        
        # Links URL y RRSS
        df$URL <- paste0("<a href='", df$URL,"' target='_blank'>", df$URL,"</a>")
        #df$RRSS <- paste0("<a href='", df$RRSS,"' target='_blank'>", df$RRSS,"</a>")
        
        pos_urls <- grep("https",df$RRSS)
        for(i in pos_urls){
            rrss_separadas <- str_split(df$RRSS[i], ",")[[1]]
            rrss_separadas <- str_trim(rrss_separadas)
            rrss_separadas <- unique(rrss_separadas)
            for(j in 1:length(rrss_separadas)){
                rrss_separadas[j] <- paste0("<a href='", rrss_separadas[j],"' target='_blank'>", rrss_separadas[j],"</a>")
            }
            df$RRSS[i] <- paste(rrss_separadas, collapse = ", ")
        }
        
        # Manejo de error: "inexistencia de datos para los filtros seleccionados" de cara al usuario
        shiny::validate(
            need(nrow(df) != 0,
                 "Atención!\nNo existen datos disponibles para el valor de los filtros seleccionados.\nModifique el valor de los filtros si lo desea.")
        )
        
        df <- datatable(df, extensions = c('FixedHeader','Buttons'), options = list(pageLength = 5,
                                           columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                           scrollX=TRUE, 
                                           scrollCollapse=TRUE,
                                           fixedHeader = TRUE,
                                           dom = 'lBfrtip', 
                                           buttons = c('copy', 'csv', 'excel', 'pdf',I('colvis'))),escape = F)
        
    })
    
    #Descarga de datos
    output$downloadDatacenso_csv <- downloadHandler(
        
        filename = paste0("Censo_filtrado","_",Sys.Date(),".csv"),
        content  = function(file) {
            if(input$tabs_censo == "Aranda de Duero"){
                df <- datos_filtrado_mapa()
            }else{
                df <- datos_filtrado_mapa_bodegas()
            }
            
            write.csv(df, file, eol="\n", sep = ",", row.names = TRUE)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
