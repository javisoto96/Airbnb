library(shiny)
library(shinythemes)
library(leaflet,quietly = T, warn.conflicts = F)
library(plotly)
library(wordcloud2)
library(tm)
library(caret)
library(tippy)


shinyUI(navbarPage(
    
    title = "Analisis AirBnb",
    theme = shinytheme("readable"),
    
    #------Inicio--------------
    tabPanel("Inicio",
             h2("ANALISIS EN AIRBNB"),
             hr(),
             p("La aplicación está dividida en 5 partes, las cuales están distribuidas gráficamente en 5 pestañas de navegación. Estas partes de la aplicación son:"),
             br(),
             tags$b("Inicio"),
             p("Es la primera pestaña al entrar a la aplicación y se encarga de mostrarnos un resumen de como funciona cada apartado de la aplicación a nivel de información."),
             tags$b("Graficos"),
             p("En este apartado se muestra mediante gráficos un análisis de los alojamientos de la ciudad seleccionada. En total se muestran 4 gráficos, divididos en 4 subpestañas, los cuales son:"),
             tags$ol(
                 tags$li("Precios/Mes: nos muestra una gráfica de puntos unidos por líneas con la media de los precios de la ciudad de cada mes del año."), 
                 tags$li("Puntuación/Barrio: nos muestra una gráfica de barras con la media de las puntuaciones de cada barrio de la ciudad."), 
                 tags$li("Cantidad/Tipo Habitación: nos muestra una gráfica de barras con la cantidad de alojamientos que hay de cada tipo de la ciudad elegida."),
                 tags$li("Tipo Habitación/Precio Medio: nos muestra una gráfica de barras con el precio medio de cada tipo de alojamiento de la ciudad.")
             ),
             tags$b("Mapa"),
             p("En este apartado se encuentra un mapa interactivo de la ciudad elegida. En el mapa se sitúan con puntos todos los alojamientos de la ciudad, los cuales pueden ser filtrados por múltiples variables, como por ejemplo el precio, el barrio de la ciudad, el tipo de alojamiento, etc. Los puntos del mapa son de diferentes colores para diferenciarlos por el tipo de alojamiento."),
             tags$b("Prediccion"),
             p("en este apartado nos encontramos con un estimador de precio en caso de que queramos poner un alojamiento en Airbnb. Para predecir el precio se tiene que rellenar los datos de nuestro alojamiento y seleccionar un modelo de predicción."),
             tags$b("Palabras"),
             p("En este apartado muestra una nube de palabras con las palabras más utilizadas en los anuncios de Airbnb, las cuales podremos filtrar ya sea por número de palabras o eliminando palabras en concreto.")
             
    ),
    
    
    
    
    #------Graficos-------------
    tabPanel("Graficos",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "ciudadGraficos", label = h4("Ciudad"), choices = c("Malaga","Barcelona","Valencia"))
                 ),
                 
                 
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Precios/Mes",plotlyOutput("preciosMesGrafica", width = 700, height = 400)),
                         tabPanel("Puntuacion/Barrio",plotlyOutput("reviewGrafica", width = 700, height = 380)),
                         tabPanel("Cantidad/Tipo Habitacion",plotlyOutput("numeroTiposHabitaciones", width = 700, height = 380)),
                         tabPanel("Tipo Habitacion/Precio Medio",plotlyOutput("tipoMediaPrecio", width = 700, height = 380))
                     )
                 )
             )
    ),
    
    #------Mapa-----------------
    tabPanel("Mapa",
             sidebarLayout(
                 sidebarPanel(
                     
                     selectInput(inputId = "ciudadMapa", label = h4("Ciudad"), choices = c("Malaga","Barcelona","Valencia")),
                     br(),
                     sliderInput(inputId = "precioSlider", label = h4("Precio"), min = 0, max = 1000, step = 50,pre = "$", sep = ",", value = c(100, 500)),
                     br(),
                     sliderInput(inputId = "ratingSlider", label = h4("Puntuacion"), min = 20, max = 100, step = 10,value = c(60, 100)),
                     br(),
                     sliderInput(inputId = "reviewSlider", label = h4("Numero de Reviews"), min = 0, max = 400, step = 50,value = c(10, 350)),
                     uiOutput("habitacionMapa"),
                     br(),
                     uiOutput("barrioMapa"),
                 ),
                 
                 mainPanel(
                     leafletOutput(outputId = "map", width = "100%", height = "800px")
                 )
             )
    ),
    
    #------Predicciones---------
    tabPanel("Prediccion",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "ciudadPrediccion", label = h4("Ciudad"), choices = c("Malaga","Barcelona","Valencia")),
                     br(),
                     selectInput(inputId = "modelo", label = h4("Modelo"), choices = c("Modelo lineal basico","Random Forest","Gradient Boosting Machine")),
                     br(),
                     radioButtons("tv", label = h4("TV"),choices = c('Si','No'), selected = "Yes"),
                     br(),
                     radioButtons("Internet", label = h4("Internet"),choices = c('Si','No'), selected = "Yes"),
                     br(),
                     sliderInput('cleaning_fee', h4("Tarifa de limpieza"),min = 0, max = 30,value=10),
                     br(),
                     sliderInput('dormitoriosPrediccion', h4('Numero de habitaciones'),min = 0, max = 5,value=2),
                     br(),
                     sliderInput('camasPrediccion', h4('Numero de camas'),min = 0, max = 4,value=2),
                     br(),
                     sliderInput('banos', h4('Numero de banos'),min = 0, max = 3,value=1),
                     br(),
                     uiOutput("opcionesMl"),
                     br(),
                     actionButton("click", "Calcular")
                     
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     verbatimTextOutput("Prediction")
                 )
             )
    )
    ,
    
    #------Nube de Palabras-----
    
    tabPanel("Palabras",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(
                         inputId = "ciudadPalabras",
                         label = "Elija la ciudad",
                         choices = c("Malaga", "Barcelona","Valencia"),
                         multiple = FALSE,
                         selected = "Malaga"
                     ),hr(),
                     checkboxInput("eliminar_palabras", "Para eliminar palabras especificas, pulse el boton", FALSE),
                     conditionalPanel(
                         condition = "input.eliminar_palabras == 1",
                         textAreaInput("palabra_eliminar_1", "Escriba la palabra a eliminar (una por linea)", rows = 1)
                     ),
                     conditionalPanel(
                         condition = "input.eliminar_palabras == 1 && input.palabra_eliminar_1.length > 0",
                         textAreaInput("palabra_eliminar_2", "", rows = 1)
                     ),
                     conditionalPanel(
                         condition = "input.eliminar_palabras == 1 && input.palabra_eliminar_2.length > 0",
                         textAreaInput("palabra_eliminar_3", "", rows = 1)
                     ),
                     conditionalPanel(
                         condition = "input.eliminar_palabras == 1 && input.palabra_eliminar_3.length > 0",
                         textAreaInput("palabra_eliminar_4", "", rows = 1)
                     ),
                     conditionalPanel(
                         condition = "input.eliminar_palabras == 1 && input.palabra_eliminar_4.length > 0",
                         textAreaInput("palabra_eliminar_5", "", rows = 1)
                     ),hr(),
                     numericInput("numPalabras", "Numero maximo de palabras (entre 10 y 100)",
                                  value = 100, min = 10
                     )
                 ),mainPanel(wordcloud2Output('nube'))
             )
    )
)
    
)
