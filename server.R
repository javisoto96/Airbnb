library(shiny)
library(leaflet,quietly = T, warn.conflicts = F)
library(plotly)
library(dplyr)
library(lubridate)
library(gbm)
library(caret)
library(sqldf)
library(rpart)
library(wordcloud2)
library(tm)



shinyServer(function(input, output) {


    #------------------------------------------------
    #-------------------Graficos---------------------
    #------------------------------------------------
    
    dataPrecio <- reactive({
        if(input$ciudadGraficos=="Malaga"){
            data = calendar_malaga
        }else if(input$ciudadGraficos=="Barcelona"){
            data = calendar_barcelona
        }else if(input$ciudadGraficos=="Valencia"){
            data = calendar_valencia
        }
        
        data$price <- gsub("[$,]","",data$price)
        data$price <- as.numeric(data$price)
        
        data$year = year(data$date)
        data$month = month(data$date)
        data$day = day(data$date)
        data <- data %>% filter(available=='t') %>% group_by(month) %>% summarise(avg_pricemo=mean(price))
        data
    })
    
    dataReview <- reactive({
        if(input$ciudadGraficos=="Malaga"){
            data = datos_malaga
        }else if(input$ciudadGraficos=="Barcelona"){
            data = datos_barcelona
        }else if(input$ciudadGraficos=="Valencia"){
            data = datos_valencia
        }
        data <- data %>% select(neighbourhood_group_cleansed, review_scores_rating) %>%
                            filter(review_scores_rating!="NA") %>%
                            group_by(neighbourhood_group_cleansed) %>%
                            summarise(avg_review=mean(review_scores_rating)) %>%
                            arrange(desc(avg_review))
        data
    })
    
    dataGrafica3 <- reactive({
        if(input$ciudadGraficos=="Malaga"){
            data = datos_malaga
        }else if(input$ciudadGraficos=="Barcelona"){
            data = datos_barcelona
        }else if(input$ciudadGraficos=="Valencia"){
            data = datos_valencia
        }
        data<- data %>% group_by(room_type) %>%
            summarise(count_type = n())
        data
    })
    
    dataGrafica4 <- reactive({
        if(input$ciudadGraficos=="Malaga"){
            data = datos_malaga
        }else if(input$ciudadGraficos=="Barcelona"){
            data = datos_barcelona
        }else if(input$ciudadGraficos=="Valencia"){
            data = datos_valencia
        }
        
        data$price <- gsub("[$,]","",data$price)
        data$price <- as.numeric(data$price)
        data<-data %>% group_by(room_type) %>%
            summarise(avg_price = round(mean(price),2))
        data
    })
    
    output$preciosMesGrafica <- renderPlotly({
        
        plot_ly(data = dataPrecio(), x = ~month, y = ~avg_pricemo, type= 'scatter', mode = 'markers+lines', color = "Set9",
                text = ~paste('Price: $', avg_pricemo)) %>%
            layout(xaxis = list(title = "Mes", type = 'category'),
                   yaxis = list(title = "Precio"))
    })
    
    output$reviewGrafica <- renderPlotly({
        plot_ly(data = dataReview(), x = ~ neighbourhood_group_cleansed, y = ~ avg_review, type= 'bar', color=I("blue")) %>%
            layout(xaxis = list(title = "Barrio"),
                   yaxis = list(title = "Puntuacion")
                   )
    })
    
    output$numeroTiposHabitaciones <- renderPlotly({
        plot_ly(data = dataGrafica3(), x = ~ room_type, y = ~ count_type, type= 'bar', color = ~room_type) %>%
            layout(xaxis = list(title = "Tipo de alojamiento"),
                   yaxis = list(title = "Cantidad")
            )
        
    })
    
    output$tipoMediaPrecio <- renderPlotly({
        plot_ly(data = dataGrafica4(), x = ~ room_type, y = ~ avg_price, type= 'bar', color = ~room_type) %>%
            layout(xaxis = list(title = "Tipo de Habitacion"),
                   yaxis = list(title = "Precio Medio")
            )
        
    })
    
    #------------------------------------------------
    #---------------------Mapa-----------------------
    #------------------------------------------------
    
    mapa <- reactive({
        
        
        if(input$ciudadMapa=="Barcelona"){
            data = datos_barcelona
        }else if(input$ciudadMapa=="Valencia"){
            data = datos_valencia
        }else if(input$ciudadMapa=="Malaga"){
            data = datos_malaga
        }
        
        data$price <- gsub("[$,]","",data$price)
        data$price <- as.numeric(data$price)
        data <- data %>% filter(neighbourhood_group_cleansed %in% input$eleccionBarrioMapa &
                                    room_type %in% input$tipoHabitacionMapa & 
                                    price >= input$precioSlider[1] &
                                    price <= input$precioSlider[2] &
                                    number_of_reviews >= input$reviewSlider[1] &
                                    number_of_reviews <= input$reviewSlider[2] &
                                    review_scores_rating >= input$ratingSlider[1] &
                                    review_scores_rating <= input$ratingSlider[2]
        ) 
    })
    
    
    output$map <- renderLeaflet({
        
        if(input$ciudadMapa=="Barcelona"){
            leaflet() %>% 
                addProviderTiles(providers$Esri.WorldStreetMap) %>%
                setView(lng = 2.15899, lat = 41.38879, zoom = 13)
        }else if(input$ciudadMapa=="Valencia"){
            leaflet() %>% 
                addProviderTiles(providers$Esri.WorldStreetMap) %>%
                setView(lng = -0.3773900, lat = 39.4697500, zoom = 13)
        }else if(input$ciudadMapa=="Malaga"){
            leaflet() %>% 
                addProviderTiles(providers$Esri.WorldStreetMap) %>%
                setView(lng = -4.42034, lat = 36.72016, zoom = 13)
        }
        
    })
    
    
    
    output$habitacionMapa <- renderUI({
        if(input$ciudadMapa=="Barcelona"){
            tagList(selectInput("tipoHabitacionMapa", label = h4("Tipo Habitacion"), datos_barcelona$room_type,multiple = TRUE,selected = datos_barcelona$room_type ))
        }else if(input$ciudadMapa=="Valencia"){
            tagList(selectInput("tipoHabitacionMapa", label = h4("Tipo Habitacion"), datos_valencia$room_type,multiple = TRUE,selected = datos_valencia$room_type ))
        }else if(input$ciudadMapa=="Malaga"){
            tagList(selectInput("tipoHabitacionMapa", label = h4("Tipo Habitacion"), datos_malaga$room_type,multiple = TRUE,selected = datos_malaga$room_type ))
            
        }
    })
    
    output$barrioMapa <- renderUI({
        if(input$ciudadMapa=="Valencia"){
            tagList(selectInput("eleccionBarrioMapa", label = h4("Barrio"), datos_valencia$neighbourhood_group_cleansed,multiple = TRUE,selected = datos_valencia$neighbourhood_group_cleansed))
        }else if(input$ciudadMapa=="Barcelona"){
            tagList(selectInput("eleccionBarrioMapa", label = h4("Barrio"), datos_barcelona$neighbourhood_group_cleansed,multiple = TRUE,selected = datos_barcelona$neighbourhood_group_cleansed))
        }else if(input$ciudadMapa=="Malaga"){
            tagList(selectInput("eleccionBarrioMapa", label = h4("Barrio"), datos_malaga$neighbourhood_group_cleansed,multiple = TRUE,selected = datos_malaga$neighbourhood_group_cleansed))
        }
        
        
    })
    
    
    
    observe({ 
        colores <- colorFactor(c("#cb3234", "#ffff00","#90ee90","#572364"),domain = c("Entire home/apt", "Private room","Shared room","Hotel room"))
        proxy <- leafletProxy("map",data = mapa()) %>%
            clearMarkerClusters() %>% 
            clearMarkers() %>%
            addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 2, color = ~colores(room_type),
                             group = "CIRCULOS",
                             popup = ~paste('<b><font color="Black">','Informacion','</font></b><br/>',
                                            'Tipo:', room_type,'<br/>',
                                            'Precio:', price,'<br/>',
                                            'Puntuacion:', review_scores_rating, '<br/>',
                                            'Numero de reviews:', number_of_reviews,'<br/>')) %>% 

            addCircleMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions(),
                             group = "CLUSTERS",
                             popup = ~paste('<b><font color="Black">','Informacion','</font></b><br/>',
                                            'Tipo: ', room_type, '<br/>',
                                            'Precio:', price,'<br/>',
                                            'Puntuacion:', review_scores_rating, '<br/>',
                                            'Numero de reviews:', number_of_reviews,'<br/>')) %>% 

            addLayersControl(
                baseGroups = c("CIRCULOS","CLUSTERS"),
                options = layersControlOptions(collapsed = FALSE)
            ) 
    })
    
    #------------------------------------------------
    #-----------------Prediccion---------------------
    #------------------------------------------------
    
    data_Ml <- reactive({
        
        
        if(input$ciudadPrediccion=="Barcelona"){
            data = datos_barcelona
        }else if(input$ciudadPrediccion=="Valencia"){
            data = datos_valencia
        }else if(input$ciudadPrediccion=="Malaga"){
            data = datos_malaga
        }
        
        names <- names(data)
        
        for (i in 1:length(names)){
            
            data[which(data[,names[i]]=='N/A'),names[i]] <- NA 
            
        }
        
        for (i in 1:length(names)){
            
            data[which(data[,names[i]]==''),names[i]] <- NA 
            
        }
        
        num_columnas <-
            c(
                'price'
                ,'security_deposit'
                ,'cleaning_fee'
            )
        
        for (i in num_columnas){
            data[[i]] <- gsub('[$]','',data[[i]])
            data[[i]] <- as.numeric(gsub(',','',data[[i]]))
        }
        
        data$security_deposit[is.na(data$security_deposit)] <- 0
        
        data$cleaning_fee[is.na(data$cleaning_fee)] <- 0
        
        medias_columnas <-
            c(
                'bathrooms'
                ,'bedrooms'
                ,'beds'
            )
        
        for (i in medias_columnas){
            data[[i]][is.na(data[[i]])] <- median(data[[i]], na.rm = T)
        }
        
        data$cancellation_policy <- ifelse(data$cancellation_policy == "strict_14_with_grace_period", "14_dias", data$cancellation_policy)
        
        data$cancellation_policy <- ifelse(data$cancellation_policy == "super_strict_30", "30_dias", data$cancellation_policy)
        
        data$cancellation_policy <- ifelse(data$cancellation_policy == "super_strict_60", "60_dias", data$cancellation_policy)
        
        factor_columnas <-
            c(
                'neighbourhood_group_cleansed'
                ,'room_type'
                ,'cancellation_policy'
            )
        
        
        for (i in factor_columnas){
            data[[i]] <- as.factor(data[[i]])
        }
        
        data <- sqldf("select *,
							case when amenities like '%Wifi%' then 'Si' else 'No' end as Wifi,
							case when amenities like '%TV%' then 'Si' else 'No' end as tv,
							case when amenities like '%Internet%' then 'Si' else 'No' end as Internet,
							case when amenities like '%Kitchen%' then 'Si' else 'No' end as Cocina,
							case when amenities like '%Heating%' then 'Si' else 'No' end as Calefaccion,
							case when amenities like '%Washer%' then 'Si' else 'No' end as Lavadora,
							case when amenities like '%Refrigerator%' then 'Si' else 'No' end as Nevera,
							case when amenities like '%Dishwasher%' then 'Si' else 'No' end as Dishwasher,
							case when amenities like '%Dryer%' then 'Si' else 'No' end as Secador,
							case when amenities like '%Microwave%' then 'Si' else 'No' end as Microondas,
							case when amenities like '%Stove%' then 'Si' else 'No' end as Estufa,
							case when amenities like '%Paid parking on premises%' then 'Si' else 'No' end as Parking_interior,
							case when amenities like '%Paid parking off premises%' then 'Si' else 'No' end as Parking_exterior
						from data")
        
        amenities <-
            c(
                'Wifi'
                ,'tv'
                ,'Internet'
                ,'Cocina'
                ,'Calefaccion'
                ,'Lavadora'
                ,'Nevera'
                ,'Dishwasher'
                ,'Secador'
                ,'Microondas'
                ,'Estufa'
                ,'Parking_interior'
                ,'Parking_exterior'
            )
        
        
        for (i in amenities){
            data[[i]] <- as.factor(data[[i]])
        }
        
        data$amenities <- NULL
        
        
        
        
        data <- data %>% select(price,room_type,cleaning_fee,
                                bedrooms,neighbourhood_group_cleansed,
                                beds,bathrooms,cancellation_policy,
                                tv,Internet)
        
        
        
        names(data)[1] <- "y"
        
       
        data
    })
    
    data_Entrada_Ml <- eventReactive(input$click,{
        data.frame(
            room_type = input$habitacionPrediccion,
            cleaning_fee = input$cleaning_fee,
            bedrooms = input$dormitoriosPrediccion,
            neighbourhood_group_cleansed = input$barrioPrediccion,
            beds = input$camasPrediccion,
            bathrooms = input$banos,
            cancellation_policy = input$cancelacionPrediccion,
            tv = input$tv,
            Internet = input$Internet)
    })
    
    
    output$opcionesMl <- renderUI({
        tagList(selectInput('barrioPrediccion', h4('Barrio'),choices = data_Ml()$neighbourhood_group_cleansed),
                br(),
                selectInput('habitacionPrediccion', h4('Tipo Habitacion'),choices = data_Ml()$room_type),
                br(),
                selectInput('cancelacionPrediccion', h4('Politica Cancelacion'),choices = data_Ml()$cancellation_policy),
                br())
    })
    
    modelo_lineal <- reactive({

        data = lm(formula = y ~ .,data = data_Ml())
    })
    
    modelo_gbm <- reactive({
    
        tc = trainControl(method = "cv", number=10)
        data = train(y ~., data=data_Ml(), method="gbm", trControl=tc)
        
    })
    
    modelo_random <- reactive({
       
        data <- rpart(y ~ ., data=data_Ml())
        
    })
    
    y_prediccion <- eventReactive(input$click,{ 
        if(input$modelo=="Modelo lineal basico"){
            modelo=modelo_lineal()
        }else if(input$modelo=="Random Forest"){
            modelo=modelo_random()
        }else if(input$modelo=="Gradient Boosting Machine"){
            modelo=modelo_gbm()
        }

        data = predict(modelo, newdata = data_Entrada_Ml())
    })
    
    output$Prediction <- renderText(print(paste("Precio Estimado:",round(y_prediccion(),2),"Dolares")))
    
    
    #------------------------------------------------
    #-----------------Palabras-----------------------
    #------------------------------------------------
    
    create_wordcloud <- function(data, numero_palabras = 100, background = "white") {


        if (is.character(data$description)) {
            corpus <- Corpus(VectorSource(data$description))
            corpus <- tm_map(corpus, tolower)
            corpus <- tm_map(corpus, removePunctuation)
            corpus <- tm_map(corpus, removeNumbers)
            corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
            corpus <- tm_map(corpus, removeWords, stopwords("english"))
            corpus <- tm_map(corpus, removeWords, c(input$palabra_eliminar_1))
            corpus <- tm_map(corpus, removeWords, c(input$palabra_eliminar_2))
            corpus <- tm_map(corpus, removeWords, c(input$palabra_eliminar_3))
            corpus <- tm_map(corpus, removeWords, c(input$palabra_eliminar_4))
            corpus <- tm_map(corpus, removeWords, c(input$palabra_eliminar_5))
            tdm <- as.matrix(TermDocumentMatrix(corpus))
            data <- sort(rowSums(tdm), decreasing = TRUE)
            data <- data.frame(word = names(data), freq = as.numeric(data))
        }


        if (!is.numeric(numero_palabras) || numero_palabras < 3) {
            num_words <- 3
        }

        data <- head(data, n = numero_palabras)
        if (nrow(data) == 0) {
            return(NULL)
        }

        wordcloud2(data, backgroundColor = background)
    }

    output$nube <- renderWordcloud2({
      if(input$ciudadPalabras=="Barcelona"){
        data = datos_barcelona
      }else if(input$ciudadPalabras=="Valencia"){
        data = datos_valencia
      }else if(input$ciudadPalabras=="Malaga"){
        data = datos_malaga
      }

      create_wordcloud(data,numero_palabras = input$numPalabras,background = "white")

    })
})
