### ANALISIS EXPLORATORIO RETO 4 ###

#Cargar librarias necesarias
library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)

#Cargar datos
maestroestr<-readRDS("Datos/maestroestr.RDS")
objetivos<-readRDS("Datos/objetivos.RDS")
tickets_enc<-readRDS("Datos/tickets_enc.RDS")

maestroestr #codigos de productos
tickets_enc #productos que ha adquirido cada cliente

options(scipen = 999)

#Examinar datos y tipos de columnas
str(maestroestr)
str(tickets_enc)

summary(maestroestr)
summary(tickets_enc)

colnames(maestroestr)
colnames(tickets_enc)

dim(maestroestr)
dim(tickets_enc)

#Ajustar tipos de columnas
tickets_enc$dia<- ymd(tickets_enc$dia)
tickets_enc$cod_est<- as.numeric(tickets_enc$cod_est)
tickets_enc$id_cliente_enc<- as.character(tickets_enc$id_cliente_enc)
maestroestr$cod_est<- as.numeric(maestroestr$cod_est)

#Comprobar
str(tickets_enc) #todos los tipos de columnas son correctos

#Definir la UI
ui<- fluidPage(
  titlePanel("Análisis Exploratorio - Reto 4"),
  tabsetPanel(
    tabPanel("Gráfico 1: Artículos por compra",
             plotOutput("grafico1")
    ),
    tabPanel("Gráfico 2: Top artículos comprados",
             plotOutput("grafico2")
    ),
    tabPanel("Gráfico 3: Tiempo de actividad de clientes",
             plotOutput("grafico3b"),
             plotOutput("grafico3h")
    ),
    tabPanel("Gráfico 4: Intervalo promedio entre compras",
             plotOutput("grafico4b"),
             plotOutput("grafico4h")
    ),
    tabPanel("Gráfico 5: Compras por día de la semana",
             plotOutput("grafico5")
    )
  )
)

#Server
server<- function(input, output, session) {
  
  #Gráfico 1
  output$grafico1 <- renderPlot({
    tickets_enc %>% 
      group_by(num_ticket, id_cliente_enc) %>% 
      summarise(ArticulosPorCompra = n(), .groups = "drop") %>%
      ggplot(aes(x = ArticulosPorCompra)) +
      geom_histogram(color = "lightblue4", fill = "lightblue", binwidth = 1) +
      labs(title = "Cantidad de artículos que se llevan por compra",
           y = "Cantidad de tickets",
           x = "Artículos por ticket") +
      theme_minimal()
  })
  
  #Gráfico 2
  output$grafico2 <- renderPlot({
    top_articulos <- tickets_enc %>% 
      group_by(cod_est) %>% 
      summarise(Cantidad = n(), .groups = "drop") %>%
      arrange(desc(Cantidad)) %>%
      head(10) %>%
      left_join(maestroestr, by = "cod_est")
    
    ggplot(top_articulos, aes(x = reorder(descripcion, Cantidad), y = Cantidad)) +
      geom_col(fill = "lightblue", color = "lightblue4") +
      labs(title = "Top 10 artículos más comprados",
           x = "Artículo",
           y = "Cantidad de veces comprado") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #Gráfico 3b y 3h
  output$grafico3b <- renderPlot({
    duraciones <- tickets_enc %>%
      group_by(id_cliente_enc) %>%
      summarise(
        primera_compra = min(dia),
        ultima_compra = max(dia),
        duracion_dias = as.numeric(ultima_compra - primera_compra),
        .groups = "drop"
      )
    
    ggplot(duraciones, aes(y = duracion_dias)) +
      geom_boxplot(fill = "lightblue", color = "lightblue4") +
      labs(title = "Distribución del tiempo de actividad de los clientes",
           y = "Duración entre primera y última compra (días)") +
      theme_minimal()
  })
  
  output$grafico3h <- renderPlot({
    duraciones <- tickets_enc %>%
      group_by(id_cliente_enc) %>%
      summarise(
        primera_compra = min(dia),
        ultima_compra = max(dia),
        duracion_dias = as.numeric(ultima_compra - primera_compra),
        .groups = "drop"
      )
    
    ggplot(duraciones, aes(x = duracion_dias)) +
      geom_histogram(binwidth = 30, fill = "lightblue", color = "lightblue4") +
      labs(title = "Tiempo de actividad de los clientes",
           x = "Duración entre primera y última compra (días)",
           y = "Número de clientes") +
      theme_minimal()
  })
  
  #Gráfico 4b y 4h
  output$grafico4b <- renderPlot({
    intervalos <- tickets_enc %>%
      arrange(id_cliente_enc, dia) %>%
      group_by(id_cliente_enc) %>%
      mutate(intervalo = as.numeric(difftime(dia, lag(dia), units = "days"))) %>%
      filter(!is.na(intervalo)) %>%
      summarise(media_intervalo = mean(intervalo), .groups = "drop")
    
    ggplot(intervalos, aes(y = media_intervalo)) +
      geom_boxplot(color = "lightblue4", fill = "lightblue") +
      labs(title = "Distribución de la frecuencia de compra de los clientes",
           y = "Días promedio entre compras") +
      coord_cartesian(ylim = c(0, 10)) +
      theme_minimal()
  })
  
  output$grafico4h <- renderPlot({
    intervalos <- tickets_enc %>%
      arrange(id_cliente_enc, dia) %>%
      group_by(id_cliente_enc) %>%
      mutate(intervalo = as.numeric(difftime(dia, lag(dia), units = "days"))) %>%
      filter(!is.na(intervalo)) %>%
      summarise(media_intervalo = mean(intervalo), .groups = "drop")
    
    ggplot(intervalos, aes(x = media_intervalo)) +
      geom_histogram(binwidth = 1, color = "lightblue4", fill = "lightblue") +
      labs(title = "Media de días entre compras por cliente",
           x = "Media de días entre compras",
           y = "Cantidad de clientes") +
      coord_cartesian(xlim = c(0, 8)) +
      theme_minimal()
  })
  
  #Gráfico 5
  output$grafico5 <- renderPlot({
    tickets_enc %>%
      mutate(DiaSemana = wday(dia)) %>%
      group_by(DiaSemana) %>%
      summarise(CantidadProductos = n(), .groups = "drop") %>%
      mutate(DiaSemana = factor(DiaSemana, levels = 1:7, labels = c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"))) %>%
      ggplot(aes(x = CantidadProductos, y = reorder(DiaSemana, CantidadProductos))) +
      geom_col(fill = "lightblue", color = "lightblue4") +
      labs(title = "Cantidad de productos comprados por día de la semana",
           x = "Cantidad de Productos",
           y = "Día de la Semana") +
      theme_minimal()
  })
}

#Ejecutar app
shinyApp(ui = ui, server = server)
