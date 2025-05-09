### ANALISIS EXPLORATORIO RETO 4 ###

#Cargar librarias necesarias
library(dplyr)
library(lubridate)
library(ggplot2)

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

#GRAFICO 1: Histograma de cuantos articulos se suelen llevar en cada compra (por ticket)
tickets_enc1<- tickets_enc %>% 
  group_by(num_ticket, id_cliente_enc) %>% 
  summarise(ArticulosPorCompra = n()) %>% 
  arrange(desc(ArticulosPorCompra))

grafico1<- ggplot(tickets_enc1, aes(x = ArticulosPorCompra)) +
  geom_histogram(color = "lightblue4", fill = "lightblue", binwidth = 1) +
  labs(title = "Cantidad de articulos que se llevan por compra",
       y = "Cantidad de tickets",
       x = "Articulos por ticket(compra)") +
  theme_minimal()


#GRAFICO 2: Articulos que más veces se compran 
tickets_enc2<- tickets_enc %>% 
  group_by(cod_est) %>% 
  summarise(Cantidad = n()) %>% 
  arrange(desc(Cantidad)) %>% 
  head(10)

tickets_con_nombres<- tickets_enc2 %>%
  left_join(maestroestr, by = "cod_est")

grafico2<- ggplot(tickets_con_nombres, aes(x = reorder(descripcion, Cantidad), y = Cantidad)) +
  geom_col(fill = "lightblue", color = "lightblue4") +
  labs(title = "Top 10 artículos más comprados",
       x = "Artículo",
       y = "Cantidad de veces comprado") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#GRAFICO 3: cuanto tiempo ha estado un cliente comprando en Eroski (diferencia entre la primera compra y la ultima)
tickets_enc3<- tickets_enc %>%
  group_by(id_cliente_enc) %>%
  summarise(primera_compra = min(dia),
            ultima_compra = max(dia)) %>%
  mutate(duracion_dias = as.numeric(ultima_compra - primera_compra)) %>%
  arrange(desc(duracion_dias))

grafico3b<- ggplot(tickets_enc3, aes(y = duracion_dias)) +
  geom_boxplot(fill = "lightblue", , color = "lightblue4") +
  labs(title = "Distribución del tiempo de actividad de los clientes",
       y = "Duración entre primera y última compra (días)") +
  theme_minimal()

grafico3h<- ggplot(tickets_enc3, aes(x = duracion_dias)) +
  geom_histogram(binwidth = 30, fill = "lightblue", color = "lightblue4") +
  labs(title = "Tiempo de actividad de los clientes",
       x = "Duración entre primera y última compra (días)",
       y = "Número de clientes") +
  theme_minimal()


#GRAFICO 4: Intervalo promedio entre compras por cliente
tickets_enc4<- tickets_enc %>%
  arrange(id_cliente_enc, dia) %>%
  group_by(id_cliente_enc) %>%
  mutate(intervalo = as.numeric(difftime(dia, lag(dia), units = "days"))) %>%
  filter(!is.na(intervalo)) %>%
  summarise(media_intervalo = mean(intervalo)) %>%
  ungroup()

grafico4b<- ggplot(tickets_enc4, aes(y = media_intervalo)) +
  geom_boxplot(color = "lightblue4",fill = "lightblue") +
  labs( title = "Distribución de la frecuencia de compra de los clientes",
        y = "Días promedio entre compras") +
  coord_cartesian(ylim = c(0, 10)) + 
  theme_minimal()

grafico4h<- ggplot(tickets_enc4, aes(x = media_intervalo)) +
  geom_histogram(binwidth = 1, color = "lightblue4", fill = "lightblue") +
  labs(title = "Media de dias entre compras por cliente",
       x = "Media de dias entre compras",
       y = "Cantidad de clientes") +
  coord_cartesian(xlim = c(0, 8)) +
  theme_minimal()


#GRAFICO 5: Días de la semana que más se compra 
tickets_enc5<- tickets_enc %>% mutate(DiaSemana = wday(dia)) %>% group_by(DiaSemana) %>% summarise(CantidadProductos = n())

tickets_enc5$DiaSemana<- factor(tickets_enc5$DiaSemana,
                      levels = c(1, 2, 3, 4, 5, 6, 7),
                      labels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo"))

grafico5<- ggplot(tickets_enc5, aes(x = CantidadProductos, y = reorder(DiaSemana, CantidadProductos))) +
  geom_col(fill = "lightblue", color = "lightblue4") +
  theme_minimal() +
  labs(title = "Cantidad de productos comprados por día de la semana",
       x = "Cantidad de Productos",
       y = "Día de la Semana")


#GRAFICO 6: Productos más comprados por meses
tickets_enc6<- tickets_enc %>% mutate(Mes = month(dia)) %>% group_by(Mes) %>% summarise(CantidadProductos = n())

grafico6<- ggplot(tickets_enc6, aes(x = ))

#######
library(dplyr)
library(ggplot2)
library(lubridate)

#Asegurar formato de fecha
tickets_enc<- tickets_enc %>%
  mutate(dia = as.Date(dia),
         Mes = month(dia, label = TRUE, abbr = FALSE))

#Conteo de tickets por mes
tickets_por_mes<- tickets_enc %>%
  group_by(Mes, num_ticket, id_cliente_enc) %>%
  summarise(CantidadTickets = n())

#Gráfico
grafico6<- ggplot(tickets_por_mes, aes(x = Mes, y = CantidadTickets)) +
  geom_col(fill = "lightblue", color = "steelblue") +
  theme_minimal() +
  labs(title = "Cantidad de tickets por mes", x = "Mes", y = "Cantidad de Tickets")



################## GUARDAR LOS GRAFICOS ################## 
ggsave("Graficos/Grafico1.png", plot = grafico1, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Graficos/Grafico2.png", plot = grafico2, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Graficos/Grafico3b.png", plot = grafico3b, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Graficos/Grafico3h.png", plot = grafico3h, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Graficos/Grafico4h.png", plot = grafico4h, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Graficos/Grafico4b.png", plot = grafico4b, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Graficos/Grafico5.png", plot = grafico5, width = 8, height = 6, dpi = 300, bg = "white")




