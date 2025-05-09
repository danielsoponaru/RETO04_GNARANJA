# Librerías necesarias
library(lubridate)
library(dplyr)
library(naniar)
library(tidyr)
library(ggplot2)
library(cluster)

# Cargar los datos
maestrostr <- readRDS("Datos//maestroestr.RDS")
objetivos <- readRDS("Datos//objetivos.RDS")
tickets_enc <- readRDS("Datos//tickets_enc.RDS")

# Formateo inicial
tickets_enc$num_ticket <- as.character(tickets_enc$num_ticket)
tickets_enc$dia <- ymd(tickets_enc$dia)

# Verificar NAs
vis_miss(tickets_enc, warn_large_data = FALSE)  # No hay NAs

# Eliminar tickets duplicados (comprobado que no se repiten)
tickets_enc <- tickets_enc %>%
  mutate(num_ticket = paste(num_ticket, id_cliente_enc))

# Agregar día de la semana
tickets_enc <- tickets_enc %>%
  mutate(DiaSemana = wday(dia, week_start = 1))

# Características de clientes para clustering
datos_clientes <- tickets_enc %>%
  group_by(id_cliente_enc) %>%
  summarise(
    total_productos = n(),
    productos_distintos = n_distinct(cod_est),
    dias_activos = as.numeric(max(dia) - min(dia)),
    compras_por_semana = ifelse(dias_activos > 0, n() / (dias_activos / 7), n()),
    compras_entre_semana = sum(DiaSemana %in% 1:5),
    compras_fin_de_semana = sum(DiaSemana %in% 6:7)
  ) %>%
  ungroup()

saveRDS(datos_clientes, "Datos/datos_para_clustering.RDS")

# Eliminar outliers usando IQR
outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  which(x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr))
}
outlier_indices <- lapply(datos_clientes[,-1], outliers_iqr)
filas_outliers <- unique(unlist(outlier_indices))
datos_sin_outliers <- datos_clientes[-filas_outliers, ]

# Escalado de variables
datos_cluster <- datos_sin_outliers %>% select(-id_cliente_enc)
datos_scaled <- scale(datos_cluster)

# Método del codo para K-means
set.seed(123)
wss <- numeric(15)
for (i in 1:15) {
  wss[i] <- kmeans(datos_scaled, centers = i, nstart = 25)$tot.withinss
}

# Graficar codo
ggplot(data.frame(k = 1:15, wss = wss), aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(title = "Método del Codo", x = "Número de Clusters", y = "Suma de Cuadrados Intra-cluster")

# Ejecutar K-means con k = 3 (puedes ajustar este valor según el gráfico del codo)
set.seed(123)
kmeans_result <- kmeans(datos_scaled, centers = 3, nstart = 25)
datos_sin_outliers$kmeans_cluster <- as.factor(kmeans_result$cluster)

# Visualización en 2D con PCA
pca_result <- prcomp(datos_scaled)
datos_sin_outliers$pca1 <- pca_result$x[,1]
datos_sin_outliers$pca2 <- pca_result$x[,2]

ggplot(datos_sin_outliers, aes(x = pca1, y = pca2, color = kmeans_cluster)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Clusters K-means", x = "Componente Principal 1", y = "Componente Principal 2") +
  scale_color_manual(values = c("red", "blue", "green"))

# Guardar clientes con cluster asignado
clientes_clusterizados <- datos_sin_outliers %>%
  select(id_cliente_enc, kmeans_cluster)
saveRDS(clientes_clusterizados, "Datos/clientes_clusterizados.RDS")

# Aplicar clusters a la matriz cliente-producto
matriz_df <- readRDS("Datos/matriz.RDS")
matriz_df$id_cliente_enc <- rownames(matriz_df)

matriz_con_cluster <- matriz_df %>%
  inner_join(clientes_clusterizados, by = "id_cliente_enc")

producto_cols <- setdiff(names(matriz_con_cluster), c("id_cliente_enc", "kmeans_cluster"))

# Crear matriz por clúster
matrices_por_cluster <- matriz_con_cluster %>%
  group_split(kmeans_cluster) %>%
  setNames(paste0("Cluster_", unique(matriz_con_cluster$kmeans_cluster))) %>%
  lapply(function(df) {
    mat <- df[, producto_cols]
    rownames(mat) <- df$id_cliente_enc
    return(as.matrix(mat))
  })

# Guardar matrices por cluster
for (i in names(matrices_por_cluster)) {
  saveRDS(matrices_por_cluster[[i]], paste0("Datos/matriz_", i, ".RDS"))
}

# Verificar dimensiones
lapply(matrices_por_cluster, dim)
