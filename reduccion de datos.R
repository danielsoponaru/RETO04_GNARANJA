library(dplyr)
library(tidyr)
library(recommenderlab)

# Cargar datos
maestrostr <- readRDS("Datos/maestroestr.RDS")
objetivos <- readRDS("Datos/objetivos.RDS")
tickets_enc <- readRDS("Datos/tickets_enc.RDS")

# Enriquecer tickets
tickets_enc <- tickets_enc %>%
  left_join(maestrostr, by = "cod_est")
 

# Crear resumen cliente-producto
resumen <- tickets_enc %>%
  count(id_cliente_enc, cod_est, name = "n_compras_producto")

# Transformar a formato ancho
t <- pivot_wider(resumen, names_from = "cod_est", values_from = "n_compras_producto") %>%
  data.frame()
rownames(t) <- t[, 1]
t <- t[, -1]

# Convertir a matriz de ratings
t_rec <- as(as.matrix(t), "realRatingMatrix")

# Clientes y productos a conservar
clientes_a_salvar <- c(
  "b51353fcf07cb61280eda45e33679871", "02ff5edaa057b63ea0a0010c5402205c", 
  "25d259d32a2bc254343715f2e347c518", "53ffb83e85fd51cf1ec2fdef3c78b4fd", 
  "26f424b3bba6aaf97952ac599ed39f75", "32cc820ac27ff143c3ea976f3fe69d34", 
  "a57938025d714b65612bf2cfde12136d", "af30d404e282749ccd5f5ad0c8e834c7", 
  "8b9aa623b654a8be21b316a5fdf41007", "e27ceb0a1576648212c4325fdf7d8002",
  "fe234baf66f020e01feb5253dfb398f0", "d85ceefcf666f2b27e3e1e1252e5a1ac", 
  "a8a16b0b76cb14783348e920a59588ed", "1d98f84a5f074ed9c7a47515d4f5f329", 
  "528435b91691a75f5a60c6ccf4c6294c", "8e8315ed119c1382c4d351bbb188510e", 
  "fe52311b246f88407a1142d891ad77ae", "503a6539df48964124fe026b9deb5d13", 
  "a809525fe25b3de695bc87e00bea215f", "ec926181c315b758d775ee64a6a8e033"
)

productos_a_salvar <- c(
  "X14351005", "X12650103", "X01027405", "X05030101", "X05030102", "X01012310", "X11040303",
  "X08230125", "X01201505", "X05040180", "X01201005", "X09070103", "X04200505",
  "X01026410", "X05040181", "X04201005", "X12670111", "X08100903", "X01013315",
  "X01027205", "X12650101"
)

# Aplicar filtros preservando clientes y productos deseados
filtro_col_1 <- colCounts(t_rec) > 66.75 | colnames(t_rec) %in% productos_a_salvar
t_rec_1 <- t_rec[, filtro_col_1]

filtro_col_2 <- colMeans(t_rec_1) > 1.293 | colnames(t_rec_1) %in% productos_a_salvar
t_rec_2 <- t_rec_1[, filtro_col_2]

filtro_row_1 <- rowCounts(t_rec_2) > 23 | rownames(t_rec_2) %in% clientes_a_salvar
t_rec_3 <- t_rec_2[filtro_row_1, ]

filtro_row_2 <- rowMeans(t_rec_3) > 1.042 | rownames(t_rec_3) %in% clientes_a_salvar
t_rec_4 <- t_rec_3[filtro_row_2, ]

# Verificaciones opcionales
data.frame(ID = clientes_a_salvar, Presente = clientes_a_salvar %in% rownames(t_rec_4))
data.frame(Producto = productos_a_salvar, Presente = productos_a_salvar %in% colnames(t_rec_4))

# --- Verificaciones ---

# Verifica que los clientes estén presentes
verif_clientes <- data.frame(
  ID = clientes_a_salvar,
  Presente = clientes_a_salvar %in% rownames(t_rec_4)
)

# Verifica que los productos estén presentes
verif_productos <- data.frame(
  Producto = productos_a_salvar,
  Presente = productos_a_salvar %in% colnames(t_rec_4)
)

# Mostrar resultados
print("Verificación de clientes:")
print(verif_clientes)

print("Verificación de productos:")
print(verif_productos)


# Convertir el objeto 'realRatingMatrix' a un formato de matriz normal
matriz <- as(t_rec_4, "matrix")

# Convertir la matriz a un data.frame
matriz_df <- as.data.frame(matriz)

# Ver las primeras filas
head(matriz_df)

saveRDS(matriz_df, "Datos/matriz.RDS")

