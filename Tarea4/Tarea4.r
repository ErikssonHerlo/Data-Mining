#  Paquetes 
library(readxl)
library(ggplot2)
library(dplyr)

#  Cargar datos 
data <- read_excel("data/base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx")

#  Variables del clustering (las mismas que venimos usando) 
data_fp <- data[, c("HEC_MES", "HEC_DEPTO", "VIC_EDAD", "VIC_ESCOLARIDAD", 
                    "VIC_EST_CIV", "VIC_GRUPET", "VIC_TRABAJA", "VIC_DEDICA", 
                    "AGR_EDAD", "AGR_ESCOLARIDAD")]
data_fp[is.na(data_fp)] <- -1

set.seed(123)
cluster_k <- kmeans(data_fp, centers = 3)

#  Carpeta de salida para los graficos 
outdir <- "Tarea4/graphics_kmeans"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)


# 1) Escolaridad víctima vs agresor (scatter + centroides)
p1 <- ggplot(data_fp, aes(x = VIC_ESCOLARIDAD, y = AGR_ESCOLARIDAD, 
                          color = as.factor(cluster_k$cluster))) +
  geom_point(alpha = 0.7) +
  geom_point(data = as.data.frame(cluster_k$centers),
             aes(x = VIC_ESCOLARIDAD, y = AGR_ESCOLARIDAD),
             color = "black", size = 4, shape = 17, inherit.aes = FALSE) +
  labs(title = "Clústeres K-Means: Escolaridad (Víctima vs Agresor)",
       x = "Escolaridad de la víctima", y = "Escolaridad del agresor",
       color = "Clúster") +
  theme_minimal()

# Guardar PNG y PDF
ggsave(file.path(outdir, "kmeans_escolaridad_victima_vs_agresor.png"),
       plot = p1, width = 9, height = 6, dpi = 300)
ggsave(file.path(outdir, "kmeans_escolaridad_victima_vs_agresor.pdf"),
       plot = p1, width = 9, height = 6)


# 2) Edad víctima vs edad agresor con elipses de confianza
p2 <- ggplot(data_fp, aes(x = VIC_EDAD, y = AGR_EDAD, 
                          color = as.factor(cluster_k$cluster))) +
  geom_point(alpha = 0.7) +
  stat_ellipse(aes(group = as.factor(cluster_k$cluster)), 
               level = 0.95, type = "norm", linewidth = 0.8) +
  geom_point(data = as.data.frame(cluster_k$centers), 
             aes(x = VIC_EDAD, y = AGR_EDAD),
             color = "black", size = 5, shape = 17, inherit.aes = FALSE) +
  labs(title = "Clústeres K-Means: Edad (Víctima vs Agresor)",
       x = "Edad víctima", y = "Edad agresor", color = "Clúster") +
  theme_minimal()

ggsave(file.path(outdir, "kmeans_edad_victima_vs_agresor_ellipse.png"),
       plot = p2, width = 9, height = 6, dpi = 300)
ggsave(file.path(outdir, "kmeans_edad_victima_vs_agresor_ellipse.pdf"),
       plot = p2, width = 9, height = 6)


# 3) Edad víctima vs edad agresor con "convex hull" por clúster
df_plot <- data_fp |>
  mutate(cluster = factor(cluster_k$cluster))

hulls <- df_plot |>
  group_by(cluster) |>
  reframe({
    idx <- chull(VIC_EDAD, AGR_EDAD)
    tibble(VIC_EDAD = VIC_EDAD[idx], AGR_EDAD = AGR_EDAD[idx])
  })

p3 <- ggplot(df_plot, aes(VIC_EDAD, AGR_EDAD, color = cluster)) +
  geom_point(alpha = 0.7) +
  geom_polygon(data = hulls, aes(VIC_EDAD, AGR_EDAD, fill = cluster),
               alpha = 0.15, color = NA, inherit.aes = FALSE) +
  geom_point(data = as.data.frame(cluster_k$centers), 
             aes(x = VIC_EDAD, y = AGR_EDAD),
             color = "black", size = 5, shape = 17, inherit.aes = FALSE) +
  labs(title = "Clústeres K-Means (Convex Hull): Edad Víctima vs Agresor",
       x = "Edad víctima", y = "Edad agresor", color = "Clúster", fill = "Clúster") +
  theme_minimal()

ggsave(file.path(outdir, "kmeans_edad_victima_vs_agresor_hull.png"),
       plot = p3, width = 9, height = 6, dpi = 300)
ggsave(file.path(outdir, "kmeans_edad_victima_vs_agresor_hull.pdf"),
       plot = p3, width = 9, height = 6)


# Exportar asignación de clúster y centroides
write.csv(data.frame(data_fp, cluster = cluster_k$cluster),
          file.path("Tarea4", "kmeans_asignaciones.csv"), row.names = FALSE)

write.csv(as.data.frame(cluster_k$centers),
          file.path("Tarea4", "kmeans_centroides.csv"), row.names = FALSE)
