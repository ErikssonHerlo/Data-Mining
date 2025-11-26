# ==========================================================
#   ANÁLISIS DE RANDOM FOREST - VIOLENCIA INTRAFAMILIAR 2024
# ==========================================================
# Autor: Eriksson José Hernández López
# Fecha: Noviembre 2025
# Descripción: Este script realiza un análisis de clasificación
#              utilizando Random Forest sobre un conjunto de datos
#              de violencia intrafamiliar. Se incluyen pasos de
#              preprocesamiento, entrenamiento del modelo, evaluación
#              y visualización de resultados.
# ==========================================================

# 1. Librerías 
# Descomentar la siguiente linea, si no se tienen instalados los paquetes.

# install.packages(c("readxl", "dplyr", "randomForest", "caret")) 
library(readxl)
library(dplyr)
library(randomForest)
library(caret)
library(tidyr)

# 2. Carga de datos 
data <- read_excel("data/base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx")

# 3. Selección y limpieza de variables 
df_real <- data %>%
  select(
    HEC_TIPAGRE, VIC_EDAD, AGR_EDAD, VIC_SEXO, AGR_SEXO,
    VIC_GRUPET, HEC_DEPTO, VIC_REL_AGR, HEC_AREA,
    VIC_TRABAJA, AGR_TRABAJA, VIC_EST_CIV, AGR_EST_CIV
  ) %>%
  mutate(across(everything(), ~ na_if(., 99))) %>%
  mutate(across(everything(), ~ na_if(., 9))) %>%
  drop_na()

# 4. Ingeniería de variables 
df_final <- df_real %>%
  mutate(
    # Variable objetivo (target)
    CODIGO_STR = as.character(HEC_TIPAGRE),
    ES_FISICA = substr(CODIGO_STR, 1, 1) == "1",
    ES_SEXUAL = substr(CODIGO_STR, 3, 3) == "1",
    TIPO_BINARIO = as.factor(ifelse(ES_FISICA | ES_SEXUAL, "FISICA_CONTACTO", "PSICO_PATRIMONIAL")),

    # Diferencia de edad entre agresor y víctima
    GAP_EDAD = as.numeric(AGR_EDAD) - as.numeric(VIC_EDAD),

    # Relación víctima-agresor agrupada
    TIPO_RELACION = as.factor(ifelse(VIC_REL_AGR %in% c(1,2,3), "Pareja", "Familia"))
  ) %>%
  mutate(
    VIC_SEXO = as.factor(VIC_SEXO),
    AGR_SEXO = as.factor(AGR_SEXO),
    HEC_AREA = as.factor(HEC_AREA),
    VIC_TRABAJA = as.factor(VIC_TRABAJA),
    AGR_TRABAJA = as.factor(AGR_TRABAJA),
    HEC_DEPTO = as.factor(HEC_DEPTO),
    VIC_GRUPET = as.factor(VIC_GRUPET)
  ) %>%
  select(
    TIPO_BINARIO, GAP_EDAD, TIPO_RELACION, HEC_AREA,
    VIC_SEXO, AGR_SEXO, VIC_TRABAJA, AGR_TRABAJA,
    HEC_DEPTO, VIC_GRUPET
  )

# 5. Entrenamiento y prueba 
set.seed(555)
index <- sample(1:nrow(df_final), 0.8 * nrow(df_final))
train <- df_final[index, ]
test  <- df_final[-index, ]

rf_model <- randomForest(
  TIPO_BINARIO ~ .,
  data = train,
  ntree = 400,
  importance = TRUE,
  cutoff = c(0.45, 0.55)
)

# 6. Resultados del modelo 
print(rf_model)

preds <- predict(rf_model, test)
confusionMatrix(preds, test$TIPO_BINARIO)

# 7. Importancia de variables 
png("Tarea6/graphics/random_forest_importancia.png", width = 800, height = 600)
varImpPlot(rf_model, main = "Importancia de Variables - Violencia Intrafamiliar 2024")
dev.off()

# 8. Error por número de árboles 
png("Tarea6/graphics/random_forest_error_vs_trees.png", width = 800, height = 600)
plot(rf_model, main = "Evolución del Error vs Número de Árboles")
legend("topright", legend = colnames(rf_model$err.rate), col = 1:3, lty = 1)
dev.off()

# 9. Exportar predicciones 
write.csv(data.frame(test, Prediccion = preds),
          "Tarea6/graphics/predicciones_random_forest.csv", row.names = FALSE)

cat("\nAnálisis completado. Imágenes guardadas en la carpeta /graphics\n")