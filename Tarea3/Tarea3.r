library(readxl)
library(arules)

# Cargar base completa
data <- read_excel("Tarea3/data/base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx")

# Grupo 1: Hecho y denuncia 
g1 <- data[, c("HEC_TIPAGRE","QUIEN_REPORTA","HEC_AREA","HEC_RECUR_DENUN","INST_DONDE_DENUNCIO")]
g1[] <- lapply(g1, function(x){x <- as.character(x); x[is.na(x)] <- "NA"; factor(x)})
reglas_g1 <- fim4r(g1, method="fpgrowth", target="rules", supp=0.2, conf=0.5)
rf_g1 <- as(reglas_g1, "data.frame")

# Grupo 2: Relación y contexto 
g2 <- data[, c("VIC_REL_AGR","OTRAS_VICTIMAS","AGRESORES_OTROS_TOTAL","HEC_DEPTO")]
g2[] <- lapply(g2, function(x){x <- as.character(x); x[is.na(x)] <- "NA"; factor(x)})
reglas_g2 <- fim4r(g2, method="fpgrowth", target="rules", supp=0.2, conf=0.5)
rf_g2 <- as(reglas_g2, "data.frame")

# Grupo 3: Medidas y legislación 
g3 <- data[, c("MEDIDAS_SEGURIDAD","TIPO_MEDIDA","LEY_APLICABLE","ORGANISMO_JURISDICCIONAL")]
g3[] <- lapply(g3, function(x){x <- as.character(x); x[is.na(x)] <- "NA"; factor(x)})
reglas_g3 <- fim4r(g3, method="fpgrowth", target="rules", supp=0.2, conf=0.5)
rf_g3 <- as(reglas_g3, "data.frame")

# Exportar resultados 
write.csv(rf_g1, "Tarea3/fp_growth_hecho_denuncia.csv", row.names = FALSE)
write.csv(rf_g2, "Tarea3/fp_growth_relacion_contexto.csv", row.names = FALSE)
write.csv(rf_g3, "Tarea3/fp_growth_medidas_ley.csv", row.names = FALSE)
