library(readxl)
library(arules)

data <- read_excel("Tarea3/data/base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx")

# Seleccionar columnas categóricas (víctima)
victim_cols <- c("HEC_MES","HEC_DEPTO","VIC_EDAD","VIC_ESCOLARIDAD",
                 "VIC_EST_CIV","VIC_GRUPET","VIC_TRABAJA","VIC_DEDICA")

data_fp <- data[, victim_cols]

# Asegurar tipo factor/character y hacer explícitos los NA (para no perder información en patrones)
data_fp[] <- lapply(data_fp, function(x) {x <- as.character(x); x[is.na(x)] <- "NA"; factor(x)})

# FP-Growth (reglas globales, sin filtros)
# Soporte y confianza iniciales
supp <- 0.20
conf <- 0.50

reglas_fp <- fim4r(data_fp, method = "fpgrowth", target = "rules",
                   supp = supp, conf = conf)

# Ver reglas básicas
inspect(head(reglas_fp, 20))

# Convertir a data.frame y ordenar por lift y soporte
rf <- as(reglas_fp, "data.frame")
rf <- rf[order(-rf$lift, -rf$support, -rf$confidence), ]

# Guardar a CSV (opcional)
write.csv(rf, "Tarea3/reglas_fp_global.csv", row.names = FALSE)

# Reglas con items clave
subset_idx <- grepl("VIC_TRABAJA", rf$rules) | grepl("VIC_DEDICA", rf$rules) | grepl("VIC_GRUPET", rf$rules) | grepl("VIC_EST_CIV", rf$rules)
head(rf[subset_idx, ], 30)


