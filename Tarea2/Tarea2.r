# Apriori sobre "Educación Superior - Graduados 2023" (INE Guatemala)
# Autor: Eriksson Hernández
# Requisitos: install.packages(c("readxl","arules"))

library(readxl)
library(arules)

# 1) Carga de datos
df <- read_excel("data/graduados-superior-2023.xlsx")

# 2) Seleccionar columnas categóricas (evitar AÑO y Edad numérica)
cols <- c("CARRERA","Departamento","Nivel_Educativo","Sexo","Grupos_Quinquenales","Sector","Pueblo_Pertenencia")
df_cat <- df[, cols]

# 3) Convertir a "transactions" (cada fila: items atributo=valor)
#   Usamos data.frame a "transactions" con arules::transactions vía 
#   coerción: primero a "transactions" creando strings atributo=valor
fmt <- function(x) paste0(names(x), "=", x)
items_list <- apply(df_cat, 1, function(r) as.character(fmt(r)))
items_split <- lapply(items_list, function(s) strsplit(s, " ")[[1]])
items_split <- apply(df_cat, 1, function(r) paste0(names(r), "=", as.character(r)))
items_split <- lapply(1:nrow(df_cat), function(i) as.vector(items_split[,i]))

tr <- as(items_split, "transactions")

# 4) Ejecución del Algoritmo Apriori (sin filtrar por Nivel_Educativo)
#   Se elige minsup y minconf razonables para 41k filas: 
rules <- apriori(tr, parameter = list(supp = 0.20, conf = 0.50, minlen = 2))

# 5) Ordenar por lift y soporte para ver asociaciones fuertes
rules_sorted <- sort(rules, by = c("lift","support"), decreasing = TRUE)
inspect(head(rules_sorted, 50))

# 6) Exportar reglas a CSV
rules_df <- as(rules_sorted, "data.frame")
write.csv(rules_df, "Tarea2/tarea_2_reglas_apriori_global.csv", row.names = FALSE)

# 7) Filtrar reglas relevantes mencionadas en el reporte
subset_rules <- subset(rules_sorted, subset = rhs %in% c("Sector=Privado","Nivel_Educativo=Licenciatura","Pueblo_Pertenencia=Ladino","Grupos_Quinquenales=25 a 29"))
inspect(subset_rules)
