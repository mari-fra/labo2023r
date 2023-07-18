library(data.table)

dataset <- fread("buckets/b1/datasets/competencia_2023.csv.gz")

numeric <- names(dataset)[sapply(dataset, is.numeric)]
dataset <- dataset[, ..numeric]

resumen <- dataset[, lapply(.SD, sum), by = foto_mes]


# Crear una lista para almacenar la información de las variables con valor 0
variables_con_valor_cero <- list()


# Recorrer fila por fila el dataset
for (i in seq_len(nrow(resumen))) {
  foto_mes <- resumen[i, foto_mes]
  
  # Filtrar las columnas que tienen valor 0 en la columna "foto_mes" de la fila actual
  variables_con_cero <- Filter(function(x) x == 0, resumen[i, .SD, .SDcols = setdiff(names(resumen), c("foto_mes", "numero_de_cliente"))])
  #print(variables_con_cero)
  
  # Almacenar la información en la lista solo si hay variables con valor 0 en la columna "foto_mes"
  if (length(variables_con_cero) > 0) {
    variables_con_valor_cero[[i]] <- data.table(foto_mes = foto_mes, variable_con_cero = names(variables_con_cero))
  }
}

# Filtrar elementos no nulos en la lista y convertirla en un data.table
resultado <- rbindlist(variables_con_valor_cero)

agrupado <- resultado[, foto_mes, by = variable_con_cero]

concat <- agrupado[, lapply(.SD, paste0, collapse=","), by = variable_con_cero]
n_concat <-  concat[, foto_mes := paste0("c(", foto_mes, ")")]
