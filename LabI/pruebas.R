a <- dataset[foto_mes == 202109][1:10]
b <- dataset[foto_mes == 202107][1:10]

c <- dataset[foto_mes == 202107][11:20]
c$foto_mes = 201801

d <- dataset[foto_mes == 202107][21:30] 
d$foto_mes = 202006

e <- dataset[foto_mes == 202107][31:40] 
e$foto_mes = 201910

data_concat <- rbindlist(list(a, b, c, d, e))


Corregir_Frollmean <- function(dataset) {
  #variables <- c("thomebanking", "chomebanking_transacciones", "tcallcenter", "ccallcenter_transacciones", "cprestamos_personales", "mprestamos_personales", "mprestamos_hipotecarios", "ccajas_transacciones", "ccajas_consultas", "ccajas_depositos", "ccajas_extracciones", "ccajas_otras")
  variables <- c("cprestamos_personales", "mprestamos_personales")
  for (variable in variables) {
    tbl <- dataset[, .(v1 = shift(get(variable), 2, type = "lag"),
                       v2 = shift(get(variable), 2, type = "lead")),
                   by = numero_de_cliente]
    
    tbl[, promedio := rowMeans(.SD, na.rm = TRUE), .SDcols = c("v1", "v2")]
    print(tbl[1:20])
    
    dataset[, (variable) := ifelse(is.na(get(variable)) | get(variable) == 0,
                                   tbl$promedio,
                                   get(variable)),
            by = numero_de_cliente,
            with = FALSE]
  }
}



Corregir_Frollmean_2 <- function(dataset) {
  variables <- c("cprestamos_personales", "mprestamos_personales")
  for (variable in variables) {
    tbl <- dataset[, .(v1 = shift(get(variable), 2, type = "lag"),
                       v2 = shift(get(variable), 2, type = "lead")),
                   by = numero_de_cliente]
    
    tbl[, promedio := mean(c(v1, v2), na.rm = TRUE), by = numero_de_cliente]
    print(tbl[1:20])
    dataset[, (variable) := ifelse(is.na(get(variable)) | get(variable) == 0,
                                   tbl$promedio,
                                   get(variable))]
  }
}


Corregir_Frollmean_2(data_concat)


Corregir_Frollmean_3 <- function(dataset) {
  variables <- c("cprestamos_personales", "mprestamos_personales")
  for (variable in variables) {
    dataset[, paste0(variable, "_promedio") := frollmean(get(variable), 5, align = "center", na.rm = TRUE)]
    dataset[, (variable) := ifelse(is.na(get(variable)) | get(variable) == 0,
                                   get(paste0(variable, "_promedio")),
                                   get(variable))]
    dataset[, paste0(variable, "_promedio") := NULL]
  }
}


Corregir_Frollmean_3(data_concat)


CorregirCampoMes_frollmean <- function(pcampo, pmeses) {
  tbl <- dataset[, list(
    "v1" = shift(get(pcampo), 2, type = "lag"),
    "v2" = shift(get(pcampo), 2, type = "lead")
  ),
  by = numero_de_cliente
  ]
  
  print(tbl[1:20])
  
  tbl[, numero_de_cliente := NULL]
  tbl[, promedio := rowMeans(tbl, na.rm = TRUE)]
  
  dataset[
    ,
    paste0(pcampo) := ifelse(!(foto_mes %in% pmeses),
                             get(pcampo),
                             tbl$promedio
    )
  ]
  print(tbl$promedio)
}



Corregir_Frollmean_4 <- function(dataset) {
  CorregirCampoMes_frollmean("cprestamos_personales", c(201801, 201910, 202006, 202107, 202109))
  CorregirCampoMes_frollmean("mprestamos_personales", c(201801, 201910, 202006, 202107, 202109))
}

Corregir_Frollmean_4(data_concat)
