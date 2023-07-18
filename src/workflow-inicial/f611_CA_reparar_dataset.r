# Experimentos Colaborativos Default
# Workflow  Catastrophe Analysis

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "CA6114"
PARAM$dataset <- "./datasets/competencia_2023.csv.gz"

# valores posibles
#  "MachineLearning"  "EstadisticaClasica" "Ninguno" "Frollmean"
PARAM$metodo <- "Frollmean"
PARAM$home <- "~/buckets/b1/"

# FIN Parametros del script

OUTPUT <- list()

#------------------------------------------------------------------------------

options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})
#------------------------------------------------------------------------------

GrabarOutput <- function() {
  write_yaml(OUTPUT, file = "output.yml") # grabo output
}
#------------------------------------------------------------------------------

CorregirCampoMes <- function(pcampo, pmeses) {
  tbl <- dataset[, list(
    "v1" = shift(get(pcampo), 1, type = "lag"),
    "v2" = shift(get(pcampo), 1, type = "lead")
  ),
  by = numero_de_cliente
  ]

  tbl[, numero_de_cliente := NULL]
  tbl[, promedio := rowMeans(tbl, na.rm = TRUE)]

  dataset[
    ,
    paste0(pcampo) := ifelse(!(foto_mes %in% pmeses),
      get(pcampo),
      tbl$promedio
    )
  ]
}

CorregirCampoMes_Frollmean <- function(pcampo, pmeses) {
  tbl <- dataset[, list(  #Se crea el dataset tbl con cuatro columnas
    "v1" = shift(get(pcampo), 2, type = "lag"), #2 meses antes
    "v2" = shift(get(pcampo), 2, type = "lead"), #1 mes antes
    "v3" = shift(get(pcampo), 1, type = "lag"), #1 mes despues
    "v4" = shift(get(pcampo), 1, type = "lead") #2 meses despues
  ),
  by = numero_de_cliente  #Agrupa por cliente. 
  ]
  #Imp: debe estar ordenado por foto_mes y nro de cliente
  
  tbl[, numero_de_cliente := NULL] #completa con null esta variable
  tbl[, promedio := rowMeans(tbl[, list(v1, v2, v3, v4)], na.rm = TRUE)] #promedia por fila las 4 columnas generadas
  #y agrega la columna promedio
  dataset[
    ,
    paste0(pcampo) := ifelse(!(foto_mes %in% pmeses),
                             get(pcampo),
                             tbl$promedio
    )
  ]
}


#------------------------------------------------------------------------------
# reemplaza cada variable ROTA  (variable, foto_mes)
#  con el promedio entre  ( mes_anterior, mes_posterior )

Corregir_EstadisticaClasica <- function(dataset) {
  CorregirCampoMes("thomebanking", c(201801, 202006))
  CorregirCampoMes("chomebanking_transacciones", c(201801, 201910, 202006))
  CorregirCampoMes("tcallcenter", c(201801, 201806, 202006))
  CorregirCampoMes("ccallcenter_transacciones", c(201801, 201806, 202006))
  CorregirCampoMes("cprestamos_personales", c(201801, 202006))
  CorregirCampoMes("mprestamos_personales", c(201801, 202006))
  CorregirCampoMes("mprestamos_hipotecarios", c(201801, 202006))
  CorregirCampoMes("ccajas_transacciones", c(201801, 202006))
  CorregirCampoMes("ccajas_consultas", c(201801, 202006))
  CorregirCampoMes("ccajas_depositos", c(201801, 202006))
  CorregirCampoMes("ccajas_extracciones", c(201801, 202006))
  CorregirCampoMes("ccajas_otras", c(201801, 202006))

  CorregirCampoMes("ctarjeta_visa_debitos_automaticos", c(201904))
  CorregirCampoMes("mttarjeta_visa_debitos_automaticos", c(201904, 201905))
  CorregirCampoMes("Visa_mfinanciacion_limite", c(201904))

  CorregirCampoMes("mrentabilidad", c(201905, 201910, 202006))
  CorregirCampoMes("mrentabilidad_annual", c(201905, 201910, 202006))
  CorregirCampoMes("mcomisiones", c(201905, 201910, 202006))
  CorregirCampoMes("mpasivos_margen", c(201905, 201910, 202006))
  CorregirCampoMes("mactivos_margen", c(201905, 201910, 202006))
  CorregirCampoMes("ccomisiones_otras", c(201905, 201910, 202006))
  CorregirCampoMes("mcomisiones_otras", c(201905, 201910, 202006))

  CorregirCampoMes("ctarjeta_visa_descuentos", c(201910))
  CorregirCampoMes("ctarjeta_master_descuentos", c(201910))
  CorregirCampoMes("mtarjeta_visa_descuentos", c(201910))
  CorregirCampoMes("mtarjeta_master_descuentos", c(201910))
  CorregirCampoMes("ccajeros_propios_descuentos", c(201910))
  CorregirCampoMes("mcajeros_propios_descuentos", c(201910))

  CorregirCampoMes("cliente_vip", c(201911))

  CorregirCampoMes("active_quarter", c(202006))
  CorregirCampoMes("mcuentas_saldo", c(202006))
  CorregirCampoMes("ctarjeta_debito_transacciones", c(202006))
  CorregirCampoMes("mautoservicio", c(202006))
  CorregirCampoMes("ctarjeta_visa_transacciones", c(202006))
  CorregirCampoMes("ctarjeta_visa_transacciones", c(202006))
  CorregirCampoMes("cextraccion_autoservicio", c(202006))
  CorregirCampoMes("mextraccion_autoservicio", c(202006))
  CorregirCampoMes("ccheques_depositados", c(202006))
  CorregirCampoMes("mcheques_depositados", c(202006))
  CorregirCampoMes("mcheques_emitidos", c(202006))
  CorregirCampoMes("mcheques_emitidos", c(202006))
  CorregirCampoMes("ccheques_depositados_rechazados", c(202006))
  CorregirCampoMes("mcheques_depositados_rechazados", c(202006))
  CorregirCampoMes("ccheques_emitidos_rechazados", c(202006))
  CorregirCampoMes("mcheques_emitidos_rechazados", c(202006))
  CorregirCampoMes("catm_trx", c(202006))
  CorregirCampoMes("matm", c(202006))
  CorregirCampoMes("catm_trx_other", c(202006))
  CorregirCampoMes("matm_other", c(202006))
  CorregirCampoMes("cmobile_app_trx", c(202006))
}
#------------------------------------------------------------------------------

Corregir_MachineLearning <- function(dataset) {
  gc()
  # acomodo los errores del dataset

  dataset[foto_mes == 201901, ctransferencias_recibidas := NA]
  dataset[foto_mes == 201901, mtransferencias_recibidas := NA]

  dataset[foto_mes == 201902, ctransferencias_recibidas := NA]
  dataset[foto_mes == 201902, mtransferencias_recibidas := NA]

  dataset[foto_mes == 201903, ctransferencias_recibidas := NA]
  dataset[foto_mes == 201903, mtransferencias_recibidas := NA]

  dataset[foto_mes == 201904, ctransferencias_recibidas := NA]
  dataset[foto_mes == 201904, mtransferencias_recibidas := NA]
  dataset[foto_mes == 201904, ctarjeta_visa_debitos_automaticos := NA]
  dataset[foto_mes == 201904, mttarjeta_visa_debitos_automaticos := NA]
  dataset[foto_mes == 201904, Visa_mfinanciacion_limite := NA]

  dataset[foto_mes == 201905, ctransferencias_recibidas := NA]
  dataset[foto_mes == 201905, mtransferencias_recibidas := NA]
  dataset[foto_mes == 201905, mrentabilidad := NA]
  dataset[foto_mes == 201905, mrentabilidad_annual := NA]
  dataset[foto_mes == 201905, mcomisiones := NA]
  dataset[foto_mes == 201905, mpasivos_margen := NA]
  dataset[foto_mes == 201905, mactivos_margen := NA]
  dataset[foto_mes == 201905, ctarjeta_visa_debitos_automaticos := NA]
  dataset[foto_mes == 201905, ccomisiones_otras := NA]
  dataset[foto_mes == 201905, mcomisiones_otras := NA]

  dataset[foto_mes == 201910, mpasivos_margen := NA]
  dataset[foto_mes == 201910, mactivos_margen := NA]
  dataset[foto_mes == 201910, ccomisiones_otras := NA]
  dataset[foto_mes == 201910, mcomisiones_otras := NA]
  dataset[foto_mes == 201910, mcomisiones := NA]
  dataset[foto_mes == 201910, mrentabilidad := NA]
  dataset[foto_mes == 201910, mrentabilidad_annual := NA]
  dataset[foto_mes == 201910, chomebanking_transacciones := NA]
  dataset[foto_mes == 201910, ctarjeta_visa_descuentos := NA]
  dataset[foto_mes == 201910, ctarjeta_master_descuentos := NA]
  dataset[foto_mes == 201910, mtarjeta_visa_descuentos := NA]
  dataset[foto_mes == 201910, mtarjeta_master_descuentos := NA]
  dataset[foto_mes == 201910, ccajeros_propios_descuentos := NA]
  dataset[foto_mes == 201910, mcajeros_propios_descuentos := NA]

  dataset[foto_mes == 202001, cliente_vip := NA]

  dataset[foto_mes == 202006, active_quarter := NA]
  dataset[foto_mes == 202006, mrentabilidad := NA]
  dataset[foto_mes == 202006, mrentabilidad_annual := NA]
  dataset[foto_mes == 202006, mcomisiones := NA]
  dataset[foto_mes == 202006, mactivos_margen := NA]
  dataset[foto_mes == 202006, mpasivos_margen := NA]
  dataset[foto_mes == 202006, mcuentas_saldo := NA]
  dataset[foto_mes == 202006, ctarjeta_debito_transacciones := NA]
  dataset[foto_mes == 202006, mautoservicio := NA]
  dataset[foto_mes == 202006, ctarjeta_visa_transacciones := NA]
  dataset[foto_mes == 202006, mtarjeta_visa_consumo := NA]
  dataset[foto_mes == 202006, ctarjeta_master_transacciones := NA]
  dataset[foto_mes == 202006, mtarjeta_master_consumo := NA]
  dataset[foto_mes == 202006, ccomisiones_otras := NA]
  dataset[foto_mes == 202006, mcomisiones_otras := NA]
  dataset[foto_mes == 202006, cextraccion_autoservicio := NA]
  dataset[foto_mes == 202006, mextraccion_autoservicio := NA]
  dataset[foto_mes == 202006, ccheques_depositados := NA]
  dataset[foto_mes == 202006, mcheques_depositados := NA]
  dataset[foto_mes == 202006, ccheques_emitidos := NA]
  dataset[foto_mes == 202006, mcheques_emitidos := NA]
  dataset[foto_mes == 202006, ccheques_depositados_rechazados := NA]
  dataset[foto_mes == 202006, mcheques_depositados_rechazados := NA]
  dataset[foto_mes == 202006, ccheques_emitidos_rechazados := NA]
  dataset[foto_mes == 202006, mcheques_emitidos_rechazados := NA]
  dataset[foto_mes == 202006, tcallcenter := NA]
  dataset[foto_mes == 202006, ccallcenter_transacciones := NA]
  dataset[foto_mes == 202006, thomebanking := NA]
  dataset[foto_mes == 202006, chomebanking_transacciones := NA]
  dataset[foto_mes == 202006, ccajas_transacciones := NA]
  dataset[foto_mes == 202006, ccajas_consultas := NA]
  dataset[foto_mes == 202006, ccajas_depositos := NA]
  dataset[foto_mes == 202006, ccajas_extracciones := NA]
  dataset[foto_mes == 202006, ccajas_otras := NA]
  dataset[foto_mes == 202006, catm_trx := NA]
  dataset[foto_mes == 202006, matm := NA]
  dataset[foto_mes == 202006, catm_trx_other := NA]
  dataset[foto_mes == 202006, matm_other := NA]
  dataset[foto_mes == 202006, ctrx_quarter := NA]
  dataset[foto_mes == 202006, cmobile_app_trx := NA]
}

Corregir_Frollmean <- function(dataset) {  
  CorregirCampoMes_Frollmean('ctarjeta_visa_debitos_automaticos',c(201904))
  CorregirCampoMes_Frollmean('mttarjeta_visa_debitos_automaticos',c(201904))
  CorregirCampoMes_Frollmean('mrentabilidad',c(201905,201910,202006))
  CorregirCampoMes_Frollmean('mrentabilidad_annual',c(201905,201910,202006))
  CorregirCampoMes_Frollmean('mcomisiones',c(201905,201910,202006))
  CorregirCampoMes_Frollmean('mactivos_margen',c(201905,201910,202006))
  CorregirCampoMes_Frollmean('mpasivos_margen',c(201905,201910,202006))
  CorregirCampoMes_Frollmean('ccomisiones_otras',c(201905,201910,202006))
  CorregirCampoMes_Frollmean('mcomisiones_otras',c(201905,201910,202006))
  CorregirCampoMes_Frollmean('ccajeros_propios_descuentos',c(201910,202002,202006,202009,202010,202102))
  CorregirCampoMes_Frollmean('mcajeros_propios_descuentos',c(201910,202002,202006,202009,202010,202102))
  CorregirCampoMes_Frollmean('ctarjeta_visa_descuentos',c(201910,202002,202006,202009,202010,202102))
  CorregirCampoMes_Frollmean('mtarjeta_visa_descuentos',c(201910,202002,202006,202009,202010,202102))
  CorregirCampoMes_Frollmean('ctarjeta_master_descuentos',c(201910,202002,202006,202009,202010,202102))
  CorregirCampoMes_Frollmean('mtarjeta_master_descuentos',c(201910,202002,202006,202009,202010,202102))
  CorregirCampoMes_Frollmean('chomebanking_transacciones',c(201910,202006))
  CorregirCampoMes_Frollmean('active_quarter',c(202006))
  CorregirCampoMes_Frollmean('internet',c(202006))
  CorregirCampoMes_Frollmean('mcuentas_saldo',c(202006))
  CorregirCampoMes_Frollmean('ctarjeta_debito_transacciones',c(202006))
  CorregirCampoMes_Frollmean('mautoservicio',c(202006))
  CorregirCampoMes_Frollmean('ctarjeta_visa_transacciones',c(202006))
  CorregirCampoMes_Frollmean('mtarjeta_visa_consumo',c(202006))
  CorregirCampoMes_Frollmean('ctarjeta_master_transacciones',c(202006))
  CorregirCampoMes_Frollmean('mtarjeta_master_consumo',c(202006))
  CorregirCampoMes_Frollmean('cextraccion_autoservicio',c(202006))
  CorregirCampoMes_Frollmean('mextraccion_autoservicio',c(202006))
  CorregirCampoMes_Frollmean('ccheques_depositados',c(202006))
  CorregirCampoMes_Frollmean('mcheques_depositados',c(202006))
  CorregirCampoMes_Frollmean('ccheques_emitidos',c(202006))
  CorregirCampoMes_Frollmean('mcheques_emitidos',c(202006))
  CorregirCampoMes_Frollmean('ccheques_depositados_rechazados',c(202006))
  CorregirCampoMes_Frollmean('mcheques_depositados_rechazados',c(202006))
  CorregirCampoMes_Frollmean('ccheques_emitidos_rechazados',c(202006))
  CorregirCampoMes_Frollmean('mcheques_emitidos_rechazados',c(202006))
  CorregirCampoMes_Frollmean('tcallcenter',c(202006))
  CorregirCampoMes_Frollmean('ccallcenter_transacciones',c(202006))
  CorregirCampoMes_Frollmean('thomebanking',c(202006))
  CorregirCampoMes_Frollmean('ccajas_transacciones',c(202006))
  CorregirCampoMes_Frollmean('ccajas_consultas',c(202006))
  CorregirCampoMes_Frollmean('ccajas_depositos',c(202006,202105))
  CorregirCampoMes_Frollmean('ccajas_extracciones',c(202006))
  CorregirCampoMes_Frollmean('ccajas_otras',c(202006))
  CorregirCampoMes_Frollmean('catm_trx',c(202006))
  CorregirCampoMes_Frollmean('matm',c(202006))
  CorregirCampoMes_Frollmean('catm_trx_other',c(202006))
  CorregirCampoMes_Frollmean('matm_other',c(202006))
  #CorregirCampoMes_Frollmean('tmobile_app',c(202006))
  CorregirCampoMes_Frollmean('cmobile_app_trx',c(202006))
}


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
OUTPUT$PARAM <- PARAM
OUTPUT$time$start <- format(Sys.time(), "%Y%m%d %H%M%S")

setwd(PARAM$home)

# cargo el dataset
dataset <- fread(PARAM$dataset)

# tmobile_app se daño a partir de 202010
dataset[, tmobile_app := NULL]


# creo la carpeta donde va el experimento
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

GrabarOutput()
write_yaml(PARAM, file = "parametros.yml") # escribo parametros utilizados

setorder(dataset, numero_de_cliente, foto_mes)

#para varidar guardamos una tabla resumen - Antes de aplicar la funcion
columnasDF <- data.frame(columna1 = dataset$numero_de_cliente,
                         columna2 = dataset$foto_mes,
                         columna3 = dataset$mrentabilidad_annual)

res_columnasDF <- aggregate(columna3 ~ columna1 + columna2, 
                                data = columnasDF, FUN = sum)

setorder(res_columnasDF, columna1)

fwrite(res_columnasDF,
       file = "resumen_columnas.csv",
       logical01 = TRUE,
       sep = ","
)


# corrijo los  < foto_mes, campo >  que fueron pisados con cero
switch(PARAM$metodo,
  "MachineLearning"     = Corregir_MachineLearning(dataset),
  "EstadisticaClasica"  = Corregir_EstadisticaClasica(dataset),
  "Ninguno"             = cat("No se aplica ninguna correccion.\n"),
  "Frollmean"           = Corregir_Frollmean(dataset)
)

# Tabla resumen - desp de corregir el dataset
columnasDF_New <- data.frame(columna1 = dataset$numero_de_cliente, 
                             columna2 = dataset$foto_mes,
                             columna3 = dataset$mrentabilidad_annual)

res_columnasDF_New <- aggregate(columna3 ~ columna1 + columna2, 
                                data = columnasDF_New, FUN = sum)

setorder(res_columnasDF_New, columna1)

fwrite(res_columnasDF_New,
       file = "resumen_columnas_New.csv",
       logical01 = TRUE,
       sep = ","
)

#------------------------------------------------------------------------------

# grabo el dataset
fwrite(dataset,
  file = "dataset.csv.gz",
  logical01 = TRUE,
  sep = ","
)

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
  file = "dataset.campos.txt",
  sep = "\t"
)

#------------------------------------------------------------------------------
OUTPUT$dataset$ncol <- ncol(dataset)
OUTPUT$dataset$nrow <- nrow(dataset)
OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

# dejo la marca final
cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
  file = "zRend.txt",
  append = TRUE
)
