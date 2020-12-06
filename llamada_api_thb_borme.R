# Funciones app shiny Borme

# IMPORTANTE: ES NECESARIO GUARDARLO EN FORMATO UTF-8 

library(httr)
library(timeDate)
library(anytime)
library(jsonlite)
library(stringr)

#Librerías para el mapa
library(dplyr)
library(ggplot2)
library(rjson)
library(jsonlite)
library(leaflet)
library(RCurl)


#Función llamada api atributos activo
llamada_atributos <- function(){
  #Petición API GET atributos activo
  #Creación url para petición get a Thingsboard API atributos activo
  url_activo_ermua <- "http://88.99.184.239:8080/api/plugins/telemetry/ASSET/5916fe20-3cf3-11ea-8d11-75ab75e08f4f/values/attributes"
  
  #Petición API REST THB y extracción JSON
  json_activo <- GET(url_activo_ermua, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"="Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJrZXBhQHRlY2hmcmllbmRseS5lcyIsInNjb3BlcyI6WyJURU5BTlRfQURNSU4iXSwidXNlcklkIjoiNDIzZWYyMzAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiZmlyc3ROYW1lIjoiS2VwYSIsImxhc3ROYW1lIjoiQW1pZ28iLCJlbmFibGVkIjp0cnVlLCJpc1B1YmxpYyI6ZmFsc2UsInRlbmFudElkIjoiMzQ2ZWRmZDAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiY3VzdG9tZXJJZCI6IjEzODE0MDAwLTFkZDItMTFiMi04MDgwLTgwODA4MDgwODA4MCIsImlzcyI6InRoaW5nc2JvYXJkLmlvIiwiaWF0IjoxNTc5NzA2MTQ1LCJleHAiOjE2MTEyNDIxNDV9.UWBy1Es3yunXwKx7WVO-fxBSYyCynGKsfO7DASM8FtskgJsH4nDDjJrr4Kp_CrhBzetA3aEqABek5Ei4-fh7pw"))
  
  df_atributos <- jsonlite::fromJSON(content(json_activo, as="text"))
  
  return(df_atributos)
}

# Función que realiza la llamada a la API Thingsboard y devuelve un data frame con los datos solcitados estructurados
llamada_api <- function(fecha_inicial, fecha_final){
  
  #Datos
  fecha_inicial <- as.POSIXct(fecha_inicial)
  fecha_final <- as.POSIXct(fecha_final)
  
  #Tiempo inicial en formato unix
  t_inicial_unix <- format((as.numeric(anytime(fecha_inicial))*1000)-3600000,scientific = F)  #Se resta 1h para ajustar la hora a las 12am.
  
  #Tiempo final en formato unix
  t_final_unix <- format((as.numeric(anytime(fecha_final))*1000)+54000000,scientific = F)  #Añade 15h al unix timestamp para asegurar la captura de los datos.
  
  #url <- str_squish(paste("http://88.99.184.239:8080/api/plugins/telemetry/DEVICE/ba14afe0-174d-11ea-8d11-75ab75e08f4f/values/timeseries?keys=EMPRESA,Latitud,Longitud",variables_dato,
   #            "&startTs=",t_inicial_unix,"&endTs=",t_final_unix,
    #           "&interval=1&limit=1000000&agg=NONE",sep = ""))
  
  keys <- URLencode("EMPRESA,Fusion_sociedades_absorbidas,Modificaciones_estatutarias,Cambio_denominacion_social,Cambio_domicilio_social,Cambio_objeto_social,Ceses_liquiSoli,Ceses_apoderado,Ceses_adminUnico,Ceses_liquidador,Ceses_liquidador_mancom,Ceses_adminSolid,Ceses_adminMan,Ceses_socprof,Ceses_depositorio,Ceses_entidDeposit,Ceses_entdPromo,Ceses_consejero,Ceses_vicepresidente,Ceses_presidente,Ceses_secretario,Nombr_liquiSoli,Nombr_apoderado,Nombr_adminUnico,Nombr_liquidador,Nombr_liquidador_mancom,Nombr_adminSolid,Nombr_socprof,Nombr_auditor,Nombr_adminMan,Nombr_entidDeposit,Nombr_entdPromo,Nombr_consejero,Nombr_vicepresidente,Nombr_presidente,Nombr_secretario,Ampl_Cap_suscrito,Ampl_Cap_resultante_suscrito,Ampl_Cap_desembolsado,Ampl_Cap_resultante_desembolsado,Ampl_Cap_capital,declaracion_unipersonalidad_socio_unico,Reduc_Cap_importe_reduccion,Reduc_Cap_resultante_suscrito,Reelecciones_adminUnico,Reelecciones_auditor,Reelecciones_auditor_suplente,Revocaciones_auditor,Revocaciones_apoderado,Revocaciones_apoderadoMAn,Revocaciones_apoderadoSol,Sit_conc_procedimiento,Sit_conc_firme,Sit_conc_fecha_resolucion,Sit_conc_proceso,Sit_conc_juzgado,Sit_conc_juez,Sit_conc_resoluciones,Disolucion,Extincion,Const_comienzo_operaciones,Const_objeto_social,Const_domicilio,Const_capital,Otros_conceptos,Datos_registrales,Latitud,Longitud,Municipio empresa,Distancia respecto municipio km,Provincia Borme")
  
  url <- str_squish(paste("http://88.99.184.239:8080/api/plugins/telemetry/DEVICE/7b921270-77e8-11ea-bf56-13868846c781/values/timeseries?keys=",
                          keys,
                          "&startTs=",t_inicial_unix,"&endTs=",t_final_unix,
                          "&interval=1&limit=1000000&agg=NONE",sep = ""))
  
  
  #Petición API REST THB y extracción JSON
  json <- GET(url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"="Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJrZXBhQHRlY2hmcmllbmRseS5lcyIsInNjb3BlcyI6WyJURU5BTlRfQURNSU4iXSwidXNlcklkIjoiNDIzZWYyMzAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiZmlyc3ROYW1lIjoiS2VwYSIsImxhc3ROYW1lIjoiQW1pZ28iLCJlbmFibGVkIjp0cnVlLCJpc1B1YmxpYyI6ZmFsc2UsInRlbmFudElkIjoiMzQ2ZWRmZDAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiY3VzdG9tZXJJZCI6IjEzODE0MDAwLTFkZDItMTFiMi04MDgwLTgwODA4MDgwODA4MCIsImlzcyI6InRoaW5nc2JvYXJkLmlvIiwiaWF0IjoxNTc5NzA2MTQ1LCJleHAiOjE2MTEyNDIxNDV9.UWBy1Es3yunXwKx7WVO-fxBSYyCynGKsfO7DASM8FtskgJsH4nDDjJrr4Kp_CrhBzetA3aEqABek5Ei4-fh7pw"))
  
  json3 <- rjson::fromJSON(content(json, as="text"))
  
  # AVISO DE INEXISTENCIA DE DATOS EN EL INTERVALO DE FECHAS SELECCIONADO
  if(length(json3) < 1 | length(json3) < 2){
    return(0)
  }
  
  ###########################
  # CREACIÓN DE VARIABLES COMO PARES DE CLAVE VALOR INCLUIDAS EN LA URL
  #variables_nombre <- str_split(gsub("%20"," ",str_match(url, "keys=(.*?)&")[2]),",")
  variables_nombre <- names(json3)
  variables <- list()
  
  for (i in 1:length(unlist(variables_nombre))){
    #variables[[i]] <- unlist(json3[[variables_nombre[[1]][i]]])
    variables[[i]] <- unlist(json3[[variables_nombre[i]]])
    n <- length(unlist(variables[i]))
    ts <- format(str_trim(unname(variables[[i]][seq(n) %% 2 != 0])),scientific = F)
    variables[[i]] <- str_trim(unname(variables[[i]][seq(n) %% 2 == 0]))
    names(variables[[i]]) <- ts
  }
  
  variables <- setNames(variables,unlist(variables_nombre))
  
  ##########################
  #CREACIÓN DE DATA FRAME DE DATOS COMO FUENTE DE DATOS
  #Generación del id de los registros del DF con el timestamp
  
  #Inicialización DF
  #empresa <- variables[["EMPRESA"]]
  #df <- as.data.frame(empresa)
  #Inserción automática de variables en DF en base al ts
  #for(i in 2:length(names(variables))){
  #  df[,i] <- variables[[i]][match(rownames(df), names(variables[[i]]))]
  #}
  #colnames(df) <- unlist(variables_nombre)
  
  df <- data.frame(variables, stringsAsFactors = F)
  df[df==''] <- "-"
  
  #Inserción fecha de Borme en dataframe
  #df$Fecha <- format(as.POSIXct(as.numeric(row.names(df))/1000, origin="1970-01-01"), format="%Y-%m-%d")
  df$Fecha <- as.Date(as.POSIXct(as.numeric(row.names(df))/1000, origin="1970-01-01"))+1
  
  #Llamada a función api atributos
  #atributos_df <- llamada_atributos()
  #municipio_ref <- atributos_df$value[atributos_df$key == "Municipio"]
  #radio_ref <- as.numeric(atributos_df$value[atributos_df$key == "Radio"])
  
  #df$Distancia.respecto.municipio.km <- format(round(as.numeric(df$Distancia.respecto.municipio.km), 3), nsmall = 3)
  #df$Distancia.respecto.municipio.km[grepl("NA",df$Distancia.respecto.municipio.km)] <- "-"
  
  #Cálculo distancias
  #for(i in 1:nrow(df)){
    #df$Distancia[i] <- distm(x = coor_referencia, y = c(as.numeric(df$Longitud[i]), as.numeric(df$Latitud[i])))/1000
  #  if(!grepl("-",df$Distancia.respecto.municipio.km[i])){
  #    if(as.numeric(df$Distancia.respecto.municipio.km[i]) < radio_ref){
  #      df$Dentro[i] <- "Dentro"
  #    }else{
  #      df$Dentro[i] <- "Fuera"
  #    }
  #    }else{
  #    df$Dentro[i] <- NA
  #    }
  #}
  
  return(df)
}
