# Llamada api thb contrataciones del estado

library(httr)
library(timeDate)
library(anytime)
library(jsonlite)
library(rjson)
library(stringr)
library(curl)
library(shinyjs)

llamada_api_contratacionesEstado <- function(fecha_inicial, fecha_final){
  
  #Datos
  fecha_inicial <- as.POSIXct(fecha_inicial)
  fecha_final <- as.POSIXct(fecha_final)
  
  #Tiempo inicial en formato unix
  t_inicial_unix <- format((as.numeric(anytime(fecha_inicial))*1000),scientific = F)
  
  #Tiempo final en formato unix
  t_final_unix <- format((as.numeric(anytime(fecha_final))*1000)+84924000,scientific = F)
  
  # IMPORTANTE: en caso de utilizar la función curl_escape falla el encoding al subirlo a shiny.io
  
  #claves <- curl_escape("Expediente,Objeto del contrato,Tipo de Contrato:,Código CPV,Presentación,Órgano de Contratación,Estado de la Licitación,Resultado,Presupuesto base de licitación sin impuestos,Valor estimado del contrato:,Lugar de Ejecución,Procedimiento de contratación,Adjudicatario,Nº de Licitadores Presentados,Importe de Adjudicación,Enlace licitación,Fecha fin de presentación de oferta,Documento: Formalización,Documento: Anuncio de Licitación,Documento: Pliego,Documento: Adjudicación")
  
  claves <- "Expediente%2CObjeto%20del%20contrato%2CTipo%20de%20Contrato%3A%2CC%C3%B3digo%20CPV%2CPresentaci%C3%B3n%2C%C3%93rgano%20de%20Contrataci%C3%B3n%2CEstado%20de%20la%20Licitaci%C3%B3n%2CResultado%2CPresupuesto%20base%20de%20licitaci%C3%B3n%20sin%20impuestos%2CValor%20estimado%20del%20contrato%3A%2CLugar%20de%20Ejecuci%C3%B3n%2CProcedimiento%20de%20contrataci%C3%B3n%2CAdjudicatario%2CN%C2%BA%20de%20Licitadores%20Presentados%2CImporte%20de%20Adjudicaci%C3%B3n%2CEnlace%20licitaci%C3%B3n%2CFecha%20fin%20de%20presentaci%C3%B3n%20de%20oferta%2CDocumento%3A%20Formalizaci%C3%B3n%2CDocumento%3A%20Anuncio%20de%20Licitaci%C3%B3n%2CDocumento%3A%20Pliego%2CDocumento%3A%20Adjudicaci%C3%B3n"
  
  #Creación url para petición get a Thingsboard API
  url <- paste("http://88.99.184.239:8080/api/plugins/telemetry/DEVICE/54615300-5ed9-11ea-bf56-13868846c781/values/timeseries?keys=",
               claves,
               "&startTs=",
               t_inicial_unix,"&endTs=",t_final_unix,
               "&interval=1&limit=1000&agg=NONE",sep = "")
  
  #Petición API REST THB y extracción JSON
  json_contrataciones <- httr::GET(url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"="Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJrZXBhQHRlY2hmcmllbmRseS5lcyIsInNjb3BlcyI6WyJURU5BTlRfQURNSU4iXSwidXNlcklkIjoiNDIzZWYyMzAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiZmlyc3ROYW1lIjoiS2VwYSIsImxhc3ROYW1lIjoiQW1pZ28iLCJlbmFibGVkIjp0cnVlLCJpc1B1YmxpYyI6ZmFsc2UsInRlbmFudElkIjoiMzQ2ZWRmZDAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiY3VzdG9tZXJJZCI6IjEzODE0MDAwLTFkZDItMTFiMi04MDgwLTgwODA4MDgwODA4MCIsImlzcyI6InRoaW5nc2JvYXJkLmlvIiwiaWF0IjoxNTc5NzA2MTQ1LCJleHAiOjE2MTEyNDIxNDV9.UWBy1Es3yunXwKx7WVO-fxBSYyCynGKsfO7DASM8FtskgJsH4nDDjJrr4Kp_CrhBzetA3aEqABek5Ei4-fh7pw"))
  
  json_contrataciones_filtrado <- rjson::fromJSON(content(json_contrataciones, as="text"))
  
  # AVISO DE INEXISTENCIA DE DATOS EN EL INTERVALO DE FECHAS SELECCIONADO
  if(length(json_contrataciones_filtrado) < 1){
    return(0)
  }
  
  ###########################
  # CREACIÓN DE VARIABLES COMO PARES DE CLAVE VALOR INCLUIDAS EN LA URL
  #variables_nombre <- str_split(str_match(curl_unescape(url), "keys=(.*?)&")[2],",")
  variables_nombre <- names(json_contrataciones_filtrado)
  variables <- list()
  
  for (i in 1:length(variables_nombre)){
    variables[[i]] <- unlist(json_contrataciones_filtrado[[variables_nombre[i]]])
    
    n <- length(unlist(variables[i]))
    ts <- format(str_trim(unname(variables[[i]][seq(n) %% 2 != 0])),scientific = F)
    variables[[i]] <- str_trim(unname(variables[[i]][seq(n) %% 2 == 0]))
    names(variables[[i]]) <- ts
  }
  
  variables <- setNames(variables,unlist(variables_nombre))
  
  df <- data.frame(variables, stringsAsFactors = F)
  df[df==''] <- "-"
  
  # Subset solo publicadas
  #df <- df[df[ ,7] == "Publicada", ]
  
  # Eliminación duplicados
  df <- unique(df)
  
  names(df)[names(df) == "Presentación"] <- "Fecha"
  
  # AVISO DE INEXISTENCIA DE DATOS EN EL INTERVALO DE FECHAS SELECCIONADO
  if(nrow(df) < 1){
    return(0)
  }
  
  return(df)
  
}