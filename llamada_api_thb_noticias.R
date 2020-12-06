# Funciones app shiny noticias

# IMPORTANTE: ES NECESARIO GUARDARLO EN FORMATO ISO 8859 PARA PARSEAR ADECUADAMENTE LA URL

library(httr)
library(timeDate)
library(anytime)
library(jsonlite)
library(rjson)
library(stringr)
library(curl)
library(shinyjs)

llamada_api_noticias <- function(fecha_inicial, fecha_final){
  
  #Datos
  fecha_inicial <- as.POSIXct(fecha_inicial)
  fecha_final <- as.POSIXct(fecha_final)
  
  #Tiempo inicial en formato unix
  t_inicial_unix <- format((as.numeric(anytime(fecha_inicial))*1000),scientific = F)
  
  #Tiempo final en formato unix
  t_final_unix <- format((as.numeric(anytime(fecha_final))*1000)+54000000,scientific = F)
  
  # IMPORTANTE: en caso de utilizar la función curl_escape falla el encoding al subirlo a shiny.io
  
  #claves <- curl_escape("Noticia Título,Noticia Descripción,Noticia Categorías,Noticia link")
  claves <- "Noticia%20T%C3%ADtulo,Noticia%20Descripci%C3%B3n,Noticia%20Categor%C3%ADas,Noticia%20link,Peri%C3%B3dico%20T%C3%ADtulo"
  
  #Creación url para petición get a Thingsboard API
  url <- paste("http://88.99.184.239:8080/api/plugins/telemetry/DEVICE/20693a40-1749-11ea-8d11-75ab75e08f4f/values/timeseries?keys=",
               claves,
               "&startTs=",
               t_inicial_unix,"&endTs=",t_final_unix,
               "&interval=1&limit=1000&agg=NONE",sep = "")
  
  #Petición API REST THB y extracción JSON
  json_noticias <- httr::GET(url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"="Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJrZXBhQHRlY2hmcmllbmRseS5lcyIsInNjb3BlcyI6WyJURU5BTlRfQURNSU4iXSwidXNlcklkIjoiNDIzZWYyMzAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiZmlyc3ROYW1lIjoiS2VwYSIsImxhc3ROYW1lIjoiQW1pZ28iLCJlbmFibGVkIjp0cnVlLCJpc1B1YmxpYyI6ZmFsc2UsInRlbmFudElkIjoiMzQ2ZWRmZDAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiY3VzdG9tZXJJZCI6IjEzODE0MDAwLTFkZDItMTFiMi04MDgwLTgwODA4MDgwODA4MCIsImlzcyI6InRoaW5nc2JvYXJkLmlvIiwiaWF0IjoxNTc5NzA2MTQ1LCJleHAiOjE2MTEyNDIxNDV9.UWBy1Es3yunXwKx7WVO-fxBSYyCynGKsfO7DASM8FtskgJsH4nDDjJrr4Kp_CrhBzetA3aEqABek5Ei4-fh7pw"))
  
  json_noticias_filtrado <- rjson::fromJSON(content(json_noticias, as="text"))
  
  
  # AVISO DE INEXISTENCIA DE DATOS EN EL INTERVALO DE FECHAS SELECCIONADO
  if(length(json_noticias_filtrado) < 1){
    return(0)
  }
  
  ###########################
  # CREACIÓN DE VARIABLES COMO PARES DE CLAVE VALOR INCLUIDAS EN LA URL
  #variables_nombre <- str_split(str_match(curl_unescape(url), "keys=(.*?)&")[2],",")
  variables_nombre <- names(json_noticias_filtrado)
  variables <- list()
  
  for (i in 1:length(variables_nombre)){
    variables[[i]] <- unlist(json_noticias_filtrado[[variables_nombre[i]]])

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
  noticia_titulo <- variables[[1]]
  df <- as.data.frame(noticia_titulo)
  #Inserción automática de variables en DF en base al ts
  for(i in 2:length(names(variables))){
    df[,i] <- variables[[i]][match(rownames(df), names(variables[[i]]))]
  }
  colnames(df) <- unlist(variables_nombre)
  
  #Inserción fecha de Borme en dataframe
  df$Fecha <- format(as.POSIXct(as.numeric(row.names(df))/1000, origin="1970-01-01"), format="%Y-%m-%d")
  
  df[is.na(df)] <- "-"
  
  return(df)
  
}
