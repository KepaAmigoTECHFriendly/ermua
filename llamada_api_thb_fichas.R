# Funciones app shiny fichas empresas

library(httr)
library(timeDate)
library(anytime)
library(jsonlite)
library(rjson)
library(stringr)
library(curl)
library(shinyjs)

llamada_api_fichas <- function(){

  #===============================================
  #Obtención de keys
  #===============================================
  #Creación url para petición get a Thingsboard API
  #url <- "http://88.99.184.239:8080/api/plugins/telemetry/DEVICE/38ec79a0-41b1-11ea-92db-490439f07a6c/keys/timeseries"
  url <- "http://88.99.184.239:8080/api/plugins/telemetry/DEVICE/6dbe8bb0-768a-11ea-bf56-13868846c781/keys/timeseries"
  
  #Petición API REST THB y extracción JSON
  json_keys <- httr::GET(url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"="Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJrZXBhQHRlY2hmcmllbmRseS5lcyIsInNjb3BlcyI6WyJURU5BTlRfQURNSU4iXSwidXNlcklkIjoiNDIzZWYyMzAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiZmlyc3ROYW1lIjoiS2VwYSIsImxhc3ROYW1lIjoiQW1pZ28iLCJlbmFibGVkIjp0cnVlLCJpc1B1YmxpYyI6ZmFsc2UsInRlbmFudElkIjoiMzQ2ZWRmZDAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiY3VzdG9tZXJJZCI6IjEzODE0MDAwLTFkZDItMTFiMi04MDgwLTgwODA4MDgwODA4MCIsImlzcyI6InRoaW5nc2JvYXJkLmlvIiwiaWF0IjoxNTc5NzA2MTQ1LCJleHAiOjE2MTEyNDIxNDV9.UWBy1Es3yunXwKx7WVO-fxBSYyCynGKsfO7DASM8FtskgJsH4nDDjJrr4Kp_CrhBzetA3aEqABek5Ei4-fh7pw"))

  json_keys_filtrado <- rjson::fromJSON(content(json_keys, as="text"))
  
  claves <- paste(json_keys_filtrado, collapse = ",")
  
  
  #===============================================
  #Obtención valores
  #===============================================
  
  #OBTENCIÓN FECHA ÚLTIMA DE DATOS PARA REALIZAR LLAMADA CON FECHA CORRESPONDIENTE
  
  #Tiempo inicial en formato unix
  #t_inicial_unix <- format((as.numeric(anytime(Sys.Date()-7))*1000)-3600000,scientific = F)
  #Tiempo final en formato unix
  #t_final_unix <- format((as.numeric(anytime(Sys.Date()))*1000)+54000000,scientific = F)
  
  
  t_inicial_unix <- format((as.numeric(anytime("2020-05-11"))*1000)-3600000,scientific = F)
  t_final_unix <- format((as.numeric(anytime("2020-05-11"))*1000)+54000000,scientific = F)
  

  #claves <- curl_escape("Noticia Título,Noticia Descripción,Noticia Categorías,Noticia link")
  claves <- URLencode(claves)
  
  #url2 <- "http://88.99.184.239:8080/api/plugins/telemetry/DEVICE/38ec79a0-41b1-11ea-92db-490439f07a6c/values/timeseries?keys="
  url2 <- "http://88.99.184.239:8080/api/plugins/telemetry/DEVICE/6dbe8bb0-768a-11ea-bf56-13868846c781/values/timeseries?keys="
  #Creación url para petición get a Thingsboard API
  url <- paste(url2,
               claves,
               "&startTs=",
               t_inicial_unix,"&endTs=",t_final_unix,
               "&interval=1&limit=1000&agg=NONE",sep = "")
  
  #Petición API REST THB y extracción JSON
  json_fichas <- httr::GET(url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"="Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJrZXBhQHRlY2hmcmllbmRseS5lcyIsInNjb3BlcyI6WyJURU5BTlRfQURNSU4iXSwidXNlcklkIjoiNDIzZWYyMzAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiZmlyc3ROYW1lIjoiS2VwYSIsImxhc3ROYW1lIjoiQW1pZ28iLCJlbmFibGVkIjp0cnVlLCJpc1B1YmxpYyI6ZmFsc2UsInRlbmFudElkIjoiMzQ2ZWRmZDAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiY3VzdG9tZXJJZCI6IjEzODE0MDAwLTFkZDItMTFiMi04MDgwLTgwODA4MDgwODA4MCIsImlzcyI6InRoaW5nc2JvYXJkLmlvIiwiaWF0IjoxNTc5NzA2MTQ1LCJleHAiOjE2MTEyNDIxNDV9.UWBy1Es3yunXwKx7WVO-fxBSYyCynGKsfO7DASM8FtskgJsH4nDDjJrr4Kp_CrhBzetA3aEqABek5Ei4-fh7pw"))
  json_fichas_filtrado <- rjson::fromJSON(content(json_fichas, as="text"))
  
  # AVISO DE INEXISTENCIA DE DATOS EN EL INTERVALO DE FECHAS SELECCIONADO
  if(length(json_fichas_filtrado) < 1){
    return(0)
  }
  
  #Obtención del máximo timestamp para definir la fecha de exrtracción de datos.
  t_max <- max(as.numeric(unlist(json_fichas_filtrado)[grep("ts", names(unlist(json_fichas_filtrado)))]))
  fecha_max <- anydate(t_max/1000)
  
  # LLAMADA API
  
  #Tiempo inicial en formato unix
  t_inicial_unix <- format((as.numeric(anytime(fecha_max))*1000)-3600000,scientific = F)
  #Tiempo final en formato unix
  t_final_unix <- format((as.numeric(anytime(fecha_max))*1000)+54000000,scientific = F)
  
  
  #claves <- curl_escape("Noticia Título,Noticia Descripción,Noticia Categorías,Noticia link")
  claves <- URLencode(claves)
  
  #url2 <- "http://88.99.184.239:8080/api/plugins/telemetry/DEVICE/38ec79a0-41b1-11ea-92db-490439f07a6c/values/timeseries?keys="
  url2 <- "http://88.99.184.239:8080/api/plugins/telemetry/DEVICE/6dbe8bb0-768a-11ea-bf56-13868846c781/values/timeseries?keys="
  #Creación url para petición get a Thingsboard API
  url <- paste(url2,
               claves,
               "&startTs=",
               t_inicial_unix,"&endTs=",t_final_unix,
               "&interval=1&limit=1000&agg=NONE",sep = "")
  
  #Petición API REST THB y extracción JSON
  json_fichas <- httr::GET(url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"="Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJrZXBhQHRlY2hmcmllbmRseS5lcyIsInNjb3BlcyI6WyJURU5BTlRfQURNSU4iXSwidXNlcklkIjoiNDIzZWYyMzAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiZmlyc3ROYW1lIjoiS2VwYSIsImxhc3ROYW1lIjoiQW1pZ28iLCJlbmFibGVkIjp0cnVlLCJpc1B1YmxpYyI6ZmFsc2UsInRlbmFudElkIjoiMzQ2ZWRmZDAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiY3VzdG9tZXJJZCI6IjEzODE0MDAwLTFkZDItMTFiMi04MDgwLTgwODA4MDgwODA4MCIsImlzcyI6InRoaW5nc2JvYXJkLmlvIiwiaWF0IjoxNTc5NzA2MTQ1LCJleHAiOjE2MTEyNDIxNDV9.UWBy1Es3yunXwKx7WVO-fxBSYyCynGKsfO7DASM8FtskgJsH4nDDjJrr4Kp_CrhBzetA3aEqABek5Ei4-fh7pw"))
  json_fichas_filtrado <- rjson::fromJSON(content(json_fichas, as="text"))
  
  ###########################
  # CREACIÓN DE VARIABLES COMO PARES DE CLAVE VALOR INCLUIDAS EN LA URL
  variables_nombre <- str_split(str_match(curl_unescape(url), "keys=(.*?)&")[2],",")
  variables <- list()
  
  for (i in 1:length(unlist(variables_nombre))){
    variables[[i]] <- unlist(json_fichas_filtrado[[variables_nombre[[1]][i]]])
    n <- length(unlist(variables[i]))
    ts <- format(str_trim(unname(variables[[i]][seq(n) %% 2 != 0])),scientific = F)
    variables[[i]] <- str_trim(unname(variables[[i]][seq(n) %% 2 == 0]))
    names(variables[[i]]) <- ts
  }
  
  variables <- setNames(variables,unlist(variables_nombre))
  
  df <- data.frame(variables, stringsAsFactors = F)
  df[df==''] <- "-"
  
  #ref_nombres_en <- c("link","var_emp","var_url","var_prv","var_lnk","var_cap","var_n_empleados","var_facturacion","var_fo_juridica",
  #                    "var_sector_empr","var_direccion","var_tamanyo","var_constitucion","var_actividad","var_ultimo_depo","var_capital_soc",
  #                    "var_cnae","var_empresite_fax","var_empresite_lat","var_empresite_lon","var_empresite_eml","var_empresite_web",
  #                    "var_empresite_obj","var_empresite_vts","var_bing_url","var_ultimo_camb","var_longitud","var_latitud","var_empresite_cif",
  #                      "var_empresite_tel","var_mun","var_telefono","var_empresite_dom")
  
  #names(ref_nombres_en) <- seq(1,33)
  
  # Matching de nombres columnas dataframe datos con ref_nombres_en
  #matching_df_ref_nombres <- c()
  #for(i in 1:length(names(df))){
  #  matching_df_ref_nombres <- c(matching_df_ref_nombres, grep(paste("^",names(df)[i],"$",sep=""),ref_nombres_en))
  #}
  
  #ref_nombres_es <-  c("Link", "Empresa", "URL", "Provincia", "link", "Actividad específica", "Número de empleados", "Facturación", "Forma jurídica", "Sector", 
  #                     "Dirección", "Tamaño", "Constitución", "Actividad", "Último depósito", "Capital social", "CNAE", "FAX",
  #                     "Lat", "Long", "E-mail", "Web", "Objeto social", "Rango de ventas", "URL Bing", "Última cambio", "Longitud",
  #                    "Latitud", "CIF", "Teléfono","Municipio", "Teléfono2", "Domicilio social")
  #names(ref_nombres_es) <- seq(1,33)
  
  #nombres_matching <- ref_nombres_es[matching_df_ref_nombres]
  #names(df) <- nombres_matching
  
  #orden <- subset(df, select = c("Empresa", "Forma jurídica", "Sector", "Actividad", "Actividad específica", "Facturación", "Número de empleados",
  #                               "Capital social", "CIF", "CNAE"))
  
  #df <- df[ , -which(names(df) %in% c("Empresa", "Forma jurídica", "Sector", "Actividad", "Actividad específica", "Facturación", "Número de empleados",
  #                                    "Capital social", "CIF", "CNAE"))]
  
  #df <- data.frame(orden,df)
  
  df <- df[, c(4,2,1,8,10,3,5,9,6,7)]
  
  names(df) <- c("Empresa", "Dirección", "CNAE", "Objeto social", "Ventas", "Empleados", "Fecha creación", "teléfono", "Latitud", "Longitud")
  
  return(df)
  
}