# Funciones app shiny análisis Twitter.

library(httr)
library(timeDate)
library(anytime)
library(jsonlite)
library(rjson)
library(stringr)
library(curl)
library(shinyjs)


llamada_api_twitter <- function(fecha_inicial = (Sys.Date()), fecha_final = Sys.Date()){
  
  #Tiempo inicial en formato unix
  t_inicial_unix <- format((as.numeric(anytime(fecha_inicial))*1000)-3600000,scientific = F)
  
  #Tiempo final en formato unix
  t_final_unix <- format((as.numeric(anytime(fecha_final))*1000)+54000000,scientific = F)
  
  #===============================================
  #Obtención de keys
  #===============================================
  #Creación url para petición get a Thingsboard API
  url <- "http://88.99.184.239:8080/api/plugins/telemetry/DEVICE/69126d40-41db-11ea-92db-490439f07a6c/keys/timeseries"
  
  #Petición API REST THB y extracción JSON
  json_keys <- httr::GET(url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"="Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJrZXBhQHRlY2hmcmllbmRseS5lcyIsInNjb3BlcyI6WyJURU5BTlRfQURNSU4iXSwidXNlcklkIjoiNDIzZWYyMzAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiZmlyc3ROYW1lIjoiS2VwYSIsImxhc3ROYW1lIjoiQW1pZ28iLCJlbmFibGVkIjp0cnVlLCJpc1B1YmxpYyI6ZmFsc2UsInRlbmFudElkIjoiMzQ2ZWRmZDAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiY3VzdG9tZXJJZCI6IjEzODE0MDAwLTFkZDItMTFiMi04MDgwLTgwODA4MDgwODA4MCIsImlzcyI6InRoaW5nc2JvYXJkLmlvIiwiaWF0IjoxNTc5NzA2MTQ1LCJleHAiOjE2MTEyNDIxNDV9.UWBy1Es3yunXwKx7WVO-fxBSYyCynGKsfO7DASM8FtskgJsH4nDDjJrr4Kp_CrhBzetA3aEqABek5Ei4-fh7pw"))
  
  json_keys_filtrado <- rjson::fromJSON(content(json_keys, as="text"))
  
  claves <- paste(json_keys_filtrado, collapse = ",")
  
  #===============================================
  #Obtención valores
  #===============================================
  
  claves <- URLencode(claves)
  
  #Creación url para petición get a Thingsboard API
  url <- paste("http://88.99.184.239:8080/api/plugins/telemetry/DEVICE/69126d40-41db-11ea-92db-490439f07a6c/values/timeseries?keys=",
               claves,
               "&startTs=",
               t_inicial_unix,"&endTs=",t_final_unix,
               "&interval=1&limit=1000&agg=NONE",sep = "")
  
  #Petición API REST THB y extracción JSON
  json_twitter <- httr::GET(url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"="Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJrZXBhQHRlY2hmcmllbmRseS5lcyIsInNjb3BlcyI6WyJURU5BTlRfQURNSU4iXSwidXNlcklkIjoiNDIzZWYyMzAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiZmlyc3ROYW1lIjoiS2VwYSIsImxhc3ROYW1lIjoiQW1pZ28iLCJlbmFibGVkIjp0cnVlLCJpc1B1YmxpYyI6ZmFsc2UsInRlbmFudElkIjoiMzQ2ZWRmZDAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiY3VzdG9tZXJJZCI6IjEzODE0MDAwLTFkZDItMTFiMi04MDgwLTgwODA4MDgwODA4MCIsImlzcyI6InRoaW5nc2JvYXJkLmlvIiwiaWF0IjoxNTc5NzA2MTQ1LCJleHAiOjE2MTEyNDIxNDV9.UWBy1Es3yunXwKx7WVO-fxBSYyCynGKsfO7DASM8FtskgJsH4nDDjJrr4Kp_CrhBzetA3aEqABek5Ei4-fh7pw"))
  
  json_twitter_filtrado <- rjson::fromJSON(content(json_twitter, as="text"))
  
  # AVISO DE INEXISTENCIA DE DATOS EN EL INTERVALO DE FECHAS SELECCIONADO
  if(length(json_twitter_filtrado) < 1){
    return(0)
  }
  
  ###########################
  # CREACIÓN DE VARIABLES COMO PARES DE CLAVE VALOR INCLUIDAS EN LA URL
  variables_nombre <- str_split(str_match(curl_unescape(url), "keys=(.*?)&")[2],",")
  variables <- list()
  
  for (i in 1:length(unlist(variables_nombre))){
    variables[[i]] <- unlist(json_twitter_filtrado[[variables_nombre[[1]][i]]])
    n <- length(unlist(variables[i]))
    ts <- format(str_trim(unname(variables[[i]][seq(n) %% 2 != 0])),scientific = F)
    variables[[i]] <- str_trim(unname(variables[[i]][seq(n) %% 2 == 0]))
    names(variables[[i]]) <- ts
  }
  
  variables <- setNames(variables,unlist(variables_nombre))
  
  df <- data.frame(variables, stringsAsFactors = F)
  df[df==''] <- "-"
  
  # Referencia nombres (enumeración nombres columnas como vector nombrado para matching posterior con traducción)
  ref_nombres_en <- c("account_created_at.x", "account_created_at.y", "account_lang.x", "account_lang.y", "bbox_coords", "cadena_de_busqueda.x",
                      "cadena_de_busqueda.y", "coords_coords" , "country", "country_code", "created_at", "description.x",          
                      "description.y", "display_text_width", "ext_media_expanded_url", "ext_media_t.co", "ext_media_type", "ext_media_url",          
                      "favorite_count", "favourites_count", "followers_count", "friends_count", "geo_coords", "hashtags",      
                      "is_quote", "is_retweet", "lang", "listed_count", "location.x", "location.y",    
                      "media_expanded_url", "media_t.co", "media_type", "media_url",  "mentions_screen_name", "mentions_user_id",     
                      "name.x", "name.y", "place_full_name", "place_name", "place_type", "place_url",     
                      "profile_background_url", "profile_banner_url", "profile_expanded_url.x", "profile_expanded_url.y", "profile_image_url.x", "profile_image_url.y",    
                      "profile_url", "protected", "quote_count", "quoted_created_at", "quoted_description", "quoted_favorite_count",  
                      "quoted_followers_count", "quoted_friends_count", "quoted_location", "quoted_name", "quoted_retweet_count", "quoted_screen_name",     
                      "quoted_source", "quoted_statuses_count", "quoted_status_id", "quoted_text", "quoted_user_id", "quoted_verified",        
                      "reply_count", "reply_to_screen_name", "reply_to_status_id", "reply_to_user_id", "retweet_count", "retweet_created_at",    
                      "retweet_description", "retweet_favorite_count", "retweet_followers_count", "retweet_friends_count", "retweet_location", "retweet_name",  
                      "retweet_retweet_count", "retweet_screen_name", "retweet_source", "retweet_statuses_count", "retweet_status_id", "retweet_text",  
                      "retweet_user_id", "retweet_verified", "screen_name.x", "screen_name.y", "source", "statuses_count",         
                      "status_id", "status_url", "symbols", "text", "url", "urls_expanded_url", "urls_t.co", "urls_url", "user_id", "usuario_gps_latitud", "usuario_gps_longitud", "verified")
  
  names(ref_nombres_en) <- seq(1,102)
  
  # Matching de nombres columnas dataframe datos twitter con ref_nombres_en
  matching_df_ref_nombres <- c()
  for(i in 1:length(names(df))){
    matching_df_ref_nombres <- c(matching_df_ref_nombres, grep(paste("^",names(df)[i],"$",sep=""),ref_nombres_en))
  }
  
  ref_nombres_es <-  c("Cuenta creada en x", "Cuenta creada en y", "Idioma cuenta x", "Idioma cuenta y", "Coordenadas bbox", "Cadena de búsqueda x", "Cadena de búsqueda y",
                      "Coordenadas coor", "País", "Código país", "Creado en", "Descripción x", "Descripción y", "Tamaño de texto", "URL medio exterior expandido",
                      "ext_media_t.co", "Tipo medio exterior", "URL medio exterior", "Contador favorito", "Contador favoritos 2", "Contador seguidores", "Contador seguidos", "Coordenadas",
                      "Hashtags", "Es cita", "Es un retweet", "Idioma", "Contador listado", "Localización x", "Localización y", "URL medio expandido", "media_t.co",
                      "Tipo de medio", "URL medio", "Cuentas mecionadas", "ID menciones usuario", "Nombre cuenta x", "Nombre cuenta y", "Nombre completo lugar", "Nombre lugar",
                      "Tipo lugar", "URL lugar", "URL fondo perfil", "Banner perfil", "URL perfil expandido x", "URL perfil expandido y", "Imagen perfil y", "Imagen perfil x",
                      "URL perfil", "Protegido", "Contador de citados", "Citado creado en", "Descripción citado", "Contador me gustas en citado", "Contador seguidores citado",
                      "Contador seguidos citados", "Localización citado", "Nombre citado", "Contador retweets citado", "Nombre pantalla citado", "Fuente citado", "ID status citados", "Contador status citados",
                      "Texto citado", "ID usuario citado", "Citado verificado", "Contador respuestas", "Nombre respuesta a pantalla", "ID respuesta status", "ID respuesta a usuario",
                      "Contador retweets", "Retweet creado en", "Descripición retweet", "Contador favoritos", "Contador seguidores cuenta retweeteada", "Contador seguidos cuenta retweeteada",
                      "Localización retweet", "Nombre retweet", "Contador retweets retweet", "Nombre pantalla retweet", "Fuente retweet", "ID status retweet", "Contador statuses retweet",
                      "Texto retweet", "ID usuario retweet", "Retweet verificado", "Nombre pantalla x", "Nombre pantalla y", "Fuente", "Contador status", "ID status", "Link tweet", 
                      "Símbolos", "Texto", "URL", "URL urls expandido", "urls_t.co", "URL urls", "ID usuario", "Latitud usuario", "Longitud usuario", "Verificado")
  
  ref_nombres_es <- str_replace_all(ref_nombres_es,"[.]", " ")
  
  names(ref_nombres_es) <- seq(1,102)
  
  nombres_matching <- ref_nombres_es[matching_df_ref_nombres]
  
  names(df) <- nombres_matching
  
  orden <- subset(df, select = c("Texto", "Hashtags", "Es un retweet", "Creado en", "Contador favoritos", "Contador retweets", "Tamaño de texto", "Cuentas mecionadas", "Localización x", "Link tweet",
                                 "Nombre cuenta x", "Imagen perfil y", "Banner perfil", "Descripción y", "Contador seguidores", "Contador seguidos", "Cuenta creada en y","Cadena de búsqueda x", "Cadena de búsqueda y"))
  
  df <- df[ , -which(names(df) %in% c("Texto", "Hashtags", "Es un retweet", "Creado en", "Contador favoritos", "Contador retweets", "Tamaño de texto", "Cuentas mecionadas", "Localización x", "Link tweet",
                                      "Nombre cuenta x", "Imagen perfil y", "Banner perfil", "Descripción y", "Contador seguidores", "Contador seguidos", "Cuenta creada en y", "Cadena de búsqueda x", "Cadena de búsqueda y"))]
  df <- data.frame(orden,df)
  names(df) <- str_replace_all(names(df),"[.]", " ")
  
  df <- df[ , -which(names(df) %in% c("Descripción x", "Idioma cuenta y", "Imagen perfil x", "Link", "URL", "URL perfil expandido x",
                                      "urls_t co", "URL medio exterior expandido", "Código país", "Cuenta creada en y", "URL fondo perfil", "ext_media_t.co", "Nombre cuenta y",
                                      "Localización y", "URL lugar", "Coordenadas bbox", "media_t co", "Contador favorito", "Contador favoritos 2", "ext_media_t co", "media_t co"))]
  
  return(df)
}


llamada_api_twitter_cuentas <- function(fecha_inicial, fecha_final){
  
  #Tiempo inicial en formato unix
  t_inicial_unix <- format((as.numeric(anytime(fecha_inicial))*1000)-3600000,scientific = F)
  
  #Tiempo final en formato unix
  t_final_unix <- format((as.numeric(anytime(fecha_final))*1000)+54000000,scientific = F)
  
  #===============================================
  #Obtención de keys
  #===============================================
  #Creación url para petición get a Thingsboard API
  url <- "http://88.99.184.239:8080/api/plugins/telemetry/DEVICE/1ebb8610-4824-11ea-9b68-4f4bba077da2/keys/timeseries"
  
  #Petición API REST THB y extracción JSON
  json_keys <- httr::GET(url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"="Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJrZXBhQHRlY2hmcmllbmRseS5lcyIsInNjb3BlcyI6WyJURU5BTlRfQURNSU4iXSwidXNlcklkIjoiNDIzZWYyMzAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiZmlyc3ROYW1lIjoiS2VwYSIsImxhc3ROYW1lIjoiQW1pZ28iLCJlbmFibGVkIjp0cnVlLCJpc1B1YmxpYyI6ZmFsc2UsInRlbmFudElkIjoiMzQ2ZWRmZDAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiY3VzdG9tZXJJZCI6IjEzODE0MDAwLTFkZDItMTFiMi04MDgwLTgwODA4MDgwODA4MCIsImlzcyI6InRoaW5nc2JvYXJkLmlvIiwiaWF0IjoxNTc5NzA2MTQ1LCJleHAiOjE2MTEyNDIxNDV9.UWBy1Es3yunXwKx7WVO-fxBSYyCynGKsfO7DASM8FtskgJsH4nDDjJrr4Kp_CrhBzetA3aEqABek5Ei4-fh7pw"))
  
  json_keys_filtrado <- rjson::fromJSON(content(json_keys, as="text"))
  
  claves <- paste(json_keys_filtrado, collapse = ",")
  
  #===============================================
  #Obtención valores
  #===============================================
  
  claves <- URLencode(claves)
  
  #Creación url para petición get a Thingsboard API
  url <- paste("http://88.99.184.239:8080/api/plugins/telemetry/DEVICE/1ebb8610-4824-11ea-9b68-4f4bba077da2/values/timeseries?keys=",
               claves,
               "&startTs=",
               t_inicial_unix,"&endTs=",t_final_unix,
               "&interval=1&limit=1000&agg=NONE",sep = "")
  
  #Petición API REST THB y extracción JSON
  json_twitter <- httr::GET(url, add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"="Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJrZXBhQHRlY2hmcmllbmRseS5lcyIsInNjb3BlcyI6WyJURU5BTlRfQURNSU4iXSwidXNlcklkIjoiNDIzZWYyMzAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiZmlyc3ROYW1lIjoiS2VwYSIsImxhc3ROYW1lIjoiQW1pZ28iLCJlbmFibGVkIjp0cnVlLCJpc1B1YmxpYyI6ZmFsc2UsInRlbmFudElkIjoiMzQ2ZWRmZDAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiY3VzdG9tZXJJZCI6IjEzODE0MDAwLTFkZDItMTFiMi04MDgwLTgwODA4MDgwODA4MCIsImlzcyI6InRoaW5nc2JvYXJkLmlvIiwiaWF0IjoxNTc5NzA2MTQ1LCJleHAiOjE2MTEyNDIxNDV9.UWBy1Es3yunXwKx7WVO-fxBSYyCynGKsfO7DASM8FtskgJsH4nDDjJrr4Kp_CrhBzetA3aEqABek5Ei4-fh7pw"))
  
  json_twitter_filtrado <- rjson::fromJSON(content(json_twitter, as="text"))
  

  
  # AVISO DE INEXISTENCIA DE DATOS EN EL INTERVALO DE FECHAS SELECCIONADO
  if(length(json_twitter_filtrado) < 80){
    return(0)
  }
  
  ###########################
  # CREACIÓN DE VARIABLES COMO PARES DE CLAVE VALOR INCLUIDAS EN LA URL
  variables_nombre <- str_split(str_match(curl_unescape(url), "keys=(.*?)&")[2],",")
  variables <- list()
  
  for (i in 1:length(unlist(variables_nombre))){
    variables[[i]] <- unlist(json_twitter_filtrado[[variables_nombre[[1]][i]]])
    n <- length(unlist(variables[i]))
    ts <- format(str_trim(unname(variables[[i]][seq(n) %% 2 != 0])),scientific = F)
    variables[[i]] <- str_trim(unname(variables[[i]][seq(n) %% 2 == 0]))
    names(variables[[i]]) <- ts
  }
  
  variables <- setNames(variables,unlist(variables_nombre))
  text <- variables["text"]
  variables2 <- variables[names(variables) != "text"]
  
  variables_nombre <- names(variables2)
  
  #Inicialización DF
  df <- as.data.frame(text)
  #Inserción automática de variables en DF en base al ts
  for(i in 2:length(names(variables))){
    df[,i] <- variables2[[i-1]][match(rownames(df), names(variables2[[i-1]]))]
  }
  
  variables_nombre <- c("text",variables_nombre)
  colnames(df) <- unlist(variables_nombre)
  
  df[is.na(df)] <- "-"
  
  # Referencia nombres (enumeración nombres columnas como vector nombrado para matching posterior con traducción)
  ref_nombres_en <- c("text", "account_created_at", "bbox_coords", "coords_coords", "created_at", "description",
                      "display_text_width", "ext_media_expanded_url",  "ext_media_t.co", "ext_media_url",  "favorite_count", "favourites_count",      
                      "followers_count", "friends_count", "geo_coords", "hashtags", "is_quote", "is_retweet",          
                      "lang", "listed_count", "location",  "media_expanded_url", "media_t.co", "media_type",             
                      "media_url",  "mentions_screen_name", "mentions_user_id", "name", "profile_background_url", "profile_banner_url",    
                      "profile_expanded_url", "profile_image_url", "profile_url", "protected", "quoted_created_at", "quoted_description",    
                      "quoted_favorite_count", "quoted_followers_count", "quoted_friends_count", "quoted_location", "quoted_name", "quoted_retweet_count",  
                      "quoted_screen_name", "quoted_source",  "quoted_statuses_count", "quoted_status_id", "quoted_text", "quoted_user_id",         
                      "quoted_verified", "reply_to_screen_name", "reply_to_status_id", "reply_to_user_id", "retweet_count", "retweet_created_at",     
                      "retweet_description", "retweet_favorite_count", "retweet_followers_count", "retweet_friends_count", "retweet_location", "retweet_name",           
                      "retweet_retweet_count", "retweet_screen_name", "retweet_source", "retweet_statuses_count", "retweet_status_id", "retweet_text",           
                      "retweet_user_id", "retweet_verified", "screen_name", "source", "statuses_count", "status_id",             
                      "status_url", "Twitter objetivo", "url", "urls_expanded_url", "urls_t.co", "urls_url",               
                      "user_id", "verified" )
  
  names(ref_nombres_en) <- seq(1,80)
  
  # Matching de nombres columnas dataframe datos twitter con ref_nombres_en
  matching_df_ref_nombres <- c()
  for(i in 1:length(names(df))){
    matching_df_ref_nombres <- c(matching_df_ref_nombres, grep(paste("^",names(df)[i],"$",sep=""),ref_nombres_en))
  }
  
  ref_nombres_es <-  c("Texto", "Cuenta creada en", "Coordenadas bbox", "Coordenadas coor", "Creado en", "Descripción",
                       "Tamaño de texto", "URL medio exterior expandido", "ext_media_t.co", "URL medio exterior", "Contador favorito", "Contador favoritos 2", 
                       "Contador seguidores", "Contador seguidos", "Coordenadas", "Hashtags", "Es cita", "Es un retweet",
                       "Idioma", "Contador listado", "Localización", "URL medio expandido", "media_t.co", "Tipo de medio", 
                       "URL medio", "Cuentas mencionadas", "ID menciones usuario", "Nombre cuenta", "URL fondo perfil", "Banner perfil",
                       "URL en perfil", "Imagen perfil", "URL perfil 2", "Protegido", "Citado creado en", "Descripción citado",
                       "Contador me gustas en citado", "Contador seguidores citado", "Contador seguidos citados", "Localización citado", "Nombre citado", "Contador retweets citado",
                       "Nombre pantalla citado", "Fuente citado", "Contador status citados", "ID status citados", "Texto citado", "ID usuario citado",
                       "Citado verificado", "Nombre respuesta a pantalla", "ID respuesta status", "ID respuesta a usuario", "Contador retweets", "Retweet creado en",
                       "Descripición retweet", "Contador favoritos", "Contador seguidores cuenta retweeteada", "Contador seguidos cuenta retweeteada","Localización retweet", "Nombre retweet",
                       "Contador retweets retweet", "Nombre pantalla retweet", "Fuente retweet", "Contador statuses retweet", "ID status retweet","Texto retweet",
                       "ID usuario retweet", "Retweet verificado", "Nombre pantalla", "Fuente", "Contador status", "ID status",
                       "Link tweet","Twitter objetivo", "URL", "URL urls expandido", "urls_t.co", "URL urls", 
                       "ID usuario", "Verificado")
  
  ref_nombres_es <- str_replace_all(ref_nombres_es,"[.]", " ")
  names(ref_nombres_es) <- seq(1,80)
  
  nombres_matching <- ref_nombres_es[matching_df_ref_nombres]
  
  names(df) <- nombres_matching
  
  orden <- subset(df, select = c("Texto" ,"Twitter objetivo", "Hashtags", "Es un retweet", "Creado en", "Contador favoritos", "Contador retweets", "Cuentas mencionadas", "Tamaño de texto", "Link tweet", 
                                 "Nombre cuenta", "Imagen perfil",  "Banner perfil", "Descripción", "Contador seguidores", "Contador seguidos", "URL en perfil"))
  
  df <- df[ , -which(names(df) %in% c("Texto" ,"Twitter objetivo", "Hashtags", "Es un retweet", "Creado en", "Contador favoritos", "Contador retweets", "Tamaño de texto", "Link tweet", 
                                      "Nombre cuenta", "Imagen perfil",  "Banner perfil", "Descripción", "Contador seguidores", "Contador seguidos", "URL en perfil"))]
  df <- data.frame(orden,df)
  
  names(df) <- str_replace_all(names(df),"[.]", " ")
  
  df <- df[ , -which(names(df) %in% c("Contador favorito", "Contador favoritos 2", "Coordenadas", "Coordenadas bbox", "Coordenadas coor", "URL medio expandido", "media_t co",
                                      "URL perfil 2", "urls_t co", "URL urls", "ext_media_t co"))]
  
  return(df)
}