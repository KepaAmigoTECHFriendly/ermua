# POST a función fechas BORME en paquete borme

library(httr)
library(rjson)

#Scripts R apoyo externos
source("llamada_api_thb_borme.R")


llamada_api_fechas <- function(fecha_inicial, fecha_final){
  
  fecha <- paste(as.character(fecha_inicial),", ",as.character(fecha_final), sep = "")
  
  municipio <- tolower(llamada_atributos()$value[llamada_atributos()$key == "Municipio"])
  radio <- tolower(llamada_atributos()$value[llamada_atributos()$key == "Radio"])
  provincias <- tolower(llamada_atributos()$value[llamada_atributos()$key == "Provincias"])
  
  
  #Petición POST con actualizacióna ctivos y flag de llamada REST API a función opencpu borme fechas a 1
  post_flag_1 <- httr::POST(url = "http://88.99.184.239:8080/api/plugins/telemetry/ASSET/77bc5270-78d1-11ea-bf56-13868846c781/attributes/SERVER_SCOPE", 
                      add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"="Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJrZXBhQHRlY2hmcmllbmRseS5lcyIsInNjb3BlcyI6WyJURU5BTlRfQURNSU4iXSwidXNlcklkIjoiNDIzZWYyMzAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiZmlyc3ROYW1lIjoiS2VwYSIsImxhc3ROYW1lIjoiQW1pZ28iLCJlbmFibGVkIjp0cnVlLCJpc1B1YmxpYyI6ZmFsc2UsInRlbmFudElkIjoiMzQ2ZWRmZDAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiY3VzdG9tZXJJZCI6IjEzODE0MDAwLTFkZDItMTFiMi04MDgwLTgwODA4MDgwODA4MCIsImlzcyI6InRoaW5nc2JvYXJkLmlvIiwiaWF0IjoxNTc5NzA2MTQ1LCJleHAiOjE2MTEyNDIxNDV9.UWBy1Es3yunXwKx7WVO-fxBSYyCynGKsfO7DASM8FtskgJsH4nDDjJrr4Kp_CrhBzetA3aEqABek5Ei4-fh7pw"),
                      body = list(flag="1",Municipio=municipio,Radio=radio,Provincias=provincias,fecha=fecha),
                      encode = "json",verbose()
  )
  
  #Tiempo de espera para resetear el flag a 0.
  Sys.sleep(5)
  
  
  #Petición POST para resetear (0) flag de llamada REST API a función opencpu borme fechas
  post_flag_0 <- httr::POST(url = "http://88.99.184.239:8080/api/plugins/telemetry/ASSET/77bc5270-78d1-11ea-bf56-13868846c781/attributes/SERVER_SCOPE", 
                      add_headers("Content-Type"="application/json","Accept"="application/json","X-Authorization"="Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJrZXBhQHRlY2hmcmllbmRseS5lcyIsInNjb3BlcyI6WyJURU5BTlRfQURNSU4iXSwidXNlcklkIjoiNDIzZWYyMzAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiZmlyc3ROYW1lIjoiS2VwYSIsImxhc3ROYW1lIjoiQW1pZ28iLCJlbmFibGVkIjp0cnVlLCJpc1B1YmxpYyI6ZmFsc2UsInRlbmFudElkIjoiMzQ2ZWRmZDAtMTI5NS0xMWVhLThkMTEtNzVhYjc1ZTA4ZjRmIiwiY3VzdG9tZXJJZCI6IjEzODE0MDAwLTFkZDItMTFiMi04MDgwLTgwODA4MDgwODA4MCIsImlzcyI6InRoaW5nc2JvYXJkLmlvIiwiaWF0IjoxNTc5NzA2MTQ1LCJleHAiOjE2MTEyNDIxNDV9.UWBy1Es3yunXwKx7WVO-fxBSYyCynGKsfO7DASM8FtskgJsH4nDDjJrr4Kp_CrhBzetA3aEqABek5Ei4-fh7pw"),
                      body = list(flag="0"),
                      encode = "json",verbose()
                      
  )
  
  return(post_flag_1$status_code)

}

