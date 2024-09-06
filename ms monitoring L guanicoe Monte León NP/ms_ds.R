# Estimación distance sampling guanacos 2015 en adelante

data_r <- data %>% 
  filter(Especie == "G") %>% 
  filter(Muestreo > 19)

#%>% 
#  split(.$Muestreo)

# uno por uno
trans <- data_r %>% 
  filter(Muestreo == 20)

# con objetos por muestreo
hist(trans$distance)
res <- fit.hn.uni.haz.list.group(trans,600)
a <- map_df(res,extract_model_data_table)
b <- map_df(res,extract_model_res_table)

# con lista de muestreos
histogramas <- map(data_r,~hist(.x$distance))
res <- map(data_r,~fit.hn.uni.haz.list.group(.x,600))


procesar_muestreo <- function(muestreo_valor, truncado_valor,data){
  trans <- data %>% 
    filter(Muestreo == muestreo_valor)
  hist(trans$distance)
  res <- fit.hn.uni.haz.list.group(trans,truncado_valor)
  a <- map_df(res,extract_model_data_table)
  b <- map_df(res,extract_model_res_table)
  
  return(list(a = a, b = b))
}

map(20,procesar_muestreo())
procesar_muestreo(20,600,data_r)

lista_muestreos <- c(20:21)
map_df(lista_muestreos,~procesar_muestreo(muestreo_valor = .x,
                                       truncado_valor = 600,
                                       data = data_r))
