# funciones para ajustar funciones de detección

# función para ajustar 3 key functions con mismas características
# hn uniform y hazard rate para grupos de individuos dht_group F
# monotonicity strict, máximo 3 adjustments:
fit.hn.uni.haz.list.group <- function(data, trunc) {
  # Purpose: fit 3 key functions to transect data and print model selection table
  # Input: data to analyse, truncation distance, print flag
  # Output: model object (class `dsmodel`)
  # unidades: distancia - esfuerzo - area
  conversion.factor <- convert_units("Meter", "Kilometer", "Square kilometer")
  models <- list()
  models$hn.cos <- ds(data, trun=trunc, key="hn", adj="cos", dht_group = F,
                      max_adjustments = 3, monotonicity = "strict",
                      convert_units = conversion.factor)
  models$uni.cos <- ds(data, trun=trunc, key="unif", adj="cos", dht_group = F,
                       max_adjustments = 3, monotonicity = "strict",
                       convert_units = conversion.factor)
  models$haz.poly <- ds(data, trun=trunc, key="hr", adj="poly", dht_group = F,
                        max_adjustments = 3, monotonicity = "strict",
                        convert_units = conversion.factor)
  
  return(models)
}

# función para extraer datos del modelo
extract_model_data_table <- function(model){
  # extrae elementos elegidos del ajuste del modelo
  out <- as_tibble(matrix(0, nrow = 1, ncol = 5))
  colnames(out) <-c("key","AIC","CvonM_p","P_enc_medio","SE_P_enc_medio")
  out[[1]] <- summary(model)$ds$key
  out[[2]] <- model$ddf$criterion
  out[[3]] <- ddf.gof(model$ddf, qq=FALSE)$dsgof$CvM$p
  out[[4]] <- summary(model)$ds$average.p
  out[[5]] <- summary(model)$ds$average.p.se[1]
  return(out)
}

# extrae elementos de los resultados del modelo

extract_model_res_table <- function(model){
  # extrae elementos elegidos del resultado de un modelo y arma una tabla
  out <- as_tibble(matrix(0, nrow = 1, ncol = 11))
  colnames(out) <-c('Modelo','Región','D_est','SE','Lcl','Ucl','Group_Size_est','SE_grupo','N_est','LclN','UclN')
  out[[1]] <- model$call$key
  out[[2]] <- model$dht$clusters$summary$Region
  out[[3]] <- model$dht$individuals$D$Estimate
  out[[4]] <- model$dht$individuals$D$se
  out[[5]] <- model$dht$individuals$D$lcl
  out[[6]] <- model$dht$individuals$D$ucl
  out[[7]] <- model$dht$Expected.S$Expected.S
  out[[8]] <- model$dht$Expected.S$se.Expected.S
  out[[9]] <- model$dht$individuals$N$Estimate
  out[[10]] <- model$dht$individuals$N$lcl
  out[[11]] <- model$dht$individuals$N$ucl
  return(out)  
}

procesar_muestreo <- function(data, muestreo_valor, especie, truncado_valor){
  trans <- data %>% 
    filter(Especie == especie) %>% 
    filter(Muestreo == muestreo_valor)
  par(mfrow = c(1,1),mar=c(3,3,2,1),cex.main=1)
  h <- hist(trans$distance, main = paste("Muestreo ", muestreo_valor, 
                                         "\nDistancia truncada ", truncado_valor,"m"))
  res <- fit.hn.uni.haz.list.group(trans,truncado_valor)
  a <- map_df(res,extract_model_data_table) %>% 
    mutate(Muestreo = muestreo_valor)
  b <- map_df(res,extract_model_res_table) %>% 
    mutate(Muestreo = muestreo_valor)
  
  # output plots
  par(mar=c(2,2,0,0.2),cex.main=2)
  layout(matrix(c(1,2,3,4,5,6,7),ncol=1),heights=c(1.5,1.1,3.4,1.1,3.4,1.1,3.4))
  # Main title:
  plot.new()
  text(0.5,0.5,cex = 1.8,
       paste("Muestreo ", muestreo_valor,"\nDistancia truncada ", truncado_valor,"m"))
  # Panels:
  plot.new()
  text(0.1,0.3,"Half-normal:",cex=1.5)
  plot(res$hn.cos,cex=1.8)
  plot.new()
  text(0.1,0.3,"Uniforme:",cex=1.5)
  plot(res$uni.cos,cex=1.8)
  plot.new()
  text(0.1,0.3,"Hazard rate:",cex=1.5)
  plot(res$haz.poly,cex=1.8)
  # Restore default
  par(mfrow = c(1,1),mar=c(3,3,2,1),cex.main=1)
  
  return(list(a = a, b = b))
}
