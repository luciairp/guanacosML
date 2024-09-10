library(Distance)
# Estimación distance sampling guanacos

data_G <- data %>% 
  filter(Especie == "G",
         !is.na(size))

# uno por uno
trans <- data_G %>% 
  filter(Muestreo == 20)

# con objetos por muestreo
hist(trans$distance)
res <- fit.hn.uni.haz.list.group(trans,600)
a <- map_df(res,extract_model_data_table)
b <- map_df(res,extract_model_res_table)

# con lista de muestreos
histogramas <- map(data_G,~hist(.x$distance))
res <- map(data_G,~fit.hn.uni.haz.list.group(.x,600))



procesar_muestreo(data = data, muestreo_valor =20,especie = "G", truncado_valor =600)

data_r <- data_G %>% filter(Muestreo == 20)
res <- fit.hn.uni.haz.list.group(data_r,500)
# prueba salida gráficos
par(mar=c(2,2,0,0.2),cex.main=2)
layout(matrix(c(1,2,3,4,5,6,7),ncol=1),heights=c(1.5,1.1,3.4,1.1,3.4,1.1,3.4))
# Main title
plot.new()
text(0.5,0.5,"Muestreo .. Distancia truncada",cex=2)
plot.new()
text(0.1,0.3,"Half-normal:",cex=1.5)
plot(res$hn.cos,cex=1.8)
plot.new()
text(0.1,0.3,"Uniforme:",cex=1.5)
plot(res$uni.cos,cex=1.8)
plot.new()
text(0.1,0.3,"Hazard rate:",cex=1.5)
plot(res$haz.poly,cex=1.8)
par(mfrow = c(1, 1))

lista_muestreos <- c(20:21)
res_20y21 <- map_df(lista_muestreos,~procesar_muestreo(data = data,
                                          muestreo_valor = .x,
                                          especie = "G",
                                       truncado_valor = 600))

data_old <- data %>% filter(Especie == "G",Muestreo<20)
lista_muestreos <- c(1:19)
res_1a19 <- map_df(lista_muestreos,~procesar_muestreo(data = data_old,
                                                       muestreo_valor = .x,
                                                       especie = "G",
                                                       truncado_valor = 400))

check_data <- data_old %>% group_by(Sample.Label,Muestreo) %>% reframe(n(),unique(Effort))
ggplot(res_1a19)+
  geom_point(aes(x=a$Muestreo,y=b$D_est,col=a$key))+
  theme_minimal()
  
lista_muestreos <- c(1:41)
res_1a42 <- map_df(lista_muestreos,~procesar_muestreo(data = data_G,
                                                      muestreo_valor = .x,
                                                      especie = "G",
                                                      truncado_valor = 400))

write.csv(res_1a42,"res_1a42.csv")

# resultados período completo
ggplot(res_1a42)+
  geom_point(aes(x=a$Muestreo,y=b$D_est,col=a$key))+
  theme_minimal()

ggplot(res_1a42)+
  geom_point(aes(x=a$Muestreo,y=a$P_enc_medio,col=a$key))+
  theme_minimal()

res_1a42_clean <- res_1a42 %>% 
  filter(complete.cases(a$AIC,a$CvonM_p,a$P_enc_medio,a$SE_P_enc_medio)) %>% 
  filter(a$CvonM_p > 0.05) %>% 
  group_by(a$Muestreo) %>% 
  slice_min(order_by = a$AIC) %>% 
  mutate(Muestreo = b$Muestreo) %>% 
  left_join(muestreos, by="Muestreo")
write.csv(res_1a42_clean,"res_1a42_clean.csv")

ggplot(res_1a42_clean,aes(x=fecha,y=b$D_est))+
  geom_point(aes(col = a$key))+
  #geom_smooth(aes(x=a$Muestreo,y=b$D_est))+
  geom_ribbon(aes(ymin = b$Lcl, ymax = b$Ucl), alpha = 0.2)+
  theme_minimal()+
  facet_wrap(vars(season))

res_1a42_clean %>% filter(season == "Autumn"|season=="Spring") %>% 
ggplot(aes(x=fecha,y=b$D_est))+
  geom_point()+
  #geom_smooth(aes(x=a$Muestreo,y=b$D_est))+
  geom_ribbon(aes(ymin = b$Lcl, ymax = b$Ucl), alpha = 0.2)+
  theme_minimal()+
  theme(legend.position = "none")+
  ylab(expression("Densidad de guanacos (" ~ km^2*")"))+
  xlab("Fecha")+
  facet_grid(rows= vars(season),
             labeller= labeller(season = c("Autumn" = "otoño","Spring"="primavera")))

esf_muestreo %>% filter(season == "Autumn"|season=="Spring") %>% 
ggplot(aes(x=fecha,y=eff))+
  geom_col(fill="blue",alpha=.5)+
  theme_minimal()+
  xlab("Fecha")+ylab("Esfuerzo: Km recorridos")

ggplot(esf_muestreo,aes(x=fecha,y=eff,fill=season))+
  geom_col(width = 85)+
  scale_fill_manual(values = c("Winter" = "skyblue", 
                                 "Summer" = "orange", 
                                 "Autumn" = "gold",
                                 "Spring" = "lightgreen"),
                    labels = c("Winter" = "Invierno",
                               "Summer" = "Verano",
                               "Autumn" = "Otoño",
                               "Spring" = "Primavera"))+
  theme_minimal()+
  xlab("Fecha")+ylab("Esfuerzo: Km recorridos")+
  labs(fill = "Estación")


ggplot(res_1a42_clean)+
  geom_point(aes(x=a$Muestreo,y=a$P_enc_medio,col=a$key))+
  theme_minimal()

creci %>% filter(season=="Autumn") %>% filter(Muestreo>32) %>% 
  summarise(media = mean(D), min = min(D), max= max(D))

creci %>% filter(season=="Spring") %>% filter(Muestreo>30) %>% 
  summarise(media = mean(D), min = min(D), max= max(D))

