# datos para análisis crecimiento
creci <- tibble(
  Muestreo = res_1a42_clean$Muestreo,
  fecha = res_1a42_clean$fecha,
  season = res_1a42_clean$season,
  N = res_1a42_clean$b.N_est,
  Nlcl = res_1a42_clean$b.LclN,
  Nucl = res_1a42_clean$b.UclN,
  D = res_1a42_clean$b.D_est,
  DSE = res_1a42_clean$b.SE,
  Dlcl = res_1a42_clean$b.Lcl,
  Ducl = res_1a42_clean$b.Ucl,
  Grupo = res_1a42_clean$b.Group_Size_est,
  GrupoES = res_1a42_clean$b.SE_grupo
) %>% 
  mutate(logN = log(N),
         logNlcl = log(Nlcl), logNucl = log(Nucl)) %>% 
  mutate(season = factor(season, levels = c("Summer","Autumn","Winter","Spring"), ordered = T))


  
# figura con N estimado
ggplot(creci,aes(x=fecha,y=N))+
  geom_line()+
  geom_point(aes(col=season),size=3.5)+
  scale_color_manual(values = c("Summer" = "orange", "Autumn" = "gold", 
                               "Winter" = "skyblue", "Spring" = "lightgreen"),
                     name = "Estación",
                     labels = c("Summer" = "verano", "Autumn" = "otoño", 
                                "Winter" = "invierno", "Spring" = "primavera")) +
  geom_ribbon(aes(ymin = Nlcl, ymax = Nucl), alpha = 0.2)+
  ggtitle("Abundancia poblacional estimada (N) \nPeríodo 2007-2023") +
  ylab("Abundancia (N)")+xlab("Fecha")+
  theme_minimal()

# figura en escala log
ggplot(creci,aes(x=fecha,y=logN))+
  geom_point()+
  geom_ribbon(aes(ymin = logNlcl, ymax = logNucl), alpha = 0.2)+
  ggtitle("Logaritmo de la abundancia poblacional estimada - log(N) \nPeríodo 2007-2023") +
  ylab("Log Abundancia")+xlab("Fecha")+
  theme_minimal()+
  coord_cartesian(ylim = c(0,15))

# tamaño de grupo estimado
ggplot(creci,aes(x=fecha,y=Grupo))+
  geom_point()+
  geom_ribbon(aes(ymin = Grupo-GrupoES, ymax = Grupo+GrupoES), alpha = 0.2)+
  ggtitle("Tamaño de grupo estimado y error estándar \nPeríodo 2007-2023") +
  ylab("Tamaño de grupo")+xlab("Fecha")+
  theme_minimal()+
  facet_wrap(vars(season))+
  coord_cartesian(ylim = c(0,20))

ggplot(creci, aes(Grupo,season))+
  geom_boxplot()

ggplot(creci,aes(season,Grupo))+
  geom_boxplot(outlier.colour = "darkgray", outlier.shape = 1, fill = "lightgray")+
  scale_y_continuous(limits = c(0,20))+
  scale_x_discrete(labels=c("verano","otoño","invierno","primavera"))+
  xlab("Estación")+ylab("Tamaño de grupo")+
  theme_minimal()

# fabrico información de tasa de crecimiento lambda
# muestreo a muestreo
lambdaN <- vector(length=40)
lambdaN[1] <- NA
lambdaN[2:40] <- creci$N[2:40]/creci$N[1:39]
creci <- creci %>% 
  mutate(lambdaN = lambdaN)

# lambda vs fecha
ggplot(creci,aes(x=fecha,y=lambdaN))+
  geom_point()+
  geom_smooth()+
  ggtitle("Tasa de crecimiento discreta \nPeríodo 2007-2023") +
  ylab("Tasa de crecimiento")+xlab("Fecha")+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA))

# lambda vs densidad
ggplot(creci,aes(x=D,y=lambdaN))+
  geom_point()+
  #geom_smooth()+
  ggtitle("Tasa de crecimiento discreta \nen función de densidad de guanacos") +
  ylab("Tasa de crecimiento")+xlab(expression("Densidad de guanacos (" ~ km^2*")"))+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA))

# lambda vs chulengos.CPUE observados
ggplot(creci,aes(x=chu.CPUE,y=lambdaN))+
  geom_point()+
  #geom_smooth()+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA))

# Modelos de crecimiento
mod.N1<-lm(creci$lambdaN~creci$N)
summary(mod.N1)

ggplot(data=creci,aes(x=N,y=lambdaN))+
  geom_point()+
  ggtitle("Tasa de crecimiento discreta \nen función de abundancia de guanacos")+
  ylab("Tasa de crecimiento")+xlab("Abundancia de guanacos")+
  geom_hline(yintercept=1,color='pink',lty=2,lwd=1.3)+
  geom_smooth(method='lm',se=F)+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA))

# filtrado para otoño:
mod.N2<-lm(creci$lambdaN[creci$season=="Autumn"]~creci$N[creci$season=="Autumn"])
summary(mod.N2)
creci %>% filter(season == "Autumn") %>% 
  ggplot(aes(x=N,y=lambdaN))+
  geom_point()+
  ggtitle("Tasa de crecimiento discreta \nen función de abundancia de guanacos
          \notoño")+
  ylab("Tasa de crecimiento")+xlab("Abundancia de guanacos")+
  geom_hline(yintercept=1,color='pink',lty=2,lwd=1.3)+
  geom_smooth(method='lm',se=F)+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA))

# filtrado para primavera:
mod.N3<-lm(creci$lambdaN[creci$season=="Spring"]~creci$N[creci$season=="Spring"])
summary(mod.N3)
creci %>% filter(season == "Spring") %>% 
  ggplot(aes(x=N,y=lambdaN))+
  geom_point()+
  ggtitle("Tasa de crecimiento discreta \nen función de abundancia de guanacos
          \nprimavera")+
  ylab("Tasa de crecimiento")+xlab("Abundancia de guanacos")+
  geom_hline(yintercept=1,color='pink',lty=2,lwd=1.3)+
  geom_smooth(method='lm',se=F)+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA))

# filtrado para verano:
mod.N4<-lm(creci$lambdaN[creci$season=="Summer"]~creci$N[creci$season=="Summer"])
summary(mod.N4)
creci %>% filter(season == "Summer") %>% 
  ggplot(aes(x=N,y=lambdaN))+
  geom_point()+
  ggtitle("Tasa de crecimiento discreta \nen función de abundancia de guanacos
          \nverano")+
  ylab("Tasa de crecimiento")+xlab("Abundancia de guanacos")+
  geom_hline(yintercept=1,color='pink',lty=2,lwd=1.3)+
  geom_smooth(method='lm',se=F)+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA))


# creci anual - por estación ----------------------------------------------


# OTOÑO -------------------------------------------------------------------
library(lubridate)
oto_yr <- c(2008:2023)
oto <- creci %>% 
  filter(season=="Autumn") %>% 
  mutate(fecha = year(fecha))
creci_oto <- tibble(fecha = oto_yr) %>% 
  left_join(oto,by="fecha")

lambdaNoto <- vector(length=16)
lambdaNoto[1] <- NA
lambdaNoto[2:16] <- creci_oto$N[2:16]/creci_oto$N[1:15]
creci_oto <- creci_oto %>% 
  mutate(lambdaNoto = lambdaNoto)

# lambda vs fecha
ggplot(creci_oto,aes(x=fecha,y=lambdaNoto))+
  geom_point()+
  geom_smooth()+
  ggtitle("Tasa de crecimiento anual \nPeríodo 2007-2023 \notoño") +
  ylab("Tasa de crecimiento")+xlab("Fecha")+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA))

# lambda vs densidad
ggplot(creci_oto,aes(x=D,y=lambdaNoto))+
  geom_point()+
  #geom_smooth()+
  ggtitle("Tasa de crecimiento anual \nen función de densidad de guanacos \notoño") +
  ylab("Tasa de crecimiento")+xlab(expression("Densidad de guanacos (" ~ km^2*")"))+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA))

# Modelos de crecimiento
mod.o<-lm(creci_oto$lambdaNoto~creci_oto$N)
summary(mod.o)

ggplot(data=creci_oto,aes(x=N,y=lambdaNoto))+
  geom_hline(yintercept=1,color='pink',lty=2,lwd=1.3)+
  geom_point(size=2,position=position_jitter(height = .05))+
  ggtitle("Tasa de crecimiento anual \nen función de abundancia de guanacos
          \notoño")+
  ylab("Tasa de crecimiento")+xlab("Abundancia de guanacos")+
  geom_smooth(method='lm',se=F)+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA))


# PRIMAVERA ---------------------------------------------------------------

pri_yr <- c(2010:2023)
pri <- creci %>% 
  filter(season=="Spring") %>% 
  mutate(fecha = year(fecha))
creci_pri <- tibble(fecha = pri_yr) %>% 
  left_join(pri,by="fecha")

lambdaNpri <- vector(length=14)
lambdaNpri[1] <- NA
lambdaNpri[2] <- NA
lambdaNpri[3:14] <- creci_pri$N[3:14]/creci_pri$N[1:12]
creci_pri <- creci_pri %>% 
  mutate(lambdaNpri = lambdaNpri)

# lambda vs fecha
ggplot(creci_pri,aes(x=fecha,y=lambdaNpri))+
  geom_point()+
  #geom_smooth()+
  ggtitle("Tasa de crecimiento anual \nPeríodo 2007-2023 \nprimavera") +
  ylab("Tasa de crecimiento")+xlab("Fecha")+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA))

# lambda vs densidad
ggplot(creci_pri,aes(x=D,y=lambdaNpri))+
  geom_point()+
  #geom_smooth()+
  ggtitle("Tasa de crecimiento anual \nen función de densidad de guanacos \nprimavera") +
  ylab("Tasa de crecimiento")+xlab(expression("Densidad de guanacos (" ~ km^2*")"))+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA))

# Modelos de crecimiento
mod.p<-lm(creci_pri$lambdaNpri~creci_pri$N)
summary(mod.p)

ggplot(data=creci_pri,aes(x=N,y=lambdaNpri))+
  geom_hline(yintercept=1,color='pink',lty=2,lwd=1.3)+
  geom_point(size=2,position=position_jitter(height = .05))+
  ggtitle("Tasa de crecimiento bianual \nen función de abundancia de guanacos
          \nprimavera")+
  ylab("Tasa de crecimiento")+xlab("Abundancia de guanacos")+
  geom_smooth(method='lm',se=F)+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA))


# VERANO ------------------------------------------------------------------

ver_yr <- c(2007:2022)
ver <- creci %>% 
  filter(season=="Summer") %>% 
  mutate(fecha = year(fecha))
creci_ver <- tibble(fecha = ver_yr) %>% 
  left_join(ver,by="fecha")

lambdaNver <- vector(length=18)
lambdaNver[1] <- NA
lambdaNver[2:18] <- creci_ver$N[2:18]/creci_ver$N[1:17]
creci_ver <- creci_ver %>% 
  mutate(lambdaNver = lambdaNver)

# lambda vs fecha
ggplot(creci_ver,aes(x=fecha,y=lambdaNver))+
  geom_point()+
  #geom_smooth()+
  ggtitle("Tasa de crecimiento anual \nPeríodo 2007-2023 \nverano") +
  ylab("Tasa de crecimiento")+xlab("Fecha")+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA))

# lambda vs densidad
ggplot(creci_ver,aes(x=D,y=lambdaNver))+
  geom_point()+
  #geom_smooth()+
  ggtitle("Tasa de crecimiento anual \nen función de densidad de guanacos \nverano") +
  ylab("Tasa de crecimiento")+xlab(expression("Densidad de guanacos (" ~ km^2*")"))+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA))

# Modelos de crecimiento
# lineal:
mod.v<-lm(creci_ver$lambdaNver~creci_ver$N)
summary(mod.v)

ggplot(data=creci_ver,aes(x=N,y=lambdaNver))+
  geom_hline(yintercept=1,color='pink',lty=2,lwd=1.3)+
  geom_point(size=2,position=position_jitter(height = .05))+
  ggtitle("Tasa de crecimiento anual \nen función de abundancia de guanacos
          \nverano")+
  ylab("Tasa de crecimiento")+xlab("Abundancia de guanacos")+
  #geom_smooth(method='lm',se=F)+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA),xlim = c(0,NA))

# exponencial:
library(minpack.lm)
# Ajuste de la función exponencial con nlsLM y límites en los parámetros
ver_ajuste <- creci_ver %>% filter(!is.na(lambdaNver))
mod.v.2 <- nls(ver_ajuste$lambdaNver ~ a * exp(b * ver_ajuste$N), 
                 start = list(a = 1, b = -0.0001),data = ver_ajuste)

mod.v.2 <- nlsLM(ver_ajuste$lambdaNver ~ a * exp(b * ver_ajuste$N), 
                 start = list(a = 1, b = -0.0001),data = ver_ajuste)
# a_start <- 2
# b_start <- -0.0001
# predicted_values <- a_start * exp(b_start *creci_ver$N)
# print(predicted_values)

a <- coef(mod.v.2)["a"]
b <- coef(mod.v.2)["b"]
x <- seq(min(ver_ajuste$N),max(ver_ajuste$N),length.out=50)
y <- a * exp(b*x)
pred_data <- tibble(x,y)

ggplot(data=ver_ajuste,aes(x=N,y=lambdaNver))+
  geom_hline(yintercept=1,color='pink',lty=2,lwd=1.3)+
  geom_point(data=pred_data,aes(x=x,y=y))+
  geom_point(size=2,position=position_jitter(height = .05))+
  ggtitle("Tasa de crecimiento anual \nen función de abundancia de guanacos
          \nverano")+
  ylab("Tasa de crecimiento")+xlab("Abundancia de guanacos")+
  #geom_smooth(formula = y ~ poly(x))+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA),xlim = c(0,NA))

# ajuste polinómico
mod.v.3<-lm(ver_ajuste$lambdaNver~ poly(ver_ajuste$N,2,raw = T),
            data = ver_ajuste)
summary(mod.v.3)

ver_ajuste <- ver_ajuste %>% 
  mutate(ver_ajuste,ypoly = predict(mod.v.3))

ggplot(data=ver_ajuste,aes(x=N,y=lambdaNver))+
  geom_hline(yintercept=1,color='pink',lty=2,lwd=1.3)+
  geom_line(aes(y=ypoly),col = "navy",lwd=1.2)+
  geom_point(size=2)+
  ggtitle("Tasa de crecimiento anual \nen función de abundancia de guanacos
          \nverano \najuste polinómico grado 2")+
  ylab("Tasa de crecimiento")+xlab("Abundancia de guanacos")+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA),xlim = c(0,NA))

x <- seq(min(ver_ajuste$N),max(ver_ajuste$N),length.out=50)
a <- coef(mod.v.3)["(Intercept)"]
b <- coef(mod.v.3)["poly(ver_ajuste$N, 2, raw = T)1"]
c <- coef(mod.v.3)["poly(ver_ajuste$N, 2, raw = T)2"]
y_pol <- a + b * x + c * x^2
pred_data <- pred_data %>% mutate(y_pol = y_pol)

ggplot(data=ver_ajuste,aes(x=N,y=lambdaNver))+
  geom_hline(yintercept=1,color='pink',lty=2,lwd=1.3)+
  geom_point(data= pred_data,aes(x = x,y=y_pol),col = "navy")+
  geom_point(size=2)+
  ggtitle("Tasa de crecimiento anual \nen función de abundancia de guanacos
          \nverano \najuste polinómico grado 2")+
  ylab("Tasa de crecimiento")+xlab("Abundancia de guanacos")+
  theme_minimal()+
  coord_cartesian(ylim = c(0,NA),xlim = c(0,NA))
