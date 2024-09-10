library(tidyverse)

# archivo datos preparados para ms
data <- read_delim("guanacosML_ms.csv",
                 locale=locale(decimal_mark = ",", grouping_mark = "."),
                 delim = ";",
                 col_types = cols(
                   Region.Label = col_factor(levels=c("ALTO","BAJO")),
                   Sample.Label = col_factor(),
                   Especie = col_factor()
                 ),
                 n_max = 6879
) %>% 
  mutate(Sample.Label = fct_collapse(Sample.Label,
                                     ANT1 = c("Antena 1", "ANTENA 1"),
                                     ANT2 = c("Antena 2", "ANTENA 2"),
                                     LAG1 = c("Laguna 1", "LAGUNA 1"),
                                     LAG2 = c("Laguna 2", "LAGUNA 2"),
                                     LAG3 = c("Laguna 3", "LAGUNA 3"),
                                     RP63A1 = c("RP63 A1", "Rp63 A1"),
                                     RP63B1 = c("RP63 B1", "Rp63 B1"),
                                     RP63B2 = c("RP63 B2", "Rp63 B2"),
                                     RP63B3 = c("RP63 B3"),
                                     RP63B4 = c("RP63 B4", "Rp63 B4"),
                                     RP63B5 = c("RP63 B5"),
                                     TRI1 = c("TRIANGULO 1", "Triangulo 1"),
                                     TRI2 = c("TRIANGULO 2", "Triangulo 2"),
                                     LIMS1 = c("limite sur 1","LIMITE SUR 1","Limite Sur 1"),
                                     LIMS2 = c("LIMITE SUR 2","Limite Sur 2")
  )) %>% 
  mutate(distance = abs(sin(Angle)*RadialDistance),
         distance_km = distance/1000) %>% 
  # cambio Region.Label a ML con su area total
  # y paso estratos a col stratum
  rename(stratum = Region.Label,
         stratum_area = Area) %>% 
  mutate(Region.Label = "ML",
         Area = 277+338) %>% 
  mutate(muestreo = as.factor(Muestreo))

guess_encoding("guanacosML_ms.csv")

# tablas accesorias
guess_encoding("trans14_ms.csv")
transectas2014 <- read_delim("trans14_ms.csv",
                           locale=locale(decimal_mark = ",", grouping_mark = "."),
                           delim = ";",
                       col_names = T,
                       col_types = cols(
                         Transecta = col_factor(NULL),
                         Estrato = col_factor(),
                         'Longitud (Km)' = col_double()
                       ))

transectas2015 <- read_csv("trans15_ms.csv", 
                         locale = locale(decimal_mark = ","),
                       col_names = T,
                       col_types = cols(
                         Transecta = col_factor(NULL),
                         Estrato = col_factor(levels=c("ALTO","BAJO")),
                         Numero = col_double(),
                         LongKm = col_double()
                       )) %>% 
  rename(Sample.Label = "Transecta")

guess_encoding("muestreos_ms.csv")
library(lubridate)
muestreos <- read_delim("muestreos_ms.csv", locale = locale(decimal_mark = ",",
                                                          grouping_mark = "."),
                      delim = ";",
                      col_types = cols(
                        est = col_factor(NULL),
                        muestreo = col_double(),
                        anio = col_double()
                      )) %>% 
  rename(Muestreo = "muestreo") %>% 
  mutate(fecha = make_date(day=dia1,month=mes1,year=anio),
         season = case_when(
    mes1 %in% c(12,1) ~ "Summer",
    mes1 %in% c(3, 4) ~ "Autumn",
    mes1 %in% c(6, 7, 8) ~ "Winter",
    mes1 %in% c(9,10) ~ "Spring"
  ))

guess_encoding("estratos_ms.csv")
estratos <- read_csv("estratos_ms.csv", locale = locale(decimal_mark = ","),
                  col_types = cols(
                    Region.Label = col_factor(NULL),
                    AreaKm2 = col_double()
                  ))

# Tengo 5 archivos:
# datos completos
# tabla de datos de transectas hasta 2014
# tabla de datos de transectas desde 2015
# tabla de datos de muestreos
# tabla de datos de estratos

# Cuentas generales
gral <- data %>% 
  filter(Especie == "G") %>% 
  #filter(Muestreo > 19) %>% 
  group_by(Muestreo) %>% 
  summarise(grupos = sum(size > 0,na.rm = T), # sum(size con cond) es como usar recuento n()
            guanacos = sum(size, na.rm = T), 
            adultos = sum(AdultJuv,na.rm = TRUE), chu = sum(Crias,na.rm = TRUE), 
            size_grupo = guanacos/grupos , ratio = chu/adultos,
            esfuerzo = sum(unique(Effort, na.rm = T),na.rm = T),
            gr.CPUE = grupos/esfuerzo,gua.CPUE = guanacos/esfuerzo,
            ad.CPUE = adultos/esfuerzo, chu.CPUE = chu/esfuerzo) %>% 
  left_join(muestreos, by = "Muestreo") %>% 
  select(-dia1, -mes1, -est) %>% 
  mutate(season = factor(season, levels = c("Summer","Autumn","Winter","Spring"), ordered = T))

class(gral$season)

sum(gral$grupos) # desde 2015: 4008 # completo: 5859
mean(gral$gr.CPUE) # completo 1.66 gr/km recorrido
sum(gral$guanacos) # desde 2015: 34423 # completo: 51989
mean(gral$gua.CPUE) # completo 14.73527 gua/km recorrido
sum(gral$esfuerzo) # completo 3869.92 km recorridos
sum(gral$chu) # completo: 4233 chules
(4233/51989)*100 # 8.14% crías

# sobre tamaño de grupo
mean(gral$size_grupo) # 9.99 media de medias contando todo, tmb grupos de 1
data %>% filter(Especie == "G",size>0) %>% summarise(mean(size)) # 8.87 registros contando grupos de 1 
data %>% filter(Especie == "G",size>1) %>% summarise(mean(size)) # 12.9 registros si más de 2 juntos
solos <- data %>% filter(Especie == "G") %>% 
  group_by(Muestreo) %>% 
  summarise(conteo_solos=sum(size==1,na.rm = T),gr_y_solos=n(),rel=conteo_solos/gr_y_solos) %>% 
  left_join(muestreos,by="Muestreo")
ggplot(solos)+
  geom_point(aes(x=Muestreo,y=rel))+
  facet_wrap(vars(season))
ggplot(solos)+
  geom_boxplot(aes(factor(season,levels=c("Summer","Autumn","Winter","Spring")),rel),
               outlier.colour = "darkgray", outlier.shape = 1, fill="lightgrey")+
  scale_x_discrete(labels=c("verano","otoño","invierno","primavera"))+
  theme_minimal()+
  ylim(c(0,NA))+
  xlab("Estación")+ylab("Proporción registros de individuos solos")


# tabla para crías
crias <- gral %>% 
  group_by(season) %>% 
  summarise(sum = sum(chu.CPUE), media = mean(chu.CPUE))

# figura ratio chule/adultjuv por muestreo
season_periods <- data.frame(
  season = c("Summer","Summer", "Autumn", "Winter","Spring"),
  start_day = c(1,336,60,152,244),  # Approximate day of the year for each season start
  end_day = c(31,365,120,243,304)   # Approximate day of the year for each season end
)

generate_season_rects <- function(year_range, season_periods) {
  rects <- data.frame()
  for (year in year_range) {
    for (i in 1:nrow(season_periods)) {
      start_date <- as.Date(paste0(year, "-", season_periods$start_day[i], "-01"), format = "%Y-%j")
      end_date <- as.Date(paste0(year, "-", season_periods$end_day[i], "-01"), format = "%Y-%j")
      if (season_periods$end_day[i] < season_periods$start_day[i]) {
        end_date <- end_date + months(1) - days(1)
      }
      rects <- rbind(rects, data.frame(
        xmin = start_date,
        xmax = end_date,
        ymin = -Inf,
        ymax = Inf,
        fill = season_periods$season[i]
      ))
    }
  }
  return(rects)
}

years <- unique(year(gral$fecha))
season_rects <- generate_season_rects(years, season_periods)


# figura ratio chu/adultjuv by tiempo
ggplot()+
  geom_point(data = gral,aes(y = ratio, x=fecha))+
  geom_rect(data = season_rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.2) +
  scale_fill_manual(values = c("Winter" = "blue", "Summer" = "green", "Autumn" = "orange","Spring" = "red"))+
  theme_gray()

ggplot()+
  geom_point(data = gral,aes(y = ratio, x=fecha))+
  geom_rect(data = season_rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.2) +
  scale_fill_manual(values = c("Winter" = "blue", "Summer" = "green", "Autumn" = "orange","Spring" = "red"))+
  theme_gray()+
  facet_wrap(vars(season))

# y boxplot por estaciones
ggplot(gral,aes(season,ratio))+
  geom_boxplot(outlier.colour = "darkgray", outlier.shape = 1, fill="lightgrey")+
  scale_x_discrete(labels=c("verano","otoño","invierno","primavera"))+
  xlab("Estación")+ylab("Razón crías/adultos")+
  theme_minimal()


# figura chu y adultjuv por UE by time
ggplot()+
  geom_point(data = gral,aes(y = ad.CPUE, x=fecha),shape=15, size=2)+
  geom_point(data = gral,aes(y = chu.CPUE, x=fecha))+
  geom_rect(data = season_rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.2) +
  scale_fill_manual(values = c("Winter" = "blue", "Summer" = "green", "Autumn" = "orange","Spring" = "red"))+
  theme_gray()  


# figura size por muestreo
ggplot()+
  geom_point(data=gral,aes(y = guanacos, x=fecha),shape=18, size=3)+
  geom_point(data=gral,aes(y = grupos,x=fecha))+
  geom_rect(data = season_rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.2) +
  scale_fill_manual(values = c("Winter" = "blue", "Summer" = "green", "Autumn" = "orange","Spring" = "red"))+
  theme_gray()

ggplot()+
  geom_point(data=gral,aes(y = gua.CPUE, x=fecha),shape=18, size=3)+
  geom_point(data=gral,aes(y = size_grupo,x=fecha))+
  geom_rect(data = season_rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.2) +
  scale_fill_manual(values = c("Winter" = "blue", "Summer" = "green", "Autumn" = "orange","Spring" = "red"))+
  theme_gray()

# figura tamaño grupos
ggplot()+
  geom_point(data=gral,aes(y=size_grupo,x=fecha))+
  ylim(c(0,18))+
  geom_rect(data = season_rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.2) +
  scale_fill_manual(values = c("Winter" = "blue", "Summer" = "green", "Autumn" = "orange","Spring" = "red"))+
  theme_gray()

ggplot()+
  geom_point(data=gral,aes(y=size_grupo,x=fecha))+
  ylim(c(0,18))+
  geom_rect(data = season_rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.2) +
  scale_fill_manual(values = c("Winter" = "blue", "Summer" = "green", "Autumn" = "orange","Spring" = "red"))+
  theme_gray()+
  facet_wrap(vars(season))


left_join(data,muestreos,by = "Muestreo") %>%
  filter(Especie == "G",size > 1 ) %>% 
  ggplot()+
  geom_boxplot(aes(y=size,x=Muestreo, group = Muestreo),
               outlier.colour = "darkgray", outlier.shape = 1, fill = "lightgray")+
  ylim(c(0,254))+ #máximo size guanacos 254
  theme_minimal()+
  facet_wrap(vars(season))

left_join(data,muestreos,by = "Muestreo") %>%
  filter(Especie == "G",size > 1 ) %>% 
  ggplot()+
  geom_boxplot(aes(y=size,x=Muestreo, group = Muestreo),
               outlier.colour = "pink", outlier.shape = 19, fill = "lightgray")+
  ylim(c(0,15))+
  theme_minimal()+
  facet_grid(~factor(season,levels=c("Summer","Autumn","Winter","Spring")))

# boxplot con datos originales en data
left_join(data,muestreos,by = "Muestreo") %>%
  filter(Especie == "G",size > 1 ) %>% 
  ggplot(aes(season,size))+
  geom_boxplot(outlier.colour = "darkgray", outlier.shape = 1, fill = "lightgray")+
  #scale_y_continuous(limits = c(0,30))+
  scale_x_discrete(labels=c("verano","otoño","invierno","primavera"))+
  xlab("Estación")+ylab("Tamaño de grupo")+
  theme_minimal()

#boxplot con datos ya interpretados por muestreo
ggplot(gral,aes(season,size_grupo))+
  geom_boxplot(outlier.colour = "darkgray", outlier.shape = 1, fill = "lightgray")+
  scale_y_continuous(limits = c(0,20))+
  scale_x_discrete(labels=c("verano","otoño","invierno","primavera"))+
  xlab("Estación")+ylab("Tamaño de grupo")+
  theme_minimal()


# esfuerzo muestreo y transecta
# esfuerzo recorrido
esf_detal <- data %>%
  filter(Especie=="G") %>% 
  group_by(Muestreo,Sample.Label) %>% summarise(esf=mean(Effort,na.rm = T))

esf_muestreo <- esf_detal %>% 
  group_by(Muestreo) %>% summarise(eff = sum(esf,na.rm = T)) %>%  
  left_join(muestreos,by="Muestreo") %>% 
  select(Muestreo, eff, fecha,season)


sum(esf_muestreo$eff) # 4934.32
range(esf_muestreo$eff) # entre 35.17 y 153.5
mean(esf_muestreo$eff) #120.35 km

# figura de timeline

ggplot(muestreos, aes(x = month(fecha), y = year(fecha))) +
  geom_tile(aes(fill = season),color = "white", width = 1) +  # Create tiles for each event
  geom_text(aes(label = Muestreo), vjust = 0.5, hjust = 0.5) +  # Add event labels inside tiles
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  scale_fill_manual(values = c("Summer" = "orange", "Autumn" = "gold", 
                               "Winter" = "skyblue", "Spring" = "lightgreen")) +
  scale_y_reverse(breaks = seq(2007,2023, by=1)) +
  geom_hline(yintercept = 2014.55,linetype = "dashed", col = "darkgrey",linewidth=1)+
  labs(title = "Monitoreo de guanacos PNML - 2007 a 2023",
       x = "Mes",y = "Año", fill= "Season") +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),  # Remove grid lines for clarity
        axis.text.y = element_text(size = 10)) 



data_G <- data %>% 
  filter(Especie == "G")

# exploro qué pasa con las zonas alta y baja
# si hay registros suficientes como para tratarlo como áreas diferentes
registros <- data_G %>% 
  filter(size > 0) %>% 
  group_by(Muestreo, Region.Label) %>% 
  summarise(
    n = n()
  )

registros %>% filter(Region.Label== "BAJO")
registros %>% filter(n < 30)

# se ve que no alcanzan casi nunca los registros en el bajo
# por eso reformulo tabla:
# Region.Label pasa a ser ML con el área total
# los estratos pasan a ser la columna stratum y eventualmente: formula = ~stratum

boxplot(data_G$distance~data_G$stratum, xlab="estrato", ylab="Distance (m)") # completo
boxplot(data_G$distance~data_G$Muestreo, xlab="sampling event", ylab="Distance (m)") # completo

ggplot(data_G,aes(distance, stratum))+
  geom_vline(xintercept = 400,col="pink",lty=2,lwd=1.5)+
  geom_boxplot(outlier.colour = "darkgray", outlier.shape = 1, 
               position = position_dodge(.9))+
  scale_x_continuous(limits = c(NA,999))+
  xlab("distancia (m)")+
  theme_minimal()

ggplot(data_G,aes(distance, muestreo))+
  geom_vline(xintercept = 400,col="pink",lty=2,lwd=1.5)+
  geom_boxplot(outlier.colour = "darkgray", outlier.shape = 1, 
               position = position_dodge(.9))+
  scale_x_continuous(limits = c(NA,999))+
  xlab("distancia (m)")+
  theme_minimal()


ggplot(data_G,aes(distance, muestreo, fill = stratum))+
 geom_boxplot(outlier.colour = "darkgray", outlier.shape = 1, 
              position = position_dodge(.9))+
  scale_x_continuous(limits = c(NA,999))+
  theme_minimal()



  
# Se ve que no hay una diferencia clara en lo que pasa en los estratos
# a priori no parece necesario estratificar
# deja con menos datos cada parte, y peor estimación
