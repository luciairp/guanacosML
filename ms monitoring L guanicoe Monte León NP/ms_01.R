library(tidyverse)

# archivo datos preparados para ms
data <- read_csv("guanacosML_ms.csv",
                 locale=locale(decimal_mark = ",", grouping_mark = "."),
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
muestreos <- read_csv("muestreos_ms.csv", locale = locale(decimal_mark = ",", grouping_mark = "."),
                      col_types = cols(
                        est = col_factor(NULL),
                        muestreo = col_double(),
                        año = col_double()
                      )) %>% 
  rename(Muestreo = "muestreo") %>% 
  mutate(fecha = make_date(day=dia1,month=mes1,year=año),
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
  filter(Especie == "G", size > 0) %>% 
  filter(Muestreo >19) %>% 
  group_by(Muestreo) %>% 
  summarise(grupos=n(), guanacos = sum(size), 
            adultos = sum(AdultJuv,na.rm = TRUE), chu = sum(Crias,na.rm = TRUE), 
            size_grupo = guanacos/grupos , ratio = chu/adultos) %>% 
  left_join(muestreos, by = "Muestreo") %>% 
  select(-dia1, -mes1, -est)

sum(gral$grupos) #4008
mean(gral$grupos) #182.18
sum(gral$guanacos) #34423
mean(gral$guanacos) #1564.682

# tabla para crías
crias <- gral %>% 
  group_by(season) %>% 
  summarise(sum = sum(chu), media = mean(chu))

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



ggplot()+
  geom_point(data = gral,aes(y = ratio, x=fecha))+
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

# figura tamaño grupos
ggplot()+
  geom_point(data=gral,aes(y=size_grupo,x=fecha))+
  ylim(c(0,18))+
  geom_rect(data = season_rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.2) +
  scale_fill_manual(values = c("Winter" = "blue", "Summer" = "green", "Autumn" = "orange","Spring" = "red"))+
  theme_gray()

# esfuerzo muestreo y transecta
effort <- data %>% 
  filter(Muestreo > 19) %>% 
  group_by(Muestreo, Sample.Label) %>% 
  summarise(eff = max(Effort,na.rm = TRUE))

effort_muestreo <- effort %>% 
  filter(eff !="-Inf") %>% 
  group_by(Muestreo) %>% 
  summarise(eff = sum(eff))

sum(effort_muestreo$eff) #2754.8 Km
range(effort_muestreo$eff) # entre 66.5 y 136.5
mean(effort_muestreo$eff) #125.22 km

# figura de timeline

ggplot(muestreos, aes(x = month(fecha), y = year(fecha))) +
  geom_tile(aes(fill = season),color = "white", width = 1) +  # Create tiles for each event
  geom_text(aes(label = Muestreo), vjust = 0.5, hjust = 0.5) +  # Add event labels inside tiles
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  scale_fill_manual(values = c("Summer" = "orange", "Autumn" = "gold", 
                               "Winter" = "skyblue", "Spring" = "lightgreen")) +
  scale_y_reverse(breaks = seq(2007,2023, by=1)) +
  geom_hline(yintercept = 2014.55,linetype = "dashed", col = "darkgrey",linewidth=1)+
  labs(title = "Sampling events",
       x = "Month",y = "Year", fill= "Season") +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),  # Remove grid lines for clarity
        axis.text.y = element_text(size = 10)) 


# trabajo con muestreo 20 en adelante (2015 en adelante)
data_r <- data %>% 
  filter(Especie == "G") %>% 
  filter(Muestreo > 19)
# exploro qué pasa con las zonas alta y baja
# si hay registros suficientes como para tratarlo como áreas diferentes
registros <- data_r %>% 
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

boxplot(data_r$distance~data_r$stratum, xlab="estrato", ylab="Distance (m)")
boxplot(data_r$distance~data_r$Muestreo, xlab="sampling event", ylab="Distance (m)")
ggplot(data_r,aes(distance, muestreo, fill = stratum))+
 geom_boxplot(outlier.colour = "darkgray", outlier.shape = 1, 
              position = position_dodge(.9))+
  scale_x_continuous(limits = c(NA,999))+
  theme_minimal()
  
# Se ve que no hay una diferencia clara en lo que pasa en los estratos
# a priori no parece necesario estratificar
# deja con menos datos cada parte, y peor estimación
