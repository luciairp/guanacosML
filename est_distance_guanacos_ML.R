
library("Distance")
data <- read.csv("2020guanacosML.csv",sep=",",dec = ",",header=T)
data$Region.Label <- as.factor(data$Region.Label)
data$Sample.Label <- as.factor(data$Sample.Label)
data$Effort <- as.numeric(data$Effort)
data$size <- as.numeric(data$size)
data$distance <- abs(sin(data$Angle)*data$RadialDistance)
alto <- data[data$Region.Label=="ALTO",]
bajo <- data[data$Region.Label=="BAJO",]

hist(data$distance)
hist(alto$distance)
hist(bajo$distance)

#halfnormal, cosine adjustements
halfnorm.data <- ds(data, key="hn", adjustment="cos", truncation = 500, dht.group = F)
plot(halfnorm.data)
gof_ds(halfnorm.data)
summary(halfnorm.data)

halfnorm.alto <- ds(alto, key="hn", adjustment="cos",truncation = 500, dht.group = F)
plot(halfnorm.alto)
gof_ds(halfnorm.alto)
summary(halfnorm.alto)

halfnorm.bajo <- ds(bajo, key="hn", adjustment="cos", dht.group = F)
plot(halfnorm.bajo)
gof_ds(halfnorm.bajo)
summary(halfnorm.bajo)

#uniform, cosine adjustments
uniform.data <- ds(data, key="unif", adjustment="cos", truncation = 500, dht.group = F)
plot(uniform.data)

uniform.alto <- ds(alto, key="unif", adjustment="cos",truncation = 500, dht.group = F)
plot(uniform.alto)
gof_ds(uniform.alto)
summary(uniform.alto)

uniform.bajo <- ds(bajo, key="unif", adjustment="cos",truncation = 201, dht.group = F)
plot(uniform.bajo)
gof_ds(uniform.bajo)
summary(uniform.bajo)

#hazard-rate, simple polynomial adjustments
hazard.data <- ds(data, key="hr",  adjustment="poly", truncation = 25, dht.group = F)
plot(hazard.data)

AIC(halfnorm.data)
AIC(hazard.data)
AIC(uniform.data)

fit.test <- ddf.gof(halfnorm.data$ddf)
(data_qq <- gof_ds(halfnorm.data))
gof_ds(halfnorm.data)
summary(halfnorm.data)




gof_ds(uniform.data)
gof_ds(halfnorm.data)
gof_ds(hazard.data)

knitr::kable(summarize_ds_models(halfnorm.data, halfnorm.alto),
             digits=3,
             caption="Model comparison table for guanaco line transect data, Monte León.")


