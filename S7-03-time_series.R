##########################################################################
# Series temporales y estadísticas básica 
##########################################################################

## ------------------------------------------------------------------------
# Cargamos la librerías necesarias
list.of.packages <- c("imputeTS", "smooth", "strucchange", "CausalImpact", "tidyverse", "GGally", "highcharter", "xts")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

sessions <- c(266, 333, 307, 213, 213, 147, 201, 147, 300, 274, 244, 179, 
              284, 340, 324, 288, 256, 230, 241, 222, 336, 341, 270, 164, 
              240, 362, 311, 242, 250, 270, 247, 252, 385, 540, 876, 214, 
              307, 398, 404, 324, 351, 413, 398, 351, 468, 412, 407, 356, 
              521, 570, 697)

hist(sessions,probability = T, breaks=20, main="Histograma: Distribución de sesiones del periodo")
curve(dnorm(x, mean(sessions), sd(sessions)), add=TRUE, col="darkblue", lwd=2)

mean(sessions)
abline(v = mean(sessions), col = "royalblue", lwd = 2)

median(sessions)
abline(v = median(sessions), col = "red", lwd = 2)

#cuartiles
quantile(sessions) 

#rango intercuartílico
IQR(sessions) 
quantile(sessions, 3/4) - quantile(sessions, 1/4)


# Outliers ----------------------------------------------------------------
# https://es.wikipedia.org/wiki/Valor_at%C3%ADpico
# Tukey’s definition of an outlier: https://en.wikipedia.org/wiki/Outlier#Tukey's_fences

boxplot(sessions)

q3 <- qnorm(0.75)
q1 <- qnorm(0.25)
iqr <- q3 - q1
r <- c(q1 - 1.5*iqr, q3 + 1.5*iqr)
r


max(sessions)
( max_sessions <- as.numeric(quantile(sessions, probs=c(.75)) + 1.5 * IQR(sessions)) )
min(sessions)
( min_sessions <- as.numeric(quantile(sessions, probs=c(.25)) - 1.5 * IQR(sessions)) )

outliers <- sessions[sessions < min_sessions | sessions > max_sessions]

plot(sessions, col=ifelse(sessions %in% outliers, "red", "blue"))
abline(h = max_sessions, col="red") 

boxplot(sessions)

clean_sessions <- sessions[!sessions %in% outliers]
plot(clean_sessions, pch="*")

hist(sessions)
hist(clean_sessions)

mean(clean_sessions)
median(clean_sessions)


# El tiempor como una variable contínua
mod <- lm(sessions ~ time(sessions))
summary(mod)


mod2 <- lm(clean_sessions ~ time(clean_sessions))
summary(mod2)

plot(clean_sessions, ylim = c(0, 900))
abline(mod2, col="red")


# Comparamos ambos modelos
par(mfrow=c(1, 2))
plot(sessions, ylim = c(0, 900), main="Con outliers")
abline(mod, col="red", lwd=3, lty=2)
plot(clean_sessions, ylim = c(0, 900), main="Sin outliers")
abline(mod2, col="red", lwd=3, lty=2)
dev.off()


##########################################################################
# Series temporales: modelar el tiempo ------------------------------------
# 
# Una serie temporal se define como una colección de observaciones (discretas o continuas) de una variable recogidas secuencialmente en el tiempo.
# 
# El tiempo es una elemento natural presente en el momento en el que se genera el dato.
# Serie es la característica fundamental de las series temporales. Quiere decir que una observación presente es influenciada por los valores pasados de la misma (Auto correlación).
# Lo modelos de series temporales usarán esta característica para predecir valores futuros.
# 
# 
# Objetivos del análisis de series temporales
# El principal objetivo es el de elaborar un modelo estadístico que describa la procencia de dicha serie.
# 
# Descriptivos: La dibujamos y consideramos sus medidas descriptivas básicas. ¿Presentan una tendencia?. ¿Existe estacionalidad?. ¿Hay valores extremos?
#   
# Predictivos: En ocasiones no sólo se trata de explicar los sucedido sino de ser capaces de predecir el futuro.
##########################################################################

# sessions con outliers
(sessions_ts <- ts(data = sessions, start = c(2014, 01), frequency = 12))


max_sessions <- quantile(sessions_ts, probs=c(.75)) + 1.5 * IQR(sessions_ts)
min_sessions <- quantile(sessions_ts, probs=c(.25)) - 1.5 * IQR(sessions_ts)
plot(sessions_ts, ylim = c(0, 900))
abline(h=max_sessions, col = "green")
abline(h=min_sessions, col = "red")


# Y si trabajamos sin outliers
(clean_sessions_ts <- ts(data = clean_sessions, start = c(2014, 01), frequency = 12))

# Nos faltan meses:
length(sessions_ts) == length(clean_sessions_ts)

# Marcamos los autliers como NA
clean_sessions_ts <- sessions_ts
clean_sessions_ts[clean_sessions_ts %in% outliers] <- NA

plot(sessions_ts, col="blue", lwd=2, lty="dotted")
lines(clean_sessions_ts, col = "red", lwd=2)

# library(imputeTS)
plotNA.distribution(clean_sessions_ts)

clean_sessions_ts
na_mean(clean_sessions_ts)
na_mean(clean_sessions_ts, option = "median")
na_kalman(clean_sessions_ts)

plotNA.imputations(clean_sessions_ts, na.mean(clean_sessions_ts))
plotNA.imputations(clean_sessions_ts, na.mean(clean_sessions_ts, option = "median"))
plotNA.imputations(clean_sessions_ts, na.kalman(clean_sessions_ts))

# Imputamos los NA
clean_sessions_ts <- na.kalman(clean_sessions_ts)
plot(sessions_ts, col="blue", lwd=2, lty="dotted")
lines(clean_sessions_ts, col = "red", lwd=2)

# Nuestra serie definitiva
rm(sessions_ts)
sessions_ts <- clean_sessions_ts
plot(sessions_ts)


# Modelos -----------------------------------------------------------------

# Modelado: Media
sessions.mean <- meanf(sessions_ts, h=12)
summary(sessions.mean)
plot(sessions.mean)

# Modelado: Naive
sessions_ts.naive <- naive(sessions_ts, h=12)
summary(sessions_ts.naive)
plot(sessions_ts.naive)

# Modelado: Media Móvil Simple
# library(smooth)
sessions_ts.sma<-sma(sessions_ts, h = 12, holdout = T, level = .95, ic = 'AIC')
sessions_ts.forecast <- forecast(sessions_ts.sma)
plot(sessions_ts.forecast)


# Componentes de una serie temporal ---------------------------------------
sessions_ts.decomp <- decompose(sessions_ts)
plot(sessions_ts.decomp)


# Serie desestacionalizada ------------------------------------------------
sessions_des.ts  <- sessions_ts-sessions_ts.decomp$seasonal
plot(sessions_ts,main="Serie normal Vs desestacionalizada", col="blue", lwd=2, lty="dotted")
lines(sessions_des.ts, col='red')

# Modelado: naive estacional
sessions_ts.snaive <- snaive(sessions_ts, h = 12)
plot(sessions_ts.snaive)

# Modelado: HoltWinters
sessions_ts.hw <- HoltWinters(sessions_ts)
plot(sessions_ts.hw)

forecast_out_sample <- forecast(sessions_ts.hw, h=12)
plot(forecast_out_sample)


# Cambios estructurales en una serie --------------------------------------

# require(strucchange)
breakpoints(sessions_ts ~ 1)
plot(sessions_ts, main="IPI breakpoints", ylab="Sesiones", col="blue", lwd=1.5)
lines(breakpoints(sessions_ts ~ 1), col="red")
lines(confint(breakpoints(sessions_ts ~ 1), level=0.90), col="red")
text(2015.5, 800, "Agosto 2016", cex=0.75, col="red", pos=4, font=3)

