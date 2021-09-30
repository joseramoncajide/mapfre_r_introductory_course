##########################################################################
# Jose Cajide - @jrcajide
##########################################################################

# Algunos paquetes traen conjuntos de datos
# install.packages('gapminder')
library(gapminder)
gapminder

# Motivación
library(tidyverse)
gapminder %>% 
  ggplot(aes(year, lifeExp, group = country, color = continent)) +
  geom_line(alpha = 1/3) 



# importación de datos ----------------------------------------------------
# install.packages('tidyverse')
library(tidyverse)

# Importa en el objeto "gapminder" el archivo que está en "data/gapminder.csv"

gapminder <- 

# EJ. Vector con los nombres de continentes si repetir


# Ej. Número de continentes


# Tabla de frecuencias con el número de observaciones por continente


# ¿Qué países de Europa han tenido la menor y la mayor esperanza media de vida durante todo el periodo? 


# Y de Europa en el último año con datos (Intenta no poner el año de forma manual)


# Obtén la media y la desviación típica de gdpPercap y pop por continete y año de los años 1952 y 2007


#Ej. Cuantas observaciones hay por continente?

# El resultado tiene que coincidir con:
table(gapminder$continent)


# Ej. Cuántas observaciones y cuántos países hay por continente?



# Ej. Calcula el gdp (PIB) por cada fila del dataset



#Ej. Calcula el incremento de la esperanza de vida desde 1952 hasta el 2007 para España 





