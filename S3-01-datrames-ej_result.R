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

gapminder <- read_csv('data/gapminder.csv')

# EJ. Vector con los nombres de continentes sin repetir
unique(gapminder$continent)

top_continentes <- gapminder %>% 
  group_by(continent) %>% 
  count() %>% 
  arrange(-n) %>% 
  head(2) %>% 
  pull(continent)

gapminder %>% 
  group_by(continent) %>% 
  count() %>% 
  ungroup() %>% 
  top_n(2) %>% 
  pull(continent) -> top_continentes

# gapminder %>% 
#   select(continent) %>% 
#   distinct()

# top_continentes <- c('Africa', 'Asia')

gapminder %>% 
  filter(continent %in% top_continentes)

# Ej. Número de continentes
length(unique(gapminder$continent))

# Tabla de frecuencias con el número de observaciones por continente
gapminder %>% 
  group_by(continent) %>% 
  count()

table(gapminder$continent)

# ¿Qué países de Europa han tenido la menor y la mayor esperanza media de vida durante todo el periodo? 
gapminder %>% 
  filter(continent == 'Europe') %>% 
  group_by(country) %>% 
  summarise(mean_lifeExp = mean(lifeExp)) %>% 
  filter(mean_lifeExp == min(mean_lifeExp) | mean_lifeExp == max(mean_lifeExp))

# Y de Europa en el último año con datos (Intenta no poner el año de forma manual)
gapminder %>% 
  filter(continent == 'Europe', year == max(year)) %>% 
  group_by(country) %>% 
  summarise(mean_lifeExp = mean(lifeExp)) %>% 
  filter(mean_lifeExp == min(mean_lifeExp) | mean_lifeExp == max(mean_lifeExp))

# Obtén la media y la desviación típica de gdpPercap y pop por continete y año de los años 1952 y 2007
# sd(gapminder$gdpPercap)
gapminder %>% 
  filter(year %in% c(1952, 2007)) %>% 
  group_by(continent, year) %>% 
  summarise(mean_gdpPercap = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap),
            mean_pop = mean(pop),
            sd_pop = sd(pop))

gapminder %>% 
  ggplot(aes(x=continent, y=lifeExp)) + 
  geom_boxplot()


#Ej. Cuantas observaciones hay por continente?
gapminder %>% 
  group_by(continent) %>% 
  summarise(n = n())

# El resultado tiene que coincidir con:
table(gapminder$continent)


# Ej. Cuántas observaciones y cuántos países hay por continente?
gapminder %>% 
  group_by(continent) %>% 
  summarise(n = n(),
            n_countries = n_distinct(country))


# Ej. Calcula el gdp (PIB) por cada fila del dataset
gapminder %>% 
  mutate(pib = pop * gdpPercap) %>% 
  ggplot(aes(x=continent, y=pib)) + 
  geom_boxplot(outlier.shape = NA) 



#Ej. Calcula el incremento de la esperanza de vida desde 1952 hasta el 2007 para España 

summary(gapminder)

gapminder %>% 
  filter(country == 'Spain') %>% 
  arrange(-year) %>% 
  mutate(diferencia = lifeExp - last(lifeExp))

