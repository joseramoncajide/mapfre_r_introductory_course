##########################################################################
# Jose Cajide - @jrcajide
# Curso de introducción a R: Joins
##########################################################################

# https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

library(tidyverse)
library(nycflights13)
flights %>% View()
airlines
airports
planes
weather

# Relaciones: https://github.com/joseramoncajide/curso_introduccion_R/blob/master/img/relational-nycflights.png 
# flights => planes por tailnum
# flights => airlines por carrier
# flights => airports por origin y dest
# flights => weather por origin, year, month, day y hour
# 

# Identificar la claves primarias: Si n > 1 entonces no es clave primaria
planes %>% count(tailnum) %>% 
  filter(n > 1)

flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)

# Joins
flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2

flights2 %>%
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")

flights2 %>% 
  left_join(planes, by = "tailnum", "seats") %>% View()

foo <- flights2 %>% 
  left_join(airports, c("dest" = "faa")) 

summary(foo)

# semi_join: filas de x que tienen correspondencia en y.

airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()



# Extra: Visualización

usairports <- filter(airports, lat < 48.5)
usairports <- filter(usairports, lon > -130)
usairports <- filter(usairports, faa!="JFK") #todos menos JFK
jfk <- filter(airports, faa=="JFK") #sólo JFK

library(maps)
map("world", regions=c("usa"), fill=T, col="white", bg="grey15", ylim=c(21.0,50.0), xlim=c(-130.0,-65.0), main = "Conexiones con JFK")
points(usairports$lon,usairports$lat, pch=3, cex=0.1, col="chocolate1")


# install.packages('geosphere')
library(geosphere)

for (i in (1:dim(usairports)[1])) { 
  inter <- gcIntermediate(c(jfk$lon[1], jfk$lat[1]), c(usairports$lon[i], usairports$lat[i]), n=200)
  lines(inter, lwd=0.1, col="turquoise2")    
}

title(main = list("Conexiones JFK", cex = 1.5,col = "chocolate1", font = 2))
