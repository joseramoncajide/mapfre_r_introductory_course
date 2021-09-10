##########################################################################
# Jose Cajide - @jrcajide
# Curso de introducción a R: Fuentes de datos
##########################################################################

rm(list=ls()) 

library(tidyverse)

#----------------------------------------------------------------------------
# Obtención de datos desde Excel
#----------------------------------------------------------------------------

library(readxl)

url_archivo <- "http://datos.madrid.es/egob/catalogo/207831-4-accidentes-trafico.xls"

(nombre_archivo <- basename(url_archivo))

(destino <- paste0("data/", nombre_archivo))

# Mejor: 

(destino <- file.path("data", nombre_archivo))

download.file(url = url_archivo, destfile = destino)

accidentes_coche <- read_excel(destino, 
                               sheet = "2016",
                               skip = 7 )
head(accidentes_coche)

tail(accidentes_coche)

accidentes_coche <- head(accidentes_coche, -1)

tail(accidentes_coche)

names(accidentes_coche)

names(accidentes_coche) <- make.names(names(accidentes_coche))

names(accidentes_coche)[2:8] <- c("Lun", "Mar", "Mie", "Jue", "Vie", "Sab", "Dom")

accidentes_coche$Nº.Accidentes...9 <- NULL

accidentes_coche

accidentes_coche <- accidentes_coche %>% gather(key   = dia_semana,
                            value = accidentes,
                            2:ncol(accidentes_coche))

accidentes_coche %>%
  ggplot( aes(dia_semana, Rango.Horario)) + 
    geom_tile(aes(fill = accidentes),colour = "white") + 
    scale_fill_gradient(low = "white", high = "steelblue")


#----------------------------------------------------------------------------
# Obtención de datos desde XML
#----------------------------------------------------------------------------

library(xml2)
alojamientos_madrid <- read_xml("http://datos.madrid.es/egob/catalogo/300032-10037102-turismo-alojamientos.xml")

alojamientos_madrid %>%  xml_name()
alojamientos_madrid %>% xml_children()
alojamientos_madrid %>% xml_text()

nombres <- alojamientos_madrid %>% xml_find_all(".//name") %>% xml_text()
emails <- alojamientos_madrid %>% xml_find_all(".//email") %>% xml_text()
lat <- alojamientos_madrid %>% xml_find_all(".//latitude") %>% xml_text()
lon <- alojamientos_madrid %>% xml_find_all(".//longitude") %>% xml_text()

alojamientos_madrid <- tibble(nombres = nombres,
       emails = emails,
       lat = as.numeric(lat),
       lon=as.numeric(lon))

library(leaflet)

leaflet(alojamientos_madrid) %>%
  setView(lng = -3.7037902, lat = 40.4167754, zoom = 12) %>% 
  addTiles() %>% 
  addMarkers(lng = ~lon, lat = ~lat)


#----------------------------------------------------------------------------
# Exportar un dataframe
#----------------------------------------------------------------------------

gapminder <- read_csv("https://raw.githubusercontent.com/joseramoncajide/mapfre_r_introductory_course/master/data/gapminder.csv")

gapminder_por_continente <- split(gapminder, gapminder$continent) 

names(gapminder_por_continente)

gapminder_por_continente$Africa

sapply(names(gapminder_por_continente), print)

dir.create("archivos_gapminder")

# EJEMPLO: Exportar dataframe a csv
write.csv(gapminder, "archivos_gapminder/gapminder.csv")

# EJEMPLO: Archivo a generar
file.path("archivos_gapminder", paste0("CONTINENTE", ".csv"))

# EJEMPLO: lapply(vector, función)
lapply( names(gapminder_por_continente), 
        function(x) print(paste("Continente:",x))
        )

lapply(names(gapminder_por_continente),
       function (x)
         write.csv(
           gapminder_por_continente[[x]],
           row.names = F,
           file = file.path("archivos_gapminder", paste0(x, ".csv"))
         ))


#----------------------------------------------------------------------------
# Leer múltiples archivos de un directorio
#----------------------------------------------------------------------------

# EJEMPLO: list.files
list.files(path ="archivos_gapminder", pattern = "^A.*a\\.csv", full.names = FALSE)

(archivos_continentes <- list.files(path ="./archivos_gapminder", pattern = "[A-Z].*?", all.files = TRUE, full.names = TRUE))


gapminder_df <- archivos_continentes %>%
  map(read_csv) %>% 
  reduce(rbind) 

gapminder_df


plot(gapminder_df$gdpPercap, gapminder_df$lifeExp)

gapminder_df %>% 
  group_by(continent) %>%
  group_map(~plot(.x$gdpPercap, .x$lifeExp))


#----------------------------------------------------------------------------
# Bases de datos
#----------------------------------------------------------------------------

# library(gapminder)
# data(gapminder)

install.packages("RSQLite") 
library("RSQLite")

sqlite    <- dbDriver("SQLite")
db <- dbConnect(sqlite,"data/base_de_datos.db")

dbWriteTable(db,"gapminder", gapminder_df)

dbListTables(db)

dbListFields(db, "gapminder")

dbGetQuery(db,  "SELECT * FROM gapminder WHERE country = 'Spain'")

dbSendQuery(db, "CREATE TABLE spain AS  SELECT * FROM gapminder WHERE country = 'Spain'")

dbListTables(db)

dbGetQuery(db,  "SELECT * FROM spain") %>% 
  ggplot(aes(x=year, y=pop)) + geom_bar(stat = 'identity') 

dbDisconnect(db)

#----------------------------------------------------------------------------
# Bases de datos Big Data
#----------------------------------------------------------------------------

# https://console.developers.google.com/billing/freetrial?hl=en

library(bigrquery)
project <- "cpb100-162913" 
sql <- "SELECT contributor_username,title, count(contributor_username) as num_revisions
FROM [publicdata:samples.wikipedia]
WHERE title CONTAINS 'España'
GROUP BY contributor_username, title
HAVING num_revisions > 50
ORDER BY num_revisions DESC"
query_exec(sql, project = project)

