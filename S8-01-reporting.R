##########################################################################
# Jose Cajide - @jrcajide
# Reporting en Rmarkdown
##########################################################################

# http://rmarkdown.rstudio.com/
# http://rmarkdown.rstudio.com/gallery.html
# https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf
# https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf

install.packages(c("knitr", "rmarkdown"))

library(knitr)

# report1 -----------------------------------------------------------------

browseURL("reports/report1.Rmd")

# https://github.com/joseramoncajide/curso_introduccion_R/blob/master/img/rmarkdown-workflow.png

knit(input = "reports/report1.Rmd", output = "reports/report1.md")

browseURL("reports/report1.md")

knit2html('reports/report1.md', output = "reports/report1.html")

browseURL("reports/report1.html")



# report2 -----------------------------------------------------------------

# Gapminder otra vez ;)

browseURL("reports/report2.Rmd")

rmarkdown::render("reports/report2.Rmd", output_file =  "report2.html")

# ejercicio:
# Modifica reports/report2.Rmd  para que al ejecutarse quede lo más limpio posible. Ayuda en: http://rmarkdown.rstudio.com/lesson-3.html



# report3: Informes con parámetros ----------------------------------------

browseURL("reports/report3.Rmd")

rmarkdown::render("reports/report3.Rmd", params = list(
  year = "1952"
), output_file = "report3.html")

browseURL("reports/report3.html")

# Ejercicio 1:
# Agrega al informe algún otro parámetro para poder filtrar o personalizar el mismo.


# Ejercicio 2: Autogenera un informe por cada año
# Tendrás que obtener un vector con todos los distintos años disponobles en https://raw.githubusercontent.com/joseramoncajide/curso_introduccion_R/master/data/gapminder.csv
# Usando lappy y la función rmarkdown::render podrás generar un informe para cada año dentro de una carpeta. La función paste0 te ayudará a componer el nombre de cada archivo.


