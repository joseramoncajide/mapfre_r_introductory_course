
# report3: Informes con parámetros ----------------------------------------
# SOL:

gDat <- read_csv('https://raw.githubusercontent.com/joseramoncajide/curso_introduccion_R/master/data/gapminder.csv')

anos <- unique(gDat$year)

lapply(anos, function(ano) { print(ano)} )

lapply(anos, function(ano) { rmarkdown::render("reports/report3.Rmd", params = list(
  year = ano
), output_file = paste0("report_", ano,".html"))} )