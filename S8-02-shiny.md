Introducción a Shiny
================
@jrcajide

Primera app: Todo dentro de un mismo archivo
============================================

``` r
# Archivo app.R
library(shiny)

ui <- fluidPage()

server <- function(input, output) {}

# Esta debe ser la última línea
shinyApp(ui = ui, server = server)
```

-   Crea un subdirectorio app1 dentro de tu directorio de trabajo.
-   Crea un archivo `app.R` dentro del mismo con el anterior codigo
-   Inicia la aplicación ejectando el siguiente código:

``` r
shiny::runApp('app1/')
```

Primera app: Servidor e interfaz de usuario por separado.
=========================================================

**`server.r`**

``` r
library(shiny)
server <- function(input, output) {}
```

**`ui.r`**

``` r
ui <- fluidPage()
```

V.1 Comenzamos nuestra app.
===========================

------------------------------------------------------------------------

#### ui.R

`shinyUI`

``` r
ui <- fluidPage(
)
```

Agregamos elementos:

``` r
ui <- fluidPage(
  titlePanel("Evolución de la experanza media de vida (lifeExp)")
)
```

#### server.R

`shinyServer`

``` r
server <- function(input, output) {
}
```

*Para ejecutar*

``` r
library(shiny)  
runApp("directorio-app")
```

V.2
===

#### ui.R

-   sidebarLayout: <http://shiny.rstudio.com/articles/layout-guide.html>
-   Shiny UI: <https://shiny.rstudio.com/tutorial/lesson2/>

``` r
ui <- fluidPage(
  titlePanel("Evolución de la experanza media de vida (lifeExp)"),

  sidebarLayout(
    sidebarPanel("Espacio para los controles de la aplicación"),
    mainPanel("Espacio para el gráfico")
  )
)
```

#### server.R

``` r
server <- function(input, output) {
}
```

V.3 Cargando datos
==================

#### server.R

``` r
library(tidyverse)

gDat <- read_csv(file = "data/gapminder.csv")

gDat$country <- as.factor(gDat$country)

server <- function(input, output) {
}
```

Para mostrar los datos:

``` r
library(tidyverse)

gDat <- read_csv(file = "data/gapminder.csv")

gDat$country <- as.factor(gDat$country)

server <- function(input, output) {

  output$gapminder_table <- renderTable({ gDat })
}
```

#### ui.R

``` r
ui <- fluidPage(
  titlePanel("Evolución de la experanza media de vida (lifeExp)"),

  sidebarLayout(
    sidebarPanel("Espacio para los controles de la aplicación"),
    mainPanel("Espacio para el gráfico", 
              tableOutput("gapminder_table")
              )
  )
)
```

V.4 Añadiendo controles
=======================

<http://shiny.rstudio.com/gallery/widget-gallery.html>

#### ui.R

``` r
ui <- fluidPage(
  titlePanel("Evolución de la experanza media de vida (lifeExp)"),

  sidebarLayout(
    sidebarPanel("Espacio para los controles de la aplicación",
                 selectInput("select_country", 
                             label = "País",
                             choices = list("Chad", 
                                            "Iraq", 
                                            "Mali")
                 )
    ),
    mainPanel("Espacio para el gráfico",
              tableOutput("gapminder_table")
    )
  )
)
```

V.5 Haciendo que nuestra aplicación responda a los controles
============================================================

#### server.R

``` r
library(tidyverse)

gDat <- read_csv(file = "data/gapminder.csv")

gDat$country <- as.factor(gDat$country)

server <- function(input, output) {

  output$gapminder_table <- renderTable({ 
    gDat
  })
  output$output_country <- renderText({
    paste("Mostrando datos de ", input$select_country)
  })
}
```

#### ui.R

``` r
ui <- fluidPage(
  titlePanel("Evolución de la experanza media de vida (lifeExp)"),

  sidebarLayout(
    sidebarPanel("Espacio para los controles de la aplicación",
                 selectInput("select_country", 
                             label = "País",
                             choices = list("Chad", 
                                            "Iraq", 
                                            "Mali")
                 )
    ),
    mainPanel("Espacio para el gráfico",
              textOutput("output_country"),
              tableOutput("gapminder_table")
    )
  )
)
```

#### server.R

``` r
library(tidyverse)

gDat <- read_csv(file = "data/gapminder.csv")

gDat$country <- as.factor(gDat$country)

server <- function(input, output) {

  output$gapminder_table <- renderTable({ 
    # subset(gDat, country == input$select_country)
    gDat %>% filter(country == input$select_country)
  })
  
  output$output_country <- renderText({
    paste("Mostrando datos de ", input$select_country)
  })
  
}
```

Añadiendo un slider para elegir el año
======================================

#### ui.R

``` r
ui <- fluidPage(
  titlePanel("Evolución de la experanza media de vida (lifeExp)"),

  sidebarLayout(
    sidebarPanel("Espacio para los controles de la aplicación",
      selectInput("select_country", 
                  label = "País",
                  choices = list("Chad", 
                                 "Iraq", 
                                 "Mali")
                  ),
      sliderInput("year_range", 
                  label = "Elije un periodo:",
                  min = 1952, max = 2007, value = c(1955, 2005), format = "####")
    ),
    mainPanel("Espacio para el gráfico",
              textOutput("output_country"),
              tableOutput("gapminder_table")              
    )
  )
)
```

#### server.R

Filtrando los datos de la tabla por el slider

``` r
library(tidyverse)

gDat <- read_csv(file = "data/gapminder.csv")

gDat$country <- as.factor(gDat$country)

server <- function(input, output) {

  output$gapminder_table <- renderTable({ 
    # subset(gDat, country == input$select_country & year >= input$year_range[1] & year <= input$year_range[2] )
    gDat %>% filter(country == input$select_country & year >= input$year_range[1] & year <= input$year_range[2])
  })
  output$output_country <- renderText({
    paste("Mostrando datos de ", input$select_country)
  })
}
```

Debug
=====

``` r
library(shiny)
runApp('app-gapminder', display.mode= "showcase")
```

*Qué ocurre en el backend*

#### server.R

-   `str(input$select_country)`
-   `cat(input$select_country)`

``` r
library(tidyverse)

gDat <- read_csv(file = "data/gapminder.csv")

gDat$country <- as.factor(gDat$country)

server <- function(input, output) {

  output$gapminder_table <- renderTable({ 
    str(input$select_country)
    gDat %>% filter(country == input$select_country & year >= input$year_range[1] & year <= input$year_range[2])
  })
  output$output_country <- renderText({
    paste("Mostrando datos de ", input$select_country)
  })
}
```

V.6 Gráfico
===========

#### server.R

Vemos que tanto para la tabla como para el gráfico estamos usando el mismo filtro:

``` r
library(tidyverse)

gDat <- read_csv(file = "data/gapminder.csv")

gDat$country <- as.factor(gDat$country)

server <- function(input, output) {

  output$gapminder_table <- renderTable({ 
    gDat %>% filter(country == input$select_country & year >= input$year_range[1] & year <= input$year_range[2])
  })
  
  output$output_country <- renderText({
    paste("Mostrando datos de ", input$select_country)
  })
  
  output$ggplot_lifeExp <- renderPlot({
    p <-  ggplot(subset(gDat, country == input$select_country & year >= input$year_range[1] & year <= input$year_range[2]), aes(x = year, y = gdpPercap))
    p + geom_point()
  })
}
```

Vamos a tratar de limpiar el código:

``` r
library(tidyverse)

gDat <- read_csv(file = "data/gapminder.csv")

gDat$country <- as.factor(gDat$country)

server <- function(input, output) {

  one_country_data  <- gDat %>% filter(country == input$select_country & year >= input$year_range[1] & year <= input$year_range[2])
  
  output$gapminder_table <- renderTable({ 
    one_country_data
  })
  
  output$output_country <- renderText({
    paste("Mostrando datos de ", input$select_country)
  })
  
  output$ggplot_lifeExp <- renderPlot({
    p <-  ggplot(one_country_data, aes(x = year, y = gdpPercap))
    p + geom_point()
  })
}
```

Error in .getReactiveEnvironment()$currentContext()

Lo solucionamos: one\_country\_data -&gt; one\_country\_data()

``` r
library(tidyverse)

gDat <- read_csv(file = "data/gapminder.csv")

gDat$country <- as.factor(gDat$country)

server <- function(input, output) {

  one_country_data  <- reactive({
    gDat %>% filter(country == input$select_country & year >= input$year_range[1] & year <= input$year_range[2])
  })
  
  output$gapminder_table <- renderTable({ 
    one_country_data()
  })
  
  output$output_country <- renderText({
    paste("Mostrando datos de ", input$select_country)
  })
  
  output$ggplot_lifeExp <- renderPlot({
    p <-  ggplot(one_country_data(), aes(x = year, y = gdpPercap))
    p + geom_point()
  })
}
```

V.7 Listado dinámico de países
==============================

``` r
output$choose_country <- renderUI({
    selectInput("country_from_gapminder", "Selecciona un país:", as.list(levels(gDat$country)))
  })
```

#### server.R

``` r
library(tidyverse)

gDat <- read_csv(file = "data/gapminder.csv")

gDat$country <- as.factor(gDat$country)

server <- function(input, output) {

  one_country_data  <- reactive({
    if(is.null(input$country_from_gapminder)) {
      return(NULL)
    }
    gDat %>% filter(country == input$country_from_gapminder & year >= input$year_range[1] & year <= input$year_range[2])
  })
  
  
  output$choose_country <- renderUI({
    selectInput("country_from_gapminder", "Selecciona un país:", as.list(levels(gDat$country)))
  })
  
  output$gapminder_table <- renderTable({ 
    one_country_data()
  })
  
  output$output_country <- renderText({
    paste("Mostrando datos de ", input$country_from_gapminder)
  })
  
  output$ggplot_lifeExp <- renderPlot({
    p <-  ggplot(one_country_data(), aes(x = year, y = gdpPercap))
    p + geom_point()
  })
}
```

#### ui.R

``` r
ui <- fluidPage(
  titlePanel("Evolución de la experanza media de vida (lifeExp)"),

  sidebarLayout(
    sidebarPanel("Espacio para los controles de la aplicación",
      uiOutput("choose_country"),
      sliderInput("year_range", 
                  label = "Elije un periodo:",
                  min = 1952, max = 2007, value = c(1955, 2005), format = "####")
    ),
    mainPanel("Espacio para el gráfico",
              textOutput("output_country"),
              plotOutput("ggplot_lifeExp"),
              tableOutput("gapminder_table")              
    )
  )
)
```

Warning: "Warning in is.na(e2)"

#### server.R

``` r
library(tidyverse)

gDat <- read_csv(file = "data/gapminder.csv")

gDat$country <- as.factor(gDat$country)

server <- function(input, output) {

  one_country_data  <- reactive({
    if(is.null(input$country_from_gapminder)) {
      return(NULL)
    }
    gDat %>% filter(country == input$country_from_gapminder & year >= input$year_range[1] & year <= input$year_range[2])
  })
  
  output$choose_country <- renderUI({
    selectInput("country_from_gapminder", "Selecciona un país:", as.list(levels(gDat$country)))
  })
  
  output$gapminder_table <- renderTable({ 
    one_country_data()
  })
  
  output$output_country <- renderText({
    if (is.null(input$country_from_gapminder)){
      return(NULL)
    }
    paste("Mostrando datos de ", input$country_from_gapminder)
  })
  
  output$ggplot_lifeExp <- renderPlot({
    if(is.null(one_country_data())) {
      return(NULL)
    }
    p <-  ggplot(one_country_data(), aes(x = year, y = gdpPercap))
    p + geom_point()
  })
}
```

------------------------------------------------------------------------

Ejercicios:
===========

-   Cambia el aspecto de la app: <https://rstudio.github.io/shinythemes/>
-   Modificar la tabla para omitir la columna `country`
-   Añadir a la app un gráfico que permita ver la evolución de `lifeExp` en función de `gdpPercap`. Incluir en dicho gráfico una línea de tendencia. Prueba a agregarlo dentro una nueva pestaña (<https://shiny.rstudio.com/reference/shiny/latest/tabPanel.html>)
