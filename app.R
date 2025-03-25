#library(shiny)
library(shinyjs)
install.packages("leaflet")

source("data.R")


ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Najpopularniejsze trasy rowerowe w New Jersey w zaleznosci od czasu"),
  fluidRow(
    column(
      3,
      radioButtons(
        inputId = 'by',
        label = 'Grupuj wedlug:',
        choiceNames = c('miesiace', 'kwartaly', 'lata'),
        choiceValues = c('m', 'q', 'y')),
      checkboxInput(
        inputId = 'combine_years',
        label = 'Zlicz rozne lata razem',
        value = FALSE),
      checkboxGroupInput(
        inputId = 'years',
        label = 'Wybierz lata do analizy:',
        choiceNames = as.character(2016:2020),
        choiceValues = 2016:2020,
        selected = 2016:2020),
      sliderInput(
        inputId = 's_year',
        label = 'Rok',
        min = 2016,
        max = 2020,
        value = 2020,
        step = 1,
        animate = TRUE),
      sliderInput(
        inputId = 's_quarter',
        label = 'Kwartal',
        min = 1,
        max = 4,
        value = 1,
        animate = TRUE),
      sliderInput(
        inputId = 's_month',
        label = 'Miesiac',
        min = 1,
        max = 12,
        value = 1,
        step = 1,
        animate = TRUE)
    ),
    column(
      6,
      leafletOutput('map', height = 700),
      textOutput('fraction'),
      checkboxInput(
        inputId = 'show_markers',
        label = 'Pokaz markery lokalizacji stacji rowerowych',
        value = TRUE),
      numericInput(
        inputId = 'limit',
        label = 'Liczba tras:',
        value = 10,
        min = 1,
        max = 50,
        step = 1)
      
    ),
    column(
      3,
      tableOutput('table'),
      style = "overflow-y:scroll; max-height: 800px"
    )
  )
)



server <- function(input, output) {
 
  dt <- reactive(popular_routes_dt(input$years, input$by, input$combine_years, input$s_month, input$s_quarter, input$s_year))
  map <- reactive(render_map(dt(), input$limit, input$show_markers))
  dt_display <- reactive(table_to_display(dt()))
  
  
  observe({
    toggleState(id = "s_year",
                condition = (input$combine_years == FALSE | input$by == 'y'))
    toggleState(id = "s_month",
                condition = (input$by == 'm'))
    toggleState(id = 's_quarter',
                condition = (input$by == 'q'))
    toggleState(id = 'years',
                condition = (input$combine_years == TRUE))
    toggleState(id = 'combine_years',
                condition = (input$by != 'y'))
    toggleState(selector = "[value='y']",
                condition = (input$combine_years == FALSE))
  }
  )


  output$map <- renderLeaflet(map() %>%
    setView(lat = 40.7247380303807, lng = -74.05424669356333, zoom = 14))

  output$fraction <- reactive(sprintf('Uwzgledniono %s z %s przejazdow (%2s%%)',
                                      sum(dt()[1:input$limit,]$Count), sum(dt()$Count),
                                      signif(100 * sum(dt()[1:input$limit,]$Count) / sum(dt()$Count), 3)))
  
  output$table <- renderTable(dt_display()[1:input$limit, ], rownames = TRUE)


}

shinyApp(ui = ui, server = server)
