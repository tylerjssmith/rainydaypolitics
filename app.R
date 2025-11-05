################################################################################
# Rainy Day Politics

##### app.R ####################################################################
library(tidyverse)
library(shiny)
library(shinydashboard)
library(DBI)
library(pool)
library(RPostgres)
library(sf)
library(leaflet)
library(leaflet.extras)

# Also requires config package, which is not loaded to avoid conflicts.

setwd("/srv/shiny-server/rainydaypolitics")
ui_options = read_csv("data/ui_options.csv")

##### app_functions.R ##########################################################
# Function: create_pool()
create_pool = function(config_file) {
  
  config = config::get(file = config_file)
  
  pool = dbPool(
    drv      = RPostgres::Postgres(),
    host     = config$host,
    port     = config$port,
    dbname   = config$dbname,
    user     = config$user,
    password = config$password,
    
    minSize     = 3,
    maxSize     = Inf, 
    idleTimeout = 300000 # 300000 ms = 5 minutes
  )
  
  return(pool)
}

pool = create_pool(config_file = "config_app.yml")

onStop(function() {
  
  poolClose(pool)

})

# Function: query_data()
query_data = function(input, dsn = pool, 
  query_file = 'sql/query_results.sql') {    

  input_list = list(
    year         = input$year,
    election     = input$election,
    jurisdiction = input$jurisdiction,
    position     = input$position,
    candidate    = input$candidate
  )

  geometry_table = ifelse(
    input$year < 2022, 
    'precinct_geometry_2016',
    'precinct_geometry_2022'
  )

  query_text = read_file(query_file)
  query_text = gsub('\\?geometry_table', geometry_table, query_text)
  query_text = sqlInterpolate(dsn, query_text, .dots = input_list)

  st_read(
    dsn   = dsn, 
    query = query_text,
    quiet = TRUE
  )
}

# Function: make_leaflet()
make_leaflet = function(df, input, zoom = 11, minZoom = 9, bound = 1) {

  view = tibble(lng = -122.335167, lat = 47.608013)

  values = round(with(df, get( input$value )), 1)
  symbol = ifelse(input$value == 'vote_percent', '%', 'votes')

  options  = leafletOptions(minZoom = minZoom)
  bound    = bound
  palette  = colorNumeric('magma', values)
  lastname = word(input$candidate, -1)

  map = leaflet(options = options) %>%
    
    addProviderTiles(
      providers$CartoDB.PositronNoLabels
      ) %>%
    
    setView(
      lng  = view$lng, 
      lat  = view$lat, 
      zoom = zoom
      ) %>%
    
    setMaxBounds(
      lng1 = view$lng + bound,
      lat1 = view$lat + bound,
      lng2 = view$lng - bound,
      lat2 = view$lat - bound
      )
  
  map = map %>%
    addPolygons(
      data = df, 
      
      fillColor = ~palette(values), 
      fillOpacity = 0.8,
      
      color = 'black',
      weight = 1,
      
      popup = ~paste0('<b>', precinct, '</b><br/>', 
        lastname, ': ', values, ' ', symbol),
      
      highlight = highlightOptions(
        weight = 3, 
        bringToFront = TRUE
      )
    ) %>%
    
    addLegend(
      pal = palette,
      values = values,
      title = paste0(lastname, ' (', symbol, ')')
    ) %>%
    
    leaflet.extras::addResetMapButton()

  return(map)
}

##### app_ui.R #################################################################
# Header
header = dashboardHeader(
  title = 'Rainy Day Politics',
  titleWidth = 300
)

# Sidebar
sidebar = dashboardSidebar(
  width = 300,
  
  sidebarMenu(
    
    menuItem(
      'Select Result',
      startExpanded = TRUE,
      
      selectInput(
        inputId = 'year',
        label = 'Year',
        choices = paste0(c('', sort(unique(ui_options$year)))),
        selected = '',
      ),
      
      selectInput(
        inputId = 'election',
        label = 'Election',
        choices = NULL
      ),
      
      selectInput(
        inputId = 'jurisdiction',
        label = 'Jurisdiction',
        choices = NULL
      ),
      
      selectInput(
        inputId = 'position',
        label = 'Position',
        choices = NULL
      ),
      
      selectInput(
        inputId = 'candidate',
        label = 'Candidate',
        choices = NULL
      ),
      
      radioButtons(
        inputId = 'value',
        label = 'Value',
        choiceNames = c(
          'Percentage',
          'Count'
        ),
        choiceValues = c(
          'vote_percent',
          'vote_count'
        )
      ),
      
      actionButton(
        inputId = 'run',
        label = 'Run'
      )
    )
  )
)

# Body
body = dashboardBody(
  
  leafletOutput(
    outputId = 'map', 
    width = '100%',
    height = 800
  )

)

ui = dashboardPage(header, sidebar, body)

##### app_server.R #############################################################
server = function(input, output) { 

  # Reactive Input
  # (For year, filter and get elections.)
  year = reactive({
    ui_options %>% filter(year == input$year)
  })
  
  observeEvent(year(), {
    choices = sort(unique(year()$election))
    updateSelectInput(inputId = 'election', choices = choices)
  })  

  # (For election, filter and get jurisdictions)
  election = reactive({
    year() %>% filter(election == input$election)
  })
  
  observeEvent(election(), {
    choices = sort(unique(election()$jurisdiction))
    updateSelectInput(inputId = 'jurisdiction', choices = choices)
  })

  # (For jurisdiction, filter and get positions.)
  jurisdiction = reactive({
    election() %>% filter(jurisdiction == input$jurisdiction)
  })
  
  observeEvent(jurisdiction(), {
    choices = sort(unique(jurisdiction()$position))
    updateSelectInput(inputId = 'position', choices = choices)
  })

  # (For position, filter and get candidates.)
  position = reactive({
    jurisdiction() %>% filter(position == input$position)
  })
  
  observeEvent(position(), {
    choices = sort(unique(position()$candidate))
    updateSelectInput(inputId = 'candidate', choices = choices)
  })

  # (For candidate, filter.)
  candidate = reactive({
    position() %>% filter(candidate == input$candidate)
  })

  # Call: query_data()
  df = eventReactive(input$run, {

    query_data(input = input)    

  })

  # Call: make_leaflet
  data <- reactiveValues()

  observeEvent(input$run, {
    
    data$map <- make_leaflet(df = df(), input = input)
    
  })

  # Serve Output
  output$map = renderLeaflet({
    
    data$map

  })

}

##### Launch App ###############################################################
shinyApp(ui, server)
