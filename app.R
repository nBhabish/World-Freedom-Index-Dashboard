# Loading Dependencies ----------------------------------------------------

library(shiny)
library(shinyjs)
library(shinyWidgets)

library(leaflet) # for interactive map
library(tidyverse) # for data wrangling
library(sf) # for reading shape files

library(bslib) # for themes
library(thematic) # for themes

library(reactable)
library(reactablefmtr)
library(htmltools)

library(crosstalk)
library(htmltools)


# Read Data ---------------------------------------------------------------

world_shp <-
  read_sf("world shapefile/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")
freedom   <- read_csv("00_data/freedom.csv") %>%
  pivot_longer(cols = cl:pr,
               names_to = "metrics",
               values_to = "scores") %>%
  mutate(metrics = case_when(metrics == "cl" ~ "Civil Liberty",
                             TRUE ~ "Political Rights"))

freedom_with_flags <- read_csv("00_data/freedom_with_flags.csv") #includes maps flaticons to render on table

# Theme -----------------------------------------------------------------

my_theme <- bslib::bs_theme(
  version    = 4,
  bootswatch = "materia",
  fg         = "#000000",
  bg         = "white",
  base_font  = font_google("Sansita"),
  heading_font = font_google("Sansita"),
  font_scale  = 0.95,
  primary     = "#002244"
)


# Filtering options for reactable -----------------------------------------

crosstalk_data <- SharedData$new(freedom_with_flags)

region_filter <- filter_select(
  id = "region_name",
  label = "Region",
  group = ~ region_name,
  sharedData = crosstalk_data
)

country_filter <- filter_select(
  id = "country",
  label = "Country",
  group = ~ country,
  sharedData = crosstalk_data
)

year_filter <- filter_select(
  id = "year",
  label = "Year",
  group = ~ year,
  sharedData = crosstalk_data
)

cl_filter <- filter_slider(
  id = "cl",
  label = "Civil Liberty",
  column = ~ cl,
  sharedData = crosstalk_data
)

# Reactable Table  --------------------------------------------------------

tbl <- htmltools::browsable(tagList(
  tags$br(),
  br(),
  reactable(
    crosstalk_data,
    compact = TRUE,
    theme = fivethirtyeight(
      font_size = 12,
      header_font_size = 12,
      centered = TRUE
    ),
    columns = list(
      country = colDef(
        name = "country",
        style = list(borderRight = "1px solid #777")
      ),
      flag_img = colDef(
        name = "",
        style = background_img(height = "45%", width = "100%"),
        maxWidth = 20,
        sortable = FALSE,
        align = "right"
      ),
      cl = colDef(name = "Civil Liberties", align = "center"),
      pr = colDef(
        name = "Political Rights",
        align = "center",
        style = list(borderRight = "1px solid #777")
      ),
      status = colDef(name = "Free?", align = "center"),
      region_name = colDef(
        name = "Region",
        style = group_merge_sort("Region_Name"),
        align = "center"
      ),
      is_ldc = colDef(
        name = "Dev. Status",
        align = "center",
        cell = pill_buttons(freedom_with_flags, color_ref = "is_ldc_colors", opacity = 0.8)
      ),
      year   = colDef(
        name = "Year",
        maxWidth = 60,
        style = list(borderRight = "1px solid #777"),
        align = "center"
      ),
      is_ldc_colors = colDef(show = FALSE)
    ),
    fullWidth = TRUE
  )
))

# User Interface (UI) -----------------------------------------------------

ui <- navbarPage(
  title       = "United Nations and Freedom House",
  inverse     = FALSE,
  collapsible = TRUE,
  
  shinyjs::useShinyjs(),
  
  
  # Bringing in the theme ---------------------------------------------------
  
  theme = my_theme,
  
  
  # Pulling the CSS ---------------------------------------------------------
  
  
  tags$body(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  
  # UI - Page 1 -------------------------------------------------------------
  
  
  tabPanel(
    title = "",
    
    div(
      class = "container-fluid",
      id    = "header",
      
      h1(class = "page-header",
         "World Freedom Index", 
         tags$small("(1995-2020)"))
    ),
    
    hr(),
    
    
    div(class = "container-fluid",
        id    = "application_ui",
        
        div(fluidRow(
          column(width = 3,
                 
                 # Well Panel --------------------------------------------------------------
                 
                 wellPanel(
                   div(
                     id = "main_input",
                     
                     # First Input Box (Year) --------------------------------------------------
                     
                     
                     pickerInput(
                       inputId = "year_tab_1",
                       label   = h3("Year"),
                       choices = unique(freedom$year),
                       multiple = FALSE,
                       selected = 2020,
                       option   = pickerOptions(
                         actionsBox = FALSE,
                         liveSearch = TRUE,
                         size       = 5
                       )
                     ),
                     
                     # Second Input Box ----------------------------------------
                     
                     pickerInput(
                       inputId = "metrics_tab_1",
                       label   = h3("Score"),
                       choices = unique(freedom$metrics),
                       multiple = FALSE,
                       selected = "Civil Liberty",
                       options = pickerOptions(
                         actionsBox = FALSE,
                         liveSearch = TRUE,
                         size       = 2
                       )
                     ),
                     
                     
                     # Page 1 Apply Button -----------------------------------------------------
                     
                     div(
                       id = "input_buttons",
                       actionButton(
                         inputId = "apply_tab_1",
                         label = "Apply",
                         icon = icon("play")
                       ),
                       
                       
                       # Page 1 Reset Button -----------------------------------------------------
                       
                       
                       div(
                         class = "pull-right",
                         actionButton(
                           inputId = "reset_tab_1",
                           label = "Reset",
                           icon = icon("sync")
                         )
                       )
                     )
                   )
                 ), 
                 br(), 
                 br(),
                 br(),
                 br(),
                 br(),
                 wellPanel(
                   h3("About the Data"),
                   p("The data comes from Freedom House and the United Nations as a part of #TidyTuesday project."), 
                   h3("More:"),
                   p("Freedom in the World, Freedom House's flagship publication, is the standard-setting 
                   comparative assessment of global political rights and civil liberties. 
                   Published annually since 1972, the survey ratings and narrative reports on 195 countries
                   and 15 related and disputed territories are used by policymakers, 
                   the media, international corporations, civic activists, and human rights defenders.")
                 )),
          
          
          
          # Leaflet Plot ------------------------------------------------------------
          
          column(
            8,
            leafletOutput(
              outputId = "map_1",
              width = 950,
              height = 400
            ),
            #verbatimTextOutput(outputId = "test_print") # for checking if the values are printed or not
            br(),
            hr(),
            div(h3("World Freedom Index", style = "text-align: center;")),
            div(
              bscols(
                # reactable filtering options ---------------------------------------------
                widths = c(3, 3, 3),
                region_filter,
                country_filter,
                year_filter,
                cl_filter
              ),
              tbl # reactable table
            )
          )
        ))),
    
    hr()
    
    
  ),
  
  div(style = "height:100;") # allows for a little scroll at the end of the page
  
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # bs_themer()
  
  
  # Updating pickerInputs ---------------------------------------------------
  
  observeEvent(eventExpr = input$reset_tab_1, handlerExpr = {
    updatePickerInput(
      session = session,
      inputId = "year_tab_1",
      selected = c("2020")
    )
    
    
    updatePickerInput(session = session,
                      inputId = "metrics_tab_1",
                      selected = "Civil Liberty")
    
    shinyjs::delay(ms = 300, expr = {
      shinyjs::click(id = "apply_tab_1")
    })
    
  })
  
  
  # Filtering elements from user inputs -------------------------------------
  
  freedom_filter <- eventReactive(
    eventExpr = input$apply_tab_1,
    valueExpr = {
      freedom %>%
        filter(year == input$year_tab_1) %>%
        filter(metrics == input$metrics_tab_1)
    },
    ignoreNULL = FALSE
  )
  
  
  # dataset for leaflet -----------------------------------------------------
  
  world_shp_1 <- reactive({
    world_shp %>%
      left_join(
        freedom_filter(),
        by = c("WB_A2" = "country_code"),
        suffix = c("", "_wfi")
      )
  })
  
  # output$test_print <- renderPrint(world_shp_1()$year)
  
  # Leaflet color -----------------------------------------------------------
  
  
  # Theme colors come from nationalparkcolors R package
  mappalette <- reactive({
    colorFactor(
      palette = c(
        "#7DCCD3",
        "#4E7147",
        "#BE9C9D",
        "#F7ECD8",
        "#376597",
        "#9888A5",
        "#DBA662",
        "black"
      ),
      levels  = c(1, 2, 3, 4, 5, 6, 7, "NA")
    )
  })
  
  # leaflet popup -----------------------------------------------------------
  
  
  mappopup <- reactive({
    paste(
      "Country:",
      world_shp_1()$ADMIN,
      "<br>",
      "Score",
      ":",
      world_shp_1()$scores,
      "<br>"
    )
  })
  
  # Leaflet Server ----------------------------------------------------------
  output$map_1 <- renderLeaflet({
    leaflet(world_shp) %>%
      setView(lng = 18.6435,
              lat = 60.1282,
              zoom = 2) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addLegend(
        position = "bottomright",
        title    = "Score",
        pal      = mappalette(),
        values   = unique(world_shp_1()$scores)
      )
  })
  
  
  # Observing leafletProxy --------------------------------------------------
  
  observe({
    pal_1 <- mappalette()
    
    mappopup_1 <- mappopup()
    
    
    leafletProxy("map_1") %>%
      addPolygons(
        data = world_shp_1(),
        weight = 1,
        smoothFactor = 0.5,
        color = "white",
        fillColor = pal_1(world_shp_1()$scores),
        opacity = 1.0,
        fillOpacity = 0.9,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE, 
          
        ),
        popup = mappopup_1
      )
  })
  
}

# Run shinyApp() function -------------------------------------------------

shinyApp(ui, server)
