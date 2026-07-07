library(dplyr)
library(readr)
library(leaflet)
library(janitor)
library(tidyr)
library(shiny)
library(leaflet.extras)

meteorite_landings <- readRDS("meteorite_data.rds")

final_meteorite_landings <- meteorite_landings %>% 
  janitor::clean_names() %>% 
  select(
    id, 
    name, 
    mass,
    year, 
    lat, 
    long
  ) %>%
  drop_na(name, year, mass, lat, long) %>%
  filter(lat !=0 & long !=0) %>%
  mutate(
    display_mass = if_else(
      mass >= 1000, 
      paste0(round(mass / 1000, 1), " kg"), 
      paste0(round(mass, 0), " g")
    )
  )

ui <- fluidPage(
  titlePanel("Meteorite Landings Explorer"),
  mainPanel(
    leafletOutput("meteorite_map", height = "800px")
  )
)


server <- function(input, output, session) {
  

  
  output$meteorite_map <- renderLeaflet({
    pal <- colorNumeric(
      palette = "magma", 
      domain = log10(final_meteorite_landings$mass + 1)
    )
    
    leaflet(final_meteorite_landings) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(
        lng = ~long, lat = ~lat,
        radius = ~ifelse(mass > 10000, 8, 4), 
        fillColor = ~pal(log10(mass + 1)), 
        weight = 0.5,
        fillOpacity = 0.8,
        popup = ~paste0("<b>", name, "</b><br/>", 
                        "Year: ", year, "<br/>", 
                        "Mass: ", display_mass)
      )
  })
}

shinyApp(ui = ui, server = server)

