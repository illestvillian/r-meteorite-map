library(dplyr)
library(readr)
library(leaflet)
library(janitor)

final_meteorite_landings <- readRDS("meteorite_data.rds")

final_meteorite_landings <- meteorite_landings %>% janitor::clean_names() %>% 
  select(id, name, mass=mass_g, year, lat=reclat, long=reclong) %>%
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
  
  
  pal <- colorNumeric(palette = "RdBu", domain = final_meteorite_landings$mass)
  
  output$meteorite_map <- renderLeaflet({
    
    leaflet(final_meteorite_landings) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(
        lng = ~long, 
        lat = ~lat, 
        color = ~pal(mass), 
        fillOpacity = 0.7,
        radius = 5,
        clusterOptions = markerClusterOptions(
          spiderfyOnMaxZoom = TRUE,
          disableClusteringAtZoom = 10
        ),
        popup = ~paste0("<b>", name, "</b><br/>", 
                        "Year: ", year, "<br/>", 
                        "Mass: ", display_mass)
      )
  })
}

shinyApp(ui = ui, server = server)

