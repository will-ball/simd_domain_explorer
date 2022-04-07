library(leaflet)
library(shiny)
library(bslib)
library(sf)
library(DT)
library(ggplot2)
library(scales)
library(dplyr)

# Data
sf_datazones <- read_sf("Shapefile/cleaned.shp")

# Plot Theme
theme_set(theme_classic())
theme_update(panel.grid.major.y = element_line(),
             panel.grid.major.x = element_line(),
             plot.title.position = "plot")

# Variables
vars <- c("Overall", "Income", "Employment", "Health", "Education", "Geographic Access", "Crime", "Housing")

##########
### UI ###
##########

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  titlePanel("SIMD 2020 Domain Explorer"),
  
  sidebarLayout(
    sidebarPanel(h3("Choose a Domain"),
                 width = 3,
    
      selectInput("domain",
                  "",
                  vars,
                  selected = "Overall")                                 # domain selector determines which variable to assign to areas
      ),
  
    mainPanel(
      tabsetPanel(
    
      tabPanel("Map",leafletOutput("map", height = "80vh")),            # map is the choropleth
      tabPanel("Comparing SIMD Ranks", plotOutput("cross"))            # cross is the geom_point plot comparing ranks
      )
      
    )
  )
)

##########
# Server #
##########

server <- function(input, output, session){
  
  # Leaflet Map  
  output$map <- renderLeaflet({
    
    domain <- switch(input$domain,
      "Overall" = sf_datazones$rank_decile,
      "Income" = sf_datazones$inc_rank_decile,
      "Employment" = sf_datazones$emp_rank_decile,
      "Health" = sf_datazones$hlth_rank_decile,
      "Education" = sf_datazones$edu_rank_decile,
      "Geographic Access" = sf_datazones$g_acc_rank_decile,
      "Crime" = sf_datazones$crime_rank_decile,
      "Housing" = sf_datazones$house_rank_decile)
    
    mypalette <- colorFactor(palette = "viridis", domain = domain, na.color = "transparent")
    
    mylabels <- paste0(
      "Datazone: ", sf_datazones$dz_name,"<br/>",
      "Council Area: ", sf_datazones$la_name,"<br/>",
      {input$domain}," Decile: ", domain, # Should be reactive
      sep = "") %>% 
      lapply(htmltools::HTML)
    
    
    leaflet(sf_datazones) %>% 
    addTiles() %>% 
    setView( lng = -4.182,
             lat = 57.750,
             zoom = 6) %>% 
    addPolygons(
      fillColor = ~mypalette(domain), # Should point to variable from domain selection above
      weight = .5,
      opacity = 1,
      color = "grey",
      fillOpacity = .5,
      label = mylabels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "12px",
        direction = "auto")) %>% 
    addLegend(
      pal = mypalette,
      values = ~rank_decile,
      opacity = .7,
      title = paste0({input$domain},"\nDecile"), # Would be nice to add reactive text to reflect domain chosen
      position = "bottomright")
  })
  
  # Rank Comparison Plot
  output$cross <- renderPlot({
    
    domain <- switch(input$domain,
                     "Overall" = sf_datazones$rank,
                     "Income" = sf_datazones$inc_rank,
                     "Employment" = sf_datazones$emp_rank,
                     "Health" = sf_datazones$hlth_rank,
                     "Education" = sf_datazones$edu_rank,
                     "Geographic Access" = sf_datazones$g_acc_rank,
                     "Crime" = sf_datazones$crime_rank,
                     "Housing" = sf_datazones$house_rank)
    
    ggplot(data = sf_datazones, aes(x = rank, y = domain)) +  # y axis should be populated from domain input selection
      geom_point(size = 1.5, alpha = .5, colour = "dodgerblue4") +
      scale_x_continuous(limits = c(1,7000), breaks = seq(0,7000,by=1000), labels = comma) +
      scale_y_continuous(limits = c(1,7000), breaks = seq(0,7000,by=1000), labels = comma) +
      labs(x = "Overall Rank", y = paste0({input$domain}, " Rank"), subtitle = "Areas are ranked from 1 (Most Deprived) to 6,976 (Least Deprived)") # y label should reflect chosen domain automatically
      },
    res = 150,
    height = function() {
      session$clientData$output_cross_width * .9
    } )
  
}

#########
## App ##
#########

shinyApp(ui, server)