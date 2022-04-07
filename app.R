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

# Domain selector
vars <- c(
  "Overall" = "rank_decile",
  "Income" = "inc_rank_decile",
  "Employment" = "emp_rank_decile",
  "Health" = "hlth_rank_decile",
  "Education" = "edu_rank_decile",
  "Geographic Access" = "g_acc_rank_decile",
  "Crime" = "crime_rank_decile",
  "Housing" = "house_rank_decile")

# Palette
mypalette <- colorFactor(palette = "viridis", domain = sf_datazones$rank_decile, na.color = "transparent")

# Labels
mylabels <- paste0(
  "Datazone: ", sf_datazones$dz_name,"<br/>",
  "Council Area: ", sf_datazones$la_name,"<br/>",
  "Decile: ", sf_datazones$rank_decile, # Should be reactive
  sep = "") %>% 
  lapply(htmltools::HTML)

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
      tabPanel("Comparing SIMD Ranks", plotOutput("cross")),            # cross is the geom_point plot comparing ranks
      tabPanel("Data", DT::dataTableOutput("table"))                                               
      )
      
    )
  )
)

##########
# Server #
##########

server <- function(input, output, session){
  
#  domain <- reactive({
#    if ("Overall" %in% input$domain)           return(sf_datazones$rank_decile)
#    if ("Income" %in% input$domain)            return(sf_datazones$inc_rank_decile)
#    if ("Employment" %in% input$domain)        return(sf_datazones$emp_rank_decile)
#    if ("Health" %in% input$domain)            return(sf_datazones$hlth_rank_decile)        # This should assign the correct variable values to display on choropleth map & y axis of plot
#    if ("Education" %in% input$domain)         return(sf_datazones$edu_rank_decile)
#    if ("Geographic Access" %in% input$domain) return(sf_datazones$g_acc_rank_decile)
#    if ("Crime" %in% input$domain)             return(sf_datazones$crime_rank_decile)
#    if ("Housing" %in% input$domain)           return(sf_datazones$house_rank_decile)
#  })

  # Leaflet Map  
  output$map <- renderLeaflet({
    leaflet(sf_datazones) %>% 
    addTiles() %>% 
    setView( lng = -4.182,
             lat = 56.817,
             zoom = 7) %>% 
    addPolygons(
      fillColor = ~mypalette(rank_decile), # Should point to variable from domain selection above
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
      title = "Overall\nDecile", # Would be nice to add reactive text to reflect domain chosen
      position = "bottomright")
  })
  
  # Rank Comparison Plot
  output$cross <- renderPlot({
    ggplot(data = sf_datazones, aes(x = rank, y = inc_rank)) +  # y axis should be populated from domain input selection
      geom_point(size = 1.5, alpha = .5, colour = "dodgerblue4") +
      scale_x_continuous(limits = c(1,7000), breaks = seq(0,7000,by=1000), labels = comma) +
      scale_y_continuous(limits = c(1,7000), breaks = seq(0,7000,by=1000), labels = comma) +
      labs(x = "Overall Rank", y = "Income Rank", subtitle = "Areas are ranked from 1 (Most Deprived) to 6,976 (Least Deprived)") # y label should reflect chosen domain automatically
      },
    res = 150,
    height = function() {
      session$clientData$output_cross_width * .9
    } )
  
  # Table
  output$table <- renderDataTable({
    sf_datazones %>%
      st_drop_geometry() %>% 
      rename("Datazone" = "dz_name",
             "Council Area" = "la_name",
             "Income Decile" = "inc_rank_decile",
             "Employment Decile" = "emp_rank_decile",
             "Health Decile" = "hlth_rank_decile",
             "Education Decile" = "edu_rank_decile",
             "Access Decile" = "g_acc_rank_decile",
             "Crime Decile" = "crime_rank_decile",
             "Housing Decile" = "house_rank_decile",
             "Overall Decile" = "rank_decile") %>%
      select(Datazone, 'Council Area', 'Overall Decile', 'Income Decile')  # 4th variable should change based on selected domain input
  })
  
}

#########
## App ##
#########

shinyApp(ui, server)