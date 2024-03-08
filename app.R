library(shiny)
library(leaflet)
library(sf)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(leafpop)

# Import data ----

# Project data
tt_data <- read_sf("data/tt_layers_202401.geojson") 
# Partner location
tt_partner <- read_sf("data/partners.geojson")
# Building use
bld_use <- read_sf("data/bld_use_all.geojson")

# UI design ----

ui <- navbarPage("ThinkThings", id="nav",
                 
                 tabPanel("Interactive map",
                          
                          div(class="outer",
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css")
                                #includeScript("geomap.js")
                              ),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width = "100%", height = "100%"),
                              
                              # Shiny versions prior to 0.11 should use class = "modal" instead
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            
                                            h2("ThinkThings explorer"),
                                            
                                            selectInput("layers", "ThinkThings Layers:", tt_data$lnames,
                                                        selected = TRUE),
                                            
                                            plotOutput("hist", height = 200),
                                            
                                            selectInput("partner", "Partners:", tt_partner$layer),
                                            
                                            selectInput("blduse", "Building use", bld_use$bl_use_des,
                                                        multiple = FALSE)
                                            
                                            
                                            
                                            
                                
                              ),
                              
                              tags$div(id="cite",
                                       'Data provide by', tags$em('ThinkThings Project, 2023'), 'by REDEK (2023)')
                              
                              
                            
                            
                          )
                          
                          )
                 # tabPanel("Project details")
  
)

# Server design ----

server <- function(input, output, session) {
  
  # Set iconlist ----
  tt_icon <- iconList(
    trashbin = makeIcon("icon/trash-outline.svg", "icon/trash-outline.svg", 18, 18),
    #residential = makeIcon("icon/home-outline.svg", "icon/home-outline.svg", 18, 18),
    parking = makeIcon("icon/caret-down-outline.svg", "icon/caret-down-outline.svg", 18, 18),
    assemblyPoints = makeIcon("icon/people-circle-outline.svg", "icon/people-circle-outline.svg", 18, 18)
  )
  
  tt_icon[c("trashbin", "parking", "assemplyPoints")]
  
  partner_icon <- iconList(
    won = makeIcon("icon/won-project.jpg", "icon/won-project.jpg", 24, 24),
    magichand = makeIcon("icon/magickhand.png", "icon/magickhand.png", 24, 24),
    deal = makeIcon("icon/deal.png", "icon/deal.png", 24, 24),
    recycle = makeIcon("icon/recycle-bin.png", "icon/recycle-bin.png", 24, 24),
    pilot = makeIcon("icon/approved.png", "icon/approved.png", 24, 24),
    cirplas = makeIcon("icon/cirplas.jpg", "icon/cirplas.jpg", 24, 24)
  )
  
  ## Interactive Map ----
  
  # Create the map ----
  # palBld <- colorFactor(c("darkgray","cyan", "purple", "blue",
  #                         "red", "yellow", "orange", "pink", "green"), 
  #                       domain = c("สาธารณูปการ", "พาณิชยกรรม","ที่อยู่อาศัย",
  #                                  "อื่นๆ", "การใช้ประโยชน์แบบผสม", "อุตสาหกรรม",
  #                                  "สาธารณูปโภค", "พื้นที่อนุรักษ์/นันทนาการ",
  #                                  "เกษตรกรรม"))
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(providers$CartoDB) %>% 
      # addPolygons(fillColor = ~palBld(Bl_use),
      #             weight = 0,
      #             group = "การใช้ประโยชน์อาคาร") %>%
      setView(lng = 100.52214054999999, lat = 13.76973515, zoom = 15)
      # addLayersControl(
      #   position = "bottomleft",
      #   overlayGroups = c("Track", "การใช้ประโยชน์อาคาร"),
      #   options = layersControlOptions(collapsed = FALSE)
      # ) %>%
      # hideGroup(c("Track", "การใช้ประโยชน์อาคาร")) %>%
      # addLegend(position = "bottomright",
      #           pal = palBld,
      #           values = ~Bl_use)
      # addPolygons(data = bld_use,
      #           fillColor = ~palBld(bl_use_des),
      #           weight = 0,
      #           fillOpacity = 0.4,
      #           popup = paste0(
      #             "<strong>Building use: </strong>", bld_use$bl_use_des
      #           ))
  })
  
  
  # A reactive expression that  returns the set of layer
  # that are in bounds right now
  selectedLayers <- reactive({
    
    tt_data[tt_data$lnames == input$layers, ]
    
  })
  
  seletedPartner <- reactive({
    
    tt_partner[tt_partner$layer == input$partner, ]
    
  })
  
  selectedBlduse <- reactive({
    bld_use[bld_use$bl_use_des == input$blduse, ]
  })
  

  # Plot graph
  
  output$hist <- renderPlot({
    # if no selected are in view, don't plot
    ggplot(selectedLayers(), aes(y = types)) + 
      geom_bar(aes(fill = site), position = position_dodge()) +
      scale_fill_brewer(palette = "Paired") +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  
  observe({
    
    pal <- colorFactor(c("green", "yellow", "red",
                         "blue", "orange", "cyan"),
                       domain = c("Formal", "Informal", "NA",
                                  "คอนโดมิเนียม", "หอพัก", "อพารท์เม้น,แมนชั่น"))
    
    palBld <- colorFactor(c("forestgreen","pink", "yellow", "red",
                            "orange", "tan4", "blue", "gray52", "purple"),
                          domain = c("สาธารณูปการ", "พาณิชยกรรม","ที่อยู่อาศัย",
                                     "อื่นๆ", "การใช้ประโยชน์แบบผสม", "อุตสาหกรรม",
                                     "สาธารณูปโภค", "พื้นที่อนุรักษ์/นันทนาการ",
                                     "เกษตรกรรม"))

    leafletProxy("map", data = selectedLayers()) %>% 
      # clearMarkers() %>% 
      # addMarkers(icon = ~tt_icon[selectedLayers()$layers],
      #            popup = paste0(
      #              "<strong>Layers: </strong>", selectedLayers()$lnames,
      #              "<br><strong>Name: </strong>", selectedLayers()$name,
      #              "<br><strong>Types: </strong>", selectedLayers()$types,
      #              "<br><strong>Site: </strong>", selectedLayers()$site
      #            )) %>%
      clearShapes() %>% 
      addPolygons(data = selectedBlduse(),
                  fillColor = ~palBld(selectedBlduse()$bl_use_des),
                  weight = 0,
                  fillOpacity = 1,
                  popup = paste0(
                    "<strong>Building use: </strong>", selectedBlduse()$bld_use_desc2
                  )) %>%
      
      clearMarkers() %>% 
      addCircleMarkers(color = ~pal(selectedLayers()$types),
                 stroke = "black",
                 weight = 1,
                 radius = 5,
                 fillOpacity = 0.5,
                 popup = paste0(
                                "<strong>Layers: </strong>", selectedLayers()$lnames,
                                "<br><strong>Name: </strong>", selectedLayers()$name,
                                "<br><strong>Types: </strong>", selectedLayers()$types,
                                "<br><strong>Site: </strong>", selectedLayers()$site,
                                #"<br><string>Image: </strong>", selectedLayers()$image,
                                popup = popupImage(selectedLayers()$image, src = "remote")
                              )
                 ) %>%
      
      #clearMarkers() %>% 
      addMarkers(data = seletedPartner(),lng = ~long, lat = ~lat,
                 icon = ~partner_icon[seletedPartner()$icon],
                 popup = paste0(
                   "<strong>Name: </strong>", seletedPartner()$Name,
                   "<br><strong>Description: </strong>", seletedPartner()$description
                 )) %>% 
      clearControls() %>% 
      addLegend(position = "bottomright",
                pal = pal,
                values = c(selectedLayers()$types, selectedBlduse()$bld_use_desc),
                title = input$layers)
    

  })
  

  

}

shinyApp(ui, server)
