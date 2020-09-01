##########################################################################################################################################################################################################################
# Title         : AgriRoof: Potential Flat Rooftop Detection using LiDAR for Rooftop Farming
#
# Purpose       : This Script is written in the framework of AgriRoof R Shiny App development 
#
# Author        : Walid Ghariani (linkedin: https://www.linkedin.com/in/walid-ghariani-893365138/) (E-mail: walid.ghariani@stud-mail.uni-wuerzburg.de | walid11ghariani@gmail.com) 
#
# Input         : Total Buildings.shp, nDSM of All Roofs.tif, Potential Buildings.shp,  nDSM of flat roofs.tif, Sentinel 2 data.tif, Bonn AOI Tile.shp, DTM at Bonn AOI.tif, nDSM at Bonn AOI.tif, Slope at Bonn AOI.tif
#
# Processing    : Generate the the DTM, Normalize the lidar data, Filter the point clouds from the first return, remove the outlines, Slope Buildings tif, Street network shp, Buffer zones tif, 
#
# Output        : AgriRoof R Shiny App
##########################################################################################################################################################################################################################
library(rgdal)
library(raster)
library(plotly)
library(mapview)
library(plainview)
library(ggplot2)
library(tidyverse)

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(leaflet)
library(leaflet.extras)
library(widgetframe)
###############################################################################
# All Layers
############ Final Total Buildings with Area SHP
Buildings_aoi <- readOGR("./www/Buildings_AOI.shp")
Bonn_Buildings <- spTransform(Buildings_aoi, CRS("+proj=longlat +datum=WGS84"))


bins_Tot_Area <- c(0, 400, 1000, 4000, Inf)
pal_Tot_Area <- colorBin("RdPu", domain = Bonn_Buildings$Area, bins = bins_Tot_Area)

labels <- sprintf(
  "<strong>%s</strong><br/>%g m<sup>2</sup>",
  Bonn_Buildings$building, Bonn_Buildings$Area
) %>% 
  lapply(htmltools::HTML) 

###########  Lets include the nDSM of All Roofs

nDSM_All_Buildings <- raster("./www/nDSM_Buildings.tif")

bins_nDSM_AllBuildings <- c(0,5, 10, 20,30,Inf)
pal_nDSM_AllBuildings <- colorBin("Reds", values(nDSM_All_Buildings), bins = bins_nDSM_AllBuildings, na.color = "transparent")

########### the Potential Buildings : Area

Potential_Roofs_Update <- readOGR("./www/FlatRoofs.shp")
Bonn_Potential_Roofs_Update <- spTransform(Potential_Roofs_Update, CRS("+proj=longlat +datum=WGS84"))

bins_Roof_Area_Update <- c(0, 400, 1000, 4000)
pal_Roof_Area_Update <- colorBin("RdPu", domain = Bonn_Potential_Roofs_Update$Area, bins =bins_Roof_Area_Update )

labels_2_update <- sprintf(
  "<strong>%s</strong><br/>%g m<sup>2</sup>",
  Bonn_Potential_Roofs_Update$building, Bonn_Potential_Roofs_Update$Area
) %>% 
  lapply(htmltools::HTML)  

########### Lets include the nDSM of just the flat roofs

nDSM_Flat_Roofs <- raster("./www/ndsm_Flat_Roofs.tif")

bins_nDSM_FlatRoofs <- c(0,5, 10, 20,30,Inf)
pal_nDSM_FlatRoofs <- colorBin("Reds", values(nDSM_Flat_Roofs), bins = bins_nDSM_FlatRoofs, na.color = "transparent")

########### Slope Buildings : 2classes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
slope_Buildings_All <- raster("./www/slope_Buildings.tif")

bins_slope_classes <- c(0,5,Inf)
pal_slope_classes <- colorBin(palette = "YlGn", values(slope_Buildings_All), bins = bins_slope_classes, na.color = "transparent", reverse = TRUE)

########### Street network

street_network <- readOGR("./www/Street_network.shp")
Bonn_street_network <- spTransform(street_network, CRS("+proj=longlat +datum=WGS84"))

########### Shape file of Buffer ~~~~~~~~~~~~~~~~~~~~~~~
Buffer_zones <- readOGR("./www/BufferZones.shp")
Bonn_Buffer_Zones <- spTransform(Buffer_zones, CRS("+proj=longlat +datum=WGS84"))


bins_Buffer_Zones <- c(0,100,500,1000)
pal_Buffer_Zones <- colorBin("RdBu", domain = Bonn_Buffer_Zones$distance, bins =bins_Buffer_Zones)

labels_3 <- sprintf(
  "<strong>%s</strong><br/>%g m",
  Bonn_Buffer_Zones$Buffer, Bonn_Buffer_Zones$distance
) %>% 
  lapply(htmltools::HTML)  


########### DATa pLAYGROUND
#~ Sentinel 2 data
S2_BonnAoi<- stack("./www/S2_BonnAoi.tif")
names(S2_BonnAoi)<- c("R","G","B")
#~ Bonn AOI Tile
Bonn_AOI_Tile <- readOGR("./www/tile_bonnAoi.shp")
Bonn_AOI_Tile <- spTransform(Bonn_AOI_Tile, CRS("+proj=longlat +datum=WGS84"))

my_map<-leaflet() %>% 
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DM") %>%
  addPolylines(data=Bonn_AOI_Tile, 
               weight = 4,
               color = "#de2d26",
               fillOpacity = 0,
               group = "Bonn AOI",
               highlightOptions = highlightOptions(color = "#67000d", weight = 3,
                                                   bringToFront = TRUE)) %>% 
  addLayersControl(
    baseGroups = c("CartoDB.DM","OSM (default)"),
    overlayGroups = c("Bonn AOI"),
    options = layersControlOptions(collapsed = FALSE)
  )
my_map2<-viewRGB(S2_BonnAoi,"R", "G", "B", map = my_map)
# DTM at Bonn AOI
DTM_AOI <- raster("./www/DTM.tif")
pal_DTM_AOI <- colorBin("YlGnBu", values(DTM_AOI), na.color = "transparent")

# nDSM at Bonn AOI
nDSM_AOI <- raster("./www/nDSM_Bonn_Outlier_Gound_Removed.tif")

bins_nDSM_AOI <- c(0,5, 10, 20,30,Inf)
pal_nDSM_AOI <- colorBin("Reds", values(nDSM_AOI), bins = bins_nDSM_AOI, na.color = "transparent")

# Slope at Bonn AOI
Slope_AOI <- raster("./www/Slope_Bonn_Outlier_Gound_Removed.tif")
Slope_AOI

bins_Slope_AOI <- c(0,5, 10, 20,30,40,Inf)
pal_Slope_AOI <- colorBin("YlGn", values(Slope_AOI), bins = bins_Slope_AOI, na.color = "transparent")

#
library(ggplot2)
library(tidyverse)

Overall_Roofs<- read.csv("./www/Overall_Roofs_Area.csv")

#   Since we have a lot of Building type we will take the top 10 and include the rest as others by summing them up
df_filter<-Overall_Roofs %>%
  group_by(building) %>% 
  summarise(Total_area)

#~~ lets take the top 10 Overall Area
df_top10<-df_filter%>% 
  arrange(desc(Total_area)) %>% 
  mutate(building = fct_reorder(building, Total_area)) %>% 
  mutate(building= fct_lump(building, 10)) %>% 
  head(10)

#~~ Lets take the first 12 variables from the tail data
df_12tail<-df_filter%>% 
  arrange(desc(Total_area)) %>% 
  mutate(building = fct_reorder(building, Total_area)) %>% 
  mutate(building= fct_lump(building, 10)) %>% 
  tail(12)
## sum the values of Total area 
sum_tail<-sum(df_12tail$Total_area)

## Create a tibble for this data and classify it as "others" 
df_others<-tibble(building = "Others", Total_area = 11244.98)

df_others<-df_others %>% 
  mutate(building = as.factor(building))

#~~ Merge the two data frame 
Tot_Area<-rbind(df_top10, df_others) 

############ Same with the csv of the POtential rooftops

Potential_Roofs <- read.csv("./www/Potential_Roofs_Area_Update.csv")

#   Since we have a lot of Building type we will take the top 10 and include the rest as others by summing them up
df_filter2<-Potential_Roofs %>%
  group_by(building) %>% 
  summarise(Total_area)

#~~ lets take the top 10 Overall Area
df_top10_2<-df_filter2%>% 
  arrange(desc(Total_area)) %>% 
  mutate(building = fct_reorder(building, Total_area)) %>% 
  mutate(building= fct_lump(building, 10)) %>% 
  head(10)

#~~ Lets take the first 12 variables from the tail data
df_12tail_2<-df_filter2%>% 
  arrange(desc(Total_area)) %>% 
  mutate(building = fct_reorder(building, Total_area)) %>% 
  mutate(building= fct_lump(building, 10)) %>% 
  tail(12)
## sum the values of Total area 
sum_tail_2<-sum(df_12tail_2$Total_area)
## Create a tibble for this data and classify it as "others" 
df_others_2<-tibble(building = "Others", Total_area = 2176.979)

df_others_2<-df_others_2 %>% 
  mutate(building = as.factor(building))

#~~ Merge the two data frame 
Tot_PotentialArea<-rbind(df_top10_2, df_others_2) 

############################  User Interface 
ui <-  
  dashboardPage(skin = "green",
                dashboardHeader(title = "AgriRoof: Potential Flat Rooftop Detection using LiDAR and GIS for Rooftop Farming",
                                disable = FALSE, 
                                titleWidth  = 800),
                dashboardSidebar(tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",
                                            ".shiny-output-error:before { visibility: hidden; }"),
                                 width = 300,
                                 sidebarMenu(id="tabs",
                                             menuItem("Potential Rooftops", tabName="Maps", icon=icon("line-chart")),
                                             menuItem("Data Playground: Extra Data",  icon = icon("file-text-o"),
                                                      menuSubItem("Sentinel 2 Data", tabName = "S2", icon = icon("angle-right")),
                                                      menuSubItem("DTM, Height and Slope", tabName = "DTM_nDSM_Slope", icon = icon("angle-right")),
                                                      menuSubItem("Stats", tabName = "stats", icon = icon("angle-right"))
                                             ),
                                             menuItem("ReadMe", tabName = "readme", icon=icon("mortar-board")),
                                             menuItem("About", tabName = "about", icon = icon("question"))
                                 )
                ),
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "Maps",
                            fluidPage(titlePanel("Data visualisation"),
                                      fluidRow(
                                        box(title = "Bonn AOI",solidHeader = TRUE, collapsible = TRUE,status = "success",background = "black",width =12,height = 550,
                                            leafletOutput(outputId = "layers",height = "495px") %>% 
                                              withSpinner(color="#addd8e")
                                        )
                                      )
                            )
                    ),
                    tabItem(tabName = "S2",
                            fluidPage(titlePanel("Data visualisation"),
                                      fluidRow(
                                        box(title = "Sentinel 2 RGB over Bonn AOI ",solidHeader = TRUE, collapsible = TRUE,status = "success",background = "black",width =12,height = 550,
                                            leafletOutput(outputId = "S2_BonnAOI",height = "495px") %>% 
                                              withSpinner(color="#addd8e")
                                        )
                                      )
                            )
                    ),
                    tabItem(tabName = "DTM_nDSM_Slope",
                            fluidPage(titlePanel("Data visualisation"),
                                      fluidRow(
                                        box(title = "Bonn AOI",solidHeader = TRUE, collapsible = TRUE,status = "success",background = "black",width =12,height = 550,
                                            leafletOutput(outputId = "DTM_Height_Slope",height = "495px") %>% 
                                              withSpinner(color="#addd8e")
                                        )
                                      )
                            )
                    ),
                    tabItem(tabName = "stats",
                            fluidPage(titlePanel("Data Visualisation"),
                                      fluidRow(
                                        box( title = "Controls",solidHeader = TRUE,collapsible = TRUE,status = "success",background = "black",width =3,height = 470,
                                             radioButtons(inputId = "plots", label = "Select Layer",
                                                          c("Overall Rooftops" = "ORT",
                                                            "Potential Flat Rooftops" = "PFRT"))
                                        ),
                                        box(title = "Bar Chart",status = "success",solidHeader = TRUE,collapsible = TRUE,background = "black",width =9,height = 470,
                                            plotlyOutput(outputId="barplot",height="400px")%>% 
                                              withSpinner(color="#addd8e"))
                                      )
                            ) 
                    ),
                    tabItem(tabName = "readme",
                            fluidPage(
                              tags$iframe(src = './readme.html', 
                                          width = '100%', height = '800px',
                                          frameborder = 0, scrolling = 'auto'
                              )
                            )
                    ),
                    tabItem(tabName = "about",
                            fluidPage(
                              tags$iframe(src = './about.html', 
                                          width = '100%', height = '800px',
                                          frameborder = 0, scrolling = 'auto'
                              )
                            )
                    )
                  )
                )
  )
############################ Server Interface 

server <- function(input, output) {
  
  output$layers <- renderLeaflet({
    leaflet(data =  Bonn_Buildings) %>%
      setView(7.07800, 50.7400,13.5) %>% 
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DM") %>%
      addPolygons(fillColor = ~pal_Tot_Area(Area),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "2",
                  fillOpacity = 0.7,
                  group='Buildings Footprint',
                  highlightOptions = highlightOptions(color = "#67000d", weight = 3,
                                                      bringToFront = TRUE), 
                  label = labels ,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto")) %>% 
      addRasterImage(slope_Buildings_All, colors = pal_slope_classes, opacity = .8, group = "Potential Rooftops: Slope (\u00B0)") %>%
      addRasterImage(nDSM_Flat_Roofs, colors = pal_nDSM_FlatRoofs, opacity = 0.8, group = "Potential Rooftops : Height (m)") %>%
      addRasterImage(nDSM_All_Buildings, colors = pal_nDSM_AllBuildings, opacity = 0.8, group = "Buildings Footprint: Height (m)") %>%
      addPolygons(data = Bonn_Potential_Roofs_Update,
                  fillColor = ~pal_Roof_Area_Update(Area),
                  weight = 2,
                  color = "white",
                  dashArray = "1",
                  opacity = 1,
                  fillOpacity = .7,
                  group='Potential Rooftops: Area (m\u00B2)',
                  highlightOptions = highlightOptions(color = "#67000d", weight = 3,
                                                      bringToFront = TRUE), 
                  label = labels_2_update,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto")) %>% 
      addPolylines(data=Bonn_street_network, 
                   weight = 2,
                   color = "#de2d26",
                   label = ~highway,
                   fillOpacity = 0,
                   group = "street network",
                   highlightOptions = highlightOptions(color = "#67000d", weight = 3,
                                                       bringToFront = TRUE)) %>% 
      addPolygons(data= Bonn_Buffer_Zones,
                  fillColor = ~pal_Buffer_Zones(distance),
                  weight = 2,
                  group = "Buffer Zones",
                  highlightOptions = highlightOptions(color = "#67000d", weight = 3,
                                                      bringToFront = TRUE),
                  label = labels_3) %>% 
      addLegend(pal = pal_Tot_Area , values = ~Area, opacity = 0.7, 
                position = "bottomleft", title = 'Rooftop Area (m2)',group = 'Buildings Footprint') %>%
      addLegend(pal = pal_nDSM_AllBuildings, values = values(nDSM_All_Buildings),
                title = "Buildings Footprint: Height (m)",position = "bottomleft", group = "Buildings Footprint: Height (m)") %>%
      addLegend(pal = pal_Roof_Area_Update , values = ~Area, opacity = 0.7, 
                position = "bottomright", title = 'Potential Rooftops: Area (m\u00B2)',group = 'Potential Rooftops: Area (m\u00B2)') %>% 
      addLegend(pal = pal_slope_classes, values = values(slope_Buildings_All),
                title = "Potential Rooftops: Slope (\u00B0)",position = "bottomright", group = "Potential Rooftops: Slope (\u00B0)") %>%
      addLegend(pal = pal_nDSM_FlatRoofs, values = values(nDSM_Flat_Roofs),
                title = "Potential Rooftops : Height (m)",position = "bottomleft", group = "Potential Rooftops : Height (m)") %>%
      addLayersControl(
        baseGroups = c("CartoDB.DM","OSM (default)"),
        overlayGroups = c("Buildings Footprint","Buildings Footprint: Height (m)","Potential Rooftops: Area (m\u00B2)","Potential Rooftops: Slope (\u00B0)","Potential Rooftops : Height (m)","street network","Buffer Zones"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters",position = "topleft") %>% 
      hideGroup(c("Buildings Footprint","Buildings Footprint: Height (m)","Potential Rooftops: Area (m\u00B2)","Potential Rooftops : nDSM (m)","Potential Rooftops : Height (m)"))
  })
  output$S2_BonnAOI <-  renderLeaflet({my_map2@map}) # RGB: True Color Composite
  
  output$DTM_Height_Slope <- renderLeaflet({
    leaflet(nDSM_AOI) %>%
      setView(7.07800, 50.7400,13.5) %>% 
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DM") %>% 
      addRasterImage(nDSM_AOI, colors = pal_nDSM_AOI, opacity = 0.8, group = "nDSM at Bonn AOI (m)") %>%
      addRasterImage(DTM_AOI,colors = pal_DTM_AOI, opacity = 0.8, group = "Bonn AOI: DTM") %>% 
      addRasterImage(Slope_AOI,colors = pal_Slope_AOI, opacity = 0.8, group = "Bonn AOI: Slope (\u00B0)") %>%
      addLegend(pal = pal_DTM_AOI, values = values(DTM_AOI),
                title = "Bonn AOI: DTM",position = "bottomleft", group = "Bonn AOI: DTM") %>% 
      addLegend(pal = pal_nDSM_AOI, values = values(nDSM_AOI),
                title = "nDSM at Bonn AOI (m)",position = "bottomright", group = "nDSM at Bonn AOI (m)") %>%
      addLegend(pal = pal_Slope_AOI, values = values(Slope_AOI),
                title = "Bonn AOI: Slope (\u00B0)",position = "bottomleft", group = "Bonn AOI: Slope (\u00B0)") %>%
      addLayersControl(
        baseGroups = c("CartoDB.DM","OSM (default)"),
        overlayGroups = c("Bonn AOI: DTM","nDSM at Bonn AOI (m)","Bonn AOI: Slope (\u00B0)"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      hideGroup(c("nDSM at Bonn AOI (m)","Bonn AOI: Slope (\u00B0)"))
    
  })   
  output$barplot <- renderPlotly({
    if(input$plots == "ORT"){
      
      #~~ VIZ the Total Area of overall rooftops by Building Types
      viz_TotArea<-Tot_Area %>%
        arrange(desc(Total_area)) %>% 
        mutate(building = fct_reorder(building, Total_area)) %>% 
        ggplot(aes(building, Total_area))+
        geom_col(aes(fill= Total_area))+
        scale_fill_gradient(low = "#fff7f3", high = "#49006a", na.value = NA)+
        coord_flip()+
        geom_text(aes(label=round(Total_area)), vjust=0, color="#525252",
                  size=3.5,
                  hjust=0)+
        theme(legend.position = "none")+
        theme(axis.line.y = element_blank(),
              panel.grid = element_blank(),
              plot.title = element_text(size=14, lineheight = 0.9,face="bold"),
              plot.subtitle = element_text(size=13, lineheight = 0.9),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              legend.title=element_text(size=12),
              legend.text=element_text(size=10),
              axis.title.y = element_text(size=12,face = "bold"),
              axis.title.x = element_text(size=12,face="bold"))+
        labs(x="Building Types",y = "Total Area (m\u00B2)")+
        ggtitle("Total Area of Overall Rooftops by Building Types")
      viz_TotArea
    }
    else if (input$plots == "PFRT"){
      
      #~~ VIZ the Total Area of Potential rooftops by Building Types
      viz_PotArea<-Tot_PotentialArea %>%
        arrange(desc(Total_area)) %>% 
        mutate(building = fct_reorder(building, Total_area)) %>% 
        ggplot(aes(building, Total_area))+
        geom_col(aes(fill= Total_area))+
        scale_fill_gradient(low = "#fff7f3", high = "#49006a", na.value = NA)+
        coord_flip()+
        geom_text(aes(label=round(Total_area)), vjust=0, color="#525252",
                  size=3.5,
                  hjust=0)+
        theme(legend.position = "none")+
        theme(axis.line.y = element_blank(),
              panel.grid = element_blank(),
              plot.title = element_text(size=14, lineheight = 0.9,face="bold"),
              plot.subtitle = element_text(size=13, lineheight = 0.9),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              legend.title=element_text(size=12),
              legend.text=element_text(size=10),
              axis.title.y = element_text(size=12,face = "bold"),
              axis.title.x = element_text(size=12,face="bold"))+
        labs(x="Building Types",y = "Total Area (m\u00B2)")+
        ggtitle("Total Area of Potential Rooftops by Building Types")
      viz_PotArea
    }
    
  })
  
  
}

shinyApp(ui = ui, server = server) 
