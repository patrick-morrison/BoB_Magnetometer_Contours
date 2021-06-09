# Written by Patrick Morrison for the Western Australian Museum, 2021

library(shiny)
library(akima) #interpolation
library(raster) #spatial rasters
library(maptools) # spatial contours
library(rgdal) #spatial
library(sp) #spatial 
library(lubridate) #dates
library(tidyverse) #general data wrangling
library(viridis) #palettes
library(leaflet)
options(digits=10)
theme_set(theme_classic())
options(shiny.maxRequestSize = 30*1024^2)

ui <- fluidPage(
    
    # Application title
    titlePanel("Magnetometer Contours"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            p("Interpolating magnetometer data using kriging, and converting it to a spatial format"),
            p("This application receives a raw BoB output as a csv file. It expects header rows,
              and the columns: Reading_Date, Reading_Time, Mag_Longitude, Mag_Latitude, and Magnetic_Field."),
            p("The BoB header info can be different lenghts, so check the file for how many rows to skip. 5 or 6 is common:"),
            numericInput("skip", "Skip rows", value=5, step = 1),
            p('If you have a custom (non-BoB) file, name the fields exactly date, time, lat, long and mag, and uncheck the raw data box below.'),
            checkboxInput("raw", "Raw data from BoB", TRUE),
            fileInput("csv", 'BoB csv file:', multiple = FALSE, accept = NULL, width = NULL),
            h3("Interpolation"),
            p('Turn up until it looks right.'),
            sliderInput("resx", h5("Resolution x"),
                        min = 20, max = 4000, value = 100),
            sliderInput("resy", h5("Resolution y"),
                        min = 20, max = 4000, value = 100),
            p("Use the graph to check output before downloading. This will output files for QGIS, in WGS84."),
            downloadButton("downloadpoints", "Download points"),
            downloadButton("downloadcontours", "Download contour"),
            downloadButton("downloadraster", "Download raster"),
            downloadButton("downloadcsv", "Download csv"),
            h3("Filtering"),
            checkboxInput("datefilter", "Filter date?", FALSE),
            dateInput("datestart", "Date start (YYYY-MM-DD)", value = "2021-05-19"),
            dateInput("dateend", "Date end (YYYY-MM-DD)", value = "2022-05-19"),
            checkboxInput("timefilter", "Filter time?", FALSE),
            textInput('timestart', "Time start (HH:MM:SS)", "03:53:45"),
            textInput('timeend', "Time end (HH:MM:SS)", "12:53:45"),
        h3("Data:"),
        tableOutput("table")),
        
        mainPanel(
            #plotOutput('finished_plot'),
            tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
            leafletOutput("mymap"),
            
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    mag <- reactive({
        req(input$csv)
        if (input$raw) {
        mag_import <- read_csv(input$csv$datapath, skip=input$skip) %>%
            dplyr::select('date' = Reading_Date, 'time' = Reading_Time, 'long' = Mag_Longitude,
                          'lat' = Mag_Latitude, 'mag' = Magnetic_Field) %>% #change this line to match data
            na_if('*') %>%  na_if(0.00) %>% na.omit() %>% mutate_at(c('lat', 'long'), as.numeric)
        }
        
        if (!input$raw) {
        mag_import <- read_csv(input$csv$datapath) %>% mutate_at(c('lat', 'long', 'mag'), as.numeric) %>% 
            na_if('*') %>%  na_if(0.00) %>% na.omit()
        }
        
        if(input$timefilter) {
            mag_import <- mag_import %>% filter(time >= hms(input$timestart),
                                                time <= hms(input$timeend))
        }
        
        if(input$datefilter) {
            mag_import <- mag_import %>% filter(dmy(date) >= ymd(input$datestart),
                                                dmy(date) <= ymd(input$dateend))
        }
        
        mag_frame <- as.data.frame(mag_import)
        coordinates(mag_frame)<-~long+lat
        try(mag_frame$time <- as.character(mag_frame$time)) #needed to write shape file properly
        return(mag_frame)
    })
    
    z <- reactive({
        req(mag())
        z <- interp(mag(),z="mag",nx=input$resx,ny=input$resy, duplicate='median')
        return(z)
    })
    
    output$mymap <- renderLeaflet({
        contours <- ContourLines2SLDF(contourLines(z()))
        pal <- colorNumeric(
            palette = "viridis",
            domain = mag()$mag,
            reverse = TRUE)
        leaflet(options = leafletOptions(minZoom = 0, maxZoom = 23, preferCanvas = TRUE)) %>%
            addTiles(group = "OpenStreetMap", options=tileOptions(updateWhenZooming = FALSE)) %>%
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron", options=tileOptions(updateWhenZooming = FALSE)) %>% 
            addProviderTiles(providers$Esri.OceanBasemap, group = "ESRI Ocean", options=tileOptions(updateWhenZooming = FALSE)) %>% 
            addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery", options=tileOptions(updateWhenZooming = FALSE)) %>% 
            addRasterImage(raster(z()), colors = rev(viridis(300)), opacity = 0.9, group = "Raster") %>%
            addPolylines(data=contours, weight=1, color="black", group = "Contours") %>% 
            addCircles(data= mag(), radius = 1, weight=4, group='Points',
                             fillColor = 'black', stroke = TRUE, color=~pal(mag), label=~mag) %>%
            hideGroup("Points") %>% 
            addLayersControl(
                baseGroups = c("OpenStreetMap", "ESRI Ocean", "CartoDB Positron", "ESRI World Imagery"),
                overlayGroups = c("Contours", "Raster", "Points")) %>% 
            addMeasure(primaryLengthUnit = "meters", secondaryLengthUnit  = "feet", primaryAreaUnit="sqmeters") %>% addScaleBar(position ="bottomleft")
    })

    
    output$downloadpoints <- downloadHandler(
        filename = "mag_points.geojson",
        content = function(file) {
            rgdal::writeOGR(mag(), layer='mag_points', file, driver="GeoJSON", overwrite_layer=TRUE)
        }
    )
    output$downloadcontours <- downloadHandler(
        filename = "mag_contours.geojson",
        content = function(file) {
            contours <- ContourLines2SLDF(contourLines(z()))
            rgdal::writeOGR(contours, layer='mag_contours', file, driver="GeoJSON", overwrite_layer=TRUE)
        }
    )
    output$downloadraster <- downloadHandler(
        filename = "mag_raster.tif",
        content = function(file) {
            writeRaster(raster(z()), file, overwrite=TRUE)
        }
    )
    
    output$downloadcsv <- downloadHandler(
        filename = "mag_processed.csv",
        content = function(file) {
            write_csv(data.frame(mag()), file)
        }
    )
    
    output$table <- renderTable({
        if (is.null(mag()))
            return(NULL)
        mag()
    }, digits=10)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
