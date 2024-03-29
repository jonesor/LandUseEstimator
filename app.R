# Load libraries -----

library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggthemes)
library(raster)
library(rgdal)
library(sf)

# Load the CORINE data -----
if (!exists("corine_DK")) {
  corine_DK <- raster("www/DenmarkCorineRaster.tif")
}

# UI part of the shiny app -----
ui <- shinyUI(fluidPage(
  titlePanel("Land use estimator"),
  tabsetPanel(
    tabPanel(
      "Upload Your Data",
      titlePanel(""),
      sidebarLayout(
        sidebarPanel(
          fileInput("file1", "Choose CSV File",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),
          tags$br(),
          checkboxInput("header", "Header", TRUE),
          radioButtons(
            "sep", "Separator",
            c(
              Comma = ",",
              Semicolon = ";",
              Tab = "\t"
            ),
            ","
          ),
          radioButtons(
            "quote", "Quote",
            c(
              None = "",
              "Double Quote" = '"',
              "Single Quote" = "'"
            ),
            '"'
          ),
          numericInput("buffer_m", "Buffer (m):", 2000, min = 1, max = 5000),
        ),
        mainPanel(
          tableOutput("contents")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot of locations", plotOutput("plot")),
        tabPanel(
          "Land use summary",
          tableOutput("table"),
          downloadButton("downloadData", "Download")
        )
      )
    )
  )
))

# Server part of shiny
server <- shinyServer(function(input, output, session) {
  # Create a data frame to store land use values and corresponding labels
  landUseLookUp <- data.frame(value = 1:50) %>%
    # Add a column for the land use labels
    mutate(broadLandUse = value) %>%
    # Assign "Urban" label to values 1 through 9
    mutate(broadLandUse = ifelse(broadLandUse %in% 1:9, "Urban", broadLandUse)) %>%
    # Assign "Park" label to values 10 through 11
    mutate(broadLandUse = ifelse(broadLandUse %in% 10:11, "Park", broadLandUse)) %>%
    # Assign "Agriculture" label to values 12 through 22
    mutate(broadLandUse = ifelse(broadLandUse %in% 12:22, "Agriculture", broadLandUse)) %>%
    # Assign "Forest/Seminatural" label to values 23 through 34
    mutate(broadLandUse = ifelse(broadLandUse %in% 23:34, "Forest/Seminatural", broadLandUse)) %>%
    # Assign "Wetlands" label to values 35 through 39
    mutate(broadLandUse = ifelse(broadLandUse %in% 35:39, "Wetlands", broadLandUse)) %>%
    # Assign "Water bodies" label to values 40 through 43
    mutate(broadLandUse = ifelse(broadLandUse %in% 40:43, "Water bodies", broadLandUse)) %>%
    # Assign "Ocean" label to value 44
    mutate(broadLandUse = ifelse(broadLandUse %in% 44, "Ocean", broadLandUse)) %>%
    # Remove NA values for values 48 through 50
    mutate(broadLandUse = ifelse(broadLandUse %in% 48:50, NA, broadLandUse)) %>%
    na.omit()
  
  # Join land use data with CORINE data
  corine_DK_df <- as.data.frame(corine_DK, xy = TRUE) %>%
    rename(value = DenmarkCorineRaster) %>%
    left_join(landUseLookUp) %>%
    # Remove "Ocean" and "Water bodies" entries
    filter(broadLandUse != "Ocean") %>%
    filter(broadLandUse != "Water bodies")
  
  # Import data from file uploaded by user
  df_coord_raw <- reactive({
    # Require that the input file is available
    req(input$file1)
    # Store the input file
    inFile <- input$file1
    # Read the CSV file
    df_coord_raw <- read.csv(inFile$datapath,
                             header = input$header, sep = input$sep,
                             quote = input$quote
    )
    # Return the imported data
    return(df_coord_raw)
  })
  
  # Render a table of the input data
  output$contents <- renderTable({
    df_coord_raw()
  })
  
  # Convert coordinates to EU standard and store in new data frame
  df_coord_3035 <- reactive({
    req(df_coord_raw()) ## ?req #  require that the input is available

    df_coord_raw <- df_coord_raw()

    df_coord_4326 <- df_coord_raw %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) # set as simple features, with crs = 4326

    # Convert the coordinates to the EU standard
    df_coord_3035 <- st_transform(df_coord_4326, corine_DK@crs)
    
    return(df_coord_3035)
  })

  output$plot <- renderPlot({
    df_coord_3035 <- df_coord_3035()
    dataExtent <- extent(df_coord_3035)
    dataExtent[1] <- dataExtent[1] - 15000
    dataExtent[2] <- dataExtent[2] + 15000
    dataExtent[3] <- dataExtent[3] - 15000
    dataExtent[4] <- dataExtent[4] + 15000

    corine_visualiseMap <- crop(corine_DK, dataExtent)
    corine_visualiseMap_df <- as.data.frame(corine_visualiseMap, xy = TRUE) %>%
      rename(value = DenmarkCorineRaster) %>%
      left_join(landUseLookUp) %>%
      filter(broadLandUse != "Ocean") %>%
      filter(broadLandUse != "Water bodies")

    ggplot() +
      geom_raster(data = corine_visualiseMap_df, aes(x = x, y = y, fill = broadLandUse)) +
      scale_fill_colorblind(name = "") +
      coord_equal() +
      theme_map() +
      geom_sf(data = df_coord_3035, colour = "red") +
      NULL
  })


  landUseSummary <- reactive({
    df_coord_3035 <- df_coord_3035()
    df_coord_raw <- df_coord_raw()

    # Extract land use codes
    Landcover <- raster::extract(x = corine_DK, df_coord_3035, buffer = input$buffer_m)
    names(Landcover) <- df_coord_raw$addressID

    ## Compute maximum length
    max.length <- max(sapply(Landcover, length))
    ## Add NA values to list elements
    Landcover2 <- lapply(Landcover, function(v) {
      c(v, rep(NA, max.length - length(v)))
    })
    ## cbind
    Landcover2 <- do.call(cbind, Landcover2)

    x <- data.frame(Landcover2) %>%
      pivot_longer(data = ., cols = everything(), names_to = "addressID", values_to = "value") %>%
      arrange(addressID) %>%
      left_join(landUseLookUp)

    outputLandUse <- x %>%
      mutate(item = 1) %>%
      group_by(addressID) %>%
      summarise(
        total = sum(item[!is.na(broadLandUse)], na.rm = TRUE),
        Urban = sum(item[broadLandUse == "Urban"], na.rm = TRUE),
        Park = sum(item[broadLandUse == "Park"], na.rm = TRUE),
        Agriculture = sum(item[broadLandUse == "Agriculture"], na.rm = TRUE),
        ForestSemiNat = sum(item[broadLandUse == "Forest/Seminatural"], na.rm = TRUE),
        Wetlands = sum(item[broadLandUse == "Wetlands"], na.rm = TRUE)
      ) %>%
      mutate(
        Urban = Urban / total, Park = Park / total, Agriculture = Agriculture / total,
        ForestSemiNat = ForestSemiNat / total, Wetlands = Wetlands / total
      ) %>%
      dplyr::select(-total)

    # Render the data.frame
    return(outputLandUse)
  })

  output$table <- renderTable({
    landUseSummary()
  })

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(landUseSummary(), file, row.names = FALSE)
    }
  )
})

shinyApp(ui, server)
