# Load libraries -----

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(terra)

# Prefer disk-backed operations to reduce memory pressure on shinyapps.io.
terra::terraOptions(todisk = TRUE, memfrac = 0.5)

# Raster helpers (terra) -----
MIN_BUFFER_M <- 100

read_corine <- function(path) {
  terra::rast(path)
}

raster_crs <- function(x) {
  terra::crs(x, proj = TRUE)
}

raster_extent <- function(x) {
  terra::ext(x)
}

extent_bounds <- function(ext) {
  c(
    xmin = terra::xmin(ext),
    xmax = terra::xmax(ext),
    ymin = terra::ymin(ext),
    ymax = terra::ymax(ext)
  )
}

make_extent <- function(xmin, xmax, ymin, ymax, template) {
  terra::ext(xmin, xmax, ymin, ymax)
}

raster_crop <- function(x, ext) {
  terra::crop(x, ext)
}

raster_extract <- function(x, points, buffer_m) {
  point_count <- nrow(points)
  buf <- terra::buffer(terra::vect(points), width = buffer_m)
  vals <- terra::extract(x, buf, list = TRUE)
  if (is.data.frame(vals)) {
    id_col <- if ("ID" %in% names(vals)) "ID" else names(vals)[1]
    value_cols <- setdiff(names(vals), id_col)
    value_col <- if (length(value_cols) > 0) value_cols[1] else id_col
    split_vals <- split(vals[[value_col]], vals[[id_col]])
    out <- vector("list", point_count)
    for (i in seq_len(point_count)) {
      key <- as.character(i)
      out[[i]] <- if (key %in% names(split_vals)) {
        normalize_values(split_vals[[key]])
      } else {
        numeric(0)
      }
    }
    return(out)
  }
  if (length(vals) == 0) {
    return(rep(list(numeric(0)), point_count))
  }
  out <- lapply(vals, function(v) {
    if (is.data.frame(v) && ncol(v) >= 1) {
      value_cols <- setdiff(names(v), c("ID", "id"))
      value_col <- if (length(value_cols) > 0) value_cols[1] else names(v)[1]
      return(normalize_values(v[[value_col]]))
    }
    if (is.vector(v)) {
      return(normalize_values(v))
    }
    numeric(0)
  })
  if (length(out) < point_count) {
    out <- c(out, rep(list(numeric(0)), point_count - length(out)))
  }
  out
}

raster_ncell <- function(x) {
  terra::ncell(x)
}

downsample_for_plot <- function(x, max_cells = 200000) {
  n_cells <- raster_ncell(x)
  if (is.na(n_cells) || n_cells <= max_cells) {
    return(x)
  }
  fact <- ceiling(sqrt(n_cells / max_cells))
  mode_value <- function(vals, ...) {
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) {
      return(NA_real_)
    }
    uniq <- unique(vals)
    uniq[which.max(tabulate(match(vals, uniq)))]
  }
  terra::aggregate(x, fact = fact, fun = mode_value, na.rm = TRUE)
}

raster_to_df <- function(x, na_rm = TRUE) {
  df <- terra::as.data.frame(x, xy = TRUE, na.rm = na_rm)
  value_col <- setdiff(names(df), c("x", "y"))[1]
  if (!is.null(value_col)) {
    names(df)[names(df) == value_col] <- "value"
  }
  df
}

guess_col <- function(cols, patterns, fallback) {
  lower_cols <- tolower(cols)
  hits <- which(lower_cols %in% patterns)
  if (length(hits) > 0) {
    return(cols[hits[1]])
  }
  hits <- which(grepl(paste(patterns, collapse = "|"), lower_cols))
  if (length(hits) > 0) {
    return(cols[hits[1]])
  }
  fallback
}

normalize_values <- function(x) {
  if (is.factor(x)) {
    return(as.numeric(as.character(x)))
  }
  if (is.character(x)) {
    return(suppressWarnings(as.numeric(x)))
  }
  x
}

corine_get <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      return(cache)
    }
    cache <<- tryCatch(
      read_corine("www/DenmarkCorineRaster.tif"),
      error = function(e) {
        stop("Failed to load raster at www/DenmarkCorineRaster.tif: ", e$message)
      }
    )
    cache
  }
})

# UI part of the shiny app -----
ui <- shinyUI(fluidPage(
  tags$head(
    tags$style(HTML("
      #plot {
        width: 100% !important;
        height: 70vh !important;
        min-height: 420px;
        max-height: 900px;
      }
      @media (min-width: 1400px) {
        #plot {
          height: 80vh !important;
          max-height: 1100px;
        }
      }
    "))
  ),
  titlePanel("Land use estimator"),
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
      selectInput("id_col", "ID column", choices = character(0)),
      selectInput("lon_col", "Longitude column", choices = character(0)),
      selectInput("lat_col", "Latitude column", choices = character(0)),
      numericInput("buffer_m", "Buffer (m):", 2000, min = MIN_BUFFER_M, max = 5000),
      checkboxInput("show_legend", "Show map legend", TRUE),
      downloadButton("downloadSample", "Download Sample CSV"),
      tags$hr(),
      tableOutput("contents")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot of locations", plotOutput("plot", height = "70vh")),
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
  landUseLookUp <- data.frame(value = 1:50) |>
    # Add a column for the land use labels
    mutate(broadLandUse = value) |>
    # Assign "Urban" label to values 1 through 9
    mutate(broadLandUse = ifelse(broadLandUse %in% 1:9, "Urban", broadLandUse)) |>
    # Assign "Park" label to values 10 through 11
    mutate(broadLandUse = ifelse(broadLandUse %in% 10:11, "Park", broadLandUse)) |>
    # Assign "Agriculture" label to values 12 through 22
    mutate(broadLandUse = ifelse(broadLandUse %in% 12:22, "Agriculture", broadLandUse)) |>
    # Assign "Forest/Seminatural" label to values 23 through 34
    mutate(broadLandUse = ifelse(broadLandUse %in% 23:34, "Forest/Seminatural", broadLandUse)) |>
    # Assign "Wetlands" label to values 35 through 39
    mutate(broadLandUse = ifelse(broadLandUse %in% 35:39, "Wetlands", broadLandUse)) |>
    # Assign "Water" label to values 40 through 43
    mutate(broadLandUse = ifelse(broadLandUse %in% 40:43, "Water", broadLandUse)) |>
    # Assign "Water" label to value 44
    mutate(broadLandUse = ifelse(broadLandUse %in% 44, "Water", broadLandUse)) |>
    # Treat values 48 through 50 as Water
    mutate(broadLandUse = ifelse(broadLandUse %in% 48:50, "Water", broadLandUse))

  land_use_levels <- c(
    "Urban",
    "Park",
    "Agriculture",
    "Forest/Seminatural",
    "Wetlands",
    "Water"
  )

  land_use_palette <- c(
    "Urban" = "#b2182b",
    "Park" = "#fee08b",
    "Agriculture" = "#fdae61",
    "Forest/Seminatural" = "#1b7837",
    "Wetlands" = "#a6d96a",
    "Water" = "#bdbdbd"
  )
  corine_raster <- reactive({
    corine_get()
  })
  
  # Import data from file uploaded by user (or default sample)
  df_coord_raw <- reactive({
    if (is.null(input$file1)) {
      df_coord_raw <- read.csv(
        "sample_data/sample_points.csv",
        header = TRUE,
        sep = ",",
        quote = "\"",
        stringsAsFactors = FALSE
      )
    } else {
      inFile <- input$file1
      df_coord_raw <- read.csv(
        inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        stringsAsFactors = FALSE
      )
    }

    validate(
      need(ncol(df_coord_raw) > 0, "Uploaded file has no columns."),
      need(ncol(df_coord_raw) >= 3, "CSV must have at least 3 columns.")
    )

    return(df_coord_raw)
  })
  
  # Render a table of the input data
  output$contents <- renderTable({
    df_coord_raw()
  })

  observeEvent(df_coord_raw(), {
    cols <- names(df_coord_raw())
    default_id <- guess_col(cols, c("addressid", "id", "address_id", "addr_id"), cols[1])
    default_lon <- guess_col(cols, c("longitude", "lon", "long", "lng", "x"), cols[1])
    default_lat <- guess_col(cols, c("latitude", "lat", "y"), cols[1])

    if (!is.null(input$id_col) && input$id_col %in% cols) {
      default_id <- input$id_col
    }
    if (!is.null(input$lon_col) && input$lon_col %in% cols) {
      default_lon <- input$lon_col
    }
    if (!is.null(input$lat_col) && input$lat_col %in% cols) {
      default_lat <- input$lat_col
    }

    updateSelectInput(session, "id_col", choices = cols, selected = default_id)
    updateSelectInput(session, "lon_col", choices = cols, selected = default_lon)
    updateSelectInput(session, "lat_col", choices = cols, selected = default_lat)
  }, ignoreInit = TRUE)

  resolved_cols <- reactive({
    req(df_coord_raw())
    cols <- names(df_coord_raw())

    id_col <- input$id_col
    lon_col <- input$lon_col
    lat_col <- input$lat_col

    if (is.null(id_col) || !(id_col %in% cols)) {
      id_col <- guess_col(cols, c("addressid", "id", "address_id", "addr_id"), cols[1])
      updateSelectInput(session, "id_col", choices = cols, selected = id_col)
    }
    if (is.null(lon_col) || !(lon_col %in% cols)) {
      lon_col <- guess_col(cols, c("longitude", "lon", "long", "lng", "x"), cols[1])
      updateSelectInput(session, "lon_col", choices = cols, selected = lon_col)
    }
    if (is.null(lat_col) || !(lat_col %in% cols)) {
      lat_col <- guess_col(cols, c("latitude", "lat", "y"), cols[1])
      updateSelectInput(session, "lat_col", choices = cols, selected = lat_col)
    }

    list(id = id_col, lon = lon_col, lat = lat_col)
  })
  
  # Convert coordinates to EU standard and store in new data frame
  df_coord_3035 <- reactive({
    req(df_coord_raw())
    df_coord_raw <- df_coord_raw()
    cols <- resolved_cols()
    id_col <- cols$id
    lon_col <- cols$lon
    lat_col <- cols$lat

    validate(
      need(!is.null(id_col) && !is.null(lon_col) && !is.null(lat_col),
        "Select ID, longitude, and latitude columns."
      ),
      need(lon_col %in% names(df_coord_raw) && lat_col %in% names(df_coord_raw),
        "Selected longitude/latitude columns do not exist."
      )
    )

    lon_vals <- suppressWarnings(as.numeric(df_coord_raw[[lon_col]]))
    lat_vals <- suppressWarnings(as.numeric(df_coord_raw[[lat_col]]))
    bad_lon <- is.na(lon_vals) & !is.na(df_coord_raw[[lon_col]])
    bad_lat <- is.na(lat_vals) & !is.na(df_coord_raw[[lat_col]])
    validate(
      need(!any(bad_lon) && !any(bad_lat),
        "Selected longitude and latitude columns must be numeric."
      )
    )

    df_coord_raw[[lon_col]] <- lon_vals
    df_coord_raw[[lat_col]] <- lat_vals

    df_coord_4326 <- df_coord_raw |>
      st_as_sf(coords = c(lon_col, lat_col), crs = st_crs(4326))

    df_coord_3035 <- st_transform(df_coord_4326, st_crs(raster_crs(corine_raster())))

    return(df_coord_3035)
  })

  points_in_bounds <- reactive({
    df_coord_3035 <- df_coord_3035()
    data_bbox <- st_bbox(df_coord_3035)
    ext <- raster_extent(corine_raster())
    bounds <- extent_bounds(ext)

    data_bbox["xmin"] >= bounds["xmin"] &&
      data_bbox["xmax"] <= bounds["xmax"] &&
      data_bbox["ymin"] >= bounds["ymin"] &&
      data_bbox["ymax"] <= bounds["ymax"]
  })

  corine_extract_raster <- reactive({
    df_coord_3035 <- df_coord_3035()
    buffer_m <- max(MIN_BUFFER_M, input$buffer_m)
    ext <- raster_extent(corine_raster())
    bounds <- extent_bounds(ext)
    data_bbox <- st_bbox(df_coord_3035)

    cropped_extent <- make_extent(
      max(bounds["xmin"], data_bbox["xmin"] - buffer_m),
      min(bounds["xmax"], data_bbox["xmax"] + buffer_m),
      max(bounds["ymin"], data_bbox["ymin"] - buffer_m),
      min(bounds["ymax"], data_bbox["ymax"] + buffer_m),
      corine_raster()
    )

    raster_crop(corine_raster(), cropped_extent)
  })

  output$plot <- renderPlot({
    df_coord_3035 <- df_coord_3035()
    data_bbox <- st_bbox(df_coord_3035)
    corine <- corine_raster()
    ext <- raster_extent(corine)

    in_bounds <- points_in_bounds()

    if (in_bounds) {
      dataExtent <- make_extent(
        data_bbox["xmin"] - 15000,
        data_bbox["xmax"] + 15000,
        data_bbox["ymin"] - 15000,
        data_bbox["ymax"] + 15000,
        corine
      )
      corine_visualiseMap <- raster_crop(corine, dataExtent)
    } else {
      corine_visualiseMap <- corine
    }
    corine_visualiseMap <- downsample_for_plot(corine_visualiseMap, max_cells = 40000)
    corine_visualiseMap_df <- raster_to_df(corine_visualiseMap, na_rm = TRUE) |>
      left_join(landUseLookUp) |>
      mutate(broadLandUse = ifelse(is.na(broadLandUse), "Water", broadLandUse)) |>
      mutate(broadLandUse = factor(broadLandUse, levels = land_use_levels))

    main_plot <- ggplot() +
      geom_raster(data = corine_visualiseMap_df, aes(x = x, y = y, fill = broadLandUse)) +
      scale_fill_manual(values = land_use_palette, name = "Land use", drop = FALSE) +
      coord_equal() +
      theme_void() +
      theme(
        legend.text = element_text(color = "black", size = 11),
        legend.title = element_text(color = "black", size = 12),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA)
      ) +
      geom_sf(
        data = df_coord_3035,
        shape = 21,
        fill = "yellow",
        colour = "black",
        size = 4,
        stroke = 0.8
      ) +
      labs(subtitle = if (in_bounds) NULL else "Points outside raster extent; showing full Denmark map.") +
      NULL

    if (!isTRUE(input$show_legend)) {
      main_plot <- main_plot + theme(legend.position = "none")
    }

    if (!in_bounds) {
      return(main_plot)
    }

    inset_bbox <- data.frame(
      xmin = data_bbox["xmin"],
      xmax = data_bbox["xmax"],
      ymin = data_bbox["ymin"],
      ymax = data_bbox["ymax"]
    )

    inset_raster <- downsample_for_plot(corine, max_cells = 15000)
    inset_df <- raster_to_df(inset_raster, na_rm = TRUE) |>
      left_join(landUseLookUp) |>
      mutate(broadLandUse = ifelse(is.na(broadLandUse), "Water", broadLandUse)) |>
      mutate(broadLandUse = factor(broadLandUse, levels = land_use_levels))

    inset_plot <- ggplot() +
      geom_raster(data = inset_df, aes(x = x, y = y, fill = broadLandUse)) +
      scale_fill_manual(values = land_use_palette, name = "") +
      coord_equal() +
      theme_void() +
      theme(
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
        plot.background = element_rect(color = "black", fill = NA, linewidth = 0.6)
      ) +
      geom_rect(
        data = inset_bbox,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = NA, colour = "red", linewidth = 0.4
      )

    inset_grob <- ggplotGrob(inset_plot)
    main_extent <- raster_extent(corine_visualiseMap)
    bounds <- extent_bounds(main_extent)
    x_range <- bounds["xmax"] - bounds["xmin"]
    y_range <- bounds["ymax"] - bounds["ymin"]

    inset_xmin <- bounds["xmax"] - (0.35 * x_range)
    inset_xmax <- bounds["xmax"] - (0.05 * x_range)
    inset_ymin <- bounds["ymax"] - (0.35 * y_range)
    inset_ymax <- bounds["ymax"] - (0.05 * y_range)

    main_plot +
      annotation_custom(
        inset_grob,
        xmin = inset_xmin,
        xmax = inset_xmax,
        ymin = inset_ymin,
        ymax = inset_ymax
      )
  })


  landUseSummary <- reactive({
    df_coord_3035 <- df_coord_3035()
    df_coord_raw <- df_coord_raw()
    cols <- resolved_cols()
    id_col <- cols$id
    lon_col <- cols$lon
    lat_col <- cols$lat
    buffer_m <- max(MIN_BUFFER_M, input$buffer_m)
    validate(
      need(points_in_bounds(), "Points are outside the raster extent. Please check coordinates.")
    )

    Landcover <- NULL
    withProgress(message = "Computing land use summary", value = 0, {
      incProgress(0.2, detail = "Extracting land use codes")
      Landcover <- raster_extract(corine_extract_raster(), df_coord_3035, buffer_m)
      incProgress(0.4, detail = "Processing buffers")
    })
    names(Landcover) <- df_coord_raw[[id_col]]

    ## Compute maximum length
    max.length <- max(sapply(Landcover, length))
    ## Add NA values to list elements
    Landcover2 <- lapply(Landcover, function(v) {
      c(v, rep(NA, max.length - length(v)))
    })
    ## cbind
    Landcover2 <- do.call(cbind, Landcover2)

    x <- data.frame(Landcover2) |>
      pivot_longer(cols = everything(), names_to = "addressID", values_to = "value") |>
      arrange(addressID) |>
      left_join(landUseLookUp)

    outputLandUse <- x |>
      mutate(item = 1) |>
      group_by(addressID) |>
      summarise(
        total = sum(item[!is.na(broadLandUse)], na.rm = TRUE),
        Urban = sum(item[broadLandUse == "Urban"], na.rm = TRUE),
        Park = sum(item[broadLandUse == "Park"], na.rm = TRUE),
        Agriculture = sum(item[broadLandUse == "Agriculture"], na.rm = TRUE),
        ForestSemiNat = sum(item[broadLandUse == "Forest/Seminatural"], na.rm = TRUE),
        Wetlands = sum(item[broadLandUse == "Wetlands"], na.rm = TRUE),
        Water = sum(item[broadLandUse == "Water"], na.rm = TRUE)
      ) |>
      mutate(
        Urban = Urban / total, Park = Park / total, Agriculture = Agriculture / total,
        ForestSemiNat = ForestSemiNat / total, Wetlands = Wetlands / total,
        Water = Water / total
      ) |>
      dplyr::select(-total)

    withProgress(message = "Computing land use summary", value = 0.6, {
      incProgress(0.4, detail = "Finalizing table")
    })

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

  output$downloadSample <- downloadHandler(
    filename = function() {
      "sample_points.csv"
    },
    content = function(file) {
      file.copy("sample_data/sample_points.csv", file, overwrite = TRUE)
    }
  )
})

shinyApp(ui, server)
