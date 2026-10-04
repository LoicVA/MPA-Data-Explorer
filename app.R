install.packages(
  c(
    "shiny",
    "leaflet",
    "httr2",
    "dplyr",
    "ggplot2",
    "DT"
  )
)

# ============================================================
# MPA DATA EXPLORER
# ============================================================
#
# Interactive exploration of OBIS occurrence records
# within selected marine protected areas.
#
# ============================================================


# ============================================================
# Packages
# ============================================================

library(shiny)
library(leaflet)
library(httr2)
library(dplyr)
library(ggplot2)
library(DT)


# ============================================================
# MPA definitions
# ============================================================
#
# These are deliberately simplified rectangular geometries
# for the coding assessment.
#
# In a production application, these would be replaced by
# actual licensed MPA polygons from an authoritative source.
#
# Coordinates are:
# longitude, latitude
# ============================================================

areas <- data.frame(
  
  id = c(
    "azores",
    "nazca",
    "chagos",
    "papahanaumokuakea"
  ),
  
  name = c(
    "Azores Marine Park",
    "Nazca-Desventuradas Marine Park",
    "Chagos Archipelago",
    "Papahānaumokuākea"
  ),
  
  country = c(
    "Portugal",
    "Chile",
    "British Indian Ocean Territory",
    "United States"
  ),
  
  stringsAsFactors = FALSE
)


# ============================================================
# MPA bounding boxes
# ============================================================

mpa_bounds <- list(
  
  azores = c(
    xmin = -31.8,
    ymin = 36.8,
    xmax = -24.5,
    ymax = 40.2
  ),
  
  nazca = c(
    xmin = -84.0,
    ymin = -27.5,
    xmax = -78.0,
    ymax = -23.0
  ),
  
  chagos = c(
    xmin = 70.0,
    ymin = -8.0,
    xmax = 73.5,
    ymax = -4.0
  ),
  
  papahanaumokuakea = c(
    xmin = -179.0,
    ymin = 22.0,
    xmax = -160.0,
    ymax = 30.5
  )
  
)


# ============================================================
# Convert bounding box to WKT polygon
# ============================================================

make_wkt <- function(bounds) {
  
  paste0(
    "POLYGON((",
    
    bounds["xmin"], " ", bounds["ymin"], ",",
    
    bounds["xmin"], " ", bounds["ymax"], ",",
    
    bounds["xmax"], " ", bounds["ymax"], ",",
    
    bounds["xmax"], " ", bounds["ymin"], ",",
    
    bounds["xmin"], " ", bounds["ymin"],
    
    "))"
  )
  
}


# ============================================================
# Retrieve OBIS records
# ============================================================

get_obis_records <- function(bounds) {
  
  geometry <- make_wkt(bounds)
  
  response <- request(
    "https://api.obis.org/v3/occurrence"
  ) |>
    
    req_url_query(
      
      geometry = geometry,
      
      size = 1000
      
    ) |>
    
    req_perform()
  
  
  # Check HTTP response
  
  resp_check_status(response)
  
  
  # Convert response to R object
  
  result <- resp_body_json(response)
  
  
  # Extract records
  
  records <- result$results
  
  
  # If no records are returned
  
  if (length(records) == 0) {
    
    return(
      data.frame()
    )
    
  }
  
  
  # Convert records into a data frame
  
  records <- bind_rows(records)
  
  
  # Keep useful fields
  
  records <- records |>
    
    transmute(
      
      id = id,
      
      latitude =
        suppressWarnings(
          as.numeric(decimalLatitude)
        ),
      
      longitude =
        suppressWarnings(
          as.numeric(decimalLongitude)
        ),
      
      scientificName =
        ifelse(
          is.na(scientificName) |
            scientificName == "",
          "Unknown",
          scientificName
        ),
      
      phylum =
        ifelse(
          is.na(phylum) |
            phylum == "",
          "Unassigned",
          phylum
        ),
      
      eventDate =
        ifelse(
          is.na(eventDate),
          "",
          eventDate
        ),
      
      institution =
        ifelse(
          is.na(institutionCode) |
            institutionCode == "",
          "Not reported",
          institutionCode
        )
      
    )
  
  
  # Remove records without coordinates
  
  records <- records |>
    
    filter(
      !is.na(latitude),
      !is.na(longitude)
    )
  
  
  # Extract year
  
  records <- records |>
    
    mutate(
      
      year = suppressWarnings(
        as.integer(
          substr(eventDate, 1, 4)
        )
      )
      
    )
  
  
  return(records)
  
}


# ============================================================
# User interface
# ============================================================

ui <- fluidPage(
  
  titlePanel(
    "MPA Data Explorer"
  ),
  
  
  # ----------------------------------------------------------
  # MPA selector
  # ----------------------------------------------------------
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        
        inputId = "mpa",
        
        label =
          "Select a Marine Protected Area:",
        
        choices =
          setNames(
            areas$id,
            paste(
              areas$name,
              areas$country,
              sep = " — "
            )
          ),
        
        selected = "azores"
        
      ),
      
      
      br(),
      
      
      h4("Data source"),
      
      p(
        "Ocean Biodiversity Information System (OBIS)"
      ),
      
      p(
        "Records are retrieved from the OBIS API."
      ),
      
      
      downloadButton(
        outputId = "download_data",
        label = "Download CSV"
      )
      
    ),
    
    
    # --------------------------------------------------------
    # Main panel
    # --------------------------------------------------------
    
    mainPanel(
      
      tabsetPanel(
        
        # ====================================================
        # MAP
        # ====================================================
        
        tabPanel(
          
          "Map",
          
          br(),
          
          leafletOutput(
            "map",
            height = "600px"
          )
          
        ),
        
        
        # ====================================================
        # SUMMARY
        # ====================================================
        
        tabPanel(
          
          "Summary",
          
          br(),
          
          h3("Records per year"),
          
          plotOutput(
            "year_plot",
            height = "350px"
          ),
          
          h3("Records by higher taxon"),
          
          plotOutput(
            "taxon_plot",
            height = "450px"
          )
          
        ),
        
        
        # ====================================================
        # DATA
        # ====================================================
        
        tabPanel(
          
          "Data",
          
          br(),
          
          DTOutput(
            "records_table"
          )
          
        )
        
      )
      
    )
    
  ),
  
  
  # ==========================================================
  # Citation
  # ==========================================================
  
  hr(),
  
  p(
    strong("Data source: "),
    "Ocean Biodiversity Information System (OBIS). ",
    "https://obis.org/"
  )
  
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # Retrieve records for selected MPA
  # ==========================================================
  
  obis_data <- reactive({
    
    req(input$mpa)
    
    
    bounds <-
      mpa_bounds[[input$mpa]]
    
    
    # Attempt API request
    
    tryCatch({
      
      data <- get_obis_records(
        bounds
      )
      
      
      data
      
      
    },
    
    error = function(e) {
      
      showNotification(
        
        paste(
          "Could not retrieve OBIS data:",
          e$message
        ),
        
        type = "error",
        
        duration = NULL
        
      )
      
      
      data.frame()
      
    })
    
  })
  
  
  # ==========================================================
  # MAP
  # ==========================================================
  
  output$map <- renderLeaflet({
    
    leaflet() |>
      
      addTiles(
        
        attribution =
          "&copy; OpenStreetMap contributors"
        
      ) |>
      
      setView(
        
        lng = 0,
        
        lat = 20,
        
        zoom = 2
        
      )
    
  })
  
  
  # ==========================================================
  # Update map when MPA changes
  # ==========================================================
  
  observe({
    
    req(input$mpa)
    
    
    bounds <-
      mpa_bounds[[input$mpa]]
    
    
    records <-
      obis_data()
    
    
    # --------------------------------------------------------
    # Base map
    # --------------------------------------------------------
    
    proxy <-
      leafletProxy("map") |>
      
      clearShapes() |>
      
      clearMarkers()
    
    
    # --------------------------------------------------------
    # Add MPA boundary
    # --------------------------------------------------------
    
    proxy |>
      
      addRectangles(
        
        lng1 =
          bounds["xmin"],
        
        lat1 =
          bounds["ymin"],
        
        lng2 =
          bounds["xmax"],
        
        lat2 =
          bounds["ymax"],
        
        weight = 3,
        
        fill = FALSE,
        
        color = "red",
        
        popup =
          areas$name[
            areas$id == input$mpa
          ]
        
      ) |>
      
      fitBounds(
        
        lng1 =
          bounds["xmin"],
        
        lat1 =
          bounds["ymin"],
        
        lng2 =
          bounds["xmax"],
        
        lat2 =
          bounds["ymax"]
        
      )
    
    
    # --------------------------------------------------------
    # Add OBIS records
    # --------------------------------------------------------
    
    if (nrow(records) > 0) {
      
      proxy |>
        
        addCircleMarkers(
          
          data = records,
          
          lng = ~longitude,
          
          lat = ~latitude,
          
          radius = 5,
          
          stroke = TRUE,
          
          weight = 1,
          
          fillOpacity = 0.7,
          
          popup = ~paste0(
            
            "<b>",
            scientificName,
            "</b><br>",
            
            "Phylum: ",
            phylum,
            "<br>",
            
            "Date: ",
            eventDate,
            "<br>",
            
            "Institution: ",
            institution
            
          )
          
        )
      
    }
    
  })
  
  
  # ==========================================================
  # RECORDS PER YEAR
  # ==========================================================
  
  output$year_plot <- renderPlot({
    
    data <-
      obis_data()
    
    
    validate(
      
      need(
        nrow(data) > 0,
        "No OBIS records were returned for this area."
      )
      
    )
    
    
    # Remove records without a valid year
    
    yearly <-
      data |>
      
      filter(
        !is.na(year)
      ) |>
      
      count(
        year
      )
    
    
    ggplot(
      yearly,
      aes(
        x = year,
        y = n
      )
    ) +
      
      geom_col() +
      
      labs(
        
        x = "Year",
        
        y = "Number of OBIS records",
        
        title =
          "OBIS records per year"
        
      ) +
      
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # RECORDS BY HIGHER TAXON
  # ==========================================================
  
  output$taxon_plot <- renderPlot({
    
    data <-
      obis_data()
    
    
    validate(
      
      need(
        nrow(data) > 0,
        "No OBIS records were returned for this area."
      )
      
    )
    
    
    taxon <-
      data |>
      
      count(
        phylum,
        name = "records"
      ) |>
      
      arrange(
        desc(records)
      )
    
    
    ggplot(
      
      taxon,
      
      aes(
        
        x =
          reorder(
            phylum,
            records
          ),
        
        y = records
        
      )
      
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        
        x = "Phylum",
        
        y = "Number of OBIS records",
        
        title =
          "OBIS records by higher taxon"
        
      ) +
      
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # DATA TABLE
  # ==========================================================
  
  output$records_table <- renderDT({
    
    data <-
      obis_data()
    
    
    datatable(
      
      data,
      
      options = list(
        
        pageLength = 25,
        
        scrollX = TRUE
        
      ),
      
      rownames = FALSE
      
    )
    
  })
  
  
  # ==========================================================
  # CSV DOWNLOAD
  # ==========================================================
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      
      paste0(
        
        input$mpa,
        
        "_obis_occurrences.csv"
        
      )
      
    },
    
    
    content = function(file) {
      
      write.csv(
        
        obis_data(),
        
        file,
        
        row.names = FALSE
        
      )
      
    }
    
  )
  
}


# ============================================================
# Run application
# ============================================================

shinyApp(
  ui = ui,
  server = server
)