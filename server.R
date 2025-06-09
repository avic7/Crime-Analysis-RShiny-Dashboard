server <- function(input, output, session) {
  
  # Load the data
  crime_data <- read.csv("df_clean.csv")
  
  # Convert date columns to date object
  crime_data$Crime_Reported <- as.Date(crime_data$Crime_Reported)
  crime_data$Crime_Occured <- as.Date(crime_data$Crime_Occured)
  
  # Extract year from date columns
  crime_data$Year <- lubridate::year(crime_data$Crime_Occured)
  
  # Debugging  
  print(paste("Total rows in crime_data:", nrow(crime_data)))
  print(paste("Year range in data:", min(crime_data$Year, na.rm = TRUE), "to", max(crime_data$Year, na.rm = TRUE)))
  print("Column names in data:")
  print(colnames(crime_data))
  
  observe({
    # unique values for filters
    areas <- sort(unique(crime_data$Area_Name))
    crime_types <- sort(unique(crime_data$Crime_Description))
    
    # available years 
    available_years <- sort(unique(crime_data$Year))
    min_year <- min(available_years, na.rm = TRUE)
    max_year <- max(available_years, na.rm = TRUE)
    
    # Updating inputs with choices for Overview tab
    updateSelectInput(session, "area", 
                      choices = c("All", areas),
                      selected = "All")
    
    updateSelectInput(session, "crime_type", 
                      choices = c("All", crime_types),
                      selected = "All")
    
    # Updating year drop down values for Overview tab
    updateSelectInput(session, "year_start",
                      choices = available_years,
                      selected = min_year)
    
    updateSelectInput(session, "year_end",
                      choices = available_years,
                      selected = max_year)
    
    # Updating inputs for Map tab
    updateSelectInput(session, "map_area", 
                      choices = c("All", areas),
                      selected = "All")
    
    updateSelectInput(session, "map_crime_type", 
                      choices = c("All", crime_types),
                      selected = "All")
    
    updateSelectInput(session, "map_year",
                      choices = c("All", available_years),
                      selected = max_year)
  })
  
  # Modify filtered_data reactive function for Overview tab
  filtered_data <- reactive({
    data <- crime_data
    
    # Year filter
    if (!is.null(input$year_start) && !is.null(input$year_end)) {
      data <- data %>% 
        filter(Year >= input$year_start & Year <= input$year_end)
    }
    
    # Area filter 
    if (!is.null(input$area) && length(input$area) > 0) {
      if (!("All" %in% input$area)) {
        data <- data %>% filter(Area_Name %in% input$area)
      }
    }
    
    # Crime type filter
    if (!is.null(input$crime_type) && length(input$crime_type) > 0) {
      if (!("All" %in% input$crime_type)) {
        data <- data %>% filter(Crime_Description %in% input$crime_type)
      }
    }
    
    # Debugging
    print(paste("Filtered data has", nrow(data), "rows"))
    if(nrow(data) > 0) {
      print(paste("Year range after filtering:", min(data$Year, na.rm = TRUE), "to", max(data$Year, na.rm = TRUE)))
    }
    
    return(data)
  })
  
  filtered_data_with_button <- eventReactive(input$apply_filters, {
    filtered_data()
  }, ignoreNULL = FALSE)
  
  # Map-specific filtered data
  map_filtered_data <- eventReactive(input$update_map, {
    data <- crime_data
    
    # Removing rows with missing coordinates 
    if("LAT" %in% colnames(data) && "LON" %in% colnames(data)) {
      data <- data %>% filter(!is.na(LAT) & !is.na(LON) & LAT != 0 & LON != 0)
    } else if("Latitude" %in% colnames(data) && "Longitude" %in% colnames(data)) {
      data$LAT <- data$Latitude
      data$LON <- data$Longitude
      data <- data %>% filter(!is.na(LAT) & !is.na(LON) & LAT != 0 & LON != 0)
    } else if("lat" %in% colnames(data) && "lon" %in% colnames(data)) {
      data$LAT <- data$lat
      data$LON <- data$lon
      data <- data %>% filter(!is.na(LAT) & !is.na(LON) & LAT != 0 & LON != 0)
    } else if("X" %in% colnames(data) && "Y" %in% colnames(data)) {
      data$LAT <- data$Y
      data$LON <- data$X
      data <- data %>% filter(!is.na(LAT) & !is.na(LON) & LAT != 0 & LON != 0)
    }
    
    # Year filter
    if (!is.null(input$map_year) && input$map_year != "All") {
      data <- data %>% filter(Year == input$map_year)
    }
    
    # Area filter
    if (!is.null(input$map_area) && input$map_area != "All") {
      data <- data %>% filter(Area_Name == input$map_area)
    }
    
    # Crime type filter
    if (!is.null(input$map_crime_type) && input$map_crime_type != "All") {
      data <- data %>% filter(Crime_Description == input$map_crime_type)
    }
    
    print(paste("Map filtered data has", nrow(data), "rows"))
    if(nrow(data) > 0 && "LAT" %in% colnames(data)) {
      print(paste("Coordinate range - LAT:", min(data$LAT, na.rm = TRUE), "to", max(data$LAT, na.rm = TRUE)))
      print(paste("Coordinate range - LON:", min(data$LON, na.rm = TRUE), "to", max(data$LON, na.rm = TRUE)))
    }
    
    return(data)
  }, ignoreNULL = FALSE)
  

  observeEvent(input$year_start, {
    if (!is.null(input$year_start) && !is.null(input$year_end)) {
      if (input$year_start > input$year_end) {
        updateSelectInput(session, "year_end", selected = input$year_start)
      }
    }
  })
  
  # Output e 
  output$crime_table <- DT::renderDataTable({
    crime_data
  }, options = list(
    scrollX = TRUE,
    pageLength = 10
  ))
  
  # "Get Started" button click
  observeEvent(input$get_started, {
    updateTabItems(session, "tabs", "overview")
  })
  
  
  # Value Boxes
  output$total_crimes <- renderValueBox({
    count <- nrow(filtered_data_with_button())
    valueBox(
      value = format(count, big.mark = ","),
      subtitle = "Total Crimes",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$clearance_rate <- renderValueBox({
    data <- filtered_data_with_button()
    # Calculate clearance rate (arrests/total)
    arrests <- sum(grepl("Arrest", data$Status_Of_The_Case, ignore.case = TRUE))
    rate <- ifelse(nrow(data) > 0, round(arrests / nrow(data) * 100, 1), 0)
    
    valueBox(
      value = paste0(rate, "%"),
      subtitle = "Clearance Rate",
      icon = icon("balance-scale"),
      color = "blue"
    )
  })
  
  output$most_common_crime <- renderValueBox({
    data <- filtered_data()
    if(nrow(data) > 0) {
      crime_counts <- table(data$Crime_Description)
      most_common <- names(which.max(crime_counts))
      count <- max(crime_counts)
      
      valueBox(
        value = tags$div(style = "white-space: normal; word-wrap: break-word; line-height: 1.2;", 
                         most_common),
        subtitle = paste0(format(count, big.mark = ","), " incidents"),
        icon = icon("file-alt"),
        color = "purple"
      )
    } else {
      valueBox(
        value = "N/A",
        subtitle = "Most Common Crime",
        icon = icon("file-alt"),
        color = "purple"
      )
    }
  })
  
  output$highest_crime_area <- renderValueBox({
    data <- filtered_data_with_button()
    if(nrow(data) > 0) {
      area_counts <- table(data$Area_Name)
      highest_area <- names(which.max(area_counts))
      count <- max(area_counts)
      
      valueBox(
        value = highest_area,
        subtitle = paste0(format(count, big.mark = ","), " incidents"),
        icon = icon("map-marker-alt"),
        color = "green"
      )
    } else {
      valueBox(
        value = "N/A",
        subtitle = "Highest Crime Area",
        icon = icon("map-marker-alt"),
        color = "green"
      )
    }
  })
  
  # Crime Types Chart
  output$crime_types_chart <- renderPlotly({
    data <- filtered_data_with_button()
    
    if(nrow(data) > 0) {
      crime_counts <- data %>%
        count(Crime_Description) %>%
        arrange(desc(n)) %>%
        slice_head(n = 10)
      
      p <- ggplot(crime_counts, aes(x = reorder(Crime_Description, n), y = n, fill = n)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(x = NULL, y = "Number of Incidents") +
        theme_minimal() +
        scale_fill_gradient(low = "skyblue", high = "navy")
      
      ggplotly(p) %>% layout(showlegend = FALSE)
    }
  })
  
  # Monthly Trend Chart
  output$monthly_trend_chart <- renderPlotly({
    data <- filtered_data_with_button()
    
    if(nrow(data) > 0) {
      # Extract month from date
      data$month <- format(data$Crime_Occured, "%B")
      
      # Aggregate crime counts by month across all selected years
      monthly_counts <- data %>%
        group_by(month) %>%
        summarize(n = n()) %>%
        mutate(month = factor(month, 
                              levels = c("January", "February", "March", "April", "May", "June", 
                                         "July", "August", "September", "October", "November", "December")))
      
      p <- ggplot(monthly_counts, aes(x = month, y = n, group = 1)) +
        geom_line(color = "steelblue", size = 1) +
        geom_point(color = "steelblue", size = 3) +
        labs(x = "Month", y = "Total Incidents") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
    }
  })
  
  # Case Status Chart
  output$case_status_chart <- renderPlotly({
    data <- filtered_data_with_button()
    
    if(nrow(data) > 0) {
      status_counts <- data %>%
        count(Status_Of_The_Case) %>%
        arrange(desc(n))
      
      p <- plot_ly(status_counts, labels = ~Status_Of_The_Case, values = ~n, 
                   type = 'pie',
                   marker = list(colors = c("darkblue", "blue", "lightblue", "skyblue", "teal"))) %>%
        layout(showlegend = TRUE)
      
      p
    }
  })
  
  # Income vs. Crime Chart
  output$income_crime_chart <- renderPlotly({
    data <- filtered_data_with_button()
    
    if(nrow(data) > 0) {
      income_crime <- data %>%
        group_by(Area_Name) %>%
        summarize(
          count = n(),
          median_income = as.numeric(gsub("[^0-9.]", "", 'Median Household Income'))
        ) %>%
        filter(!is.na(median_income) & median_income > 0)
      
      # Add correlation information
      correlation <- cor(income_crime$median_income, income_crime$count)
      
      p <- plot_ly(income_crime, 
                   x = ~median_income, 
                   y = ~count, 
                   type = 'scatter', 
                   mode = 'markers',
                   text = ~paste(
                     "Area: ", Area_Name, 
                     "<br>Median Income: $", format(median_income, big.mark = ","),
                     "<br>Crime Incidents: ", count
                   ),
                   hoverinfo = 'text',
                   marker = list(
                     size = 10, 
                     opacity = 0.7, 
                     color = ~count, 
                     colorscale = 'Viridis'
                   )
      ) %>%
        layout(
          title = paste("Crime Incidents vs. Median Household Income "),
          xaxis = list(
            title = "Median Household Income ($)", 
            tickformat = "$,"
          ),
          yaxis = list(title = "Number of Crime Incidents"),
          # Add a trend line
          shapes = list(
            type = "line",
            line = list(color = "red", width = 2, dash = "dot"),
            x0 = min(income_crime$median_income),
            x1 = max(income_crime$median_income),
            y0 = min(income_crime$count) + (max(income_crime$count) - min(income_crime$count)) * 
              (min(income_crime$median_income) - min(income_crime$median_income)) / 
              (max(income_crime$median_income) - min(income_crime$median_income)),
            y1 = max(income_crime$count)
          )
        )
      
      p
    }
  })
  
  
# Crime Map Tab Outputs  
  
  # Main Crime Map
  output$crime_map <- renderLeaflet({
    data <- map_filtered_data()
    
    # Los Angeles city boundaries 
    la_bounds <- list(
      north = 34.3373,   
      south = 33.7037,    
      east = -118.1553,  
      west = -118.6681   
    )
    
    # Base map focused on LA
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = -118.2437, lat = 34.0522, zoom = 11) %>%
      setMaxBounds(
        lng1 = la_bounds$west, lat1 = la_bounds$south,
        lng2 = la_bounds$east, lat2 = la_bounds$north
      )
    
    if (nrow(data) > 0 && "LAT" %in% colnames(data) && "LON" %in% colnames(data)) {
      if (input$map_view == "heatmap") {
        if (requireNamespace("leaflet.extras", quietly = TRUE)) {
          map <- map %>%
            leaflet.extras::addHeatmap(
              lng = data$LON, 
              lat = data$LAT,
              blur = 20, 
              max = 0.05,
              radius = 15
            )
        } else {
          map <- map %>%
            addCircleMarkers(
              lng = data$LON,
              lat = data$LAT,
              radius = 2,
              fillOpacity = 0.5,
              stroke = FALSE,
              color = "red"
            )
        }
      } else if (input$map_view == "markers") {
        # Adding individual markers 
        sample_data <- data %>% slice_sample(n = min(500, nrow(data)))
        
        map <- map %>%
          addCircleMarkers(
            lng = sample_data$LON,
            lat = sample_data$LAT,
            radius = 4,
            popup = paste0(
              "<b>Crime:</b> ", sample_data$Crime_Description, "<br>",
              "<b>Date:</b> ", sample_data$Crime_Occured, "<br>",
              "<b>Area:</b> ", sample_data$Area_Name, "<br>",
              "<b>Status:</b> ", sample_data$Status_Of_The_Case
            ),
            fillOpacity = 0.7,
            stroke = TRUE,
            color = "red",
            weight = 1
          )
      } else if (input$map_view == "clusters") {
        # Adding clustered markers
        map <- map %>%
          addMarkers(
            lng = data$LON,
            lat = data$LAT,
            popup = paste0(
              "<b>Crime:</b> ", data$Crime_Description, "<br>",
              "<b>Date:</b> ", data$Crime_Occured, "<br>",
              "<b>Area:</b> ", data$Area_Name
            ),
            clusterOptions = markerClusterOptions()
          )
      }
      
      # Automatically fit map to show all crime data points within LA bounds
      if(nrow(data) > 0) {
        map <- map %>% 
          fitBounds(
            lng1 = max(min(data$LON, na.rm = TRUE), la_bounds$west),
            lat1 = max(min(data$LAT, na.rm = TRUE), la_bounds$south),
            lng2 = min(max(data$LON, na.rm = TRUE), la_bounds$east), 
            lat2 = min(max(data$LAT, na.rm = TRUE), la_bounds$north)
          )
      }
    } else {
      # If no data, show LA city center
      map <- map %>%
        setView(lng = -118.2437, lat = 34.0522, zoom = 11)
    }
    
    # Adding LA city boundary outline (optional)
    map <- map %>%
      addRectangles(
        lng1 = la_bounds$west, lat1 = la_bounds$south,
        lng2 = la_bounds$east, lat2 = la_bounds$north,
        fillColor = "transparent",
        color = "blue",
        weight = 2,
        opacity = 0.5,
        dashArray = "5,5",
        popup = "Los Angeles City Boundary (Approximate)"
      )
    
    map
  })
  
  # Map Statistics Value Boxes
  output$map_total_crimes <- renderValueBox({
    count <- nrow(map_filtered_data())
    valueBox(
      value = format(count, big.mark = ","),
      subtitle = "Crimes on Map",
      icon = icon("map-pin"),
      color = "red",
      width = NULL
    )
  })
  
  output$map_crime_density <- renderValueBox({
    data <- map_filtered_data()
    # Calculating crimes per square mile (approximate)
    area_sq_miles <- 469  # LA area in square miles
    density <- ifelse(nrow(data) > 0, round(nrow(data) / area_sq_miles, 1), 0)
    
    valueBox(
      value = density,
      subtitle = "Crimes per Sq Mile",
      icon = icon("chart-area"),
      color = "yellow",
      width = NULL
    )
  })
  
  output$map_hotspot_area <- renderValueBox({
    data <- map_filtered_data()
    if(nrow(data) > 0) {
      hotspot <- data %>%
        count(Area_Name) %>%
        arrange(desc(n)) %>%
        slice(1) %>%
        pull(Area_Name)
      
      valueBox(
        value = hotspot,
        subtitle = "Top Hotspot Area",
        icon = icon("fire"),
        color = "orange",
        width = NULL
      )
    } else {
      valueBox(
        value = "N/A",
        subtitle = "Top Hotspot Area",
        icon = icon("fire"),
        color = "orange",
        width = NULL
      )
    }
  })
}
