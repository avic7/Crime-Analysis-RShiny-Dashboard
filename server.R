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
  
  observe({
    # unique values for filters
    areas <- sort(unique(crime_data$Area_Name))
    crime_types <- sort(unique(crime_data$Crime_Description))
    
    # available years 
    available_years <- sort(unique(crime_data$Year))
    min_year <- min(available_years, na.rm = TRUE)
    max_year <- max(available_years, na.rm = TRUE)
    
    # Update inputs with choices
    updateSelectInput(session, "area", 
                      choices = c("All", areas),
                      selected = "All")
    
    updateSelectInput(session, "crime_type", 
                      choices = c("All", crime_types),
                      selected = "All")
    
    # Update year drop down values
    updateSelectInput(session, "year_start",
                      choices = available_years,
                      selected = min_year)
    
    updateSelectInput(session, "year_end",
                      choices = available_years,
                      selected = max_year)
  })
  
  # Modify filtered_data reactive function
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
  
  # observer to ensure end year is never less than start year
  observeEvent(input$year_start, {
    if (!is.null(input$year_start) && !is.null(input$year_end)) {
      if (input$year_start > input$year_end) {
        updateSelectInput(session, "year_end", selected = input$year_start)
      }
    }
  })
  
  # Output the data table 
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
  
  # Overview Tab Outputs 
  
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
        value = most_common,
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
      # Extract month and year from date
      data$month_year <- format(data$Crime_Occured, "%Y-%m")
      
      monthly_counts <- data %>%
        count(month_year) %>%
        arrange(month_year)
      
      p <- ggplot(monthly_counts, aes(x = month_year, y = n, group = 1)) +
        geom_line(color = "steelblue", size = 1) +
        geom_point(color = "steelblue", size = 3) +
        labs(x = "Month", y = "Number of Incidents") +
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
                   marker = list(colors = c("darkblue", "blue", "lightblue", "skyblue", "teal")),
                   textinfo = 'label+percent') %>%
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
          median_income = mean(as.numeric(`Median Household Income`), na.rm = TRUE)
        ) %>%
        filter(!is.na(median_income))
      
      p <- plot_ly(income_crime, x = ~median_income, y = ~count, 
                   type = 'scatter', mode = 'markers',
                   text = ~Area_Name,
                   marker = list(size = 10, opacity = 0.8, color = "darkblue")) %>%
        layout(
          title = "Crime Incidents vs. Median Household Income by Area",
          xaxis = list(title = "Median Household Income ($)"),
          yaxis = list(title = "Number of Crime Incidents")
        )
      
      p
    }
  })
}