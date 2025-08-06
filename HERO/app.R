library(tidyverse)

# Load season level relief pitcher data with stats and metrics here:
hero_by_year2 <- read_csv("hero_by_year2.csv")

hero_by_year2 <- hero_by_year2 %>%
  mutate(Hero = hero_total / appearences)

# ---------------------------- Shiny App ------------------------------------------------------------
library(shiny)
library(reactable)
library(dplyr)
library(scales)
library(ggplot2)
library(plotly)

# Sample Data (We will replace this with our actual relief pitcher stats/metrics)
set.seed(123)
relief_pitchers <- data.frame(
  Pitcher = hero_by_year2$pitcher_name,
  Team = hero_by_year2$team,
  Season = hero_by_year2$game_year,
  Threat_Plus = hero_by_year2$threat_plus,
  Pressure_Plus = hero_by_year2$pressure_plus,
  Momentum_Plus = hero_by_year2$momentum_plus,
  Danger_Plus = hero_by_year2$danger_plus,
  Hero_Save = hero_by_year2$hero_save,
  Hero_Save_Plus = hero_by_year2$hero_save_plus,
  Hero_Hold = hero_by_year2$hero_hold,
  Hero_Hold_Plus = hero_by_year2$hero_hold_plus,
  Hero_Losing = hero_by_year2$hero_losing,
  Hero_Losing_Plus = hero_by_year2$hero_losing_plus,
  Hero_Total = hero_by_year2$hero_total,
  Hero_Plus = hero_by_year2$hero_plus,
  Hero = hero_by_year2$Hero,
  Danger_Final = hero_by_year2$avg_danger_final,
  Net_Danger = hero_by_year2$avg_net_danger
)

# Function to Scale and Color Metrics (White = Closer to Mean, Blue = Worse, Red = Better)
color_scale <- function(values, reverse = FALSE) {
  avg_val <- mean(values, na.rm = TRUE)
  sd_val <- sd(values, na.rm = TRUE)  # Calculate the standard deviation
  
  # Re-scale based on how far each value is from the mean in terms of standard deviations
  dist_from_mean <- (values - avg_val) / sd_val  # Standardized distance (z-scores)
  max_dist <- max(abs(dist_from_mean), na.rm = TRUE)  # Get the maximum distance for normalization
  
  # Adjust color palette to use blue (worse), white (near mean), and red (better)
  if (reverse) {
    pal <- colorRampPalette(c("lightcoral", "white", "royalblue"))  # Reverse color scheme for better values red and worse blue
  } else {
    pal <- colorRampPalette(c("royalblue", "white", "lightcoral"))  # Default color scheme
  }
  
  # Normalize based on the standardized distance from the mean
  scaled <- dist_from_mean / max_dist
  colors <- pal(100)[as.numeric(cut(scaled, breaks = 100))]
  
  # Return a function that applies the color scheme to individual values
  function(value) {
    if (is.na(value)) return(NULL)
    list(background = colors[which.min(abs(values - value))], color = "black", fontWeight = "bold")
  }
}

column_mapping <- c(
  "Pitcher" = "Pitcher Name",
  "Team" = "Team",
  "Season" = "Season",
  "Threat_Plus" = "Threat+",
  "Pressure_Plus" = "Pressure+",
  "Momentum_Plus" = "Momentum+",
  "Danger_Plus" = "Danger+",
  "Hero_Save" = "Hero Save",
  "Hero_Save_Plus" = "Hero Save+",
  "Hero_Hold" = "Hero Hold",
  "Hero_Hold_Plus" = "Hero Hold+",
  "Hero_Losing" = "Hero Losing",
  "Hero_Losing_Plus" = "Hero Losing+",
  "Hero_Total" = "Hero Total",
  "Hero_Plus" = "Hero+",
  "Hero" = "Hero",
  "Danger_Final" = "Danger Final",
  "Net_Danger" = "Net Danger"
)

# Subset Mapping (Selecting only a subset of relevant columns for the graph dropdowns)
subset_mapping <- c(
  "Threat+" = "Threat_Plus",
  "Pressure+" = "Pressure_Plus",
  "Momentum+" = "Momentum_Plus",
  "Danger+" = "Danger_Plus",
  "Hero Save" = "Hero_Save",
  "Hero Save+" = "Hero_Save_Plus",
  "Hero Hold" = "Hero_Hold",
  "Hero Hold+" = "Hero_Hold_Plus",
  "Hero Losing" = "Hero_Losing",
  "Hero Losing+" = "Hero_Losing_Plus",
  "Hero Total" = "Hero_Total",
  "Hero+" = "Hero_Plus",
  "Hero" = "Hero",
  "Danger Final" = "Danger_Final",
  "Net Danger" = "Net_Danger"
)


ui <- fluidPage(
  # Apply custom CSS styles for title and header
  tags$style(HTML("
  .title {
    font-size: 30px;
    text-align: center;
    font-weight: bold;
    color: #003366;
    margin-top: 20px;
  }
  .header {
    font-size: 20px;
    font-weight: bold;
    color: #003366;
    text-align: center;
    background-color: #E9ECEF;
    padding: 10px;
    border-radius: 10px;
    margin-top: 20px;
  }
  .panel-title {
    font-size: 18px;
    color: #003366;
    font-weight: bold;
  }
  .search-container {
    display: flex;
    align-items: center;
    gap: 10px;
    margin-bottom: 10px;
  }
  .search-label {
    font-weight: bold;
    font-size: 16px;
    color: #003366;
  }
  .shiny-output-error-validation {
    color: red;
  }
  body {
    zoom: 0.80; /* Adjust the zoom level as needed */
  }
")),
  
  # Title
  tags$div(class = "title", "MLB Relief Pitcher Stats/Metrics Dashboard (2023-2024)"),
  
  # Search bar with label (Fixed: Now inside fluidPage)
  #  tags$div(class = "search-container",
  #           tags$span(class = "search-label", "Pitcher Search:"),
  #           textInput("search", label = NULL, placeholder = "Search for a pitcher...")
  #  ),
  
  # Tabset Panel
  tabsetPanel(
    tabPanel("Relief Pitcher Stats/Metrics",
             fluidRow(
               column(3,
                      sliderInput("threat_plus_slider", "Threat+ Range:",
                                  min = 0, 
                                  max = 150,
                                  value = c(0, 150),
                                  step = 0.01)), 
               column(3,
                      sliderInput("pressure_plus_slider", "Pressure+ Range:",
                                  min = 0, 
                                  max = 150,
                                  value = c(0, 150),
                                  step = 0.01)),
               column(3,
                      sliderInput("momentum_plus_slider", "Momentum+ Range:",
                                  min = 0, 
                                  max = 150,
                                  value = c(0, 150),
                                  step = 0.01)),
               column(3,
                      sliderInput("danger_plus_slider", "Danger+ Range:",
                                  min = 0, 
                                  max = 150,
                                  value = c(0, 150),
                                  step = 0.01)),
               column(3,
                      sliderInput("hero_save_plus_slider", "Hero Save+ Range:",
                                  min = 0, 
                                  max = 150,
                                  value = c(0, 150),
                                  step = 0.01)),
               column(3,
                      sliderInput("hero_hold_plus_slider", "Hero Hold+ Range:",
                                  min = 0, 
                                  max = 150,
                                  value = c(0, 150),
                                  step = 0.01)),
               column(3,
                      sliderInput("hero_losing_plus_slider", "Hero Losing+ Range:",
                                  min = 0, 
                                  max = 150,
                                  value = c(0, 150),
                                  step = 0.01)),
               column(3,
                      sliderInput("hero_plus_slider", "Hero+ Range:",
                                  min = 0, 
                                  max = 150,
                                  value = c(0, 150),
                                  step = 0.01)),
               column(3,
                      selectInput("season_selector", "Select Season:", 
                                  choices = c("All", unique(relief_pitchers$Season)), selected = "All")),
               column(3,
                      selectInput("team_selector", "Select Team:", 
                                  choices = c("All", unique(relief_pitchers$Team)), selected = "All")),
               column(3,
                      selectInput("show_entries", "Show Entries:", 
                                  choices = c(5, 10, 20, 50, 100, "All"), selected = 'All'))
             ),
             
             # Reactable Output
             reactableOutput("pitcher_table")
    ),
    
    tabPanel("Graphing Correlation Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("x_stat", "X-Axis Stat:", 
                             choices = subset_mapping,  
                             selected = subset_mapping[1]),   
                 selectInput("y_stat", "Y-Axis Stat:", 
                             choices = subset_mapping,  
                             selected = subset_mapping[2])    
               ),
               mainPanel(
                 plotlyOutput("scatterPlot", height = "500px")
               )
             )
    )
  )
)





# Server
server <- function(input, output, session) {
  
  # Reactive expression to filter the data based on slider values, selected season, and selected team
  filtered_data <- reactive({
    data <- relief_pitchers %>%
      filter(
        (input$season_selector == "All" | Season == input$season_selector),
        (input$team_selector == "All" | Team == input$team_selector),
        Threat_Plus >= input$threat_plus_slider[1], Threat_Plus <= input$threat_plus_slider[2],
        Pressure_Plus >= input$pressure_plus_slider[1], Pressure_Plus <= input$pressure_plus_slider[2],
        Momentum_Plus >= input$momentum_plus_slider[1], Momentum_Plus <= input$momentum_plus_slider[2],
        Danger_Plus >= input$danger_plus_slider[1], Danger_Plus <= input$danger_plus_slider[2],
        Hero_Save_Plus >= input$hero_save_plus_slider[1], Hero_Save_Plus <= input$hero_save_plus_slider[2],
        Hero_Hold_Plus >= input$hero_hold_plus_slider[1], Hero_Hold_Plus <= input$hero_hold_plus_slider[2],
        Hero_Losing_Plus >= input$hero_losing_plus_slider[1], Hero_Losing_Plus <= input$hero_losing_plus_slider[2],
        Hero_Plus >= input$hero_plus_slider[1], Hero_Plus <= input$hero_plus_slider[2]
      )
    
    # Debug output
    print(paste("Season:", input$season_selector))
    print(paste("Team:", input$team_selector))
    print(paste("Threat+ range:", input$threat_plus_slider[1], "to", input$threat_plus_slider[2]))
    print(paste("Pressure+ range:", input$pressure_plus_slider[1], "to", input$pressure_plus_slider[2]))
    print(paste("Momentum+ range:", input$momentum_plus_slider[1], "to", input$momentum_plus_slider[2]))
    print(paste("Danger+ range:", input$danger_plus_slider[1], "to", input$danger_plus_slider[2]))
    print(paste("Hero Save+ range:", input$hero_save_plus_slider[1], "to", input$hero_save_plus_slider[2]))
    print(paste("Hero Hold+ range:", input$hero_hold_plus_slider[1], "to", input$hero_hold_plus_slider[2]))
    print(paste("Hero Losing+ range:", input$hero_losing_plus_slider[1], "to", input$hero_losing_plus_slider[2]))
    print(paste("Hero+ range:", input$hero_plus_slider[1], "to", input$hero_plus_slider[2]))
    print(paste("Number of rows before filtering:", nrow(relief_pitchers)))
    print(paste("Number of rows after filtering:", nrow(data)))
    
    return(data)
  })
  
  # Output for the table, using the filtered data
  output$pitcher_table <- renderReactable({
    data_to_show <- filtered_data()  # Use the filtered data
    
    # Apply the "Show Entries" filter
    if (input$show_entries != "All") {
      data_to_show <- head(data_to_show, as.numeric(input$show_entries))
    }
    
    reactable(data_to_show)
  })
  
  # Render the scatter plot in the UI
  output$scatterPlot <- renderPlotly({
    # Check if selected columns are numeric
    x_stat_data <- filtered_data()[[input$x_stat]]
    y_stat_data <- filtered_data()[[input$y_stat]]
    
    if (is.numeric(x_stat_data) && is.numeric(y_stat_data)) {
      corr_value <- cor(x_stat_data, y_stat_data, use = "complete.obs")
      corr_text <- paste("r = ", round(corr_value, 2))  # Display just r value
      
      p <- ggplot(filtered_data(), aes_string(x = input$x_stat, y = input$y_stat, text = "Pitcher_Name")) +
        geom_point(color = "darkblue") +
        labs(title = "Correlation between Stats", x = input$x_stat, y = input$y_stat) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold"))
      
      ggplotly(p, tooltip = c("text")) %>%
        layout(title = paste(input$x_stat, "vs.", input$y_stat, "Correlation (", corr_text, ")", sep = " "))
    } else {
      # Handle case when the selected columns are not numeric
      return(NULL)
    }
  })
  
  # Render Table with rows filtered based on "Show Entries"
  output$pitcher_table <- renderReactable({
    
    # Determine number of rows to display based on user input
    rows_to_display <- if (input$show_entries == "All") {
      nrow(filtered_data())
    } else {
      as.numeric(input$show_entries)
    }
    
    reactable(
      filtered_data()[1:rows_to_display, ], # Limit rows based on dropdown selection
      filterable = FALSE,  # Disable the default filter row
      searchable = TRUE,  # Allows search functionality
      sortable = TRUE,    # Allows sorting
      bordered = TRUE,
      highlight = TRUE,
      striped = TRUE,
      defaultColDef = colDef(align = "center"),
      columns = list(
        Pitcher_Name = colDef(name = "Pitcher", minWidth = 150),
        Team = colDef(name = "Team", minWidth = 150),
        Season = colDef(name = "Season", minWidth = 150),
        Threat_Plus = colDef(
          name = "Threat+", 
          style = color_scale(filtered_data()$Threat_Plus, reverse = FALSE), # Apply reverse color scale
          format = colFormat(digits = 2)
        ),
        Pressure_Plus = colDef(
          name = "Pressure+", 
          style = color_scale(filtered_data()$Pressure_Plus, reverse = FALSE), # Apply reverse color scale
          format = colFormat(digits = 2)
        ),
        Momentum_Plus = colDef(
          name = "Momentum+", 
          style = color_scale(filtered_data()$Momentum_Plus, reverse = FALSE), # Apply reverse color scale
          format = colFormat(digits = 2)
        ),
        Danger_Plus = colDef(
          name = "Danger+", 
          style = color_scale(filtered_data()$Danger_Plus, reverse = FALSE),
          format = colFormat(digits = 2)
        ),
        Hero_Save = colDef(
          name = "Hero Save", 
          style = color_scale(filtered_data()$Hero_Save, reverse = FALSE),
          format = colFormat(digits = 2)
        ),
        Hero_Save_Plus = colDef(
          name = "Hero Save+", 
          style = color_scale(filtered_data()$Hero_Save_Plus, reverse = FALSE),
          format = colFormat(digits = 2)
        ),
        Hero_Hold = colDef(
          name = "Hero Hold", 
          style = color_scale(filtered_data()$Hero_Hold, reverse = FALSE),
          format = colFormat(digits = 2)
        ),
        Hero_Hold_Plus = colDef(
          name = "Hero Hold+", 
          style = color_scale(filtered_data()$Hero_Hold_Plus, reverse = FALSE),
          format = colFormat(digits = 2)
        ),
        Hero_Losing = colDef(
          name = "Hero Losing", 
          style = color_scale(filtered_data()$Hero_Losing, reverse = FALSE),
          format = colFormat(digits = 2)
        ),
        Hero_Losing_Plus = colDef(
          name = "Hero Losing+", 
          style = color_scale(filtered_data()$Hero_Losing_Plus, reverse = FALSE),
          format = colFormat(digits = 2)
        ),
        Hero_Total = colDef(
          name = "Hero Total", 
          style = color_scale(filtered_data()$Hero_Total, reverse = FALSE),
          format = colFormat(digits = 2)
        ),
        Hero_Plus = colDef(
          name = "Hero+", 
          style = color_scale(filtered_data()$Hero_Plus, reverse = FALSE),
          format = colFormat(digits = 2)
        ),
        Hero = colDef(
          name = "Hero", 
          style = color_scale(filtered_data()$Hero, reverse = FALSE),
          format = colFormat(digits = 2)
        ),
        Danger_Final = colDef(
          name = "Danger Final", 
          style = color_scale(filtered_data()$Danger_Final, reverse = FALSE),
          format = colFormat(digits = 2)
        ),
        Net_Danger = colDef(
          name = "Net Danger", 
          style = color_scale(filtered_data()$Net_Danger, reverse = FALSE),
          format = colFormat(digits = 2)
        )
      ),
      theme = reactableTheme(
        color = "#333", backgroundColor = "#F8F9FA",
        headerStyle = list(background = "#003366", color = "white", fontWeight = "bold", fontSize = "14px"),
        rowStripedStyle = list(background = "#E9ECEF"),
        borderColor = "#DEE2E6",
        highlightColor = "#D6E9F8",
        cellPadding = "8px"
      )
    )
  })
  
  # Render Scatter Plot in Separate Tab
  output$scatterPlot <- renderPlotly({
    data <- filtered_data()
    
    # Ensure x and y columns exist and are numeric
    if (!(input$x_stat %in% names(data)) || !(input$y_stat %in% names(data))) {
      return(NULL)
    }
    
    x_vals <- data[[input$x_stat]]
    y_vals <- data[[input$y_stat]]
    
    # Check for non-numeric data
    if (!is.numeric(x_vals) || !is.numeric(y_vals)) {
      return(NULL)
    }
    
    # Compute correlation only if both columns exist and have valid numeric data
    if (length(na.omit(x_vals)) > 1 && length(na.omit(y_vals)) > 1) {
      corr_value <- cor(x_vals, y_vals, use = "complete.obs")
      corr_text <- paste("r =", round(corr_value, 2))
    } else {
      corr_text <- "r = N/A"
    }
    
    # Create scatter plot
    p <- ggplot(data, aes_string(x = input$x_stat, y = input$y_stat, text = "Pitcher")) +
      geom_point(color = "darkblue") +
      labs(title = "Correlation between Stats", x = input$x_stat, y = input$y_stat) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = c("text")) %>%
      layout(title = paste(input$x_stat, "vs.", input$y_stat, "Correlation (", corr_text, ")", sep = " "))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

