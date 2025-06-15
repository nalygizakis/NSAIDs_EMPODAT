library(shiny)
library(googleVis)
library(dplyr)
library(readr)
library(stringr)
load("data.RData")
# === Data Cleaning ===

# Clean and convert values
all_compounds <- all_compounds %>%
  filter(!grepl("Less than", Value), !is.na(Value)) %>%
  mutate(
    Value = as.numeric(Value) * 1000,
    Compound = str_trim(toupper(Compound)),
    `Sample matrix` = str_trim(`Sample matrix`)
  ) %>%
  filter(!is.na(`Name of country`), !is.na(`Sample matrix`))

# === UI ===

ui <- fluidPage(
  titlePanel("NSAID Concentration Mapping"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("compound", "Select Compound:",
                  choices = sort(unique(all_compounds$Compound))),
      uiOutput("matrix_ui")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map View",
                 htmlOutput("geoMap")),
        tabPanel("Summary Table",
                 tags$h4("Country-wise Summary:"),
                 tableOutput("summaryTable"))
      )
    )
  )
)

# === Server ===

server <- function(input, output, session) {
  
  # Update matrix dropdown based on compound selection
  output$matrix_ui <- renderUI({
    req(input$compound)
    available_matrices <- all_compounds %>%
      filter(Compound == input$compound) %>%
      pull(`Sample matrix`) %>%
      unique() %>%
      sort()
    
    selectInput("matrix", "Select Sample Matrix:", choices = available_matrices)
  })
  
  # Reactive summary
  df_summary <- reactive({
    req(input$compound, input$matrix)
    
    df <- all_compounds %>%
      filter(Compound == input$compound, `Sample matrix` == input$matrix) %>%
      group_by(`Name of country`) %>%
      summarise(
        mean_concentration = mean(Value, na.rm = TRUE),
        n = sum(!is.na(Value)),
        .groups = "drop"
      ) %>%
      filter(mean_concentration > 0)
    
    df
  })
  
  # Show table
  output$summaryTable <- renderTable({
    df_summary()
  })
  
  # Show map
  output$geoMap <- renderGvis({
    df <- df_summary()
    if (nrow(df) == 0) return(NULL)
    
    # Use log10 to rescale
    df$log_mean <- log10(df$mean_concentration + 1)  # add 1 to avoid log(0)
    
    df$tooltip <- paste0(
      "Country: ", df$`Name of country`, "\n",
      "Mean: ", signif(df$mean_concentration, 4), " µg/L\n",
      "n = ", df$n
    )

    gvisGeoChart(df,
                 locationvar = "Name of country",
                 colorvar = "log_mean",
                 hovervar = "tooltip",
                 options = list(
                   region = "150",
                   displayMode = "regions",
                   resolution = "countries",
                   colorAxis = "{colors:['#CCE5FF', '#084594']}",
                   backgroundColor = "lightgrey",
                   width = 800,
                   height = 500
                 ))

  })
  
}

# === Run the app ===
shinyApp(ui = ui, server = server)
