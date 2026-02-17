library(readr)
library(dplyr)
library(stringr)
library(googleVis)

data_folder <- "C:/Users/Nikiforos/Desktop/ELE NSAIDS/data"

# === Read all files ===
files <- list.files(path = data_folder, full.names = TRUE)

# === Function to read and process a single file ===
read_and_process <- function(file_path) {
  df <- read_csv(file_path, skip = 1, show_col_types = FALSE)
  df <- df[-1, ]
  compound_name <- str_extract(basename(file_path), "^[^ ]+")
  df$Compound <- compound_name
  return(df)
}

# === Combine all files ===
all_compounds <- files %>%
  lapply(read_and_process) %>%
  bind_rows()

# === Ensure Value is numeric ===
all_compounds$Value <- as.numeric(all_compounds$Value)*1000

# === Get unique compounds ===
compound_list <- unique(all_compounds$Compound)

# === Create and plot maps for each compound ===
geo_maps <- list()

for (compound_name in compound_list) {
  df_summary <- all_compounds %>%
    filter(Compound == compound_name) %>%
    group_by(`Name of country`) %>%
    summarise(mean_concentration = mean(Value, na.rm = TRUE)) %>%
    filter(mean_concentration > 0)

  if (nrow(df_summary) > 0) {
    geo_maps[[compound_name]] <- gvisGeoChart(df_summary,
                                              locationvar = "Name of country",
                                              colorvar = "mean_concentration",
                                              options = list(
                                                region = "150",  # ← Europe
                                                displayMode = "regions",
                                                colorAxis = "{colors:['#B3D1FF', '#08306B']}",
                                                backgroundColor = "lightgrey",
                                                resolution = "countries"
                                              ))
  }
}


library(htmltools)

html_list <- lapply(names(geo_maps), function(name) {
  h3(name)  # Title
  tagList(
    tags$h3(name),
    HTML(geo_maps[[name]]$html$chart)
  )
})

save_html(
  html = tagList(html_list),
  file = "All_GeoCharts_Europe.html"
)

browseURL("All_GeoCharts_Europe.html")



#Summary CSV file with full statistics
summary_table <- all_compounds %>%
  group_by(Compound, `Sample matrix`) %>%
  summarise(
    min = min(Value, na.rm = TRUE),
    q25 = quantile(Value, 0.25, na.rm = TRUE),
    median = median(Value, na.rm = TRUE),
    mean = mean(Value, na.rm = TRUE),
    q75 = quantile(Value, 0.75, na.rm = TRUE),
    max = max(Value, na.rm = TRUE),
    n = sum(!is.na(Value)),
    .groups = "drop"
  )

# Export to CSV
write_csv(summary_table, "NSAIDs_Matrix_Summary.csv")