# Create summary table with full statistics
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
