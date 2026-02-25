files <- list.files(
  pattern = "model_table\\.csv$",
  full.names = TRUE
)

all_tables <- lapply(files, function(f) {

  df <- read.csv(
    f,
    sep = ";",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  colnames(df) <- trimws(colnames(df))

  if (!("Estimate" %in% colnames(df)) || !("Std. Error" %in% colnames(df))) {
    stop(
      paste(
        "Missing required columns in", f,
        "\nFound:", paste(colnames(df), collapse = ", ")
      )
    )
  }

  # Extract response variable from file name
  response_var <- sub("_model_table\\.csv$", "", basename(f))

  df$Response <- response_var

  df
})

# Bind everything vertically
final_table <- do.call(rbind, all_tables)

# Optional: move Response column to the front
final_table <- final_table[, c("Response", setdiff(colnames(final_table), "Response"))]

# Save combined table
write.csv(
  final_table,
  "all_model_tables_combined.csv",
  sep = ";",
  row.names = FALSE
)
