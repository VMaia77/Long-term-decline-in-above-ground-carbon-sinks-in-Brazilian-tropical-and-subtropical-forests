files <- list.files(
  pattern = "model_table\\.csv$",
  full.names = TRUE
)

for (f in files) {

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

  lower <- df$Estimate - df$`Std. Error` * 1.96
  upper <- df$Estimate + df$`Std. Error` * 1.96

  df$`CI 95%` <- sprintf("[%.3f, %.3f]", lower, upper)

  write.csv(
    df,
    f,
    sep = ";",
    row.names = FALSE
  )
}
