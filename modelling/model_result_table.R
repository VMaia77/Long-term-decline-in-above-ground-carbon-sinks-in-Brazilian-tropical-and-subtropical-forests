library(dplyr)


model_result_table <- function(averaged_model, save_as) {

    template_table <- read.table("./modelling/template_model_tables.csv",
                                    header = TRUE, dec = ".", sep = ";")

    df_res <- as.data.frame(averaged_model$coefmat.subset)
    df_res <- tibble::rownames_to_column(df_res, "raw")

    res_table <- left_join(df_res, template_table, by = "raw") %>%
    select(-raw) %>%
    rename(Predictor = var) %>%
    relocate(Predictor, .before = Estimate  )

    write.csv(res_table, paste(save_as, "model_table.csv", sep = "_"), row.names = FALSE)

    return(res_table)

}