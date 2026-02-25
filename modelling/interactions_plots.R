library(ggplot2)


get_coefs_pvals_diversity <- function(averaged_model) {

    coefs_subset <- averaged_model$coefficients["subset", ]

    # coefs
    coefb0 <- coefs_subset[["(Intercept)"]]

    coef_year <- coefs_subset[["year_scaled"]]

    # shannon_hill_scaled
    coef_shannon <- try(coefs_subset[["shannon_hill_scaled"]], silent = TRUE) # nolint
    coef_year_shannon <- try(coefs_subset[["shannon_hill_scaled:year_scaled"]], silent = TRUE) # nolint

    # wd_sd_scaled
    coef_wd_sd <- try(coefs_subset[["wd_sd_scaled"]], silent = TRUE) # nolint
    coef_year_wd_sd <- try(coefs_subset[["wd_sd_scaled:year_scaled"]], silent = TRUE) # nolint

    # sla_sd_scaled
    coef_sla_sd <- try(coefs_subset[["sla_sd_scaled"]], silent = TRUE) # nolint
    coef_year_sla_sd <- try(coefs_subset[["sla_sd_scaled:year_scaled"]], silent = TRUE) # nolint    

    # cwm_wd_scaled
    coef_cwm_wd <- try(coefs_subset[["cwm_wd_scaled"]], silent = TRUE) # nolint
    coef_year_cwm_wd <- try(coefs_subset[["cwm_wd_scaled:year_scaled"]], silent = TRUE) # nolint

    # cwm_sla_scaled
    coef_cwm_sla <- try(coefs_subset[["cwm_sla_scaled"]], silent = TRUE) # nolint
    coef_year_cwm_sla <- try(coefs_subset[["cwm_sla_scaled:year_scaled"]], silent = TRUE) # nolint

    # sespd_scaled
    coef_sespd <- try(coefs_subset[["sespd_scaled"]], silent = TRUE) # nolint
    coef_year_sespd <- try(coefs_subset[["sespd_scaled:year_scaled"]], silent = TRUE) # nolint

    # pvals
    pval_year <- try(summary(averaged_model)$coefmat.subset["year_scaled", "Pr(>|z|)"], silent = TRUE) # nolint
    
    pval_year_shannon <- try(summary(averaged_model)$coefmat.subset["shannon_hill_scaled:year_scaled", "Pr(>|z|)"], silent = TRUE) # nolint

    pval_year_wd_sd <- try(summary(averaged_model)$coefmat.subset["wd_sd_scaled:year_scaled", "Pr(>|z|)"], silent = TRUE) # nolint

    pval_year_sla_sd <- try(summary(averaged_model)$coefmat.subset["sla_sd_scaled:year_scaled", "Pr(>|z|)"], silent = TRUE) # nolint

    pval_year_cwm_wd <- try(summary(averaged_model)$coefmat.subset["cwm_wd_scaled:year_scaled", "Pr(>|z|)"], silent = TRUE) # nolint

    pval_year_cwm_sla <- try(summary(averaged_model)$coefmat.subset["cwm_sla_scaled:year_scaled", "Pr(>|z|)"], silent = TRUE) # nolint

    pval_year_sespd <- try(summary(averaged_model)$coefmat.subset["sespd_scaled:year_scaled", "Pr(>|z|)"], silent = TRUE) # nolint

    return(list(coefb0=coefb0, 
        coef_year=coef_year, 

        coef_shannon=coef_shannon, 
        coef_wd_sd=coef_wd_sd, 
        coef_sla_sd=coef_sla_sd, 
        coef_cwm_wd=coef_cwm_wd, 
        coef_cwm_sla=coef_cwm_sla, 
        coef_sespd=coef_sespd, 

        coef_year_shannon=coef_year_shannon, 
        coef_year_wd_sd=coef_year_wd_sd, 
        coef_year_sla_sd=coef_year_sla_sd, 
        coef_year_cwm_wd=coef_year_cwm_wd, 
        coef_year_cwm_sla=coef_year_cwm_sla, 
        coef_year_sespd=coef_year_sespd, 

        pval_year=pval_year, 

        pval_year_shannon=pval_year_shannon, 
        pval_year_wd_sd=pval_year_wd_sd,
        pval_year_sla_sd=pval_year_sla_sd, 
        pval_year_cwm_wd=pval_year_cwm_wd, 
        pval_year_cwm_sla=pval_year_cwm_sla,
        pval_year_sespd=pval_year_sespd)) # nolint
}


get_coefs_pvals_clim <- function(averaged_model) {

    coefs_subset <- averaged_model$coefficients["subset", ]

    # coefs
    coefb0 <- coefs_subset[["(Intercept)"]]

    coef_year <- coefs_subset[["year_scaled"]]

    coef_clim <- coefs_subset[["warm_dry_climate_scaled"]] # nolint

    coef_year_clim <- coefs_subset[["warm_dry_climate_scaled:year_scaled"]] # nolint

    # pvals
    pval_year <- summary(averaged_model)$coefmat.subset["year_scaled", "Pr(>|z|)"] # nolint

    pval_year_clim <- summary(averaged_model)$coefmat.subset["warm_dry_climate_scaled:year_scaled", "Pr(>|z|)"] # nolint

    return(list(coefb0=coefb0, 

        coef_year_clim=coef_year_clim, 

        coef_year=coef_year, 

        coef_clim=coef_clim, # nolint

        pval_year=pval_year, 

        pval_year_clim=pval_year_clim)) # nolint
}



interaction_plot <- function(y, ylog, pred, moderator,
    b0, b_pred, b_interaction, b_moderator,
    df, moderator_level, colours_palletes, tag, xlab, ylab, colorlab,
    xlim, ylim, pval_pred, pval_int) {

    if (isTRUE(ylog == FALSE)) {
        # the effect of X at the average levels of the moderator
        func_mean <- function(x) {
            (b0 + b_pred * ((x - mean(df[[pred]])) / sd(df[[pred]])))
        }

        # effect of X at lower levels of the moderator
        func_mean_minus_sd <- function(x) {
            (b0 + b_pred * ((x - mean(df[[pred]])) / sd(df[[pred]])) +
             b_interaction * (((x - mean(df[[pred]])) / sd(df[[pred]])) *
                 -moderator_level) +
             b_moderator * (-moderator_level))
        }

        # effect of X at higher levels of the moderator
        func_mean_plus_sd <- function(x) {
            (b0 + b_pred * ((x - mean(df[[pred]])) / sd(df[[pred]])) +
             b_interaction * (((x - mean(df[[pred]])) / sd(df[[pred]])) *
                 moderator_level) +
             b_moderator * (moderator_level))
        }
    } else {
        # the effect of X at the average levels of the moderator
        func_mean <- function(x) {
            exp(b0 + b_pred * ((x - mean(df[[pred]])) / sd(df[[pred]])))
        }

        # effect of X at lower levels of the moderator
        func_mean_minus_sd <- function(x) {
            exp(b0 + b_pred * ((x - mean(df[[pred]])) / sd(df[[pred]])) +
                b_interaction * (((x - mean(df[[pred]])) / sd(df[[pred]])) *
                    -moderator_level) +
                b_moderator * (-moderator_level))
        }

        # effect of X at higher levels of the moderator
        func_mean_plus_sd <- function(x) {
            exp(b0 + b_pred * ((x - mean(df[[pred]])) / sd(df[[pred]])) +
                b_interaction * (((x - mean(df[[pred]])) / sd(df[[pred]])) *
                    moderator_level) +
                b_moderator * (moderator_level))
        }
    }

    pval_pred <- ifelse(
        pval_pred < 0.0001,
        "< 0.0001",
        paste("=", round(pval_pred, 4), sep = " ")
    )

    pval_int <- ifelse(
        pval_int < 0.0001,
        "< 0.0001",
        paste("=", round(pval_int, 4), sep = " ")
    )

    p1 <- ggplot(
        df,
        aes(x = df[[pred]], y = df[[y]], col = (df[[moderator]]))
    ) +

    geom_point(size = 2.7, alpha = 0.87) +

    stat_function(fun = func_mean,
                  colour = colours_palletes[3],
                  size = 1.07) +

    stat_function(fun = func_mean_minus_sd,
                  colour = colours_palletes[1],
                  size = 1.07) +

    stat_function(fun = func_mean_plus_sd,
                  colour = colours_palletes[4],
                  size = 1.07) +

    theme_classic() +

    labs(
        x = xlab,
        y = ylab,
        color = colorlab,
        tag = tag
    ) +

    theme(
        text = element_text(
            size = 13.07,
            colour = "black",
            family = "Helvetica"
        )
    ) +
    theme(axis.text.x = element_text(colour = "black", size = 13.07)) +
    theme(axis.text.y = element_text(colour = "black", size = 13.07)) +

    scale_color_gradientn(colours = colours_palletes) +

    theme(
        panel.border = element_rect(
            colour = "black",
            fill = NA,
            size = 0.7
        )
    ) # +

    # annotate(
    #     "text",
    #     x = xlim,
    #     y = ylim,
    #     label = as.expression(bquote(atop(
    #         Slope~.(round(b_pred, 3))~italic(P)~.(pval_pred),
    #         Interaction~.(round(b_interaction, 3))~italic(P)~.(pval_int)
    #     )))
    # )

    return(p1)
}
