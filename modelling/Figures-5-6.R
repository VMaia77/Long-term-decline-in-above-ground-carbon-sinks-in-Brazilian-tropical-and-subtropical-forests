# load packages
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)
library(gridExtra)
library(MuMIn)

# load the images containing the models for each response variable
load("models_c_stocks.RData")
load("models_net_c_sink.RData")
load("models_c_gains.RData")
load("models_c_losses.RData")
load("models_net_perc.RData")
load("models_gains_perc.RData")
load("models_losses_perc.RData")
load("models_crt.RData")


# set plot parameters
col_line <- "#C68642"   # brown–orange–gold (same for all figures)
f_col <- NA             # hollow center
shape_t <- 21           # hollow point
size_p <- 0.7
size_l <- 0.77

# Ensure Helvetica on Windows
windowsFonts(Helvetica = windowsFont("Helvetica"))

# -------------------------
# General model plot
# -------------------------
general_model_plot <- function(model_result, title, color) {

  res <- model_result

  coef_data <- as.data.frame(coefTable(res))
  coef_data <- coef_data[-1, ]
  colnames(coef_data) <- c("Estim", "SE")
  coef_data$Var <- rownames(coef_data)

  coef_data$SE <- coef_data$SE * 1.96

  # ORIGINAL CI LOGIC (unchanged)
  coef_data$sig <- ifelse(
    coef_data$Estim > 0 & coef_data$Estim - coef_data$SE > 0, "*",
    ifelse(
      coef_data$Estim < 0 & coef_data$Estim + coef_data$SE < 0, "*", " ")
  )

  ggplot(coef_data, aes(x = Var, y = Estim)) +
    geom_hline(
      yintercept = 0,
      linetype = "solid",
      colour = color,
      size = size_l
    ) +
    geom_linerange(
      aes(
        ymin = Estim - SE,
        ymax = Estim + SE
      ),
      size = size_p,
      color = ifelse(coef_data$sig == "*", "black", "gray70")
    ) +
    geom_point(
      shape = shape_t,
      size = size_p + 0.2,
      fill = f_col,
      color = ifelse(coef_data$sig == "*", "black", "gray70")
    ) +
    coord_flip() +
    scale_y_continuous("Effect size") +
    scale_x_discrete(
      " ",
      labels = c(
        "warm_dry_climate_scaled" = "Cold–wet → Warm–dry",
        "warm_dry_climate_scaled:year_scaled" = "Cold–wet → Warm–dry:Year",
        "site_area_ha_log_scaled" = "Site area",
        "year_scaled" = "Year",
        "cwm_sla_scaled" = "SLA",
        "cwm_sla_scaled:year_scaled" = "SLA:Year",
        "cwm_wd_scaled" = "WD",
        "cwm_wd_scaled:year_scaled" = "WD:Year",
        "sla_sd_scaled" = "SLA disp",
        "sla_sd_scaled:year_scaled" = "SLA disp:Year",
        "wd_sd_scaled" = "WD disp",
        "wd_sd_scaled:year_scaled" = "WD disp:Year",
        "sespd_scaled" = "PD",
        "sespd_scaled:year_scaled" = "PD:Year",
        "shannon_hill_scaled" = "TD",
        "shannon_hill_scaled:year_scaled" = "TD:Year",
        "soil_fertily_scaled" = "Soil fertility",
        "aluminum_phosphorus_scaled" = "Aluminum - phosphorus",
        "phosphorus_magnesium_scaled" = "Phosphorus → magnesium"
      )
    ) +
    labs(title = title) +
    theme_bw(base_family = "Helvetica") +
    theme(
      text = element_text(size = 7),
      axis.text = element_text(size = 6),
      axis.title = element_text(size = 7),
      plot.title = element_text(size = 7, face = "bold", hjust = 0.5),
      legend.position = "none"
    )
}

# -------------------------
# Build plots
# -------------------------
plot_c_stocks   <- general_model_plot(averaged_model_c_stocks, "Carbon stocks", col_line)
plot_net_c_sink <- general_model_plot(averaged_model_net_c_sink, "Net carbon sink", col_line)
plot_c_gains    <- general_model_plot(averaged_model_c_gains, "Carbon gains", col_line)
plot_c_losses   <- general_model_plot(averaged_model_c_losses, "Carbon losses", col_line)

plot_net_perc    <- general_model_plot(averaged_model_net_perc, "Net carbon sink (relative)", col_line)
plot_gains_perc  <- general_model_plot(averaged_model_gains_perc, "Carbon gains (relative)", col_line)
plot_losses_perc <- general_model_plot(averaged_model_losses_perc, "Carbon losses (relative)", col_line)
plot_crt         <- general_model_plot(averaged_model_crt, "Carbon residence time", col_line)

# -------------------------
# Combine panels
# -------------------------
fig5 <- plot_grid(
  plot_c_stocks, plot_net_c_sink,
  plot_c_gains, plot_c_losses,
  ncol = 2,
  labels = c("A)", "B)", "C)", "D)"),
  label_x = 0.02,
  label_y = 0.98,
  hjust = 0,
  vjust = 1,
  label_fontfamily = "Helvetica",
  label_fontface = "plain"
)

fig6 <- plot_grid(
  plot_net_perc, plot_gains_perc,
  plot_losses_perc, plot_crt,
  ncol = 2,
  labels = c("A)", "B)", "C)", "D)"),
  label_x = 0.02,
  label_y = 0.98,
  hjust = 0,
  vjust = 1,
  label_fontfamily = "Helvetica",
  label_fontface = "plain"
)


# -------------------------
# Save as vector PDFs
# -------------------------
ggsave(
  "Figure5.pdf",
  fig5,
  width = 18,
  height = 18,
  units = "cm",
  device = cairo_pdf
)

ggsave(
  "Figure6.pdf",
  fig6,
  width = 18,
  height = 18,
  units = "cm",
  device = cairo_pdf
)