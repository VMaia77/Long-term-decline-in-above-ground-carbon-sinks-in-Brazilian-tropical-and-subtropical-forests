# load packages
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyr)
library(gridExtra)
library(MuMIn)

source("./modelling/interactions_plots.R")

# load the images containing the models for each response variable
load("models_c_stocks.RData")
data_mod_full <- data_mod
load("models_net_c_sink.RData")
load("models_c_gains.RData")
load("models_c_losses.RData")
load("models_net_perc.RData")
load("models_gains_perc.RData")
load("models_losses_perc.RData")
load("models_crt.RData")

##### colors

# colorblind-safe continuous palette (general use)
colours_palletes <- viridis::plasma(6)
colours_palletes_clim <- viridis::plasma(6)

######################################

########### c_stocks ###########
coefs_pvals_c_stocks <- get_coefs_pvals_diversity(averaged_model_c_stocks)

# wd disp
p1 <- interaction_plot(y = "c_stocks_ha", ylog = TRUE,
    pred = "year",
    moderator = "wd_sd",
    b0 = coefs_pvals_c_stocks[["coefb0"]],
    b_pred = coefs_pvals_c_stocks[["coef_year"]],
    b_interaction = coefs_pvals_c_stocks[["coef_year_wd_sd"]],
    b_moderator = coefs_pvals_c_stocks[["coef_wd_sd"]],
    df = data_mod_full,
    moderator_level = 1,
    colours_palletes = colours_palletes_clim,
    tag = "A)",
    xlab = expression(Year),
    ylab = expression(Carbon~stocks~(Mg~C~ha^-1)),
    colorlab = "WD disp.",
    xlim = 1998,
    ylim = 200,
    pval_pred = coefs_pvals_c_stocks[["pval_year"]],
    pval_int = coefs_pvals_c_stocks[["pval_year_wd_sd"]])

p1

# pd
p2 <- interaction_plot(y = "c_stocks_ha", ylog = TRUE,
    pred = "year",
    moderator = "sespd",
    b0 = coefs_pvals_c_stocks[["coefb0"]],
    b_pred = coefs_pvals_c_stocks[["coef_year"]],
    b_interaction = coefs_pvals_c_stocks[["coef_year_sespd"]],
    b_moderator = coefs_pvals_c_stocks[["coef_sespd"]],
    df = data_mod_full,
    moderator_level = 1,
    colours_palletes = colours_palletes_clim,
    tag = "B)",
    xlab = expression(Year),
    ylab = expression(Carbon~stocks~(Mg~C~ha^-1)),
    colorlab = "PD",
    xlim = 1998,
    ylim = 200,
    pval_pred = coefs_pvals_c_stocks[["pval_year"]],
    pval_int = coefs_pvals_c_stocks[["pval_year_sespd"]])

p2


########### c_losses ###########
coefs_pvals_c_losses <- get_coefs_pvals_diversity(averaged_model_c_losses)

p3 <- interaction_plot(y = "c_losses", ylog = TRUE,
    pred = "year",
    moderator = "wd_sd",
    b0 = coefs_pvals_c_losses[["coefb0"]],
    b_pred = coefs_pvals_c_losses[["coef_year"]],
    b_interaction = coefs_pvals_c_losses[["coef_year_wd_sd"]],
    b_moderator = coefs_pvals_c_losses[["coef_wd_sd"]],
    df = data_mod,
    moderator_level = 1,
    colours_palletes = colours_palletes_clim,
    tag = "C)",
    xlab = expression(Year),
    ylab = expression(Carbon~losses~(Mg~C~ha^-1~yr^-1)),
    colorlab = "WD disp.",
    xlim = 1998,
    ylim = 7,
    pval_pred = coefs_pvals_c_losses[["pval_year"]],
    pval_int = coefs_pvals_c_losses[["pval_year_wd_sd"]])

p3


########### losses_perc ###########
coefs_pvals_losses_perc <- get_coefs_pvals_diversity(averaged_model_losses_perc)

p4 <- interaction_plot(y = "losses_perc", ylog = TRUE,
    pred = "year",
    moderator = "cwm_wd",
    b0 = coefs_pvals_losses_perc[["coefb0"]],
    b_pred = coefs_pvals_losses_perc[["coef_year"]],
    b_interaction = coefs_pvals_losses_perc[["coef_year_cwm_wd"]],
    b_moderator = coefs_pvals_losses_perc[["coef_cwm_wd"]],
    df = data_mod,
    moderator_level = 1,
    colours_palletes = colours_palletes_clim,
    tag = "D)",
    xlab = expression(Year),
    ylab = expression(Carbon~losses~(relative)),
    colorlab = "WD",
    xlim = 1998,
    ylim = 0.05,
    pval_pred = coefs_pvals_losses_perc[["pval_year"]],
    pval_int = coefs_pvals_losses_perc[["pval_year_cwm_wd"]])

p4

# p5 <- interaction_plot(y = "losses_perc", ylog = TRUE,
#     pred = "year",
#     moderator = "wd_sd",
#     b0 = coefs_pvals_losses_perc[["coefb0"]],
#     b_pred = coefs_pvals_losses_perc[["coef_year"]],
#     b_interaction = coefs_pvals_losses_perc[["coef_year_wd_sd"]],
#     b_moderator = coefs_pvals_losses_perc[["coef_wd_sd"]],
#     df = data_mod,
#     moderator_level = 1,
#     colours_palletes = colours_palletes_clim,
#     tag = "C)",
#     xlab = expression(Year),
#     ylab = expression(Carbon~losses~(relative)),
#     colorlab = "WD disp.",
#     xlim = 1998,
#     ylim = 0.05,
#     pval_pred = coefs_pvals_losses_perc[["pval_year"]],
#     pval_int = coefs_pvals_losses_perc[["pval_year_wd_sd"]])

# p5

# Create grids

# Figure 9
allplotslist <- align_plots(p1, p2, p3, p4, align = "hv")

fig9 <- grid.arrange(
  allplotslist[[1]],
  allplotslist[[2]],
  allplotslist[[3]],
  allplotslist[[4]],
  nrow = 2
)

# ggsave(
#   "figure7.tiff",
#   fig9,
#   height = 27,
#   width = 37,
#   units = "cm",
#   dpi = 400
# )

# system("open figure7.tiff")

ggsave(
  filename = "figure7.pdf",
  plot = fig9,
  height = 27,
  width = 37,
  units = "cm",
  device = cairo_pdf
)



