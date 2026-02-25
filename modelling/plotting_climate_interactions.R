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

colors_ <- c("darkorange1", "brown3", "darkolivegreen3", "forestgreen")
colfunc <- colorRampPalette(colors_)
colours_palletes <- colfunc(6)

colors_clim <- c("gold2", "darkorange2",
  "indianred1", "brown3", "seagreen1", "purple3", "dodgerblue4") %>% rev()
colfunc_clim <- colorRampPalette(colors_clim)
colours_palletes_clim <- colfunc_clim(6)

######################################

########### c_stocks ###########
coefs_pvals_c_stocks <- get_coefs_pvals_clim(averaged_model_c_stocks)

p1 <- interaction_plot(y = "c_stocks_ha", ylog = TRUE,
    pred = "year",
    moderator = "warm_dry_climate",
    b0 = coefs_pvals_c_stocks[["coefb0"]],
    b_pred = coefs_pvals_c_stocks[["coef_year"]],
    b_interaction = coefs_pvals_c_stocks[["coef_year_clim"]],
    b_moderator = coefs_pvals_c_stocks[["coef_clim"]],
    df = data_mod_full,
    moderator_level = 1,
    colours_palletes = colours_palletes_clim,
    tag = "A)",
    xlab = expression(Year),
    ylab = expression(Carbon~stocks~(Mg~C~ha^-1)),
    colorlab = "Cold-wet -> Warm-dry",
    xlim = 1998,
    ylim = 200,
    pval_pred = coefs_pvals_c_stocks[["pval_year"]],
    pval_int = coefs_pvals_c_stocks[["pval_year_clim"]])

p1

########### net_c_sink ###########
coefs_pvals_net_c_sink <- get_coefs_pvals_clim(averaged_model_net_c_sink)

p2 <- interaction_plot(y = "net_c_sink", ylog = FALSE,
    pred = "year",
    moderator = "warm_dry_climate",
    b0 = coefs_pvals_net_c_sink[["coefb0"]],
    b_pred = coefs_pvals_net_c_sink[["coef_year"]],
    b_interaction = coefs_pvals_net_c_sink[["coef_year_clim"]],
    b_moderator = coefs_pvals_net_c_sink[["coef_clim"]],
    df = data_mod,
    moderator_level = 1,
    colours_palletes = colours_palletes_clim,
    tag = "B)",
    xlab = expression(Year),
    ylab = expression(Net~carbon~sink~(Mg~C~ha^-1~yr^-1)),
    colorlab = "Cold-wet -> Warm-dry",
    xlim = 1998,
    ylim = -5,
    pval_pred = coefs_pvals_net_c_sink[["pval_year"]],
    pval_int = coefs_pvals_net_c_sink[["pval_year_clim"]])

p2

########### c_gains ###########
coefs_pvals_c_gains <- get_coefs_pvals_clim(averaged_model_c_gains)

p3 <- interaction_plot(y = "c_gains", ylog = TRUE,
    pred = "year",
    moderator = "warm_dry_climate",
    b0 = coefs_pvals_c_gains[["coefb0"]],
    b_pred = coefs_pvals_c_gains[["coef_year"]],
    b_interaction = coefs_pvals_c_gains[["coef_year_clim"]],
    b_moderator = coefs_pvals_c_gains[["coef_clim"]],
    df = data_mod,
    moderator_level = 1,
    colours_palletes = colours_palletes_clim,
    tag = "C)",
    xlab = expression(Year),
    ylab = expression(Carbon~gains~(Mg~C~ha^-1~yr^-1)),
    colorlab = "Cold-wet -> Warm-dry",
    xlim = 1998,
    ylim = 6,
    pval_pred = coefs_pvals_c_gains[["pval_year"]],
    pval_int = coefs_pvals_c_gains[["pval_year_clim"]])

p3

########### c_losses ###########
coefs_pvals_c_losses <- get_coefs_pvals_clim(averaged_model_c_losses)

p4 <- interaction_plot(y = "c_losses", ylog = TRUE,
    pred = "year",
    moderator = "warm_dry_climate",
    b0 = coefs_pvals_c_losses[["coefb0"]],
    b_pred = coefs_pvals_c_losses[["coef_year"]],
    b_interaction = coefs_pvals_c_losses[["coef_year_clim"]],
    b_moderator = coefs_pvals_c_losses[["coef_clim"]],
    df = data_mod,
    moderator_level = 1,
    colours_palletes = colours_palletes_clim,
    tag = "D)",
    xlab = expression(Year),
    ylab = expression(Carbon~losses~(Mg~C~ha^-1~yr^-1)),
    colorlab = "Cold-wet -> Warm-dry",
    xlim = 1998,
    ylim = 7,
    pval_pred = coefs_pvals_c_losses[["pval_year"]],
    pval_int = coefs_pvals_c_losses[["pval_year_clim"]])

p4

########### net_perc ###########
coefs_pvals_net_perc <- get_coefs_pvals_clim(averaged_model_net_perc)

p5 <- interaction_plot(y = "net_perc", ylog = FALSE,
    pred = "year",
    moderator = "warm_dry_climate",
    b0 = coefs_pvals_net_perc[["coefb0"]],
    b_pred = coefs_pvals_net_perc[["coef_year"]],
    b_interaction = coefs_pvals_net_perc[["coef_year_clim"]],
    b_moderator = coefs_pvals_net_perc[["coef_clim"]],
    df = data_mod,
    moderator_level = 1,
    colours_palletes = colours_palletes_clim,
    tag = "A)",
    xlab = expression(Year),
    ylab = expression(Net~carbon~sink~(relative)),
    colorlab = "Cold-wet -> Warm-dry",
    xlim = 1998,
    ylim = -0.05,
    pval_pred = coefs_pvals_net_perc[["pval_year"]],
    pval_int = coefs_pvals_net_perc[["pval_year_clim"]])

p5

########### gains_perc ###########
coefs_pvals_gains_perc <- get_coefs_pvals_clim(averaged_model_gains_perc)

p6 <- interaction_plot(y = "gains_perc", ylog = TRUE,
    pred = "year",
    moderator = "warm_dry_climate",
    b0 = coefs_pvals_gains_perc[["coefb0"]],
    b_pred = coefs_pvals_gains_perc[["coef_year"]],
    b_interaction = coefs_pvals_gains_perc[["coef_year_clim"]],
    b_moderator = coefs_pvals_gains_perc[["coef_clim"]],
    df = data_mod,
    moderator_level = 1,
    colours_palletes = colours_palletes_clim,
    tag = "B)",
    xlab = expression(Year),
    ylab = expression(Carbon~gains~(relative)),
    colorlab = "Cold-wet -> Warm-dry",
    xlim = 1998,
    ylim = 0.01,
    pval_pred = coefs_pvals_gains_perc[["pval_year"]],
    pval_int = coefs_pvals_gains_perc[["pval_year_clim"]])

p6


########### losses_perc ###########
coefs_pvals_losses_perc <- get_coefs_pvals_clim(averaged_model_losses_perc)

p7 <- interaction_plot(y = "losses_perc", ylog = TRUE,
    pred = "year",
    moderator = "warm_dry_climate",
    b0 = coefs_pvals_losses_perc[["coefb0"]],
    b_pred = coefs_pvals_losses_perc[["coef_year"]],
    b_interaction = coefs_pvals_losses_perc[["coef_year_clim"]],
    b_moderator = coefs_pvals_losses_perc[["coef_clim"]],
    df = data_mod,
    moderator_level = 1,
    colours_palletes = colours_palletes_clim,
    tag = "C)",
    xlab = expression(Year),
    ylab = expression(Carbon~losses~(relative)),
    colorlab = "Cold-wet -> Warm-dry",
    xlim = 1998,
    ylim = 0.05,
    pval_pred = coefs_pvals_losses_perc[["pval_year"]],
    pval_int = coefs_pvals_losses_perc[["pval_year_clim"]])

p7

########### crt ###########
coefs_pvals_crt <- get_coefs_pvals_clim(averaged_model_crt)

p8 <- interaction_plot(y = "crt", ylog = TRUE,
    pred = "year",
    moderator = "warm_dry_climate",
    b0 = coefs_pvals_crt[["coefb0"]],
    b_pred = coefs_pvals_crt[["coef_year"]],
    b_interaction = coefs_pvals_crt[["coef_year_clim"]],
    b_moderator = coefs_pvals_crt[["coef_clim"]],
    df = data_mod,
    moderator_level = 1,
    colours_palletes = colours_palletes_clim,
    tag = "D)",
    xlab = expression(Year),
    ylab = expression(CRT~(years)),
    colorlab = "Cold-wet -> Warm-dry",
    xlim = 1998,
    ylim = 123,
    pval_pred = coefs_pvals_crt[["pval_year"]],
    pval_int = coefs_pvals_crt[["pval_year_clim"]])

p8

# Create grids

# Figure S13
allplotslist <- align_plots(p1, p2, p3, p4, align = "hv")

grid <- grid.arrange(allplotslist[[1]], allplotslist[[2]], 
                     allplotslist[[3]], allplotslist[[4]], 
                    nrow = 2) # , layout_matrix=rbind(c(1,1,2,2), c(NA, 3, 3, NA))

ggsave("figureS13.tiff", grid, he = 27, wi = 41, un = "cm", dpi = 400)
system("open figureS13.tiff")

# Figure S14
allplotslist1 <- align_plots(p5, p6, p7, p8,  align = "hv")

grid1 <- grid.arrange(allplotslist1[[1]], allplotslist1[[2]], 
                     allplotslist1[[3]], allplotslist1[[4]], nrow = 2)

ggsave("figureS14.tiff", grid1, he = 27, wi = 41, un = "cm", dpi = 400)
system("open figureS14.tiff")