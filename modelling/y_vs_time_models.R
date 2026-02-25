rm(list = ls())

if (!requireNamespace("GGally", quietly = TRUE)) {
  install.packages("remotes")
  remotes::install_version(
    "GGally",
    version = "2.1.2",
    repos = "https://cloud.r-project.org"
  )
}

# load packages
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(MuMIn)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(ncf)
library(caret)
library(DHARMa)
library(performance)
library(jtools)
library(interactions)
library(ggsci)
library(viridis)
library(voxel)
library(gridExtra)
library(itsadug)
library(cowplot)
library(spdep)
library(GGally)
library(grid)

options(na.action = na.fail)

source("./modelling/model_diag.R")

# read census interval level data
data_forests1 <- read.table("census_int_level.csv",
     header = TRUE, dec = ".", sep = ",")

data_forests <- mutate_if(data_forests1,
          is.character, as.factor) %>%
     mutate(gains_perc = c_gains / c_stocks_i,
     losses_perc = c_losses / c_stocks_i,
     net_perc = net_c_sink / c_stocks_i)

# compute the empirical weights
weig <- data_forests1$interval_length ^ (1 / 3) +
     data_forests1$sampled_area_ha ^ (1 / 4) - 1

weig_clim <- data_forests1$interval_length ^ (1 / 3)

# read census level data
data_forests1_str <- read.table("census_level.csv",
     header = TRUE, dec = ".", sep = ",") %>%
  rename(c_stocks = c_stocks_ha)

head(data_forests1_str)

data_forests_str <- mutate_if(data_forests1_str, is.character, as.factor)

# compute the empirical weights
weig_str <- data_forests_str$sampled_area_ha ^ (1 / 3)

# number of neighbours for SAC
k_n_str <- 30
k_n <- 20

##################### models #####################

######## climate as function of time ########

# MAP
m_globgam_map <- gam(log(map) ~ s(year) + s(plot_code, bs = "re"),
     weights = weig_clim,
     method = "REML", data = data_forests)

gam.check(m_globgam_map)
func_mult_diag(m_globgam_map, data_forests$long, data_forests$lat, k_n)
summary(m_globgam_map)

# CWD
m_globgam_cwd <- gam(cwd ~ s(year) + s(plot_code, bs = "re"),
     weights = weig_clim,
     method = "REML", data = data_forests)

gam.check(m_globgam_cwd)
func_mult_diag(m_globgam_cwd, data_forests$long, data_forests$lat, k_n)
summary(m_globgam_cwd)

# MAT
m_globgam_mat <- gam(log(mat) ~ s(year) + s(plot_code, bs = "re"),
     weights = weig_clim,
     method = "REML", data = data_forests)

gam.check(m_globgam_mat)
func_mult_diag(m_globgam_mat, data_forests$long, data_forests$lat, k_n)
summary(m_globgam_mat)

# Maximum temperature
m_globgam_max_temp <- gam(log(max_temp) ~ s(year) + s(plot_code, bs = "re"),
     weights = weig_clim,
     method = "REML", data = data_forests)

gam.check(m_globgam_max_temp)
func_mult_diag(m_globgam_max_temp, data_forests$long, data_forests$lat, k_n)
summary(m_globgam_max_temp)

# # figure S25
# tiff("figureS25.tiff", units="cm", width=20, height=20, res=600)
# par(mfrow=c(2, 2))

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_map, residuals=TRUE, pch=16, cex=0.5, cex.lab = 1.07, col="black",lwd=2, 
#      seWithMean=TRUE, shade=TRUE,
#      shift=coef(m_globgam_map)[1], rug=FALSE, select=1, shade.col="lightsalmon", bty="l", yaxt="n", 
#      trans=exp,
#      ylab=expression(MAP~(mm)),xlab="Year")
# axis(2,las=2)
# mtext('A)', side=3, line=1, at=min(data_forests$year-1), cex=1.1) #at the first value of x
# box(col = 'black')

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_cwd, residuals=TRUE, pch=16, cex=0.5, cex.lab=1.07, col="black", lwd=2, 
#      seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_cwd)[1],rug=FALSE, select=1, shade.col="lightsalmon", bty="l", yaxt="n",
#      ylab=expression(CWD~(mm)), xlab="Year")
# axis(2,las=2)
# mtext('B)', side=3, line=1, at=min(data_forests$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_mat, residuals=TRUE, pch=16, cex=0.5, cex.lab=1.07, col="black", lwd=2, 
#      seWithMean=TRUE, shade=TRUE,
#      shift=coef(m_globgam_mat)[1], rug=FALSE, select=1,shade.col="coral",bty="l", yaxt="n", trans=exp,
#      ylab=expression(MAT~(ºC)),xlab="Year")
# axis(2,las=2)
# mtext('C)', side=3, line=1, at=min(data_forests$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_max_temp, residuals=TRUE, pch=16, cex=0.5, cex.lab=1.07,col="black", lwd=2, 
#      seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_max_temp)[1], rug=FALSE, select=1, shade.col="coral", bty="l", yaxt="n", 
#      trans=exp,
#      ylab=expression(Maximum~temperature~(ºC)), xlab="Year")
# axis(2,las=2)
# mtext('D)', side=3, line=1, at=min(data_forests$year-1), cex=1.1) #at the first value of x
# box(col = 'black')

# dev.off()
# system("open figureS25.tiff")


# ============================
# Figure S25 — Vector, Helvetica, accessible
# ============================

# # Open vector PDF device with Helvetica
# pdf(
#   "figureS25.pdf",
#   width  = 20 / 2.54,   # cm → inches
#   height = 20 / 2.54,
#   family = "Helvetica",
#   useDingbats = FALSE
# )

# # Force Helvetica in base graphics
# par(
#   family = "Helvetica",
#   mfrow = c(2, 2)
# )

# # Common shading color (color-blind safe, no pink)
# shade_col <- "#4C72B0"  # muted blue

# # ============================
# # Panel A
# # ============================

# par(mar = c(5, 6, 4, 1) + .1)
# plot(
#   m_globgam_map,
#   residuals = TRUE,
#   pch = 16,
#   cex = 0.5,
#   cex.lab = 1.07,
#   col = "black",
#   lwd = 2,
#   seWithMean = TRUE,
#   shade = TRUE,
#   shift = coef(m_globgam_map)[1],
#   rug = FALSE,
#   select = 1,
#   shade.col = shade_col,
#   bty = "l",
#   yaxt = "n",
#   trans = exp,
#   ylab = expression(MAP~(mm)),
#   xlab = "Year"
# )
# axis(2, las = 2)
# mtext("A)", side = 3, line = 1, at = min(data_forests$year - 1), cex = 1.1)
# box(col = "black")

# # ============================
# # Panel B
# # ============================

# par(mar = c(5, 6, 4, 1) + .1)
# plot(
#   m_globgam_cwd,
#   residuals = TRUE,
#   pch = 16,
#   cex = 0.5,
#   cex.lab = 1.07,
#   col = "black",
#   lwd = 2,
#   seWithMean = TRUE,
#   shade = TRUE,
#   shift = coef(m_globgam_cwd)[1],
#   rug = FALSE,
#   select = 1,
#   shade.col = shade_col,
#   bty = "l",
#   yaxt = "n",
#   ylab = expression(CWD~(mm)),
#   xlab = "Year"
# )
# axis(2, las = 2)
# mtext("B)", side = 3, line = 1, at = min(data_forests$year - 1), cex = 1.1)
# box(col = "black")

# # ============================
# # Panel C
# # ============================

# par(mar = c(5, 6, 4, 1) + .1)
# plot(
#   m_globgam_mat,
#   residuals = TRUE,
#   pch = 16,
#   cex = 0.5,
#   cex.lab = 1.07,
#   col = "black",
#   lwd = 2,
#   seWithMean = TRUE,
#   shade = TRUE,
#   shift = coef(m_globgam_mat)[1],
#   rug = FALSE,
#   select = 1,
#   shade.col = shade_col,
#   bty = "l",
#   yaxt = "n",
#   trans = exp,
#   ylab = expression(MAT~(degree*C)),
#   xlab = "Year"
# )
# axis(2, las = 2)
# mtext("C)", side = 3, line = 1, at = min(data_forests$year - 1), cex = 1.1)
# box(col = "black")

# # ============================
# # Panel D
# # ============================

# par(mar = c(5, 6, 4, 1) + .1)
# plot(
#   m_globgam_max_temp,
#   residuals = TRUE,
#   pch = 16,
#   cex = 0.5,
#   cex.lab = 1.07,
#   col = "black",
#   lwd = 2,
#   seWithMean = TRUE,
#   shade = TRUE,
#   shift = coef(m_globgam_max_temp)[1],
#   rug = FALSE,
#   select = 1,
#   shade.col = shade_col,
#   bty = "l",
#   yaxt = "n",
#   trans = exp,
#   ylab = expression(Maximum~temperature~(degree*C)),
#   xlab = "Year"
# )
# axis(2, las = 2)
# mtext("D)", side = 3, line = 1, at = min(data_forests$year - 1), cex = 1.1)
# box(col = "black")

# # ============================
# # Close device
# # ============================

# dev.off()


######## Diversity variables as function of time ########

# Species richness Hill number
m_globgam_richness <- gam(log(richness_hill) ~ s(year) +
     s(plot_code, bs = "re"),
     weights = weig_str,
     method = "REML", data = data_forests_str)

gam.check(m_globgam_richness)
func_mult_diag(m_globgam_richness,
     data_forests_str$long, data_forests_str$lat, k_n_str)
summary(m_globgam_richness)

# Shannon diversity Hill number
m_globgam_shannon <- gam(log(shannon_hill) ~ s(year) +
     s(plot_code, bs = "re"),
     weights = weig_str,
     method = "REML", data = data_forests_str)

gam.check(m_globgam_shannon)
func_mult_diag(m_globgam_shannon,
     data_forests_str$long, data_forests_str$lat, k_n_str)
summary(m_globgam_shannon)

# sPD
m_globgam_sespd <- gam(sespd ~ s(year) + s(plot_code, bs = "re"),
     weights = weig_str,
     method = "REML", data = data_forests_str)

gam.check(m_globgam_sespd)
func_mult_diag(m_globgam_sespd,
     data_forests_str$long, data_forests_str$lat, k_n_str)
summary(m_globgam_sespd)

# sMPD
m_globgam_sesmpd <- gam(sesmpd ~ s(year) + s(plot_code, bs = "re"),
     weights = weig_str,
     method = "REML", data = data_forests_str)

gam.check(m_globgam_sesmpd)
func_mult_diag(m_globgam_sesmpd,
     data_forests_str$long, data_forests_str$lat, k_n_str)
summary(m_globgam_sesmpd)

# sMNTD
m_globgam_sesmntd <- gam(sesmntd ~ s(year) + s(plot_code, bs = "re"),
     weights = weig_str,
     method = "REML", data = data_forests_str)

gam.check(m_globgam_sesmntd)
func_mult_diag(m_globgam_sesmntd,
     data_forests_str$long, data_forests_str$lat, k_n_str)
summary(m_globgam_sesmntd)

# # Figure 4
# tiff("figure4.tiff", units="cm", width=21, height=10, res=600)

# # par(mfrow=c(3,2))
# par(mfrow=c(1,2))

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_richness,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_richness)[1],rug=FALSE,select=1,shade.col="darkolivegreen2",bty="l",yaxt="n",trans=exp,
#      ylab=expression(Species~richness~(ENS)),xlab="Year")
# axis(2,las=2)
# mtext('A)', side=3, line=1, at=min(data_forests_str$year-1),cex=1.1) #at the first value of x
# box(col = 'black')


# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_sesmpd,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_sesmpd)[1],rug=FALSE,select=1,shade.col="darkolivegreen2",bty="l",yaxt="n",
#      ylab=expression(P.~diversity~(sMPD)),xlab="Year")
# axis(2,las=2)
# mtext('D)', side=3, line=1, at=min(data_forests_str$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_sesmntd,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=FALSE,
#      shift=coef(m_globgam_sesmntd)[1],rug=FALSE,select=1,shade.col="darkolivegreen2",bty="l",yaxt="n",
#      ylab=expression(P.~diversity~(sMNTD)),xlab="Year")
# axis(2,las=2)
# mtext('E)', side=3, line=1, at=min(data_forests_str$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# dev.off()
# system("open figure4.tiff")

######## Functional variables ########

# SLA CWM
m_globgam_cwm_sla <- gam(log(cwm_sla) ~ s(year) + s(plot_code, bs = "re"),
     weights = weig_str,
     method = "REML", data = data_forests_str)

gam.check(m_globgam_cwm_sla)
func_mult_diag(m_globgam_cwm_sla,
     data_forests_str$long, data_forests_str$lat, k_n_str)
summary(m_globgam_cwm_sla)

# SLA dispersion
m_globgam_sla_sd <- gam(log(sla_sd) ~ s(year) + s(plot_code, bs = "re"),
     weights = weig_str,
     method = "REML", data = data_forests_str)

gam.check(m_globgam_sla_sd)
func_mult_diag(m_globgam_sla_sd,
     data_forests_str$long, data_forests_str$lat, k_n_str)
summary(m_globgam_sla_sd)

# WD CWM
m_globgam_cwm_wd <- gam(log(cwm_wd) ~ s(year) + s(plot_code, bs = "re"),
     weights = weig_str,
     method = "REML", data = data_forests_str)

gam.check(m_globgam_cwm_wd)
func_mult_diag(m_globgam_cwm_wd,
     data_forests_str$long, data_forests_str$lat, k_n_str)
summary(m_globgam_cwm_wd)

# wd dispersion
m_globgam_wd_sd <- gam(log(wd_sd) ~ s(year) + s(plot_code, bs = "re"),
     weights = weig_str,
     method = "REML", data = data_forests_str)

gam.check(m_globgam_wd_sd)
func_mult_diag(m_globgam_wd_sd,
     data_forests_str$long, data_forests_str$lat, k_n_str)
summary(m_globgam_wd_sd)

# # Figure 4
# tiff("figure4.tiff", units="cm", width=17, height=23, res=600)

# par(mfrow=c(3,2))

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_shannon,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_shannon)[1],rug=FALSE,select=1,shade.col="darkolivegreen2",bty="l",yaxt="n",trans=exp,
#      ylab=expression(Taxonomic~diversity),xlab="Year")
# axis(2,las=2)
# mtext('A)', side=3, line=1, at=min(data_forests_str$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_sespd,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_sespd)[1],rug=FALSE,select=1,shade.col="darkolivegreen2",bty="l",yaxt="n",
#      ylab=expression(Phylogenetic~diversity),xlab="Year")
# axis(2,las=2)
# mtext('B)', side=3, line=1, at=min(data_forests_str$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_cwm_sla,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_cwm_sla)[1],rug=FALSE,select=1,shade.col="seagreen3",bty="l",yaxt="n",trans=exp,
#      ylab=expression(SLA~(mm^2~mg^-1)),xlab="Year")
# axis(2, las=2 )
# mtext('C)', side=3, line=1, at=min(data_forests_str$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_sla_sd,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_sla_sd)[1],rug=FALSE,select=1,shade.col="seagreen3",bty="l",yaxt="n",trans=exp,
#      ylab=expression(SLA~dispersion),xlab="Year")
# axis(2, las=2 )
# mtext('D)', side=3, line=1, at=min(data_forests_str$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_cwm_wd,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_cwm_wd)[1],rug=FALSE,select=1,shade.col="lightsalmon3",bty="l",yaxt="n",trans=exp,
#      ylab=expression(WD~(g~cm^-3)),xlab="Year")
# axis(2, las=2 )
# mtext('E)', side=3, line=1, at=min(data_forests_str$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_wd_sd,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_wd_sd)[1],rug=FALSE,select=1,shade.col="lightsalmon3",bty="l",yaxt="n",trans=exp,
#      ylab=expression(WD~dispersion),xlab="Year")
# axis(2, las=2 )
# mtext('F)', side=3, line=1, at=min(data_forests_str$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# dev.off()
# system("open figure4.tiff")



# ============================
# Figure 4 — Vector, Helvetica
# ============================

# Open vector PDF device with Helvetica
pdf(
  "figure4.pdf",
  width  = 17 / 2.54,   # cm → inches
  height = 23 / 2.54,
  family = "Helvetica",
  useDingbats = FALSE
)

# Force Helvetica in base graphics
par(
  family = "Helvetica",
  mfrow = c(3, 2)
)

# ============================
# Panel A
# ============================

par(mar = c(5, 6, 4, 1) + .1)
plot(
  m_globgam_shannon,
  residuals = TRUE,
  pch = 16,
  cex = 0.5,
  cex.lab = 1.07,
  col = "black",
  lwd = 2,
  seWithMean = TRUE,
  shade = TRUE,
  shift = coef(m_globgam_shannon)[1],
  rug = FALSE,
  select = 1,
  shade.col = "darkolivegreen2",
  bty = "l",
  yaxt = "n",
  trans = exp,
  ylab = expression(Taxonomic~diversity),
  xlab = "Year"
)
axis(2, las = 2)
mtext("A)", side = 3, line = 1, at = min(data_forests_str$year - 1), cex = 1.1)
box(col = "black")

# ============================
# Panel B
# ============================

par(mar = c(5, 6, 4, 1) + .1)
plot(
  m_globgam_sespd,
  residuals = TRUE,
  pch = 16,
  cex = 0.5,
  cex.lab = 1.07,
  col = "black",
  lwd = 2,
  seWithMean = TRUE,
  shade = TRUE,
  shift = coef(m_globgam_sespd)[1],
  rug = FALSE,
  select = 1,
  shade.col = "darkolivegreen2",
  bty = "l",
  yaxt = "n",
  ylab = expression(Phylogenetic~diversity),
  xlab = "Year"
)
axis(2, las = 2)
mtext("B)", side = 3, line = 1, at = min(data_forests_str$year - 1), cex = 1.1)
box(col = "black")

# ============================
# Panel C
# ============================

par(mar = c(5, 6, 4, 1) + .1)
plot(
  m_globgam_cwm_sla,
  residuals = TRUE,
  pch = 16,
  cex = 0.5,
  cex.lab = 1.07,
  col = "black",
  lwd = 2,
  seWithMean = TRUE,
  shade = TRUE,
  shift = coef(m_globgam_cwm_sla)[1],
  rug = FALSE,
  select = 1,
  shade.col = "seagreen3",
  bty = "l",
  yaxt = "n",
  trans = exp,
  ylab = expression(SLA~(mm^2~mg^-1)),
  xlab = "Year"
)
axis(2, las = 2)
mtext("C)", side = 3, line = 1, at = min(data_forests_str$year - 1), cex = 1.1)
box(col = "black")

# ============================
# Panel D
# ============================

par(mar = c(5, 6, 4, 1) + .1)
plot(
  m_globgam_sla_sd,
  residuals = TRUE,
  pch = 16,
  cex = 0.5,
  cex.lab = 1.07,
  col = "black",
  lwd = 2,
  seWithMean = TRUE,
  shade = TRUE,
  shift = coef(m_globgam_sla_sd)[1],
  rug = FALSE,
  select = 1,
  shade.col = "seagreen3",
  bty = "l",
  yaxt = "n",
  trans = exp,
  ylab = expression(SLA~dispersion),
  xlab = "Year"
)
axis(2, las = 2)
mtext("D)", side = 3, line = 1, at = min(data_forests_str$year - 1), cex = 1.1)
box(col = "black")

# ============================
# Panel E
# ============================

par(mar = c(5, 6, 4, 1) + .1)
plot(
  m_globgam_cwm_wd,
  residuals = TRUE,
  pch = 16,
  cex = 0.5,
  cex.lab = 1.07,
  col = "black",
  lwd = 2,
  seWithMean = TRUE,
  shade = TRUE,
  shift = coef(m_globgam_cwm_wd)[1],
  rug = FALSE,
  select = 1,
  shade.col = "lightsalmon3",
  bty = "l",
  yaxt = "n",
  trans = exp,
  ylab = expression(WD~(g~cm^-3)),
  xlab = "Year"
)
axis(2, las = 2)
mtext("E)", side = 3, line = 1, at = min(data_forests_str$year - 1), cex = 1.1)
box(col = "black")

# ============================
# Panel F
# ============================

par(mar = c(5, 6, 4, 1) + .1)
plot(
  m_globgam_wd_sd,
  residuals = TRUE,
  pch = 16,
  cex = 0.5,
  cex.lab = 1.07,
  col = "black",
  lwd = 2,
  seWithMean = TRUE,
  shade = TRUE,
  shift = coef(m_globgam_wd_sd)[1],
  rug = FALSE,
  select = 1,
  shade.col = "lightsalmon3",
  bty = "l",
  yaxt = "n",
  trans = exp,
  ylab = expression(WD~dispersion),
  xlab = "Year"
)
axis(2, las = 2)
mtext("F)", side = 3, line = 1, at = min(data_forests_str$year - 1), cex = 1.1)
box(col = "black")

# ============================
# Close device
# ============================

dev.off()


######## Carbon variables as function of time ########
  
# correlation between carbib dynamics variables
cor_responses <- data_forests %>% 
  mutate(c_stocks = (c_stocks_i + c_stocks_f) / 2) %>%
     select(c_stocks, net_c_sink, net_perc, c_gains, gains_perc, c_losses, losses_perc, crt) 
cor_responses

names(cor_responses) <- c("C stocks", "Net C sink", "Net C sink %", "C gains", "C gains %", "C losses", "C losses %", "CRT")

# figure S4
corplot1 <- ggpairs(cor_responses,diag = list(continuous = wrap("densityDiag", fill = "palegreen1")),
                   lower = list(continuous = wrap("points", color = "red", alpha = 0.5))) + theme_bw()    

ggsave("figureS4.tiff",corplot1, he = 22, wi = 26, un = "cm", dpi = 600)
system("open figureS4.tiff")

# Carbon stocks
m_globgam_c_stocks_f <- gam(log(c_stocks) ~ s(year) + s(plot_code, bs = "re"), 
                           weights = weig_str, method="REML", data = data_forests_str)

gam.check(m_globgam_c_stocks_f)
func_mult_diag(m_globgam_c_stocks_f, data_forests_str$long, data_forests_str$lat, k_n_str)
summary(m_globgam_c_stocks_f)

# Net carbon sink
m_globgam_net_c_sink <- gam(net_c_sink ~ s(year, k=30) + 
                             s(plot_code, bs="re"), 
                             weights = weig, method = "REML", data = data_forests)

gam.check(m_globgam_net_c_sink)
func_mult_diag(m_globgam_net_c_sink, data_forests$long, data_forests$lat, k_n)
summary(m_globgam_net_c_sink)

# Relative net carbon sink
m_globgam_net_perc <- gam(net_perc ~ s(year) +
     s(plot_code, bs = "re"),
     weights = weig, method = "REML", data = data_forests)

gam.check(m_globgam_net_perc)
func_mult_diag(m_globgam_net_perc, data_forests$long, data_forests$lat, k_n)
summary(m_globgam_net_perc)

# Carbon gains
m_globgam_c_gains <- gam(log(c_gains) ~ s(year, k = 7) +
     s(plot_code, bs = "re"),
     weights = weig, method = "REML", data = data_forests)

gam.check(m_globgam_c_gains)
func_mult_diag(m_globgam_c_gains, data_forests$long, data_forests$lat, k_n)
summary(m_globgam_c_gains)

# Relative carbon gains
m_globgam_gains_perc <- gam(log(gains_perc) ~ s(year, k = 8) +
     s(plot_code, bs = "re"), 
     weights = weig, method = "REML", data = data_forests)

gam.check(m_globgam_gains_perc)
func_mult_diag(m_globgam_gains_perc, data_forests$long, data_forests$lat, k_n)
summary(m_globgam_gains_perc)

# Carbon losses
m_globgam_c_losses <- gam(log(c_losses) ~ s(year) +
     s(plot_code, bs = "re"), 
     weights = weig, method = "REML", data = data_forests)

gam.check(m_globgam_c_losses)
func_mult_diag(m_globgam_c_losses, 
     data_forests$long, data_forests$lat, k_n)
summary(m_globgam_c_losses)

# Relative carbon losses
m_globgam_losses_perc <- gam(log(losses_perc) ~ s(year) +
     s(plot_code, bs = "re"),
     weights = weig, method = "REML", data = data_forests)

gam.check(m_globgam_losses_perc)
func_mult_diag(m_globgam_losses_perc, 
     data_forests$long, data_forests$lat, k_n)
summary(m_globgam_losses_perc)

# CRT
m_globgam_crt <- gam(log(crt) ~ s(year) +
     s(plot_code, bs = "re"),
     weights = weig, method = "REML", data = data_forests)

gam.check(m_globgam_crt)
func_mult_diag(m_globgam_crt, 
     data_forests$long, data_forests$lat, k_n)
summary(m_globgam_crt)

#### plots ####

# # Figure 2
# shade_col = "orange"

# tiff("figure2.tiff", units="cm", width=19, height=16, res=600)

# par(mfrow=c(2,2))

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_c_stocks_f,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_c_stocks_f)[1],rug=FALSE,select=1,shade.col=shade_col,bty="l",yaxt="n",trans=exp,
#      ylab=expression(Carbon~stocks~(Mg~C~ha^-1)),xlab="Year")
# axis(2, las=2 )
# mtext('A)', side=3, line=1, at=min(data_forests_str$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_net_c_sink,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_net_c_sink)[1],rug=FALSE,select=1,shade.col=shade_col,bty="l",yaxt="n",
#      ylab=expression(Net~carbon~sink~(Mg~C~ha^-1~yr^-1)),xlab="Year")
# axis(2, las=2 )
# abline(h=0, col="red", lty=2)
# mtext('B)', side=3, line=1, at=min(data_forests$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_c_gains,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_c_gains)[1],rug=FALSE,select=1,shade.col=shade_col,bty="l",yaxt="n",trans=exp,
#      ylab=expression(Carbon~gains~(Mg~C~ha^-1~yr^-1)),xlab="Year")
# axis(2, las=2 )
# mtext('C)', side=3, line=1, at=min(data_forests$year-1),cex=1.1) #at the first value of x]
# box(col = 'black')

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_c_losses,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_c_losses)[1],rug=FALSE,select=1,shade.col=shade_col,bty="l",yaxt="n",trans=exp,
#      ylab=expression(Carbon~losses~(Mg~C~ha^-1~yr^-1)),xlab="Year")
# axis(2, las=2 )
# mtext('D)', side=3, line=1, at=min(data_forests$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# dev.off()
# system("open figure2.tiff")


# # Figure 3
# shade_col = "salmon1"

# tiff("figure3.tiff", units="cm", width=19, height=16, res=600)

# par(mfrow=c(2,2))

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_net_perc,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_net_perc)[1],rug=FALSE,select=1,shade.col=shade_col,bty="l",yaxt="n",
#      ylab=expression(Net~carbon~sink~("relative")),xlab="Year")
# axis(2, las=2 )
# abline(h=0, col="red", lty=2)
# mtext('A)', side=3, line=1, at=min(data_forests$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_gains_perc,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_gains_perc)[1],rug=FALSE,select=1,shade.col=shade_col,bty="l",yaxt="n",trans=exp,
#      ylab=expression(Carbon~gains~("relative")),xlab="Year")
# axis(2, las=2 )
# mtext('B)', side=3, line=1, at=min(data_forests$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_losses_perc,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_losses_perc)[1],rug=FALSE,select=1,shade.col=shade_col,bty="l",yaxt="n",trans=exp,
#      ylab=expression(Carbon~losses~("relative")),xlab="Year")
# axis(2, las=2 )
# mtext('C)', side=3, line=1, at=min(data_forests$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# par(mar=c(5,6,4,1)+.1)
# plot(m_globgam_crt,residuals=TRUE,pch=16,cex=0.5,cex.lab=1.07,col="black",lwd=2,seWithMean=TRUE,shade=TRUE,
#      shift=coef(m_globgam_crt)[1],rug=FALSE,select=1,shade.col=shade_col,bty="l",yaxt="n",trans=exp,
#      ylab=expression(CRT~(years)),xlab="Year")
# axis(2, las=2 )
# mtext('D)', side=3, line=1, at=min(data_forests$year-1),cex=1.1) #at the first value of x
# box(col = 'black')

# dev.off()
# system("open figure3.tiff")



# Ensure Helvetica everywhere
par(family = "Helvetica")

# ============================
# Figure 2
# ============================

shade_col <- "orange"

pdf(
  "figure2.pdf",
  width = 19 / 2.54,
  height = 16 / 2.54,
  family = "Helvetica",
  useDingbats = FALSE
)

par(mfrow = c(2, 2))

par(mar = c(5, 6, 4, 1) + .1)
plot(
  m_globgam_c_stocks_f,
  residuals = TRUE, pch = 16, cex = 0.5, cex.lab = 1.07,
  col = "black", lwd = 2, seWithMean = TRUE, shade = TRUE,
  shift = coef(m_globgam_c_stocks_f)[1], rug = FALSE, select = 1,
  shade.col = shade_col, bty = "l", yaxt = "n", trans = exp,
  ylab = expression(Carbon~stocks~(Mg~C~ha^-1)), xlab = "Year"
)
axis(2, las = 2)
mtext("A)", side = 3, line = 1, at = min(data_forests_str$year - 1), cex = 1.1)
box(col = "black")

par(mar = c(5, 6, 4, 1) + .1)
plot(
  m_globgam_net_c_sink,
  residuals = TRUE, pch = 16, cex = 0.5, cex.lab = 1.07,
  col = "black", lwd = 2, seWithMean = TRUE, shade = TRUE,
  shift = coef(m_globgam_net_c_sink)[1], rug = FALSE, select = 1,
  shade.col = shade_col, bty = "l", yaxt = "n",
  ylab = expression(Net~carbon~sink~(Mg~C~ha^-1~yr^-1)), xlab = "Year"
)
axis(2, las = 2)
abline(h = 0, col = "red", lty = 2)
mtext("B)", side = 3, line = 1, at = min(data_forests$year - 1), cex = 1.1)
box(col = "black")

par(mar = c(5, 6, 4, 1) + .1)
plot(
  m_globgam_c_gains,
  residuals = TRUE, pch = 16, cex = 0.5, cex.lab = 1.07,
  col = "black", lwd = 2, seWithMean = TRUE, shade = TRUE,
  shift = coef(m_globgam_c_gains)[1], rug = FALSE, select = 1,
  shade.col = shade_col, bty = "l", yaxt = "n", trans = exp,
  ylab = expression(Carbon~gains~(Mg~C~ha^-1~yr^-1)), xlab = "Year"
)
axis(2, las = 2)
mtext("C)", side = 3, line = 1, at = min(data_forests$year - 1), cex = 1.1)
box(col = "black")

par(mar = c(5, 6, 4, 1) + .1)
plot(
  m_globgam_c_losses,
  residuals = TRUE, pch = 16, cex = 0.5, cex.lab = 1.07,
  col = "black", lwd = 2, seWithMean = TRUE, shade = TRUE,
  shift = coef(m_globgam_c_losses)[1], rug = FALSE, select = 1,
  shade.col = shade_col, bty = "l", yaxt = "n", trans = exp,
  ylab = expression(Carbon~losses~(Mg~C~ha^-1~yr^-1)), xlab = "Year"
)
axis(2, las = 2)
mtext("D)", side = 3, line = 1, at = min(data_forests$year - 1), cex = 1.1)
box(col = "black")

dev.off()

# ============================
# Figure 3
# ============================

shade_col <- "salmon1"

pdf(
  "figure3.pdf",
  width = 19 / 2.54,
  height = 16 / 2.54,
  family = "Helvetica",
  useDingbats = FALSE
)

par(mfrow = c(2, 2))

par(mar = c(5, 6, 4, 1) + .1)
plot(
  m_globgam_net_perc,
  residuals = TRUE, pch = 16, cex = 0.5, cex.lab = 1.07,
  col = "black", lwd = 2, seWithMean = TRUE, shade = TRUE,
  shift = coef(m_globgam_net_perc)[1], rug = FALSE, select = 1,
  shade.col = shade_col, bty = "l", yaxt = "n",
  ylab = expression(Net~carbon~sink~("relative")), xlab = "Year"
)
axis(2, las = 2)
abline(h = 0, col = "red", lty = 2)
mtext("A)", side = 3, line = 1, at = min(data_forests$year - 1), cex = 1.1)
box(col = "black")

par(mar = c(5, 6, 4, 1) + .1)
plot(
  m_globgam_gains_perc,
  residuals = TRUE, pch = 16, cex = 0.5, cex.lab = 1.07,
  col = "black", lwd = 2, seWithMean = TRUE, shade = TRUE,
  shift = coef(m_globgam_gains_perc)[1], rug = FALSE, select = 1,
  shade.col = shade_col, bty = "l", yaxt = "n", trans = exp,
  ylab = expression(Carbon~gains~("relative")), xlab = "Year"
)
axis(2, las = 2)
mtext("B)", side = 3, line = 1, at = min(data_forests$year - 1), cex = 1.1)
box(col = "black")

par(mar = c(5, 6, 4, 1) + .1)
plot(
  m_globgam_losses_perc,
  residuals = TRUE, pch = 16, cex = 0.5, cex.lab = 1.07,
  col = "black", lwd = 2, seWithMean = TRUE, shade = TRUE,
  shift = coef(m_globgam_losses_perc)[1], rug = FALSE, select = 1,
  shade.col = shade_col, bty = "l", yaxt = "n", trans = exp,
  ylab = expression(Carbon~losses~("relative")), xlab = "Year"
)
axis(2, las = 2)
mtext("C)", side = 3, line = 1, at = min(data_forests$year - 1), cex = 1.1)
box(col = "black")

par(mar = c(5, 6, 4, 1) + .1)
plot(
  m_globgam_crt,
  residuals = TRUE, pch = 16, cex = 0.5, cex.lab = 1.07,
  col = "black", lwd = 2, seWithMean = TRUE, shade = TRUE,
  shift = coef(m_globgam_crt)[1], rug = FALSE, select = 1,
  shade.col = shade_col, bty = "l", yaxt = "n", trans = exp,
  ylab = expression(CRT~(years)), xlab = "Year"
)
axis(2, las = 2)
mtext("D)", side = 3, line = 1, at = min(data_forests$year - 1), cex = 1.1)
box(col = "black")

dev.off()

############### Table 1 ###############

# create the new data
new_data <- cbind.data.frame(year = c(2010, 2020))

pred_c_stocks <- exp(predict(m_globgam_c_stocks_f, newdata = new_data, exclude = "s(plot_code)", 
                            newdata.guaranteed = TRUE))

pred_net_c_sink <- (predict(m_globgam_net_c_sink, newdata = new_data, exclude = "s(plot_code)", 
                           newdata.guaranteed = TRUE))

pred_net_perc <- (predict(m_globgam_net_perc, newdata = new_data, 
                         exclude = "s(plot_code)", newdata.guaranteed = TRUE))

pred_c_gains <- exp(predict(m_globgam_c_gains, newdata = new_data, 
                           exclude = "s(plot_code)", newdata.guaranteed = TRUE))

pred_c_gains_perc <- exp(predict(m_globgam_gains_perc, newdata = new_data, 
                                exclude = "s(plot_code)", newdata.guaranteed = TRUE))

pred_c_losses <- exp(predict(m_globgam_c_losses, newdata = new_data, 
                            exclude = "s(plot_code)", newdata.guaranteed = TRUE))

pred_losses_perc <- exp(predict(m_globgam_losses_perc, newdata = new_data, 
                               exclude = "s(plot_code)", newdata.guaranteed = TRUE))

pred_crt <- exp(predict(m_globgam_crt, newdata = new_data, exclude = "s(plot_code)", 
                            newdata.guaranteed = TRUE))

# combine the predictions
df <- t(cbind.data.frame(data.frame(pred_c_stocks),
                 data.frame(pred_net_c_sink),
                 data.frame(pred_net_perc),
                 data.frame(pred_c_gains),
                 data.frame(pred_c_gains_perc),
                 data.frame(pred_c_losses),
                 data.frame(pred_losses_perc), 
                 data.frame(pred_crt)))

names(df) <- c("2010", "2020")
df

# model tables

modeltab1 <- data.frame(summary(m_globgam_c_stocks_f)$s.table)[1, ] %>% mutate(Response = "Carbon stocks")

modeltab2 <- data.frame(summary(m_globgam_net_c_sink)$s.table)[1, ] %>% mutate(Response = "Net carbon sink")

modeltab3 <- data.frame(summary(m_globgam_net_perc)$s.table)[1, ] %>% mutate(Response = "Net carbon sink (relative)")

modeltab4 <- data.frame(summary(m_globgam_c_gains)$s.table)[1, ] %>% mutate(Response = "Carbon gains")

modeltab5 <- data.frame(summary(m_globgam_gains_perc)$s.table)[1, ] %>% mutate(Response = "Carbon gains (relative)")

modeltab6 <- data.frame(summary(m_globgam_c_losses)$s.table)[1, ] %>% mutate(Response = "Carbon losses")

modeltab7 <- data.frame(summary(m_globgam_losses_perc)$s.table)[1, ] %>% mutate(Response = "Carbon losses (relative)")

modeltab8 <- data.frame(summary(m_globgam_crt)$s.table)[1, ] %>% mutate(Response = "CRT")

modeltab9 <- data.frame(summary(m_globgam_map)$s.table)[1, ] %>% mutate(Response = "MAP")

modeltab10 <- data.frame(summary(m_globgam_cwd)$s.table)[1, ] %>% mutate(Response = "CWD")

modeltab11 <- data.frame(summary(m_globgam_mat)$s.table)[1, ] %>% mutate(Response = "MAT")

modeltab12 <- data.frame(summary(m_globgam_max_temp)$s.table)[1, ] %>% mutate(Response = "Max. temp")

modeltab13 <- data.frame(summary(m_globgam_shannon)$s.table)[1, ] %>% mutate(Response = "Taxonomic diversity (Shannon)")

modeltab14 <- data.frame(summary(m_globgam_sespd)$s.table)[1, ] %>% mutate(Response = "Phylogenetic diversity (PD)")

modeltab15 <- data.frame(summary(m_globgam_cwm_wd)$s.table)[1, ] %>% mutate(Response = "WD")

modeltab16 <- data.frame(summary(m_globgam_wd_sd)$s.table)[1, ] %>% mutate(Response = "WD dispersion")

modeltab17 <- data.frame(summary(m_globgam_cwm_sla)$s.table)[1, ] %>% mutate(Response = "SLA")

modeltab18 <- data.frame(summary(m_globgam_sla_sd)$s.table)[1, ] %>% mutate(Response = "SLA dispersion")

gam_model_tables <- rbind(modeltab1, modeltab2, modeltab3, modeltab4, modeltab5, modeltab6, modeltab7, modeltab8,
     modeltab9, modeltab10, modeltab11, modeltab12, modeltab13, modeltab14, modeltab15, modeltab16, modeltab17, modeltab18) %>%
  relocate(Response, .before = edf)

write.csv(gam_model_tables, "tableS1.csv" , row.names = FALSE)
