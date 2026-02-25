rm(list = ls()) # cleaning the environment

library(dplyr)
library(factoextra)
library(FactoMineR)
library(ggplot2)

# read data
data_forests <- read.table("census_int_level.csv",
    header = TRUE, dec = ".", sep = ",")

names(data_forests)

clim_data_ <- data_forests[,
    c("plot_code_interval", "mat", "max_temp", "map", "cwd")] %>%
    unique() %>%
    select(-plot_code_interval)

clim_data <- cbind.data.frame(plot_code_interval =
    data_forests$plot_code_interval %>%
    unique(),
    as.data.frame(scale(clim_data_)))

row.names(clim_data) <- NULL

data_var <- clim_data[, -1]
str(data_var)

# pca
pcas <- FactoMineR::PCA(data_var, scale.unit = TRUE, graph = FALSE, ncp = 11)
pcas

# eigenvalues
factoextra::get_eig(pcas) %>% round(2)

# eigenvalues plot
factoextra::fviz_eig(pcas,
    addlabels = TRUE, ylim = c(0, 100), ggtheme = theme_classic())
ggsave("clim_pca_screeplot_census_int.tiff",
    he = 15, wi = 20, un = "cm", dpi = 300)
system("open clim_pca_screeplot_census_int.tiff")

# fviz_pca_var(pcas,
#              col.var = "contrib",
#              gradient.cols = c("#bb2e00", "#002bbb"),
#              repel = TRUE)
# ggsave("clim_pca_contributions_census_int.tiff", he = 15, wi = 20, un = "cm", dpi = 300)
# system("open clim_pca_contributions_census_int.tiff")

scores <- pcas$ind$coord %>% as.data.frame()
scores$plot_code_interval <- clim_data$plot_code_interval
write.csv(scores, "./outputs/clim_pca_scores_census_int.csv")

loadings <- as.data.frame(sweep(pcas$var$coord, 2,
    sqrt(pcas$eig[seq_len(ncol(pcas$var$coord)), 1]), FUN = "/"))

# selection
pcas$var$contrib %>%
  tibble::as_tibble() %>%
  dplyr::mutate(var = rownames(pcas$var$contrib)) %>%
  dplyr::select(var, Dim.1, Dim.2, Dim.3) %>%
  dplyr::arrange(desc(Dim.1))

# contributions
factoextra::fviz_contrib(pcas,
    choice = "var", axes = 1, ggtheme = theme_classic())
ggsave("clim_pca_contrib1_census_int.tiff",
    he = 15, wi = 20, un = "cm", dpi = 300)
system("open clim_pca_contrib1_census_int.tiff")

factoextra::fviz_contrib(pcas,
    choice = "var", axes = 2, ggtheme = theme_classic())
ggsave("clim_pca_contrib2_census_int.tiff",
    he = 15, wi = 20, un = "cm", dpi = 300)
system("open clim_pca_contrib2_census_int.tiff")

factoextra::fviz_contrib(pcas,
    choice = "var", axes = 3, ggtheme = theme_classic())
ggsave("clim_pca_contrib3_census_int.tiff",
    he = 15, wi = 20, un = "cm", dpi = 300)
system("open clim_pca_contrib3_census_int.tiff")

# biplot
fviz_pca_var(pcas, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             legend.title = "Contrib.")
ggsave("clim_pca_biplot_census_int.tiff",
    he = 15, wi = 20, un = "cm", dpi = 300)
system("open clim_pca_biplot_census_int.tiff")