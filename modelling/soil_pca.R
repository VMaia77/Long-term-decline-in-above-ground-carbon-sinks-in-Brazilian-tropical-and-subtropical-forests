rm(list = ls()) # cleaning the environment

library(dplyr)
library(factoextra)
library(FactoMineR)
library(ggplot2)

# read data
data_forests <- read.table("census_level.csv",
    header = TRUE, dec = ".", sep = ",")

names(data_forests)

soil_data_ <- data_forests[,
    c("plot_code", "ph", "p", "k", "ca", "mg", "al", "sb")] %>%
    unique() %>%
    select(-plot_code)

soil_data <- cbind.data.frame(plot_code = data_forests$plot_code %>%
    unique(),
    as.data.frame(scale(soil_data_)))

row.names(soil_data) <- NULL

data_var <- soil_data[, -1]
str(data_var)

# pca
pcas <- FactoMineR::PCA(data_var, scale.unit = TRUE, graph = FALSE, ncp = 11)
pcas

# eigenvalues
factoextra::get_eig(pcas) %>% round(2)

# eigenvalues plot
factoextra::fviz_eig(pcas,
    addlabels = TRUE, ylim = c(0, 100), ggtheme = theme_classic())
ggsave("soil_pca_screeplot.tiff", he = 15, wi = 20, un = "cm", dpi = 300)
system("open soil_pca_screeplot.tiff")

# fviz_pca_var(pcas,
#              col.var = "contrib",
#              gradient.cols = c("#bb2e00", "#002bbb"),
#              repel = TRUE)
# ggsave("soil_pca_contributions.tiff", he = 15, wi = 20, un = "cm", dpi = 300)
# system("open soil_pca_contributions.tiff")

scores <- pcas$ind$coord %>% as.data.frame()
scores$plot_code <- soil_data$plot_code
write.csv(scores, "./outputs/soil_pca_scores.csv")

loadings <- as.data.frame(sweep(pcas$var$coord, 2,
    sqrt(pcas$eig[seq_len(ncol(pcas$var$coord)), 1]), FUN = "/"))

# selection
pcas$var$contrib %>%
  tibble::as_tibble() %>%
  dplyr::mutate(var = rownames(pcas$var$contrib)) %>%
  dplyr::select(var, Dim.1, Dim.2, Dim.3, Dim.4) %>%
  dplyr::arrange(desc(Dim.1))

# contributions
factoextra::fviz_contrib(pcas,
    choice = "var", axes = 1, ggtheme = theme_classic())
ggsave("soil_pca_contrib1.tiff", he = 15, wi = 20, un = "cm", dpi = 300)
system("open soil_pca_contrib1.tiff")

factoextra::fviz_contrib(pcas,
    choice = "var", axes = 2, ggtheme = theme_classic())
ggsave("soil_pca_contrib2.tiff", he = 15, wi = 20, un = "cm", dpi = 300)
system("open soil_pca_contrib2.tiff")

factoextra::fviz_contrib(pcas,
    choice = "var", axes = 3, ggtheme = theme_classic())
ggsave("soil_pca_contrib3.tiff", he = 15, wi = 20, un = "cm", dpi = 300)
system("open soil_pca_contrib3.tiff")

# biplot
fviz_pca_var(pcas, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             legend.title = "Contrib.")
ggsave("soil_pca_biplot.tiff", he = 15, wi = 20, un = "cm", dpi = 300)
system("open soil_pca_biplot.tiff")

# biplot
fviz_pca_var(pcas, col.var = "contrib", axes = c(1, 3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             legend.title = "Contrib.")
ggsave("soil_pca_biplot2.tiff", he = 15, wi = 20, un = "cm", dpi = 300)
system("open soil_pca_biplot2.tiff")
