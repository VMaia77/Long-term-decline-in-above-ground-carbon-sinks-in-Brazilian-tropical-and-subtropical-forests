library(Taxonstand)
library(tidyverse)
library(phytools)
library(geiger)
library(picante)

source("./utils/boot_raref.R")
source("./utils/ba_matrix.R")

# vegetation
vegetation_data <- read.table("vegetation_data.csv",
    header = TRUE, dec = ".", sep = ",")

# sp_list <- cbind.data.frame(splist = unique(vegetation_data$species))

# spp <- TPL(sp_list$splist)

# write.csv(spp, "./outputs/phylo_raw_output_tpl.csv") # Lista de spp. DIRETO do TPL

spp <- read.table("./outputs/phylo_raw_output_tpl.csv",
    header = TRUE, dec = ".", sep = ",")

spp_tpl <- spp %>%
    select(Taxon, Family, New.Genus, New.Species) %>%
    mutate(New.Taxon = str_c(New.Genus, "_", New.Species))

spp_tpl$species <- spp_tpl$Taxon
spp_tpl$Taxon <- gsub(" ", "_", spp_tpl$Taxon)

# origin tree from github
allotb <- read.tree("./phylogeny/ALLOTB_ori.tre")

# Podando a arvore para casar com a lista de especies
# 801 tips
tree_allotb <- drop.tip(allotb, which(!allotb$tip.label %in% spp_tpl$New.Taxon))

# Conferindo que spp. nao estao na arvore
unique_spp <- spp_tpl %>%
    select(New.Taxon) %>%
    distinct() %>%
    na.omit()

row.names(unique_spp) <- unique_spp$New.Taxon
missingtips <- name.check(tree_allotb, unique_spp)

# write.csv(missingtips$data_not_tree,
#   "./outputs/phylo_missing_tips.csv")

vegetation_data_live <- vegetation_data[vegetation_data$dbhq > 0, ]

# modify species names
vegetation_data_live <- left_join(vegetation_data_live,
    spp_tpl %>%
    select(species, New.Taxon), by = "species") %>%
    drop_na(New.Taxon) %>%
    mutate(species = New.Taxon) %>%
    filter(species %in% tree_allotb$tip.label)

commatrixab <- ba_matrix(vegetation_data_live) %>%
    remove_rownames() %>%
    column_to_rownames(var = "site_year")

sespd_allotb_ab <- ses.pd(commatrixab, tree_allotb,
    null.model = "taxa.labels", runs = 999)

sesmpd_allotb_ab <- ses.mpd(commatrixab, cophenetic(tree_allotb),
                            null.model = "taxa.labels",
                            abundance.weighted = TRUE, runs = 999)

sesmntd_allotb_ab <- ses.mntd(commatrixab, cophenetic(tree_allotb),
                            null.model = "taxa.labels",
                            abundance.weighted = TRUE, runs = 999)

write.table(sespd_allotb_ab, "./outputs/phylo_sespd.csv")
write.table(sesmpd_allotb_ab, "./outputs/phylo_sesmpd.csv")
write.table(sesmntd_allotb_ab, "./outputs/phylo_sesmntd.csv")

# all_spp <- unique(vegetation_data_live$New.Taxon)
# all_spp_in_tree <- all_spp[all_spp %in% tree_allotb$tip.label]

# censuses <- unique(vegetation_data$site_year)

# result_df <- cbind.data.frame()

# n_sim <- 100

# for (census in censuses) {

# 	df <- vegetation_data_live %>%
# 	  filter(site_year == census) %>%
# 	  filter(species %in% tree_allotb$tip.label)

# 	results_temp <- cbind.data.frame(pd = NULL, mpd = NULL, mntd = NULL)

# 	for (i in seq_len(n_sim)) {
# 		sampled_df <- individual_sampling(df, minimum_n = minimum_n)

# 		commatrixab <- ba_matrix(sampled_df) %>%
# 			remove_rownames() %>%
# 			column_to_rownames(var = "site_year")

# 		pd_ab <- pd(commatrixab, tree_allotb)
# 		mpd_ab <- mpd(commatrixab, cophenetic(tree_allotb),
# 			abundance.weighted = TRUE)
# 		mntd_ab <- mntd(commatrixab, cophenetic(tree_allotb),
# 			abundance.weighted = TRUE)

# 		results_temp[i, "pd"] <- pd_ab
# 		results_temp[i, "mpd"] <- mpd_ab
# 		results_temp[i, "mntd"] <- mntd_ab
# 	}
# 	result_df_temp <- cbind.data.frame(site_year = census,
# 		pd = mean(results_temp$pd),
# 		mpd = mean(results_temp$mpd), 
# 		mntd = mean(results_temp$mntd))

# 	result_df <- rbind(result_df, result_df_temp)
# }

# ses_pd_ab = ses.pd(commatrixab, tree_allotb, null.model = "taxa.labels", runs = 3)

#         miss = all_spp_in_tree[!all_spp_in_tree %in%  names(commatrixab)]
#         vector1 = rep(0, length(miss))
#         names(vector1) = miss
            
#         dfmiss = data.frame(as.list(vector1))
#         ccomm = cbind(commatrixab, dfmiss)
#         pd_ab <- pd(ccomm, tree_allotb)


# com_alp = ccomm %>% select(sort(names(.)))
# mpd_ab <- mpd(com_alp, cophenetic(tree_allotb), abundance.weighted = T)


# write.table(pd_ab, "ab_pd-tree-allotb_2022.csv")
# write.table(mpd_ab, "ab_mpd-tree-allotb_2022.csv")
# write.table(mntd_ab, "ab_mntd-tree-allotb_2022.csv")
