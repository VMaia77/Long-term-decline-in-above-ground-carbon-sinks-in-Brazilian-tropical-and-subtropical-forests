library(dplyr)
library(tidyr)

# vegetation
data_for_dynamics <- read.table("vegetation_data.csv",
    header = TRUE, dec = ".", sep = ",")


# Modify specific rows in ind_key
data_for_dynamics$ind_key[which(data_for_dynamics$ind_key == "Actinostemon verticillatus_sem n\xa7_SUB_03_SUB_03_8_")] <- 
    "Actinostemon verticillatus_sem_avoid_error_SUB_03_SUB_03_8_"

# Modify specific rows in ind_year_key
data_for_dynamics$ind_year_key[which(data_for_dynamics$ind_year_key == "Actinostemon verticillatus_sem n\xa7_SUB_03_SUB_03_8__2014.604")] <- 
    "Actinostemon verticillatus_sem_avoid_error_SUB_03_SUB_03_8__2014.604"

data_for_dynamics$ind_year_key[which(data_for_dynamics$ind_year_key == "Actinostemon verticillatus_sem n\xa7_SUB_03_SUB_03_8__2017.7535")] <- 
    "Actinostemon verticillatus_sem_avoid_error_SUB_03_SUB_03_8__2017.7535"

data_for_dynamics1 <- mutate_if(data_for_dynamics, is.character, as.factor)

############################################

data_dynamics_ind_level <- data_for_dynamics1 %>%
    group_by(ind_key) %>%
    arrange(ind_key, ind_year_key) %>%
    mutate(lag.value_agwb = lag(agwb),
        lag.value_dbhq = lag(dbhq),
        lag.value_year = lag(yr_round),
        lag.value_census_data = lag(census_data)) %>%
    ungroup()

# View(data_dynamics_ind_level[data_dynamics_ind_level$ind_key == "Actinostemon verticillatus_sem_avoid_error_SUB_03_SUB_03_8_", ])


## check for strange increases in dbhq

delta_dbhq <- data_dynamics_ind_level %>%
    mutate(delta_dbhq = dbhq - lag.value_dbhq) %>%
    arrange(delta_dbhq)

delta_dbhq1 <- delta_dbhq %>%
    filter(delta_dbhq < 0 & dbhq != 0) %>%
    arrange(delta_dbhq)

# View(delta_dbhq %>% arrange(desc(delta_dbhq) ))

### ok

#######
data_dynamics_ind_level[is.na(data_dynamics_ind_level$agwb), ]
data_dynamics_ind_level[data_dynamics_ind_level$agwb == 0, ]

# View(data_dynamics_ind_level[data_dynamics_ind_level$plot_code == "PEB_1", ])
# View(data_dynamics_ind_level[data_dynamics_ind_level$plot_code == "SUB_02", ])
# View(data_dynamics_ind_level[data_dynamics_ind_level$plot_code == "IBI_01", ])

#####################################

data_dynamics_ind_level2 <- data_dynamics_ind_level %>%
    group_by(site_year) %>%
    mutate(year_adyn = round(mean(yr_round, na.rm = TRUE)),
        lag.value_year_adyn = round(mean(lag.value_year, na.rm = TRUE)),
        census_data_adyn = mean(census_data, na.rm = TRUE),
        lag.value_census_data_adyn = mean(lag.value_census_data, na.rm = TRUE))

# View(data_dynamics_ind_level2)

# sum(data_dynamics_ind_level2$yr_round == data_dynamics_ind_level2$year_adyn) # correct

# View(data_dynamics_ind_level2[data_dynamics_ind_level2$plot_code == "MGT_08", ])

# View(data_dynamics_ind_level2[data_dynamics_ind_level2$plot_code == "PEB_1", ])

# View(data_dynamics_ind_level2[data_dynamics_ind_level2$plot_code == "SUB_02", ])

# data_dynamics_ind_level2_check <-
#     (data_dynamics_ind_level2[
#         data_dynamics_ind_level2$lag.value_year_adyn == "NaN", ])

# summary(data_dynamics_ind_level2_check$census_number)
# dim(data_dynamics_ind_level2_check)

# data_dynamics_ind_level2_check1 <-  # nolint
#     (data_dynamics_ind_level2[
#         data_dynamics_ind_level2$census_number == 1, ])

# summary(data_dynamics_ind_level2_check1$lag.value_year_adyn)
# dim(data_dynamics_ind_level2_check1)

# sum(data_dynamics_ind_level2_check1$ind_year_key ==
#     data_dynamics_ind_level2_check$ind_year_key)

########################################

data_dynamics_ind_level3 <- data_dynamics_ind_level2

data_dynamics_ind_level3$status <- NA
head(as.data.frame(data_dynamics_ind_level3))

data_dynamics_ind_level3$status[data_dynamics_ind_level3$agwb > 0 &
    (data_dynamics_ind_level3$census_number == 1 |
        data_dynamics_ind_level3$lag.value_agwb > 0)] <- "survival"

data_dynamics_ind_level3$status[data_dynamics_ind_level3$agwb > 0 &
    (is.na(data_dynamics_ind_level3$lag.value_agwb) &
        data_dynamics_ind_level3$census_number != 1)] <- "recruit"

data_dynamics_ind_level3$status[data_dynamics_ind_level3$agwb == 0 &
    data_dynamics_ind_level3$lag.value_agwb > 0] <- "dead"

# View(data_dynamics_ind_level3)

summary(as.factor(data_dynamics_ind_level3$status))

# View(data_dynamics_ind_level3[data_dynamics_ind_level3$plot_code == "MGT_08", ])
# View(data_dynamics_ind_level3[data_dynamics_ind_level3$plot_code == "PEB_1", ])
# View(data_dynamics_ind_level3[data_dynamics_ind_level3$plot_code == "SUB_02", ])
# View(data_dynamics_ind_level3[data_dynamics_ind_level3$plot_code == "IBI_01", ])

##################################

data_dynamics_ind_level4 <- as.data.frame(data_dynamics_ind_level3)

data_dynamics_ind_level5 <- data_dynamics_ind_level4 %>%
    mutate(growth_raw = ifelse(status != "recruit", agwb - lag.value_agwb, agwb), # nolint
        year_cat = paste(lag.value_year_adyn, year_adyn, sep = "_"))

# View(data_dynamics_ind_level5)

data_dynamics_ind_level6 <- data_dynamics_ind_level5 %>%
    mutate(growth_surv = ifelse(growth_raw > 0 &
            status == "survival", growth_raw, 0),
        growth_rec = ifelse(growth_raw > 0 &
            status == "recruit", growth_raw, 0),
        decrement_surv = ifelse(growth_raw < 0 &
            status == "survival", (growth_raw * -1), 0),
        decrement_mortality = ifelse(growth_raw < 0 &
            status == "dead", (growth_raw * -1), 0),
        interval_length = census_data_adyn - lag.value_census_data_adyn)

## for cwm_dyn
# write.csv(data_dynamics_ind_level6,"cwm_dyn_raw.csv")

#####

data_dynamics_ind_level6_1 <-
    data_dynamics_ind_level6[!(data_dynamics_ind_level6$status == "recruit" |
        data_dynamics_ind_level6$census_number == 1), ]

dim(data_dynamics_ind_level6_1)

sum(data_dynamics_ind_level6_1$lag.value_year ==
    data_dynamics_ind_level6_1$lag.value_year_adyn)

# data_dynamics_ind_level6[data_dynamics_ind_level6$plot_code == "IBI_01",]
# View(data_dynamics_ind_level6[data_dynamics_ind_level6$plot_code=="IBI_01",])

# write.csv(data_dynamics_ind_level6,"data_dynamics_ind_level6.csv")

#############
data_dynamics_ind_level7 <-
    data_dynamics_ind_level6[data_dynamics_ind_level6$census_number != 1, ]

data_dynamics_ind_level7 <- data_dynamics_ind_level7 %>%
    mutate(plot_code_interval = paste(plot_code, year_cat, sep = "_"))

# View(data_dynamics_ind_level7)
summary(data_dynamics_ind_level7)

data_dynamics_ind_level7_check <-
    data_dynamics_ind_level7[is.na(data_dynamics_ind_level7$growth_raw), ]

# write.csv(data_dynamics_ind_level7_check, "data_dynamics_ind_level7_check.csv")

# View(data_dynamics_ind_level7_check)

# data_dynamics_ind_level7_check[!is.na(data_dynamics_ind_level7_check$lag.value_agwb), ]

# data_dynamics_ind_level7_check[
#    data_dynamics_ind_level7_check$ind_year_key == "Amaioua intermedia_R679_SUB_02_SUB_02_51__2017.534", ]

##############
dyn_site_level1 <- data_dynamics_ind_level7 %>%
    group_by(plot_code_interval) %>%
    summarise(net_prod = sum(growth_raw),
        gain_surv = sum(growth_surv),
        gain_rec = sum(growth_rec),
        losses_surv = sum(decrement_surv),
        losses_dead = sum(decrement_mortality),
        year_i = mean(lag.value_year_adyn),
        year_f = mean(year_adyn),
        census_data_i = mean(lag.value_census_data_adyn),
        census_data_f = mean(census_data_adyn))

summary(dyn_site_level1)

# write.csv(dyn_site_level1, "dyn_site_level.csv")

#################################

data_for_dynamics_check61 <- data_dynamics_ind_level6 %>%
    group_by(plot_code, site_year) %>%
    summarise(agwb = sum(agwb), year = mean(yr_round), census_data = mean(census_data))

data_dynamics_ind_level62 <- data_for_dynamics_check61 %>%
    group_by(plot_code) %>%
    arrange(plot_code, site_year) %>%
    mutate(lag.value_agwb = lag(agwb), lag.value_year = lag(year), lag.value_census_data = lag(census_data)) %>%
    ungroup()

data_for_dynamics_check63 <- data_dynamics_ind_level62[
        !is.na(data_dynamics_ind_level62$lag.value_agwb), ]

dim(data_for_dynamics_check63)

data_for_dynamics_check64 <- as.data.frame(data_for_dynamics_check63 %>%
    mutate(net_prod_data2 = agwb - lag.value_agwb,
        plot_code_interval = paste(plot_code, lag.value_year, year, sep = "_")))

head(data_for_dynamics_check64)

head(as.data.frame(dyn_site_level1))

# see data_dynamics_ind_level6_completa
# individuos que desaparecerem sem morrer enviesam produtividade e biomassa
# incluir biomassa deles na mortalidade
# assim as produtividades batem

sum(data_for_dynamics_check64$plot_code_interval ==
    dyn_site_level1$plot_code_interval)

df_final_dyn <- cbind.data.frame(dyn_site_level1, data_for_dynamics_check64 %>%
    select(site_year, agwb, lag.value_agwb, net_prod_data2))
head(df_final_dyn)

df_final_dyn1 <- df_final_dyn %>%
    mutate(diff_prod_ind_minus_plot = net_prod - net_prod_data2)

min(df_final_dyn1$diff_prod_ind_minus_plot)
max(df_final_dyn1$diff_prod_ind_minus_plot)

df_final_dyn2 <- df_final_dyn1 %>%
    mutate(net_prod3 = net_prod - diff_prod_ind_minus_plot) %>%
    mutate(diff_prod_ind_minus_plot2 = net_prod3 - net_prod_data2,
        losses_dead_updated = losses_dead + diff_prod_ind_minus_plot)

min(df_final_dyn2$diff_prod_ind_minus_plot2)
max(df_final_dyn2$diff_prod_ind_minus_plot2)

data_dynamics1 <- df_final_dyn2 %>%
    mutate(net_prod_calc = gain_surv + gain_rec - losses_surv - losses_dead)

max(data_dynamics1$net_prod_calc - df_final_dyn2$net_prod3)
min(data_dynamics1$net_prod_calc - df_final_dyn2$net_prod3)

data_dynamics1 <- df_final_dyn2 %>% mutate(net_prod_calc =
    gain_surv + gain_rec - losses_surv - losses_dead_updated)

max(data_dynamics1$net_prod_calc - df_final_dyn2$net_prod3)
min(data_dynamics1$net_prod_calc - df_final_dyn2$net_prod3)

#################
head(df_final_dyn2)

df_final_dyn3 <- df_final_dyn2 %>%
    select(site_year, plot_code_interval, year_i, year_f, census_data_i, census_data_f, lag.value_agwb, agwb,
    gain_surv, gain_rec, losses_surv,
    losses_dead = losses_dead_updated,
    net_prod = net_prod3)

# View(df_final_dyn3)

# write.csv(df_final_dyn3, "./outputs/dyn_raw.csv", row.names = FALSE)