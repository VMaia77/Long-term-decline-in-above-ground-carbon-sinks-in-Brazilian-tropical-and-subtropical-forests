# load packages
library(dplyr)
library(tidyr)
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

r2_c_stocks <- r.squaredGLMM(best_model_c_stocks)
r2_net_c_sink <- r.squaredGLMM(best_model_net_c_sink)
r2_c_gains <- r.squaredGLMM(best_model_c_gains)
r2_c_losses <- r.squaredGLMM(best_model_c_losses)
r2_net_perc <- r.squaredGLMM(best_model_net_perc)
r2_gains_perc <- r.squaredGLMM(best_model_gains_perc)
r2_losses_perc <- r.squaredGLMM(best_model_losses_perc)
r2_crt <- r.squaredGLMM(best_model_crt)

df_r2 <- rbind.data.frame(r2_c_stocks, r2_net_c_sink,
                          r2_c_gains, r2_c_losses, 
                          r2_net_perc,
                          r2_gains_perc, r2_losses_perc,
                          r2_crt) %>%
  mutate(var =
           c("r2_c_stocks", "r2_net_c_sink",
             "r2_c_gains", "r2_c_losses", 
             "r2_net_perc",
             "r2_gains_perc", "r2_losses_perc",
             "r2_crt"))

write.csv(df_r2, "models_r2.csv", row.names = FALSE)
