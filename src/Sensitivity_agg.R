# Load Packages -----------------------------------------------------------

library(lubridate)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(data.table)
library(purrr)
library(tidyr)
library(dplyr)
library(stringr)


# Arguments -----------------------------------------------------------------------------------
if (!interactive()) {
  (output_dir <- as.character(commandArgs(TRUE)[[1]]))
  (config_loc <- as.character(commandArgs(TRUE)[[2]]))
}

config <- config::get(config = "sensitivity")

# Read Sensitivity files ----------------------------------------------------------------------

# Set path to folder
path <- paste0(output_dir, "/sensitivity/output/")

# List all files in the folder
file_list <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Read all files in the folder
data <- rbindlist(lapply(file_list, fread))

# Read Auxiliary Files
data_prov <- readRDS(paste0(output_dir, "/sensitivity/data_prov.rds"))
pob <- fread(paste0(config$sbatch$wd, "data/pre_processed/poblacion_prov.csv"))[,1:3]

# Calculate Error, Weight and Weighted Error --------------------------------------------------

# Waves
waves <- copy(data_prov)
waves <- waves[, k := ifelse(x1 < 43, 1, 2)]
waves <- waves[, .(y_obs = sum(n, na.rm = TRUE)), by = .(dpt_cdc, prov_cdc, k)]
waves <- waves[pob, on = .(prov_cdc, dpt_cdc), nomatch = NA]
waves <- waves[, w_k := ifelse(
  k == 1, y_obs / sum(waves$y_obs[waves[,k == 1]]),
  y_obs / sum(waves$y_obs[waves[,k == 2]]))
]
waves

# Predicted 
data_sens <- copy(data)

data_sens <- (
  data_sens
  [ !is.na(y_obs)]
  [, k := ifelse(x1 < 43, 1, 2)]
  [waves, on = .(prov_cdc, dpt_cdc, k), nomatch = NA]
  [, e := (exp(y_obs) * 1e6 - exp(y_hat)*1e6)^2]
  [, we := e * w_k]
)

# Saving the Predicted theta-specific Weighted Errors -----------------------------------------

df_we_k   <- data_sens[, .(we_k = sum(we[is.finite(we)])), by = .(theta_dpt, theta_prov, k)]
df_we_l   <- data_sens[, .(we_k = sum(we)), by = .(theta_dpt, theta_prov,l = x1)]
df_we_ki  <- data_sens[, .(we_k = sum(we)), by = .(theta_dpt, theta_prov, k, dpt_cdc)]
df_we_kij <- data_sens[, .(we_k = sum(we)), by = .(theta_dpt, theta_prov, k, dpt_cdc, prov_cdc)]

fwrite(waves, paste0(output_dir, "/sensitivity/waves.csv")) 
fwrite(df_we_k, paste0(output_dir, "/sensitivity/predicted_we/df_we_k.csv")) 
fwrite(df_we_l, paste0(output_dir, "/sensitivity/predicted_we/df_we_l.csv"))
fwrite(df_we_ki, paste0(output_dir, "/sensitivity/predicted_we/df_we_ki.csv"))
fwrite(df_we_kij,  paste0(output_dir, "/sensitivity/predicted_we/df_we_kij.csv"))

print("Script Completed!")
