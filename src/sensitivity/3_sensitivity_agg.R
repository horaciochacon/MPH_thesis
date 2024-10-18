####################################################################################################
## Author:      Horacio Chacon Torrico
##
## Description: This script aggregates the results of the MR-BRT provincial-level sensitivity 
##              analysis. It reads in the predicted values from multiple theta combinations, merges 
##              these with observed data, calculates errors and weighted errors, and saves the 
##              aggregated results. The script distinguishes between different waves of data and 
##              ensures that the output is organized by various levels of granularity.
##
## Passed args: output_dir [character] -- directory to save outputs
##              config_loc [character] -- path to the configuration file
##
## Requires:    sensitivity analysis predictions (all gz files in "MR_BRT/output/")
##              processed data object for MR-BRT ("MR_BRT/models/data_prov.rds")
##              provincial population data ("data/pre_processed/poblacion_prov.csv")
##
## Outputs:     aggregated weighted errors by different levels:
##                - weighted errors by wave ("estimates/predicted_we/df_we_k.csv")
##                - weighted errors by week ("estimates/predicted_we/df_we_l.csv")
##                - weighted errors by wave and department ("estimates/predicted_we/df_we_ki.csv")
##                - weighted errors by wave, dept, and prov ("estimates/predicted_we/df_we_kij.csv")
##
####################################################################################################

# Load Packages -----------------------------------------------------------

library(lubridate)
library(data.table)
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
path <- paste0(output_dir, "/MR_BRT/output/")

# List all files in the folder
file_list <- list.files(path, pattern = "\\.gz$", full.names = TRUE)

# Read all files in the folder
data <- rbindlist(
  lapply(file_list, 
         function(file) {
           df <- fread(file)
           temp <- str_split(file, "//",simplify = TRUE)[,2]
           df$theta <- as.numeric(str_extract(temp, "(?<=_)[^_]+(?=\\.gz)"))
           return(df)
           }
         )
  )

# Read Auxiliary Files
data_prov <- readRDS(paste0(output_dir, "/MR_BRT/models/data_prov.rds")) %>% as.data.table()
pob <- fread(paste0(config$sbatch$wd, "data/pre_processed/poblacion_prov.csv")) %>% 
  rename(id_prov = id)

data_prov <- data_prov[pob, on = .(dpt_cdc , prov_cdc), nomatch = NA] %>% 
  select(id_dpt, id_prov, pob, x1, n, y1, y_obs = ylog, dpt_cdc, prov_cdc)

# Processing data -----------------------------------------------------------------------------

data <- data[
  data_prov[,.(id_prov, id_dpt, x1, dpt_cdc, prov_cdc, y_obs)], 
  on = .(id_prov, id_dpt, x1),
  nomatch = NA
  ]

data <- data[x1 >= 2]

# Calculate Error, Weight and Weighted Error --------------------------------------------------

# Waves
waves <- copy(data_prov)
waves <- waves[, k := ifelse(x1 < 43, 1, 2)]
waves <- waves[, .(y_obs = sum(n, na.rm = TRUE)), by = .(id_prov, id_dpt, k)]
waves <- waves[pob, on = .(id_prov, id_dpt), nomatch = NA]
waves <- waves[, w_k := ifelse(
  k == 1, y_obs / sum(waves$y_obs[waves[,k == 1]]),
  y_obs / sum(waves$y_obs[waves[,k == 2]]))
]

# Predicted 
data_sens <- copy(data)

data_sens <- (
  data_sens
  [ !is.na(y_obs)]
  [, k := ifelse(x1 < 43, 1, 2)]
  [waves[,.(id_prov, id_dpt, k, w_k)], on = .(id_prov, id_dpt, k), nomatch = NA]
  [, e := (exp(y_obs) * 1e6 - exp(y_hat)*1e6)^2]
  [, we := e * w_k]
)

# Saving the Predicted theta-specific Weighted Errors -----------------------------------------

df_we_k   <- data_sens[, .(we_k = sum(we[is.finite(we)])), by = .(theta, k)]
df_we_l   <- data_sens[, .(we_k = sum(we)), by = .(theta,l = x1)]
df_we_ki  <- data_sens[, .(we_k = sum(we)), by = .(theta, k, dpt_cdc)]
df_we_kij <- data_sens[, .(we_k = sum(we)), by = .(theta, k, dpt_cdc, prov_cdc)]

fwrite(waves, paste0(output_dir, "/estimates/waves.csv")) 
fwrite(df_we_k, paste0(output_dir, "/estimates/predicted_we/df_we_k.csv")) 
fwrite(df_we_l, paste0(output_dir, "/estimates/predicted_we/df_we_l.csv"))
fwrite(df_we_ki, paste0(output_dir, "/estimates/predicted_we/df_we_ki.csv"))
fwrite(df_we_kij,  paste0(output_dir, "/estimates/predicted_we/df_we_kij.csv"))

print("Script Completed!")
