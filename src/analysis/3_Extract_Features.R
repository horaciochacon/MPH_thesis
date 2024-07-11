

library(data.table)
library(lubridate)
library(ggplot2)
library(tidyr)
library(purrr)
library(dplyr)
library(cowplot)

# Arguments -----------------------------------------------------------------------------------
if (!interactive()) {
  (output_dir <- as.character(commandArgs(TRUE)[[1]]))
  (config_loc <- as.character(commandArgs(TRUE)[[2]]))
}

config <- config::get(file = config_loc, config = "analysis")

# Read Data -----------------------------------------------------------------------------------

data_prov         <- fread(paste0(output_dir,"/data_prov.csv"))
pred_cascade_prov <- fread(paste0(output_dir,"/MR_BRT/output/pred_cascade_prov.csv"))
pred_cascade_dpt  <- fread(paste0(output_dir,"/MR_BRT/output/pred_cascade_dpt.csv"))
pop_prov          <- fread(paste0(config$sbatch$wd, "/data/pre_processed/poblacion_prov.csv")) 

# Process Data --------------------------------------------------------------------------------

prov_mort <- pred_cascade_prov[, mort := exp(pred) * 1e5]
prov_mort <- prov_mort[, .(x1, prov_cdc, dpt_cdc, mort)]
prov_mort <- prov_mort[pop_prov, on = .(prov_cdc, dpt_cdc)]
prov_mort <- prov_mort[, deaths :=  mort / 1e5 * pob]

# Feature Extraction --------------------------------------------------------------------------

prov_mort %>% 
  filter(prov_cdc == "CHACHAPOYAS") %>% 
  ggplot(aes(x = x1, y = mort)) +
  geom_line()



