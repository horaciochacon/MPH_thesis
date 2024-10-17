

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
source(paste0(config$sbatch$wd, "R/functions.R"))

# Read Data -----------------------------------------------------------------------------------

data_prov         <- fread(paste0(output_dir,"/data_prov.csv"))
pred_cascade_prov <- fread(paste0(output_dir,"/MR_BRT/output/pred_cascade_prov.csv"))
pred_cascade_dpt  <- fread(paste0(output_dir,"/MR_BRT/output/pred_cascade_dpt.csv"))
pop_prov          <- fread(paste0(config$sbatch$wd, "/data/pre_processed/poblacion_prov.csv")) 
pop_dpt           <- pop_prov[, .(pob = sum(pob)), by = .(dpt_cdc) ]

# Process Data --------------------------------------------------------------------------------

prov_mort <- pred_cascade_prov[, mort := exp(pred)]
prov_mort <- prov_mort[pop_prov, on = .(prov_cdc, dpt_cdc)]
prov_mort <- prov_mort[, deaths :=  mort * pob]
prov_mort <- prov_mort[, .(x1, prov_cdc, dpt_cdc, mort, deaths, pob)]

dpt_mort  <- pred_cascade_dpt[, mort := exp(pred)]
dpt_mort  <- dpt_mort[pop_dpt, on = .(dpt_cdc)]
dpt_mort  <- dpt_mort[, deaths :=  mort * pob]
dpt_mort  <- dpt_mort[, .(x1, dpt_cdc, mort, deaths, pob)]

# Feature Extraction --------------------------------------------------------------------------


# Plot peaks
create_department_panel_plots(
  config, prov_mort, data_prov, dpt_mort,
  output_dir = paste(output_dir, "plots/peaks/")
  )

# Feature Extraction --------------------------------------------------------------------------


extract_wave_features <- function(data, group_vars) {
  data[, wave := ifelse(x1 < 43, 1, 2)]
  
  data[, {
    peaks <- get_peaks_epidemic_scalable(
      mort * 1e5,  # Convert to per 100,000 for consistency
      window_size = 25,
      min_threshold = 5,
      min_peak_height_pct = 0.02,
      min_peak_prominence_pct = 0.01
    )
    peak_indices <- which(peaks)
    
    if (length(peak_indices) > 0) {
      peak_weeks <- x1[peak_indices]
      peak_mortalities <- mort[peak_indices] * 1e5
      
      list(
        peak_week = paste(peak_weeks, collapse = ";"),
        peak_mortality = paste(peak_mortalities, collapse = ";"),
        n_peaks = length(peak_indices),
        cumulative_deaths = sum(deaths)
      )
    } else {
      list(
        peak_week = NA_character_,
        peak_mortality = NA_character_,
        n_peaks = 0,
        cumulative_deaths = sum(deaths)
      )
    }
  }, by = c(group_vars, "wave")]
}

extract_all_features <- function(data, group_vars) {
  wave_features <- extract_wave_features(data, group_vars)
  
  all_features <- data[, {
    peaks <- get_peaks_epidemic_scalable(
      mort * 1e5,
      window_size = 25,
      min_threshold = 5,
      min_peak_height_pct = 0.02,
      min_peak_prominence_pct = 0.01
    )
    peak_indices <- which(peaks)
    
    if (length(peak_indices) > 0) {
      peak_weeks <- x1[peak_indices]
      peak_mortalities <- mort[peak_indices] * 1e5
      
      list(
        peak_week_all = paste(peak_weeks, collapse = ";"),
        peak_mortality_all = paste(peak_mortalities, collapse = ";"),
        n_peaks_all = length(peak_indices),
        cumulative_deaths_all = sum(deaths)
      )
    } else {
      list(
        peak_week_all = NA_character_,
        peak_mortality_all = NA_character_,
        n_peaks_all = 0,
        cumulative_deaths_all = sum(deaths)
      )
    }
  }, by = group_vars]
  
  merged_features <- merge(wave_features, all_features, by = group_vars)
  dcast(merged_features, paste(paste(group_vars, collapse = " + "), "~ wave"), 
        value.var = c("peak_week", "peak_mortality", "n_peaks", "cumulative_deaths"))
}


prov_features <- extract_all_features(prov_mort, c("prov_cdc", "dpt_cdc"))

