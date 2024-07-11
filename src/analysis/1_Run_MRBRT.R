
# Load Packages -----------------------------------------------------------
library(data.table)
library(lubridate)
library(ggplot2)
library(reticulate)
library(tidyr)
library(dplyr)
reticulate::use_python("/ihme/code/mscm/miniconda3/envs/mrtool_0.0.1/bin/python")
cw <- reticulate::import("crosswalk")
mr <- reticulate::import("mrtool")
library(mrbrt003, lib.loc = "/ihme/code/mscm/Rv4/dev_packages/")

# Arguments -----------------------------------------------------------------------------------
if (!interactive()) {
  (output_dir <- as.character(commandArgs(TRUE)[[1]]))
  (config_loc <- as.character(commandArgs(TRUE)[[2]]))
}

config <- config::get(file = config_loc, config = "analysis")

# Loading and Formatting the data -----------------------------------------

# Death count per day dataset
death_count_day <- fread(paste0(config$sbatch$wd,"data/pre_processed/death_count_prov_day.csv"))

# Province level population
poblacion_prov <- fread(paste0(config$sbatch$wd,"data/pre_processed/poblacion_prov.csv"))

# Province level death counts
data_prov <- (
  death_count_day
  [, fecha_fallecimiento := floor_date(fecha_fallecimiento, "weeks", week_start = 1) ]
  [, .(n = sum(n)), by = .(fecha_fallecimiento, dpt_cdc, prov_cdc) ]
  [ poblacion_prov[, 2:5], on = .(prov_cdc) ]
  [, y1 := n / pob ]
  [, x1 := (as.numeric(fecha_fallecimiento) - 18323) / 7 ]
  [, sd := sqrt((y1 * (1 - y1)) / pob) ]
  [, ylog := cw$utils$linear_to_log(mean = array(y1), sd = array(sd))[[1]]]
  [, sdlog := cw$utils$linear_to_log(mean = array(y1), sd = array(sd))[[2]]]
  [, !"fecha_fallecimiento"]
)

# Baseline Province level data y1 = 0
province_baseline <- purrr:::map(
  .x = list(-3,-2,-1),
  .f = ~ data_prov %>%
    group_by(dpt_cdc, prov_cdc, pob) %>%
    summarize(x1 = min(x1)) %>%
    mutate(
      x1 = 0 + .x,
      y1 = 0,
      ylog = log(min(data_prov$y1)/2),
      n = 0,
      sd = 1,
      ylog = cw$utils$linear_to_log(mean = array(0 + min(data_prov$y1)/2), sd = array(1))[[1]],
      sdlog = 0.01
    )
) %>%
  bind_rows()

# Final provincial dataset with baseline dummy data
data_prov <- data_prov %>%
  bind_rows(province_baseline) %>%
  arrange(dpt_cdc, prov_cdc, pob, x1) %>%
  mutate(
    id = row_number(),
    x1 = x1 + 3
  ) %>% 
  fill(id_dpt, .direction = "up")

# MR-BRT Model ---------------------------------------------------------------

# Load data in MRData object
nat_mrbrt <- mr$MRData()
nat_mrbrt$load_df(
  data = data_prov,
  col_obs = "ylog",
  col_obs_se = "sdlog",
  col_covs = list("x1"),
  col_study_id = "id"
)

# Create MRBRT model and configure covariates
mod_nat <- mr$MRBRT(
  data = nat_mrbrt,
  cov_models = list(
    mr$LinearCovModel("intercept", use_re = FALSE),
    mr$LinearCovModel(
      alt_cov = "x1",
      use_spline = TRUE,
      spline_knots = config$mrbrt$spline_knots,
      spline_degree = config$mrbrt$spline_degree,
      spline_knots_type = config$mrbrt$spline_knots_type,
      prior_spline_maxder_gaussian = config$mrbrt$prior_spline_maxder_gaussian
    )
  )
)

# Fit MRBRT model
mod_nat$fit_model(inner_print_level = 5L, inner_max_iter = 1000L)

# Save sensitivity national model
py_save_object(mod_nat, paste0(output_dir,"/", "MR_BRT/models/mod_nat.pickle"), pickle = "dill")
saveRDS(data_prov, paste0(output_dir,"/","MR_BRT/models/data_prov.rds"))

# Cascade spline at department level --------------------------------------

# Create and fit MRBRT department-level cascade spline model
mod_cascade_dpt <- run_spline_cascade(
  stage1_model_object = mod_nat,
  gaussian_prior = TRUE,
  df = data_prov,
  col_obs = "ylog",
  col_obs_se = "sdlog",
  col_study_id = "id",
  stage_id_vars = "dpt_cdc",
  thetas = config$cascade$theta_dpt,
  output_dir = paste0(output_dir,"/MR_BRT/models/cascade/"),
  model_label = paste0("thetadpt_", config$cascade$theta_dpt),
  overwrite_previous = TRUE
)

# Obtain predicted values from the department-level cascade spline model
pred_cascade_dpt <- predict_spline_cascade(
  fit = mod_cascade_dpt,
  newdata = expand.grid(
    x1 = seq(0, 85, by = config$pred$res),
    dpt_cdc = unique(data_prov$dpt_cdc)
  ) 
)



# Create and fit MRBRT province-level cascade spline model ------------------------------------
mod_cascade_prov <- run_spline_cascade(
  stage1_model_object = mod_nat,
  df = data_prov,
  gaussian_prior = TRUE,
  col_obs = "ylog",
  col_obs_se = "sdlog",
  col_study_id = "id",
  stage_id_vars = c("dpt_cdc", "prov_cdc"),
  thetas = c(config$cascade$theta_dpt, config$cascade$theta_prov),
  output_dir = paste0(output_dir,"/MR_BRT/models/cascade/"),
  model_label = paste0(
    "thetaprov_",config$cascade$theta_prov,"_thetadpt_", config$cascade$theta_dpt
    ),
  overwrite_previous = TRUE
)


# Create prediction data frame to fill predicted values
df_pred <- expand.grid(
  stringsAsFactors = FALSE,
  x1 = seq(0, 85, by = config$pred$res),
  prov_cdc = unique(data_prov$prov_cdc)
  ) %>%
  left_join(poblacion_prov %>% select(-pob))

# Obtain predicted values from the provincial-level cascade spline model
pred_cascade_prov <- predict_spline_cascade(
  fit = mod_cascade_prov,
  newdata = df_pred
  ) 


# Saving MR-BRT Results -----------------------------------------------------------------------

# Saving MRBRT Predictions
fwrite(
  pred_cascade_prov,
  paste0(output_dir,"/MR_BRT/output/pred_cascade_prov.csv"),
  row.names = FALSE
)

fwrite(
  pred_cascade_dpt,
  paste0(output_dir,"/MR_BRT/output/pred_cascade_dpt.csv"),
  row.names = FALSE
)

# Saving Observed data
fwrite(
  data_prov,
  paste0(output_dir,"/data_prov.csv"),
  row.names = FALSE
)

print("Script Completed!")
