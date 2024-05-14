# Packages and Dependencies -------------------------------------------------------------------
library(data.table)
reticulate::use_python("/ihme/code/mscm/miniconda3/envs/mrtool_0.0.1/bin/python")
library(mrbrt003, lib.loc = "/ihme/code/mscm/Rv4/dev_packages/")

# Arguments -----------------------------------------------------------------------------------
if (!interactive()) {
  (theta_dpt <- as.numeric(commandArgs(TRUE)[[1]]))
  (theta_prov <- as.numeric(commandArgs(TRUE)[[2]]))
  (output_dir <- as.character(commandArgs(TRUE)[[3]]))
  (config_loc <- as.character(commandArgs(TRUE)[[4]]))
}

config <- config::get(file = config_loc, config = "sensitivity")

# Load data and model -------------------------------------------------------------------------
mod_nat <- py_load_object(paste0(output_dir , "/sensitivity/mod_nat.pickle"))
data_prov <- readRDS(paste0(output_dir , "/sensitivity/data_prov.rds"))
poblacion_prov <- fread(paste0(config$sbatch$wd, "data/pre_processed/poblacion_prov.csv"))


print(paste("Theta Department = ", theta_dpt))
print(paste("Theta Province = ", theta_prov))

# Create and fit MRBRT province-level cascade spline model ------------------------------------
mod_cascade_prov <- run_spline_cascade(
  stage1_model_object = mod_nat,
  df = data_prov,
  gaussian_prior = TRUE,
  col_obs = "ylog",
  col_obs_se = "sdlog",
  col_study_id = "id",
  stage_id_vars = c("dpt_cdc", "prov_cdc"),
  thetas = c(theta_dpt, theta_prov),
  output_dir = paste0(output_dir,"/sensitivity/models"),
  model_label = paste0("thetaprov_",theta_prov,"_thetadpt_", theta_dpt),
  overwrite_previous = TRUE
)

# Prediction ----------------------------------------------------------------------------------

# Create prediction data frame to fill predicted values
df_pred <- expand.grid(
  stringsAsFactors = FALSE,
  x1 = seq(0, 85, by = config$pred$res),
  prov_cdc = unique(data_prov$prov_cdc)
  ) %>%
  left_join(poblacion_prov)

# Obtain predicted values from the provincial-level cascade spline model
pred_cascade_prov <- predict_spline_cascade(
  fit = mod_cascade_prov,
  newdata = df_pred
  ) %>% 
  as.data.table() 

# Add Required columns

data_pred <- (
  pred_cascade_prov
  [, y_hat := pred][, pred := NULL]
  )

data_pred <-  merge(data_pred, data_prov[,c(1:3,7:11)], all.x = TRUE)

data_pred <- (
  data_pred
  [, y_obs := ylog][, ylog := NULL]
  [, theta_dpt := theta_dpt][, theta_prov := theta_prov]
  )

fwrite(
  data_pred,
  paste0(
    output_dir,
    "/sensitivity/output/",
    "errors_prov_",theta_prov,
    "_dpt_", theta_dpt,
    ".csv" ),
  row.names = FALSE
  )

print("Script Completed!")