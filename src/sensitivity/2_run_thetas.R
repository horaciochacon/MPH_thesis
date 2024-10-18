####################################################################################################
## Author:      Horacio Chacon Torrico
##
## Description: This script takes specific theta parameter values (theta_dpt and theta_prov) to run 
##              a provincial-level cascade spline model using the MR-BRT framework. It loads the 
##              national-level model and preprocessed data, fits the provincial-level model, and 
##              generates predictions based on the fitted model. The predictions are then saved, and 
##              the intermediate model outputs are cleaned up.
##
## Passed args: theta_dpt [numeric] -- theta parameter for department level
##              theta_prov [numeric] -- theta parameter for province level
##              output_dir [character] -- directory to save outputs
##              config_loc [character] -- path to the configuration file
##
## Requires:    MR-BRT national model object ("MR_BRT/models/mod_nat.pickle")
##              processed data object for MR-BRT ("MR_BRT/models/data_prov.rds")
##              provincial population data ("data/pre_processed/poblacion_prov.csv")
##
## Outputs:     provincial-level cascade spline model predictions
##              predictions file ("MR_BRT/output/prov_<theta_prov>-dpt_<theta_dpt>.gz")
##
####################################################################################################

# Packages and Dependencies -------------------------------------------------------------------
library(data.table)
reticulate::use_python("/ihme/code/mscm/miniconda3/envs/mrtool_0.0.1/bin/python")
library(mrbrt003, lib.loc = "/ihme/code/mscm/Rv4/dev_packages/")
library(purrr)

# Arguments -----------------------------------------------------------------------------------
if (!interactive()) {
  args <- commandArgs(TRUE)
  print("Arguments received:")
  print(args)
  
  # Combine department name parts if split due to spaces
  prov_parts <- args[1:(length(args)-4)]
  prov <- paste(prov_parts, collapse=" ")
  
  theta_prov <- as.numeric(args[length(args)-3])
  n_fold <- as.numeric(args[length(args)-2])
  output_dir <- args[length(args)-1]
  config_loc <- args[length(args)]
  
  print(paste("Province:", prov))
  print(paste("Theta Province:", theta_prov))
  print(paste("Fold:", n_fold))
  print(paste("Output Directory:", output_dir))
  print(paste("Config Location:", config_loc))
}

# Check if config_loc is a directory
if (dir.exists(config_loc)) {
  config_loc <- file.path(config_loc, "config.yml")
  print(paste("Updated Config Location:", config_loc))
}

# Load configuration
config <- config::get(file = config_loc, config = "sensitivity")

# Load data and model -------------------------------------------------------------------------
poblacion_prov <- fread(paste0(config$sbatch$wd, "data/pre_processed/poblacion_prov.csv"))
dpt <- unique(poblacion_prov[prov_cdc == prov, dpt_cdc])
mod_dpt <- py_load_object(paste0(output_dir ,  "/MR_BRT/models/mod_", dpt, ".pickle"))
data_prov <- as.data.table(readRDS(paste0(output_dir , "/MR_BRT/models/data_prov.rds")))


list_pred <- list()

for (k_fold in 1:n_fold) {
  # Select fold-specific data for province ------------------------------------------------------
  
  data_dpt_no_prov <- data_prov[dpt_cdc == dpt & prov_cdc != prov]
  data_prov_no_fold <- data_prov[prov_cdc == prov & fold != k_fold]
  x_fold <- sort(data_prov[prov_cdc == prov & fold == k_fold, x1])
  data_k_fold <- bind_rows(data_dpt_no_prov, data_prov_no_fold)
  
  # Create and fit MRBRT province-level cascade spline model ------------------------------------
  mod_cascade_prov <- run_spline_cascade(
    stage1_model_object = mod_dpt,
    df = data_k_fold,
    gaussian_prior = TRUE,
    col_obs = "ylog",
    col_obs_se = "sdlog",
    col_study_id = "id",
    stage_id_vars = c("prov_cdc"),
    thetas = c(theta_prov),
    output_dir = paste0(output_dir,"/MR_BRT/models/cascade/"),
    model_label = paste0(prov, "_fold_", k_fold,"_theta_",theta_prov),
    overwrite_previous = TRUE
  )
  
  # Prediction ----------------------------------------------------------------------------------
  
  # Create prediction data frame to fill predicted values
  df_pred <- expand.grid(
    stringsAsFactors = FALSE,
    x1 = x_fold,
    prov_cdc = prov
  ) %>%
    left_join(poblacion_prov)
  
  # Obtain predicted values from the provincial-level cascade spline model
  pred_cascade_prov <- predict_spline_cascade(
    fit = mod_cascade_prov,
    newdata = df_pred
  ) %>% 
    as.data.table() 
  
  # Add Required columns
  
  pred_cascade_prov <- (
    pred_cascade_prov
    [, y_hat := pred][, pred := NULL][, cascade_prediction_id := NULL]
    [, id_prov := id][, id := NULL][,prov_cdc := NULL][,dpt_cdc := NULL]
    [, c(1,3,5,4)]
  )
  
  list_pred[[k_fold]] <- pred_cascade_prov

  # Cleanup
  print("Deleting Cascade Spline MR-BRT Output")
  unlink(
    paste0(
      output_dir,"/MR_BRT/models/cascade/", 
      prov, "_fold_", k_fold,
      "_theta_",theta_prov
    ), 
    recursive = TRUE
  )
  
}

# Combining all folds -------------------------------------------------------------------------

pred_prov <- list_pred %>% 
  map2(1:n_fold, ~.x %>% mutate(fold = .y)) %>% 
  bind_rows()

# Writing the predictions
fwrite(
  pred_prov,
  paste0(
    output_dir,
    "/MR_BRT/output/",
    prov, "_theta_",theta_prov,
    ".gz" ),
  row.names = FALSE,
  compress = "gzip"
)

print("Script Completed!")
