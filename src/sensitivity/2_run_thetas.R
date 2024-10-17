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

# Arguments -----------------------------------------------------------------------------------
if (!interactive()) {
  args <- commandArgs(TRUE)
  print("Arguments received:")
  print(args)
  
  # Combine department name parts if split due to spaces
  dpt_parts <- args[1:(length(args)-3)]
  dpt <- paste(dpt_parts, collapse=" ")
  
  theta_prov <- as.numeric(args[length(args)-2])
  output_dir <- args[length(args)-1]
  config_loc <- args[length(args)]
  
  print(paste("Department:", dpt))
  print(paste("Theta Province:", theta_prov))
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
mod_nat <- py_load_object(paste0(output_dir , "/MR_BRT/models/mod_nat.pickle"))
data_prov <- readRDS(paste0(output_dir , "/MR_BRT/models/data_prov.rds"))
data_prov <- data_prov[dpt_cdc == dpt]
poblacion_prov <- fread(paste0(config$sbatch$wd, "data/pre_processed/poblacion_prov.csv"))

# Create and fit MRBRT province-level cascade spline model ------------------------------------
mod_cascade_prov <- run_spline_cascade(
  stage1_model_object = mod_nat,
  df = data_prov,
  gaussian_prior = TRUE,
  col_obs = "ylog",
  col_obs_se = "sdlog",
  col_study_id = "id",
  stage_id_vars = c("prov_cdc"),
  thetas = c(theta_prov),
  output_dir = paste0(output_dir,"/MR_BRT/models/cascade/"),
  model_label = paste0(dpt,"_thetaprov_",theta_prov),
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

pred_cascade_prov <- (
  pred_cascade_prov
  [, y_hat := pred][, pred := NULL][, cascade_prediction_id := NULL]
  [, id_prov := id][, id := NULL][,prov_cdc := NULL][,dpt_cdc := NULL]
  [, c(1,3,5,4)]
)


# Writing the predictions
fwrite(
  pred_cascade_prov,
  paste0(
    output_dir,
    "/MR_BRT/output/",
    dpt,
    "_",theta_prov,
    ".gz" ),
  row.names = FALSE,
  compress = "gzip"
  )

# Cleanup
print("Deleting Cascade Spline MR-BRT Output")
unlink(
  paste0(
    output_dir,"/MR_BRT/models/cascade/", 
    dpt,"_thetaprov_",theta_prov
    ), 
  recursive = TRUE
  )

print("Script Completed!")