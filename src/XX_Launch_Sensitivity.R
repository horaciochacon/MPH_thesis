####################################################################################################
## Author:      Horacio Chacon Torrico
## Description: Launches a COVID-19 MR-BRT Peru Sensitivity Run
####################################################################################################

library(data.table)

# Source functions and config file
config <- config::get(config = "sensitivity")
config_loc <- paste0(config$sbatch$wd,"config.yml")
source("R/sens_functions.R")
run_date <- make_time_stamp()

# Create Output Directory
message(run_date)
output_dir <- paste0(config$sbatch$output_dir, run_date)
message(output_dir)
dir.create(output_dir, showWarnings = FALSE)
dir.create(paste0(output_dir,"/sensitivity"))
dir.create(paste0(output_dir,"/sensitivity/models"))
dir.create(paste0(output_dir,"/sensitivity/output"))
dir.create(paste0(output_dir,"/sensitivity/predicted_we"))
dir.create(paste0(output_dir,"/sensitivity/best_theta"))
dir.create(paste0(output_dir,"/logs"))
dir.create(paste0(output_dir,"/sensitivity/Plots"))
dir.create(paste0(output_dir,"/sensitivity/Plots/Heatmaps"))
dir.create(paste0(output_dir,"/sensitivity/Plots/epidemic_curves"))
dir.create(paste0(output_dir,"/sensitivity/Plots/epidemic_curves/wave1"))
dir.create(paste0(output_dir,"/sensitivity/Plots/epidemic_curves/wave2"))
dir.create(paste0(output_dir,"/sensitivity/Plots/epidemic_curves/combined"))
dir.create(paste0(output_dir,"/sensitivity/Plots/epidemic_curves/combined_k1"))
dir.create(paste0(output_dir,"/sensitivity/Plots/epidemic_curves/combined_k2"))
dir.create(paste0(output_dir,"/sensitivity/Plots/epidemic_curves/grid_plots"))
dir.create(paste0(output_dir,"/sensitivity/Plots/epidemic_curves/grid_plots_k1"))
dir.create(paste0(output_dir,"/sensitivity/Plots/epidemic_curves/grid_plots_k2"))


## Retrieve and save config file
file.copy(
  config_loc, 
  paste0(output_dir, "/config.yml"),
  overwrite = TRUE
  )

config_loc <- paste0(output_dir, "/config.yml")


# Generating the Predictions from thetas combinations -----------------------------------------

source(paste0(config$sbatch$wd,"src/XX_Sensitivity_Input.R"))


# Extracting errors from the predictions ------------------------------------------------------

rscript <- paste0(config$sbatch$wd, "src/Sensitivity_agg.R")
jobname <- "agg_predictions_sensitivity"
args <- c(output_dir, config_loc)

sys.sub <- paste0(
  "sbatch --parsable -A proj_lsae ",
  " -J ", jobname,
  " -o ", paste0(output_dir, "/logs/", "%x.o%j"),
  " -c ", config$sbatch$threads,
  " --mem ", config$sbatch$memory_agg,
  " -t ", config$sbatch$time,
  " -p all.q",
  " --dependency=afterok:", paste(job_ids, collapse = ":")
)

command <- paste(sys.sub, rshell, rscript, paste(args, collapse = " "))
job_id_agg <- system(command, intern = TRUE)


# Applying Best Theta algorithm ---------------------------------------------------------------

rscript <- paste0(config$sbatch$wd, "src/Sensitivity_best.R")
jobname <- "best_theta"
args <- c(output_dir, config_loc)

sys.sub <- paste0(
  "sbatch --parsable -A proj_lsae ",
  " -J ", jobname,
  " -o ", paste0(output_dir, "/logs/", "%x.o%j"),
  " -c ", config$sbatch$threads,
  " --mem ", config$sbatch$memory_agg,
  " -t ", config$sbatch$time,
  " -p all.q",
  " --dependency=afterok:", paste(job_id_agg, collapse = ":")
)

command <- paste(sys.sub, rshell, rscript, paste(args, collapse = " "))
job_id_best <- system(command, intern = TRUE)


# Plots ---------------------------------------------------------------------------------------

rscript <- paste0(config$sbatch$wd, "src/Sensitivity_plots.R")
jobname <- "plot"
args <- c(output_dir, config_loc)

sys.sub <- paste0(
  "sbatch --parsable -A proj_lsae ",
  " -J ", jobname,
  " -o ", paste0(output_dir, "/logs/", "%x.o%j"),
  " -c ", config$sbatch$threads,
  " --mem ", config$sbatch$memory_agg,
  " -t ", config$sbatch$time,
  " -p all.q",
  " --dependency=afterok:", paste(job_id_best, collapse = ":")
)

command <- paste(sys.sub, rshell, rscript, paste(args, collapse = " "))
job_id_plot <- system(command, intern = TRUE)


