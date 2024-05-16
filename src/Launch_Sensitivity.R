####################################################################################################
## Author:      Horacio Chacon Torrico
##
## Description: This script manages the entire COVID-19 MR-BRT sensitivity analysis for Peru.
##              It sets up the environment, generates predictions, aggregates errors, identifies
##              the best theta combinations, and produces plots. The script runs various sub-scripts 
##              to perform these tasks, managing dependencies and resource allocation through SLURM.
##
## Passed args: None
##
## Requires:    Configuration file ("config.yml")
##              Various R scripts for different stages of the analysis:
##                - "src/sensitivity/1_generate_sensitivity_input.R" for generating sensitivity inputs
##                - "src/sensitivity/3_sensitivity_agg.R" for aggregating prediction errors
##                - "src/sensitivity/4_sensitivity_best.R" for identifying the best theta combinations
##                - "src/sensitivity/5_sensitivity_plots.R" for generating plots
##
## Outputs:     Creates output directory with results including:
##                - sensitivity analysis predictions
##                - aggregated errors
##                - best theta combinations
##                - plots of epidemic curves
##
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
create_directories(output_dir = output_dir)

## Retrieve and save config file
file.copy(
  config_loc, 
  paste0(output_dir, "/config.yml"),
  overwrite = TRUE
  )

config_loc <- paste0(output_dir, "/config.yml")

# Generating the Predictions from thetas combinations -----------------------------------------

source(paste0(config$sbatch$wd,"src/sensitivity/1_generate_sensitivity_input.R"))

# Extracting errors from the predictions ------------------------------------------------------

rscript <- paste0(config$sbatch$wd, "src/sensitivity/3_sensitivity_agg.R")
jobname <- "agg_predictions_sensitivity"
args <- c(output_dir, config_loc)

sys.sub <- paste0(
  "sbatch --parsable -A proj_lsae ",
  " -J ", jobname,
  " -o ", paste0(output_dir, "/logs/", "%x.o%j", ".txt"),
  " -c ", config$sbatch$threads,
  " --mem ", config$sbatch$memory_agg,
  " -t ", config$sbatch$time,
  " -p all.q",
  " --dependency=afterok:", paste(job_ids, collapse = ":")
)

command <- paste(sys.sub, rshell, rscript, paste(args, collapse = " "))
job_id_agg <- system(command, intern = TRUE)


# Applying Best Theta algorithm ---------------------------------------------------------------

rscript <- paste0(config$sbatch$wd, "src/sensitivity/4_sensitivity_best.R")
jobname <- "best_theta"
args <- c(output_dir, config_loc)

sys.sub <- paste0(
  "sbatch --parsable -A proj_lsae ",
  " -J ", jobname,
  " -o ", paste0(output_dir, "/logs/", "%x.o%j", ".txt"),
  " -c ", config$sbatch$threads,
  " --mem ", config$sbatch$memory_agg,
  " -t ", config$sbatch$time,
  " -p all.q",
  " --dependency=afterok:", paste(job_id_agg, collapse = ":")
)

command <- paste(sys.sub, rshell, rscript, paste(args, collapse = " "))
job_id_best <- system(command, intern = TRUE)


# Plots ---------------------------------------------------------------------------------------

rscript <- paste0(config$sbatch$wd, "src/sensitivity/5_sensitivity_plots.R")
jobname <- "plot"
args <- c(output_dir, config_loc)

sys.sub <- paste0(
  "sbatch --parsable -A proj_lsae ",
  " -J ", jobname,
  " -o ", paste0(output_dir, "/logs/", "%x.o%j",".txt"),
  " -c ", config$sbatch$threads,
  " --mem ", config$sbatch$memory_agg,
  " -t ", config$sbatch$time,
  " -p all.q"
  # " --dependency=afterok:", paste(job_id_best, collapse = ":")
)

command <- paste(sys.sub, rshell, rscript, paste(args, collapse = " "))
job_id_plot <- system(command, intern = TRUE)
