
library(data.table)

# Source functions and config file
config <- config::get(config = "analysis")
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

# Stage 1: MR-BRT  ----------------------------------------------------------------------------

rscript <- "src/analysis/1_Run_MRBRT.R"
jobname <- "mrbrt_predictions"
rshell <- "/ihme/singularity-images/rstudio/shells/execRscript.sh -s"
args <- c(output_dir, config_loc)


job_id_mrbrt <- send_job(
  config = config,
  jobname = jobname,
  output_dir = output_dir,
  config_loc = config_loc,
  rshell = rshell,
  rscript_rel_path = rscript,
  args = args
)

# Stage 2: Plot Predictions -------------------------------------------------------------------

rscript <- "src/analysis/2_Plot_Predictions.R"
jobname <- "mrbrt_plots"
rshell <- "/ihme/singularity-images/rstudio/shells/execRscript.sh -s"
args <- c(output_dir, config_loc)


job_id_mrbrt <- send_job(
  config = config,
  jobname = jobname,
  output_dir = output_dir,
  config_loc = config_loc,
  rshell = rshell,
  rscript_rel_path = rscript,
  args = args,
  job_ids = job_id_mrbrt
)

# Stage 3: Feature Extraction -----------------------------------------------------------------


