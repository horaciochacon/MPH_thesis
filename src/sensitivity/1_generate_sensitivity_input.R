####################################################################################################
## Author:      Horacio Chacon Torrico
##
## Description: This script preprocesses data and generates the MR-BRT model for national-level 
##              sensitivity analysis. It processes death counts and population data at the provincial 
##              level, formats it for the MR-BRT model, and fits the model. Additionally, it sets up 
##              and submits array jobs to run the model for various combinations of theta parameters 
##              (theta_dpt and theta_prov), which are essential for sensitivity analysis.
##
## Passed args: config_loc [character] -- path to the configuration file
##              output_dir [character] -- directory to save outputs
##
## Requires:    pre-processed death count data ("data/pre_processed/death_count_prov_day.csv")
##              provincial population data ("data/pre_processed/poblacion_prov.csv")
##              MR-BRT package (Python) via reticulate
##              sensitivity configuration (loaded from config file)
##
## Outputs:     processed provincial dataset with baseline dummy data
##              MR-BRT national model object ("MR_BRT/models/mod_nat.pickle")
##              processed data object for MR-BRT ("MR_BRT/models/data_prov.rds")
##              submission of batch jobs for various theta combinations
##
####################################################################################################

# Load Packages -----------------------------------------------------------

library(lubridate)
library(ggplot2)
library(reticulate)
library(ggpubr)
library(cowplot)
library(tidyr)
library(dplyr)
reticulate::use_python("/ihme/code/mscm/miniconda3/envs/mrtool_0.0.1/bin/python")
cw <- reticulate::import("crosswalk")
mr <- reticulate::import("mrtool")
config <- config::get(file = config_loc, config = "sensitivity")
library(mrbrt003, lib.loc = "/ihme/code/mscm/Rv4/dev_packages/")

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

# Cascade splines Prov --------------------------------------------------------

dpts <- config$pred$depts
theta_prov <- config$sensitivity$theta_prov
prev_batch_job_ids <- NULL
job_ids <- c() 

for (i in dpts) {
  current_batch_job_ids <- c() # Initialize for the current batch of i
  
  for (j in theta_prov) {
    # Replace spaces with underscores for job name
    i_clean <- gsub(" ", "_", i)
    jobname <- paste0("mrbrtcovid_pipeline_dpt_", i_clean, "prov", j)
    
    # Construct arguments as a single quoted string
    args <- sprintf("'%s' %s '%s' '%s'", i, j, output_dir, config_loc)
    rscript <- shQuote(paste0(getwd(), "/src/sensitivity/2_run_thetas.R"))
    singularity <- "-i /ihme/singularity-images/rstudio/ihme_rstudio_4222.img"
    rshell <- paste0("/ihme/singularity-images/rstudio/shells/execRscript.sh ", singularity, " -s")
    
    # Properly quote output file path
    output_file <- shQuote(paste0(output_dir, "/logs/", "%x_o%j", ".txt"))
    
    sys.sub <- paste(
      "sbatch --parsable -A proj_lsae",
      "-J", shQuote(jobname),
      "-o", output_file,
      "-c", config$sbatch$threads,
      "--mem", config$sbatch$memory,
      "-t", config$sbatch$time,
      "-p all.q"
    )
    
    # Add dependency if there are previous batch job IDs
    if (!is.null(prev_batch_job_ids) && length(prev_batch_job_ids) > 0) {
      dependency_string <- paste(prev_batch_job_ids, collapse=",")
      sys.sub <- paste(sys.sub, sprintf("--dependency=afterok:%s", dependency_string))
    }
    
    # Construct the final command
    command <- paste(sys.sub, rshell, rscript, args)
    print(paste("Submitting job (Prov =", j, "; Dpt =", i, "):", command))
    
    # Launch job and capture job ID
    job_id <- system(command, intern = TRUE)
    job_ids <- c(job_ids, job_id) # Store job ID in the array
    current_batch_job_ids <- c(current_batch_job_ids, job_id) # Store job ID
    
    # Debugging prints
    print("Constructed command:")
    print(command)
    print("Arguments passed to the R script:")
    print(args)
    print("Current batch job IDs:")
    print(current_batch_job_ids)
  }
  
  # Update previous batch job IDs for the next iteration
  prev_batch_job_ids <- current_batch_job_ids
  print("Previous batch job IDs updated:")
  print(prev_batch_job_ids)
}

print("All jobs submitted. Final job IDs:")
print(job_ids)

