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
py_save_object(mod_nat, paste0(output_dir,"/", "sensitivity/mod_nat.pickle"), pickle = "dill")
saveRDS(data_prov, paste0(output_dir,"/","sensitivity/data_prov.rds"))

# Cascade splines Prov --------------------------------------------------------

theta_dpt <- config$sensitivity$theta_dpt
theta_prov <- config$sensitivity$theta_prov
prev_batch_job_ids <- NULL
job_ids <- c() 

for (i in theta_dpt) {
  current_batch_job_ids <- c() # Initialize for the current batch of i
  
  for (j in theta_prov) {
    args <- c(i, j, output_dir, config_loc) 
    rscript <- paste0(getwd(), "/src/XX_Run_Thetas.R") 
    rshell <- "/ihme/singularity-images/rstudio/shells/execRscript.sh -s"
    jobname <- paste0("mrbrtcovid_pipeline_dpt_", i, "prov", j)
    
    sys.sub <- paste0(
      "sbatch --parsable -A proj_lsae ", 
      " -J ", jobname,
      " -o ", paste0(output_dir,"/logs/","%x_o%j",".txt"),
      " -c ", config$sbatch$threads, 
      " --mem ", config$sbatch$memory,
      " -t ", config$sbatch$time, 
      " -p all.q"
    )
    
    # Add dependency if there are previous batch job IDs
    if (!is.null(prev_batch_job_ids)) {
      dependency_string <- paste(prev_batch_job_ids, collapse=":")
      sys.sub <- paste0(sys.sub, " --dependency=afterok:", dependency_string)
    }
    
    command <- paste(sys.sub, rshell, rscript, paste(args, collapse = " "))
    print(paste("Submitting job (Prov =", j, "; Dpt =", i,"):", command))
    # Launch job and capture job ID
    job_id <- system(command, intern = TRUE)
    job_ids <- c(job_ids, job_id) # Store job ID in the array
    current_batch_job_ids <- c(current_batch_job_ids, job_id) # Store job ID
  }
  
  # Update previous batch job IDs for the next iteration
  prev_batch_job_ids <- current_batch_job_ids
}
