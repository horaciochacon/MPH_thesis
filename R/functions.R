
# Peak functions ----------------------------------------------------------

get_peaks <- function(x) {
  r <- emd(x)
  y <- apply( r$imf[,5:6], 1, sum ) + mean(r$residue)
  plot(x, type="l", col="grey")
  lines( y, type="l", lwd=2)
  n <- length(y)
  i <- y[2:(n-1)] > y[1:(n-2)] & y[2:(n-1)] > y[3:n]
  points( which(i), y[i], pch=15 )
}

get_peaks_first <- function(x) {
  r <- emd(x)
  y <- apply( r$imf[,5:6], 1, sum ) + mean(r$residue)
  n <- length(y)
  i <- y[2:(n-1)] > y[1:(n-2)] & y[2:(n-1)] > y[3:n]
  which(i)[1]
}


get_peak <- function(x) {
  peak <- vector()
  for (i in 1:length(x)) {
    if (x[i] > 10) {
      peak[i] <- x[i] > x[i-1]  & x[i] > x[i+1]
    }
  }
  peak
}


get_peak_province <- function(x, min_threshold, variation_threshold) {
  peak <- c(TRUE, TRUE, rep(NA, 482), FALSE, FALSE)
  nadir <- c(TRUE, TRUE, rep(NA, 482), FALSE,  FALSE)
  for (i in 1:(length(x)-1)) {
    if (x[i] > min_threshold ) {
      peak[i] <- x[i] > x[i-1]  & x[i] >= x[i+1] &
        x[i] > x[i-2]  & x[i] >= x[i+2]
      
      nadir[i] <- x[i] < x[i-1]  & x[i] <= x[i+1] &
        x[i] < x[i-2]  & x[i] <= x[i+2]
    } else{
      peak[i] <- FALSE
    } 
    if(peak[i] & (x[i] <=
                  (x[which(nadir)][length(which(nadir))] +
                   variation_threshold))) {
      peak[i] <- FALSE
    }
    if(is.na(peak[i])) {
      peak[i] <- FALSE
    }
  }
  peak
}

library(zoo)

get_peaks_general <- function(x, window_size = 5, smoothing = TRUE, 
                              min_threshold = NULL, prominence_threshold = NULL) {
  n <- length(x)
  
  # Apply smoothing if requested
  if (smoothing) {
    x_smooth <- rollmean(x, k = window_size, fill = "extend")
  } else {
    x_smooth <- x
  }
  
  # Calculate gradient
  grad <- c(0, diff(x_smooth))
  
  # Identify potential peaks
  peak_candidates <- which(
    (grad[1:(n-1)] > 0 & grad[2:n] <= 0) |  # Local maxima
      (grad[1:(n-1)] == 0 & grad[2:n] < 0)    # Plateau followed by decrease
  )
  
  # Apply minimum threshold if specified
  if (!is.null(min_threshold)) {
    peak_candidates <- peak_candidates[x_smooth[peak_candidates] > min_threshold]
  }
  
  # Calculate prominence if threshold is specified
  if (!is.null(prominence_threshold)) {
    prominences <- sapply(peak_candidates, function(i) {
      left_min <- min(x_smooth[max(1, i - window_size):i])
      right_min <- min(x_smooth[i:min(n, i + window_size)])
      return(x_smooth[i] - max(left_min, right_min))
    })
    peak_candidates <- peak_candidates[prominences > prominence_threshold]
  }
  
  # Create logical vector
  peaks <- logical(n)
  peaks[peak_candidates] <- TRUE
  
  return(peaks)
}

get_peaks_epidemic_scalable <- function(x, 
                                        window_size = 5, 
                                        smoothing = TRUE, 
                                        min_threshold = NULL, 
                                        min_peak_height_pct = NULL, 
                                        min_peak_prominence_pct = NULL,
                                        down_trend_pct = NULL) {
  n <- length(x)
  
  if (smoothing) {
    x_smooth <- rollmean(x, k = window_size, fill = "extend")
  } else {
    x_smooth <- x
  }
  
  peak_candidates <- c()
  for (i in (window_size + 1):(n - window_size)) {
    if (x_smooth[i] == max(x_smooth[(i - window_size):(i + window_size)])) {
      peak_candidates <- c(peak_candidates, i)
    }
  }
  
  valid_peaks <- c()
  max_value <- max(x_smooth)
  
  for (peak in peak_candidates) {
    # Check if the peak meets the minimum threshold
    if (!is.null(min_threshold) && x_smooth[peak] <= min_threshold) {
      next
    }
    
    left_min <- min(x_smooth[max(1, peak - window_size):peak])
    right_min <- min(x_smooth[peak:min(n, peak + window_size)])
    peak_height <- x_smooth[peak] - min(left_min, right_min)
    
    # Check relative peak height
    if (!is.null(min_peak_height_pct) && (peak_height / max_value < min_peak_height_pct)) {
      next
    }
    
    # Check relative peak prominence
    peak_prominence <- x_smooth[peak] - max(left_min, right_min)
    if (!is.null(min_peak_prominence_pct) && (peak_prominence / max_value < min_peak_prominence_pct)) {
      next
    }
    
    # Check if there's a significant down trend after the peak
    if (!is.null(down_trend_pct)) {
      post_peak_min <- min(x_smooth[peak:min(n, peak + window_size)])
      if ((x_smooth[peak] - post_peak_min) / x_smooth[peak] < down_trend_pct) {
        next
      }
    }
    
    valid_peaks <- c(valid_peaks, peak)
  }
  
  peaks <- logical(n)
  peaks[valid_peaks] <- TRUE
  
  return(peaks)
}

# Plotting functions ------------------------------------------------------

geom_gam_dates <- function() {
    geom_label_repel(
      aes(x = x, y = y, label = label),
      nudge_y = 150,
      data = labels,
      arrow = arrow(length = unit(0.015, "npc"))
    )
}

geom_covid_gam <- function(k, title, label_df, nudge_y, ...) {
  list(
    geom_line(alpha = 0.5),
    geom_smooth(method = "gam", formula = y ~ s(x, k = k),
                method.args = list(family = "quasipoisson")),
    geom_label_repel(
      aes(x = x, y = y, label = label),
      nudge_y = nudge_y,
      data = label_df,
      arrow = arrow(length = unit(0.015, "npc"))
    ),
    labs(title = title, ...)
  )
}


# ggplot theme ------------------------------------------------------------

goldenScatterCAtheme <- theme(
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill = "white"),
  aspect.ratio = ((1 + sqrt(5))/2)^(-1),
  axis.ticks.length = unit(0.5, "char"),
  axis.line.x.top = element_line(size = 0.2),
  axis.line.x.bottom = element_line(size = 0.2), 
  axis.ticks.x = element_line(size = 0.2), 
  axis.text.x = element_text(color = "black", size = 12),
  axis.title.x = element_text(size = 14, margin = margin(t = 7.5, r = 0, b = 0, l = 0)), 
  axis.ticks.y = element_blank(),
  axis.text.y = element_text(color = "black", size = 12, margin = margin(t = 0, r = -4, b = 0, l = 0)),
  axis.title.y = element_text(size = 10,margin = margin(t = 0, r = 7.5, b = 0)),
  legend.key = element_rect(fill = NA, color = NA),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "gray45", size = 0.2),
  strip.background = element_blank(),
  strip.text.x = element_text(size=12), 
  strip.text.y = element_blank(), 
  strip.placement = "outside", 
  panel.spacing.x = unit(1.25, "lines"), 
  panel.spacing.y = unit(1, "lines")
)

goldenScatter <- theme(
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill = "white"),
  aspect.ratio = ((1 + sqrt(5))/2)^(-1),
  axis.ticks.length = unit(0.5, "char"),
  axis.line.x.top = element_line(size = 0.2),
  axis.line.x.bottom = element_line(size = 0.2), 
  axis.ticks.x = element_line(size = 0.2), 
  axis.text.x = element_text(color = "black", size = 12),
  axis.title.x = element_text(size = 14, margin = margin(t = 7.5, r = 0, b = 0, l = 0)), 
  axis.ticks.y = element_blank(),
  axis.text.y = element_text(color = "black", size = 12, margin = margin(t = 0, r = -4, b = 0, l = 0)),
  axis.title.y = element_text(size = 14,margin = margin(t = 0, r = 7.5, b = 0)),
  legend.key = element_rect(fill = NA, color = NA),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "gray45", size = 0.2),
  strip.background = element_blank(),
  strip.text.x = element_text(size=12), 
  strip.text.y = element_blank(), 
  strip.placement = "outside", 
  panel.spacing.x = unit(1.25, "lines"), 
  panel.spacing.y = unit(1, "lines")
)

goldenScatterbp <- theme(
  plot.title = element_text(hjust = 0.5),
  panel.background = element_rect(fill = "white"),
  aspect.ratio = ((1 + sqrt(5))/2)^(-1),
  axis.ticks.length = unit(0.5, "char"),
  axis.line.x.top = element_line(size = 0.2),
  axis.line.x.bottom = element_line(size = 0.2), 
  axis.ticks.x = element_line(size = 0.2), 
  axis.text.x = element_blank(),
  axis.title.x = element_blank(), 
  axis.ticks.y = element_blank(),
  axis.text.y = element_text(color = "black", size = 12, margin = margin(t = 0, r = -4, b = 0, l = 0)),
  axis.title.y = element_text(size = 18,margin = margin(t = 0, r = 7.5, b = 0)),
  legend.key = element_rect(fill = NA, color = NA),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "gray45", size = 0.2),
  strip.background = element_blank(),
  strip.text.x = element_text(size=12), 
  strip.text.y = element_blank(), 
  strip.placement = "outside", 
  panel.spacing.x = unit(1.25, "lines"), 
  panel.spacing.y = unit(1, "lines"),
  plot.caption = element_text(size = 12)
)


theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}


# Add UI base plot --------------------------------------------------------

add_ui <- function(dat, x_var, lo_var, hi_var, color = "darkblue", opacity = 0.2) {
  polygon(
    x = c(dat[, x_var], rev(dat[, x_var])),
    y = c(dat[, lo_var], rev(dat[, hi_var])),
    col = adjustcolor(col = color, alpha.f = opacity), border = FALSE
  )
}


# Create peak panel ---------------------------------------------------------------------------

create_department_panel_plots <- function(
    config, prov_mort, data_prov, dpt_mort, output_dir = NULL
    ) {
  for (dept in config$pred$depts) {
    # Get provinces for this department
    dept_provs <- unique(data_prov[dpt_cdc == dept]$prov_cdc)
    n_provs <- length(dept_provs)
    
    # y max for ylim
    ymax <- max(
      c(
        prov_mort[dpt_cdc == dept]$mort * 1e5,
        data_prov[dpt_cdc == dept]$y1 * 1e5,
        dpt_mort[dpt_cdc == dept]$mort * 1e5
      )
    ) * 1.05
    
    if (n_provs == 0) {
      warning(paste("No provinces found for department:", dept))
      next
    }
    
    # Set up the panel layout
    n_cols <- min(4, n_provs)
    n_rows <- ceiling(n_provs / n_cols) + 1  # +1 for department plot
    
    # Define file name and path if output directory is provided
    if (!is.null(output_dir)) {
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      file_path <- file.path(output_dir, paste0("Department_", dept, ".pdf"))
      pdf(file = file_path, width = 5 * n_cols, height = 4 * n_rows)
    } else {
      dev.new(width = 5 * n_cols, height = 4 * n_rows)
    }
    
    # Adjust margins and layout
    layout_matrix <- matrix(
      c(rep(1, n_cols), 2:(n_provs + 1)), ncol = n_cols, byrow = TRUE)
    layout_matrix[n_rows, layout_matrix[n_rows,] == 1] <- max(2:(n_provs + 1))
    layout(layout_matrix)
    
    par(oma = c(2, 2, 3, 1))
    
    # Plot department-level data
    dept_data <- dpt_mort[dpt_cdc == dept]
    plot(
      dept_data$x1,
      dept_data$mort * 1e5,lwd = 2,
      type = "l",
      xlab = "", ylab = "",
      cex.main = 1,
      ylim = c(0, ymax)
    )
    
    # Add observed data points for department
    obs_dept_data <- data_prov[dpt_cdc == dept]
    points(
      obs_dept_data$x1,
      obs_dept_data$y1 * 1e5,
      col = "blue", pch = 19, cex = 0.8
    )
    
    # Detect peaks for department
    dept_peaks <- get_peaks_epidemic_scalable(
      dept_data$mort * 1e5,
      window_size = 25,
      min_threshold = 5,
      min_peak_height_pct = 0.02,
      min_peak_prominence_pct = 0.01
    )
    
    if (any(dept_peaks)) {
      points(
        dept_data$x1[dept_peaks],
        dept_data$mort[dept_peaks] * 1e5,
        col = "red", pch = 19, cex = 2
      )
    }
    
    # Plot province-level data
    for (i in dept_provs) {
      mort_data <- prov_mort[prov_cdc == i]$mort * 1e5
      x_data <- prov_mort[prov_cdc == i]$x1
      
      # Check if we have valid data to plot
      if (length(mort_data) == 0 || length(x_data) == 0 || all(is.na(mort_data)) || all(is.na(x_data))) {
        plot.new()
        title(main = paste(i, "- No Data"), cex.main = 0.8)
        next
      }
      
      # Remove NA values
      valid_indices <- !is.na(mort_data) & !is.na(x_data)
      mort_data <- mort_data[valid_indices]
      x_data <- x_data[valid_indices]
      
      plot(
        x_data, 
        mort_data, lwd = 2,
        type = "l",
        xlab = "", ylab = "",
        main = i,
        cex.main = 0.8,
        ylim = c(0, ymax)
      )
      
      obs_data <- data_prov[prov_cdc == i]
      if (nrow(obs_data) > 0) {
        points(
          obs_data$x1,
          obs_data$y1 * 1e5, 
          col = "blue", pch = 19, cex = 0.8
        )
      }
      
      peaks <- get_peaks_epidemic_scalable(
        mort_data,
        window_size = 25,
        min_threshold = 5,
        min_peak_height_pct = 0.02,
        min_peak_prominence_pct = 0.01
      )
      
      if (any(peaks)) {
        points(
          x_data[peaks], 
          mort_data[peaks], 
          col = "red", pch = 19, cex = 2
        )
      }
    }
    
    # Add overall x and y labels
    mtext("Week", side = 1, outer = TRUE, line = 0.5)
    mtext("Mortality", side = 2, outer = TRUE, line = 0.5)
    
    # Add an overall title for the department
    mtext(paste("Department:", dept), outer = TRUE, cex = 1.2, line = 1)
    
    # Close the PDF device if output directory is provided
    if (!is.null(output_dir)) {
      dev.off()
    }
    
  }
}
