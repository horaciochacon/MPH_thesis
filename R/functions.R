
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

get_peak_province <- function(x, threshold) {
  peak <- vector()
  for (i in 2:length(x)) {
    if (x[i] > threshold) {
      peak[i] <- x[i] > x[i-1]  & x[i] >= x[i+1] &
        x[i] > x[i-2]  & x[i] >= x[i+2]
    } else{
      peak[i] <- NA
    }
  }
  peak
}


get_peak_province <- function(x, min_threshold, variation_threshold) {
  peak <- c(TRUE, TRUE, rep(NA, 557), FALSE)
  nadir <- c(TRUE, TRUE, rep(NA, 557), FALSE)
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
  }
  peak
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
