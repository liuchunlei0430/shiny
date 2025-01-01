library(reshape2)
library(plyr)
library(tidyverse)
library(RColorBrewer)
library(gtools)
library(scales)

minmax <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

minmax2 <- function(x, new_min, new_max) {
  scaled_x <- (x - new_min) / (new_max - new_min)
  return(scaled_x)
}




