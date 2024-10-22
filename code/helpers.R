##########################
# library helper functions
##########################

## set number of bootstrap samples
n_bootstrap_samples <- 100



## library packages
library(tidyverse)
library(data.table)
library(gt)
library(janitor)
library(here)
library(sf)
library(raster)
library(exactextractr)
library(autumn)
library(cowplot)
library(caret) # For pre-processing
library(psych) # For PCA
library(viridis)
library(ggthemes)
library(xtable)
library(srvyr)
library(networksurvival)
library(kableExtra)

## white background instead of transparent
ggsave <- function(filename, plot = last_plot(), ..., dpi = 300, bg = 'white') {
  ggplot2::ggsave(filename, plot, ..., dpi = dpi, bg = bg)
}


## custom color schemes
cudb <- c("#49b7fc", "#ff7b00", "#17d898", "#ff0083", "#0015ff", "#e5d200", "#999999")
cud <- c("#D55E00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2", "#E69F00", "#F0E442", "#999999")
