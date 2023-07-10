library(dplR)
library(tidyverse)
files <- list.files("../Crossdated complete/", full.names = TRUE)
raw_data <- lapply(files, function(x) { 
  dplR::read.rwl(x)  })
