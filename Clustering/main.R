library(dplR)
library(tidyverse)
files <- list.files("../Crossdated complete/", full.names = TRUE)
raw_data <- lapply(files, function(x) { 
  dplR::read.rwl(x)  })

detrended_data_collection <- lapply(detrended_data, function(x) {
  x %>% 
    rownames_to_column("Years") %>%  # First make a column from rownames 
    mutate(Years = as.numeric(Years)) }) %>% # Second transform it to numeric
  plyr::join_all() %>% # Use new column to stick all of the individual data frames together
  column_to_rownames("Years") # finally reverse the manipulation of the data frame in order to have rwl file again

library(dendextend) # for fancy dendrogram colouring

dd <- detrended_data_collection %>%
  t() %>% #transpose
  dist() %>% #create distance matrix
  hclust() %>% #algorithm tool
  as.dendrogram() #save as dendrogram

# 
cols <- as.factor(substr(colnames(detrended_data_collection),1,3))
labels_colors(dd) <- as.numeric(cols)[order.dendrogram(dd)]
dd <- dd %>% colour_branches(k=2) %>% set("branches_lwd", 2)

par(cex=0.8)
plot(dd, horiz=TRUE)

detrended_data_collection %>%
  select(KHH01, KHH03, KHH07, KHH08, KHH09, KHH18, KHH19, KHF18, KHF23) %>% 
  rownames_to_column("Years") %>%  # First make a column from rownames 
  mutate(Years = as.numeric(Years)) %>%
  pivot_longer(-Years) %>%
  ggplot(aes(x=Years, y=value, colour = name)) + geom_line()

detrended_spline_data <- lapply(raw_data, function(x) { # detrends all of the series individually with negative exponential function
  x %>%
    as.data.frame() %>%
    dplR::detrend(method="Spline") })

dd_spline <- as.dendrogram(hclust(dist(t(detrended_spline_data_collection))))

cols <- as.factor(substr(colnames(detrended_spline_data_collection),1,3))
labels_colors(dd_spline) <- as.numeric(cols)[order.dendrogram(dd_spline)]

dd_spline <- dd_spline %>% colour_branches(k=2) %>% set("branches_lwd", 2)

par(cex=0.8)
plot(dd_spline, horiz=TRUE)

dl <- dendlist(dd, dd_spline)
tanglegram(dl, sort = TRUE, common_subtrees_color_lines = TRUE,
           highlight_distinct_edges  = FALSE, highlight_branches_lwd = FALSE)

