library(tidyverse)
library(boot)
library(dplR)

script_series <- read.csv("chrono_metadata.csv")

ref_files <- list.files('../EU chronologies/Czech_RWL KR KH/', full.names = T)

# Krusne Hory  	
# KHF 	50.69747093 	13.5705516
# KHH 	50.3970285N 	12.7945259E
# KHM 	50.3883908N 	13.0036600E
# KHS  	50.5347674N 	13.2230808E
# 
# 
# Krokonose Mts
# KRKF 	50.726689 N 	15.62114E
# KRKT 	50.62878 N 	15.87939E
# KRKZ 	50.76396N  	15.45061E

ref_df <- data.frame(complete_filepath = ref_files,
                     Lat = c(50.69747093, 50.3970285, 50.3883908, 50.5347674,
                             50.726689, 50.62878, 50.76396),
                     Lon = c(13.5705516, 12.7945259, 13.0036600,13.2230808,
                             15.62114, 15.87939, 15.45061))

print(ref_df) # This has to be checked manually with the table above

ss <- script_series[-36,] %>% 
  mutate(complete_filepath = paste0('./EU chronologies/EU Preped Chron/',Filepath)) %>%
  filter(Lon < 40) %>%
  select(complete_filepath, Lat, Lon) %>%
  bind_rows(ref_df)

print(tail(ss))

set.seed(420)
drop_eval <- function(filepath, p1 = 1966:1975, p2 = 1976:1985, ...) {
  dat_chron <- read.rwl(filepath) %>% 
    dplR::detrend(...) %>%
    chron()
  chron_p1 <- dat_chron[as.character(c(p1)),'std']
  chron_p2 <- dat_chron[as.character(c(p2)),'std']
  
  med1 <- boot(chron_p1, function(x,i) { median(x[i], na.rm=TRUE)}, R=2000)
  med2 <- boot(chron_p2, function(x,i) { median(x[i], na.rm=TRUE)}, R=2000)
  perc1 <- boot.ci(med1, type="perc")$percent
  perc2 <- boot.ci(med2, type='perc')$percent
  if(perc2[[5]] < perc1[[4]]) { 
    outcome <- TRUE 
  } else {
    outcome <- FALSE
  }
  
  return(outcome)
}


drop_eval_ser <- function(filepaths = ss$complete_filepath, dtr = 'ModNegExp'){
  map_lgl(filepaths, drop_eval, method=dtr)
}

ss <- ss %>% mutate(eval = drop_eval_ser(complete_filepath))


### Base map ###
library(ggthemes)
library(rgdal)
library(ggmap)
library(maptools)
nat <- readOGR("ne_50m_admin_0_countries.geojson")
nat_map <- fortify(nat)

library(raster)
theme_map2 <- function (base_size = 9, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(panel.background = element_blank(), 
          panel.border = element_blank(), 
          panel.spacing = unit(0, "lines"), 
          panel.grid = element_blank(),
          plot.background = element_blank(), 
          #          legend.justification = c(0, 0), 
          legend.position = c(0.9,0.1),
          axis.text = element_text(size=12),
          axis.title = element_text(size=16),
          axis.ticks = element_blank())
}

black_triangle <- data.frame(lon = c(14.5639, 12.5264, 16.9609), lat=c(51.489, 49.9705, 50.126)) %>% 
  mutate(lonend = case_when(is.na(lead(lon)) ~ first(lon),
                            TRUE ~ lead(lon))) %>%
  mutate(latend = case_when(is.na(lead(lat)) ~ first(lat),
                            TRUE ~ lead(lat)))

if(!file.exists('./GRAY_HR_SR_OB_DR/GRAY_HR_SR_OB_DR.tif')) {
  temp <- tempfile()
  download.file('https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/raster/GRAY_HR_SR_OB_DR.zip', temp)
  unzip(temp, exdir = './GRAY_HR_SR_OB_DR')
}

raster('./GRAY_HR_SR_OB_DR/GRAY_HR_SR_OB_DR.tif') %>%
  crop(extent(3, 25, 43, 55)) %>% as.data.frame(xy=T) %>% ggplot() + geom_raster(aes(x=x, y=y, fill=GRAY_HR_SR_OB_DR)) +
  geom_map(data=nat_map, map=nat_map, aes(map_id=id), fill='#ddddf000', color='black', size=0.25) +
  theme_map2() + coord_equal(ratio=1.5) +
  geom_segment(aes(x=lon, y=lat, xend=lonend, yend=latend), colour='black', size=2, data=black_triangle, inherit.aes = FALSE) +

  scale_x_continuous(limits=lon_lim, expand = c(0, 0), breaks=c(5,10,15,20,25,30)) +
  scale_y_continuous(limits=lat_lim, expand = c(0, 0), position="left") +
  scale_fill_gradient(low='white',high='grey40', guide=FALSE) +
  xlab("") + ylab("") + theme(axis.text = element_blank()) +
  geom_point(mapping= aes(x=Lon, y=Lat, colour = Growth), inherit.aes=FALSE, data = ss %>% filter(Lon < 40) %>% arrange(eval) %>%
               mutate(Growth = case_when(eval ~ "Reduced", TRUE ~ "Not reduced"))) +
  scale_color_manual(values=c('blue', 'red')) +
  guides(color = guide_legend(title.position='top')) +
  theme(legend.background = element_rect(colour='black', size=1),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))

ch <- map(ss$complete_filepath, function(x) { fn <- read.rwl(x); fn_dt <- dplR::detrend(fn, method='ModNegExp'); ch <- chron(fn_dt); ch %>% mutate(filename = x) %>% rownames_to_column(var='Year') %>% mutate(Year = as.numeric(Year)) %>% return() })

ch_dat <- ch %>% bind_rows() %>% left_join(ss %>% filter(Lon < 40), by=c('filename' = 'complete_filepath')) %>% filter(Year > 1960) 

gg_detail <- ch_dat[grepl('KRKT|czec006|czec007|czec012|czec014|czec015', ch_dat$filename),] %>%
#  ch_dat %>%
  ggplot(aes(x=Year, y=std, group = filename)) + 
  geom_line(alpha=0.7) + 
  facet_wrap(~eval, ncol=1, 
             labeller = function(variable, value) { ifelse(as.logical(value), 'Series with 1980s drop', 'Series without 1980s drop')} )+
  theme_bw() + 
  ylab('Tree Ring Width Index')

colnames(ch_dat)
ch_dat_report <- ch_dat %>% group_by(filename, Lat, Lon) %>% summarize(response = mean(eval),
                                                                       n = max(samp.depth, na.rm=T)) %>% 
  mutate(response = ifelse(response == 1, 'Yes', 'No'))

write_csv(ch_dat_report, 'table_st1_growth_drop_response.csv')

