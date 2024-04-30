library(performance)
library(ggplot2)
library(mgcv)
library(gstat)
library(gratia)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Upload data

RWI_poll <- read.csv("RWI_pollution_data.csv")
RWI_poll$Country <- as.factor(RWI_poll$Country)
RWI_poll$Number <- as.factor(RWI_poll$Number)

# Switzerland removed because no growth decline:
gam1 <- gam(RWI ~ s(Year, bs = "cr", by = Country) , method = "REML",correlation = corAR1(form = ~Year|Number), data = RWI_poll) 
plot(gam1)
RWI_pollnoCH <- RWI_poll[RWI_poll$Country!="Switzerland",] # Exclude CH as RWIs do not show any reductions (and therefore they cannot be explained by deposition)

## GAMM RWI - pollution

gam1 <- gam(RWI ~ s(TDEP_SOX, bs = "cr") + s(TDEP_NOY, bs = "cr") + s(TDEP_NHX, bs = "cr"), method = "REML", data = RWI_pollnoCH) 

acf(residuals(gam1)) # Temporal autocorrelation, include autocorrelation structure of order 1:  

gam1 <- gamm(RWI ~ s(TDEP_SOX, bs = "cr") + s(TDEP_NOY, bs = "cr") + s(TDEP_NHX, bs = "cr"), method = "REML", correlation = corAR1(form = ~Year|Number), data = RWI_pollnoCH) 

plot(ACF(gam1$lme,maxLag=25,resType="normalized")) # No autocorrelation anymore

check_collinearity(gam1) # Moderate correlation TDEP_NHX (VIF 6.37) -> exclude

# Final model:
gam1 <- gamm(RWI ~ s(TDEP_SOX, bs = "cr") + s(TDEP_NOY, bs = "cr"), method = "REML",  correlation = corAR1(form = ~Year|Number), data = RWI_pollnoCH) 

# Check spatial autocorrelation residuals
RWI_pollnoCH$gam1_residuals <- residuals(gam1$gam)
ggplot(RWI_pollnoCH, aes(Longitude, Latitude, colour = gam1_residuals)) +
  scale_color_gradient2() +
  geom_point(size = 3)  
var_resid_gam <- variogram(gam1_residuals~1, loc= ~Latitude+Longitude, data=RWI_pollnoCH)
plot(var_resid_gam)

plot(ACF(gam1$lme,maxLag=25,resType="normalized")) 

check_collinearity(gam1) # Check collinearity

# Diagnostic plots
appraise(gam1$gam) 

# Model results
summary(gam1$lme) # details of underlying lme fit
summary(gam1$gam) # gam style summary of fitted model

# Figure S6
draw(gam1) & theme_bw() 
