#### Logistic regression all sites

library(doBy)
library(afex)
library(effects)
library(latticeExtra)
library(car)
library(MuMIn)
library(r2glmm)
library(ggplot2)
library(export)
library(DescTools)

data <- read.csv("Data.csv",fileEncoding='latin1')

# Select best model with dredge (AIC) 
start_time <- Sys.time()
options(na.action = na.fail) 
fit=glm(reduction ~ SO2_1990 + NOX_1990 + Latitude + Longitude + pH_5.15 + cec_5.15 + Elevation + PM10_1990,family=binomial(link=logit), data=data)
AIC(fit)
dd <- dredge(fit, rank="AIC", trace = 2)
dd
subset(dd, delta < 4)
options(na.action = "na.omit") 
end_time <- Sys.time()
end_time - start_time 
# Best model: 
fit=glm(reduction ~ SO2_1990 + NOX_1990 + PM10_1990 + cec_5.15 + Longitude,family=binomial(link=logit),data=data)
vif(fit) # test multicollinearity (variables highly correlated with each other). If VIF > 5 remove variable
# multicollinearity so PM10_1990 + cec_5.15 + Longitude removed.
# Best model without multicollinearity: 
fit=glm(reduction ~ SO2_1990 + NOX_1990,family=binomial(link=logit),data=data)

AIC(fit)
summary(fit) 

# Check spatial autocorrelation
data$m_glm_residuals <- residuals(fit)
ggplot(data, aes(Longitude, Latitude, colour = m_glm_residuals)) +
  scale_color_gradient2() +
  geom_point(size = 3)

# Plot
plot(allEffects(mod=fit),rescale.axis=F,ylim=c(0,1), ylab="Probability growth reduction (%)",main="" )
graph2ppt() 

# r2

PseudoR2(fit, which = "all")

#Expected increase in odds of growth reduction for each SO2/NOx change
exp(cbind(OR=coef(fit), confint(fit,level=0.95))) 

# Dispersion
fit2=glm(reduction ~ SO2_1990 + NOX_1990,family=quasibinomial,data=data)
summary(fit2)

residualPlots(fit)

# Outliers and influential observations 

outlierTest(fit)
influenceIndexPlot(fit, vars=c("Studentized","Bonf"))
cd=cooks.distance(fit)
influenceIndexPlot(fit, vars="Cook")

