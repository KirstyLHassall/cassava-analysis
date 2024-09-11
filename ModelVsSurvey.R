
# ModelVsSurvey

# A comparison of the observed cassava production vs modelled cassava production (from CassavaMap) across buffered locations in Cote d'Ivoire and Uganda incorporating population and settlement information.

# To investigate the impact of the spatial resolution of cassava production and harvested area of CassavaMap predictions along with any potential biases associated with settlement and population density in the surveyed locations, a systematic regression framework was used for six response variables: total cassava density, total cassava density under monoculture, total cassava density under intercropping and their associated weighted versions. Firstly, to understand the spatial representativeness of CassavaMap, rather than considering the point predictions as an explanatory variable, the extracted aggregated summaries for predicted cassava production density, were each considered in turn. The form of the regression model was constrained to one of four types, 1) a linear relationship, 2) a logarithmic relationship, 3) a quadratic relationship and 4) a non-parametric spline. Secondly, a measure of population density was included (in addition to the measure of predicted cassava) through one of the extracted aggregated summaries as listed in Supplementary Table 1. The population density variable was constrained to one of four relationships in the model, 1) linear, 2) logarithmic 3) independent non-parametric spline or 4) dependent 2-d non-parametric spline with predicted cassava. Thirdly, a measure of settlement density was included (in addition to the measure of predicted cassava) through one of the extracted aggregated summaries as listed in Supplementary Table 1. The settlement density variable was constrained to one of four relationships in the model, 1) linear, 2) logarithmic 3) independent non-parametric spline or 4) dependent 2-d non-parametric spline with predicted cassava. Finally, we considered including measures of both population and settlement density in the model through the relationships described above and an additional 2-d non-parametric spline over both variables. 


library(ggplot2)
library(ggcorrplot)
library(mgcv)
library(data.table)
library(car)
library(predictmeans)

rm(list=ls())

# read in processed data. DOI 10.6084/m9.figshare.26983603
dat_civ <- read.csv("dat_civ.csv")
dat_uga <- read.csv("dat_uga.csv")

# read in extracted raster data. DOI 10.6084/m9.figshare.26983603
CIV_buff_pointstats <- read.csv("CIV_buff_pointstats.csv")
UGA_buff_pointstats <- read.csv("UGA_buff_pointstats.csv")

# merge data
dat_civ$Centre_Lon <- round(dat_civ$Centre_Lon, 7)
dat_civ$Centre_Lat <- round(dat_civ$Centre_Lat, 7)
CIV_buff_pointstats$Centre_Lon <- round(CIV_buff_pointstats$Centre_Lon, 7)
CIV_buff_pointstats$Centre_Lat <- round(CIV_buff_pointstats$Centre_Lat, 7)
CIVdata <- merge(dat_civ, CIV_buff_pointstats, by=c("Centre_Lon", "Centre_Lat"), all.x=TRUE, all.y=TRUE)

dat_uga$Centre_Lon <- round(dat_uga$Centre_Lon, 7)
dat_uga$Centre_Lat <- round(dat_uga$Centre_Lat, 7)
UGA_buff_pointstats$Centre_Lon <- round(UGA_buff_pointstats$Centre_Lon, 7)
UGA_buff_pointstats$Centre_Lat <- round(UGA_buff_pointstats$Centre_Lat, 7)
UGAdata <- merge(dat_uga, UGA_buff_pointstats, by=c("Centre_Lon", "Centre_Lat"), all.x=TRUE, all.y=TRUE)

# create subset of presence data only
# i.e. remove zeros
CIVdata_pres <- CIVdata[CIVdata$tot_cassava_area >0, ] 
UGAdata_pres <- UGAdata[UGAdata$tot_cassava_area >0, ] 


# response variables
res_names <- c("tot_cassava_area", "tot_monoculture_area", "tot_intercrop_area", "tot_area_ind_plants", "tot_cassava_area_w","tot_monoculture_area_w", "tot_intercrop_area_w")

# explanatory variables from survey
xsurvey_names <- c("tot_buildings")

# explanatory variables for topography
xtopo_names <- c("Centre_Lon","Centre_Lat")

# explanatory variables for modelled cassava
xmodel_names <- c("Cass_Prod",
                  "mean_Prod.2000","mean_Prod.5000","mean_Prod.10000",
                  "sd_Prod.2000","sd_Prod.5000","sd_Prod.10000",
                  "min_Prod.2000" ,"lowerQ_Prod.2000","median_Prod.2000","upperQ_Prod.2000","max_Prod.2000",
                  "min_Prod.5000","lowerQ_Prod.5000","median_Prod.5000","upperQ_Prod.5000","max_Prod.5000",
                  "lowerQ_Prod.10000","median_Prod.10000","upperQ_Prod.10000","max_Prod.10000",
                  "Cass_HA",
                  "mean_Harv.2000","mean_Harv.5000","mean_Harv.10000",
                  "sd_Harv.2000","sd_Harv.5000","sd_Harv.10000",
                  "min_Harv.2000","lowerQ_Harv.2000","median_Harv.2000","upperQ_Harv.2000","max_Harv.2000",
                  "min_Harv.5000","lowerQ_Harv.5000","median_Harv.5000","upperQ_Harv.5000","max_Harv.5000",
                  "lowerQ_Harv.10000","median_Harv.10000","upperQ_Harv.10000","max_Harv.10000")

# explanatory variables for population layer
xpop_names <- c("Population",
                "mean_lspop.2000","mean_lspop.5000","mean_lspop.10000",
                "sd_lspop.2000","sd_lspop.5000","sd_lspop.10000",
                "min_lspop.2000" ,"lowerQ_lspop.2000","median_lspop.2000","upperQ_lspop.2000","max_lspop.2000",
                "lowerQ_lspop.5000","median_lspop.5000","upperQ_lspop.5000","max_lspop.5000",
                "lowerQ_lspop.10000","median_lspop.10000","upperQ_lspop.10000","max_lspop.10000")

# explanatory variables for settlement layer
xsettle_names <- c("mean_settle.2000","mean_settle.5000","mean_settle.10000")





# Correlations ----------

# using robust measures of correlation to get ranks as relationships may well not be linear

# survey data
ggcorrplot(cor(CIVdata[, c(res_names)], use="pairwise", method="spearman"), type="upper", title="Ivory Coast")
ggcorrplot(cor(UGAdata[, c(res_names)], use="pairwise", method="spearman"), type="upper", title="Uganda")

ggcorrplot(cor(CIVdata_pres[, c(res_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata[, c("tot_cassava_area", xsurvey_names, xtopo_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata_pres[, c("tot_cassava_area", xsurvey_names, xtopo_names)], use="pairwise", method="spearman"))

# observed data to model and covariate layers
ggcorrplot(cor(CIVdata[, c("tot_cassava_area", xmodel_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata[, c("tot_cassava_area", xpop_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata[, c("tot_cassava_area", xsettle_names)], use="pairwise", method="spearman"))

ggcorrplot(cor(CIVdata_pres[, c("tot_cassava_area", xmodel_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata_pres[, c("tot_cassava_area", xpop_names)], use="pairwise", method="spearman")) # gets stronger having removed zeros
ggcorrplot(cor(CIVdata_pres[, c("tot_cassava_area", xsettle_names)], use="pairwise", method="spearman")) # gets stronger having removed zeros

# are population and settlement layers correlated?
ggcorrplot(cor(CIVdata_pres[, c(xsettle_names, xpop_names)], use="pairwise", method="spearman"))


# modelled data to covariate layers
ggcorrplot(cor(CIVdata[, c("Cass_Prod", xpop_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata[, c("mean_Prod.2000","mean_Prod.5000","mean_Prod.10000", xpop_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata[, c("max_Prod.2000","max_Prod.5000","max_Prod.10000", xpop_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata[, c("min_Prod.2000","min_Prod.5000","min_Prod.10000", xpop_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata[, c("lowerQ_Prod.2000","lowerQ_Prod.5000","lowerQ_Prod.10000", xpop_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata[, c("upperQ_Prod.2000","upperQ_Prod.5000","upperQ_Prod.10000", xpop_names)], use="pairwise", method="spearman"))

ggcorrplot(cor(CIVdata[, c("Cass_Prod", xsettle_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata[, c("mean_Prod.2000","mean_Prod.5000","mean_Prod.10000", xsettle_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata[, c("max_Prod.2000","max_Prod.5000","max_Prod.10000", xsettle_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata[, c("min_Prod.2000","min_Prod.5000","min_Prod.10000", xsettle_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata[, c("lowerQ_Prod.2000","lowerQ_Prod.5000","lowerQ_Prod.10000", xsettle_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata[, c("upperQ_Prod.2000","upperQ_Prod.5000","upperQ_Prod.10000", xsettle_names)], use="pairwise", method="spearman"))



# the different data layers
ggcorrplot(cor(CIVdata[, c(xmodel_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata[, c(xpop_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(CIVdata[, c(xsettle_names)], use="pairwise", method="spearman"))

ggcorrplot(cor(UGAdata[, c(xmodel_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(UGAdata[, c(xpop_names)], use="pairwise", method="spearman"))
ggcorrplot(cor(UGAdata[, c(xsettle_names)], use="pairwise", method="spearman"))





# Modelling --------

# Model fitting strategy 
# Step 1: response ~ diff modelled values


# *******************
# Step 1: response ~ diff modelled values
# Step 2: Response ~ Model x Population
# Step 3: Response ~ Model x Settlement
# Step 4: Response ~ Model x Population x Settlement
# Step 5: loop over response variables


# Function to fit regression models with a single explanatory variable
# 4 models are considered, simple linear regression, a log transformation of the explanatory variable, a quadratic model and a spline model.
# function outputs details of the model fitted and model summaries
fitModel <- function(x_name, y_name, dat){
  
  dat$x <- dat[, x_name]
  dat$y <- dat[, y_name]
  if (min(dat$x, na.rm=TRUE) <=0 ){
    c <- min(dat$x[dat$x > 0], na.rm=TRUE) / 2
  } else {
    c <- 0
  }
  
  m1 <- lm(y ~ x, data=dat)
  m2 <- lm(y ~ log(x + c), data=dat)
  m3 <- lm(y ~ x + I(x^2), data=dat)
  m4 <- gam(y ~ s(x), data=dat)
  
  formula <- c(formula(m1), formula(m2), formula(m3), formula(m4))
  AIC <- c(AIC(m1), AIC(m2), AIC(m3), AIC(m4))
  BIC <- c(BIC(m1), BIC(m2), BIC(m3), BIC(m4))
  adjR <- c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared, summary(m3)$adj.r.squared, summary(m4)$r.sq)
  
  response <- y_name
  explanatory <- c(x_name, paste("log(", x_name, ")", sep=""), paste(x_name, "+", x_name, "^2", sep=""), paste("s(", x_name, ")", sep=""))
  
  # include more details of explanatory model
  cass_model <- x_name
  population <- "not included"
  settlement <- "not included"
  modeltype <- c(rep("lm", 3), rep("gam", 1))
  
  out <- data.frame(response, explanatory, AIC, BIC, adjR, cass_model, population, settlement, modeltype)
  return(out)
}

## Test
# y_name="tot_cassava_area"
# x_name=xmodel_names[49]
# dat=CIVdata
# fitModel(x_name, y_name, dat)





# Step 2: Response ~ Model x Population
# Step 3: Response ~ Model x Settlement

# Function to fit regression models with two explanatory variables (x, z)
# 6 models are considered, additive (x + z), a log transformation of one or other or both of the explanatory variables, an additive spline model and a 2D spline model.
# function outputs details of the model fitted and model summaries
fitModelZ <- function(x_name, z_name, y_name, dat, type){
  
  dat$x <- dat[, x_name]
  dat$z <- dat[, z_name]
  dat$y <- dat[, y_name]
  if (min(dat$x, na.rm=TRUE) <=0 ){
    c <- min(dat$x[dat$x > 0], na.rm=TRUE) / 2
  } else {
    c <- 0
  }
  
  if (min(dat$z, na.rm=TRUE) <=0 ){
    cz <- min(dat$z[dat$z > 0], na.rm=TRUE) / 2
  } else {
    cz <- 0
  }
  
  m1 <- lm(y ~ x + z, data=dat)
  m2 <- lm(y ~ log(x + c) + z, data=dat)
  m3 <- lm(y ~ x + log(z + cz), data=dat)
  m4 <- lm(y ~ log(x + c) + log(z + cz), data=dat)
  m5 <- gam(y ~ s(x) + s(z), data=dat)
  m6 <- gam(y ~ s(x,z), data=dat)
  
  AIC <- c(AIC(m1), AIC(m2), AIC(m3), AIC(m4), AIC(m5), AIC(m6))
  BIC <- c(BIC(m1), BIC(m2), BIC(m3), BIC(m4), BIC(m5), BIC(m6))
  adjR <- c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared, summary(m3)$adj.r.squared, summary(m4)$adj.r.squared, summary(m5)$r.sq, summary(m6)$r.sq)
  
  response <- y_name
  explanatory <- c(paste(x_name, "+", z_name, sep=""), paste("log(",x_name, ")+", z_name, sep=""),paste(x_name, "+log(", z_name,")", sep=""), paste("log(",x_name, ")+log(", z_name,")", sep=""),paste("s(",x_name, ")+s(", z_name,")", sep=""),paste("s(",x_name, ":", z_name,")", sep=""))
  
  # include more details of explanatory model
  cass_model <- x_name
  if (type == "population"){
    population <- z_name
    settlement <- "not included"
  } else if (type == "settlement"){
    population <- "not included"
    settlement <- z_name
  }

  modeltype <- c(rep("lm", 4), rep("gam", 2))
  
  out <- data.frame(response, explanatory, AIC, BIC, adjR, cass_model, population, settlement, modeltype)
  return(out)
  
  
}

## Test
# y_name="tot_cassava_area"
# x_name=xmodel_names[49]
# z_name="mean_lspop.2000"
# dat=CIVdata
# fitModelZ(x_name, z_name, y_name, dat)


# create outer loop
fitModelZ_outer <- function(z_name, y_name, dat, type){
  # loop over all possible model variables
  temp <- lapply(xmodel_names, fitModelZ, z_name=z_name, y_name=y_name, dat=dat, type=type)
  # unlist
  temp_df <- rbindlist(temp)
  return(temp_df)
}





# Step 4: Response ~ Model x Population x Settlement
# Function to fit regression models with three explanatory variables (x, z1, z2)
# 10 models are considered, additive, a log transformation of one, two or three of the explanatory variables, an additive spline model and all possible 2D spline models.
# function outputs details of the model fitted and model summaries
fitModelZZ <- function(x_name, z_name1, z_name2, y_name, dat){
  
  dat$x <- dat[, x_name]
  dat$z1 <- dat[, z_name1]
  dat$z2 <- dat[, z_name2]
  dat$y <- dat[, y_name]
  if (min(dat$x, na.rm=TRUE) <=0 ){
    c <- min(dat$x[dat$x > 0], na.rm=TRUE) / 2
  } else {
    c <- 0
  }
  
  if (min(dat$z1, na.rm=TRUE) <=0 ){
    cz1 <- min(dat$z1[dat$z1 > 0], na.rm=TRUE) / 2
  } else {
    cz1 <- 0
  }
  
  if (min(dat$z2, na.rm=TRUE) <=0 ){
    cz2 <- min(dat$z2[dat$z2 > 0], na.rm=TRUE) / 2
  } else {
    cz2 <- 0
  }
  
  m1 <- lm(y ~ x + z1 + z2, data=dat)
  m2 <- lm(y ~ log(x + c) + z1 + z2, data=dat)
  m3 <- lm(y ~ x + log(z1 + cz1) + z2, data=dat)
  m4 <- lm(y ~ x + z1 + log(z2 + cz2), data=dat)
  m5 <- lm(y ~ x + log(z1 + cz1) + log(z2 + cz2), data=dat)
  m6 <- lm(y ~ log(x + c) + log(z1 + cz1) + log(z2 + cz2), data=dat)
  
  m7 <- gam(y ~ s(x) + s(z1) + s(z2), data=dat)
  m8 <- gam(y ~ s(x,z1) + s(z2), data=dat)
  m9 <- gam(y ~ s(x,z2) + s(z1), data=dat)
  m10 <- gam(y ~ s(x) + s(z1,z2), data=dat)
  # m11 <- gam(y ~ s(x,z1,z2), data=dat)
  # fails to fit
  
  
  AIC <- c(AIC(m1), AIC(m2), AIC(m3), AIC(m4), AIC(m5), AIC(m6), AIC(m7), AIC(m8), AIC(m9), AIC(m10))
  BIC <- c(BIC(m1), BIC(m2), BIC(m3), BIC(m4), BIC(m5), BIC(m6), BIC(m7), BIC(m8), BIC(m9), BIC(m10))
  adjR <- c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared, summary(m3)$adj.r.squared, summary(m4)$adj.r.squared, summary(m5)$adj.r.squared, summary(m6)$adj.r.squared, summary(m7)$r.sq, summary(m8)$r.sq, summary(m9)$r.sq, summary(m10)$r.sq)
  
  response <- y_name
  explanatory <- c(paste(x_name, "+", z_name1, "+", z_name2, sep=""), 
                   paste("log(",x_name, ")+", z_name1, "+", z_name2, sep=""),
                   paste(x_name, "+log(", z_name1,")", "+", z_name2, sep=""),
                   paste(x_name, "+", z_name1, "+log(", z_name2,")", sep=""),
                   paste(x_name, "+log(", z_name1,")", "+log(", z_name2,")", sep=""),
                   paste("log(",x_name, ")+log(", z_name1,")", "+log(", z_name2,")", sep=""),
                   paste("s(",x_name, ")+s(", z_name1,")+s(", z_name2,")", sep=""),
                   paste("s(",x_name, ":", z_name1,")+s(", z_name2,")", sep=""),
                   paste("s(",x_name, ":", z_name2,")+s(", z_name1,")", sep=""),
                   paste("s(",x_name, ")+s(", z_name1,":", z_name2,")", sep=""))
  
  # include more details of explanatory model
  cass_model <- x_name
  population <- z_name1
  settlement <- z_name2
  modeltype <- c(rep("lm", 6), rep("gam", 4))
  
  out <- data.frame(response, explanatory, AIC, BIC, adjR, cass_model, population, settlement, modeltype)
  return(out)
  
}

## Test
# y_name="tot_cassava_area"
# x_name=xmodel_names[49]
# z_name1="mean_lspop.2000"
# z_name2="mean_settle.2000"
# dat=CIVdata
# fitModelZZ(x_name, z_name1, z_name2, y_name, dat)

# create outer loops
fitModelZZ_outer1 <- function(z_name1, z_name2, y_name, dat){
  # loop over all possible model variables
  temp <- lapply(xmodel_names, fitModelZZ, z_name1=z_name1, z_name2=z_name2, y_name=y_name, dat=dat)
  # unlist
  temp_df <- rbindlist(temp)
  return(temp_df)
}

fitModelZZ_outer2 <- function(z_name2, y_name, dat){
  # loop over all possible pop variables (and model variables)
  temp <- lapply(xpop_names, fitModelZZ_outer1, z_name2=z_name2, y_name=y_name, dat=dat)
  # unlist
  temp_df <- rbindlist(temp)
  return(temp_df)
}




# Step 5: loop over response variables

getAllModels <- function(y_name, dat){
  # loop over all possible model variables
  step1 <- lapply(xmodel_names, fitModel, y_name=y_name, dat=dat)
  # unlist
  step1_df <- rbindlist(step1)
  print("Finished Step 1")
  # head(step1_df)
  
  # loop over all possible model variables and population summaries
  step2 <- lapply(xpop_names, fitModelZ_outer, y_name=y_name, dat=dat, type="population")
  # unlist
  step2_df <- rbindlist(step2)
  print("Finished Step 2")
  # head(step2_df)
  
  # loop over all possible model variables and settlement summaries
  step3 <- lapply(xsettle_names, fitModelZ_outer, y_name=y_name, dat=dat, type="settlement")
  # unlist
  step3_df <- rbindlist(step3)
  print("Finished Step 3")
  # head(step3_df)
  
  # loop over all possible settlement summaries (and pop and model variables)
  step4 <- lapply(xsettle_names, fitModelZZ_outer2, y_name=y_name, dat=dat)
  # unlist
  step4_df <- rbindlist(step4)
  print("Finished Step 4")
  # head(step4_df)
  
  all_models <- rbind(step1_df, step2_df, step3_df, step4_df)
  
}


# takes a little time to complete
r1_CIV <- getAllModels("tot_cassava_area", dat=CIVdata)
r2_CIV <- getAllModels("tot_monoculture_area", dat=CIVdata)
r3_CIV <- getAllModels("tot_intercrop_area", dat=CIVdata)
r4_CIV <- getAllModels("tot_area_ind_plants", dat=CIVdata)
r5_CIV <- getAllModels("tot_cassava_area_w", dat=CIVdata)
r6_CIV <- getAllModels("tot_monoculture_area_w", dat=CIVdata)
r7_CIV <- getAllModels("tot_intercrop_area_w", dat=CIVdata)

# presence only
r1_CIV_pres <- getAllModels("tot_cassava_area", dat=CIVdata_pres)
r2_CIV_pres <- getAllModels("tot_monoculture_area", dat=CIVdata_pres)
r3_CIV_pres <- getAllModels("tot_intercrop_area", dat=CIVdata_pres)
r4_CIV_pres <- getAllModels("tot_area_ind_plants", dat=CIVdata_pres)
r5_CIV_pres <- getAllModels("tot_cassava_area_w", dat=CIVdata_pres)
r6_CIV_pres <- getAllModels("tot_monoculture_area_w", dat=CIVdata_pres)
r7_CIV_pres <- getAllModels("tot_intercrop_area_w", dat=CIVdata_pres)


# Step 7: repeat for uganda

r1_UGA <- getAllModels("tot_cassava_area", dat=UGAdata)
r2_UGA <- getAllModels("tot_monoculture_area", dat=UGAdata)
r3_UGA <- getAllModels("tot_intercrop_area", dat=UGAdata)
r4_UGA <- getAllModels("tot_area_ind_plants", dat=UGAdata)
r5_UGA <- getAllModels("tot_cassava_area_w", dat=UGAdata)
r6_UGA <- getAllModels("tot_monoculture_area_w", dat=UGAdata)
r7_UGA <- getAllModels("tot_intercrop_area_w", dat=UGAdata)

# presence only
r1_UGA_pres <- getAllModels("tot_cassava_area", dat=UGAdata_pres)
r2_UGA_pres <- getAllModels("tot_monoculture_area", dat=UGAdata_pres)
r3_UGA_pres <- getAllModels("tot_intercrop_area", dat=UGAdata_pres)
r4_UGA_pres <- getAllModels("tot_area_ind_plants", dat=UGAdata_pres)
r5_UGA_pres <- getAllModels("tot_cassava_area_w", dat=UGAdata_pres)
r6_UGA_pres <- getAllModels("tot_monoculture_area_w", dat=UGAdata_pres)
r7_UGA_pres <- getAllModels("tot_intercrop_area_w", dat=UGAdata_pres)




# step 8: analyse, interpret (and refine)

# Function to reformat model outputs from above
getSummaries <- function(r1){
  r1$cass_type <- NA
  r1$cass_type[grep("Harv", r1$cass_model)] <- "Harv"
  r1$cass_type[grep("HA", r1$cass_model)] <- "Harv"
  r1$cass_type[grep("Prod", r1$cass_model)] <- "Prod"
  table(r1$cass_type, useNA="ifany")
  r1$cass_summary <- sapply(r1$cass_model, FUN=function(x)strsplit(as.character(x), "_", fixed=TRUE)[[1]][1])
  r1$cass_summary[r1$cass_summary == "Cass"] <- "mean" # mean of a single point = point estimate
  table(r1$cass_summary, useNA="ifany")
  r1$cass_dist <- NA
  r1$cass_dist[grep("5", r1$cass_model)] <- "5km"
  r1$cass_dist[grep("2", r1$cass_model)] <- "2km"
  r1$cass_dist[grep("10", r1$cass_model)] <- "10km"
  r1$cass_dist[is.na(r1$cass_dist)] <- "0km"
  table(r1$cass_dist, useNA="ifany")
  # table(r1$cass_dist,r1$cass_model, useNA="ifany")
  
  r1$population_type <- NA
  r1$population_type[grep("not included", r1$population)] <- "not included"
  r1$population_type[grep("Population", r1$population)] <- "population"
  r1$population_type[grep("lspop", r1$population)] <- "population"
  table(r1$population_type, useNA="ifany")
  r1$population_summary <- sapply(r1$population, FUN=function(x)strsplit(as.character(x), "_", fixed=TRUE)[[1]][1])
  r1$population_summary[r1$population_summary %in% c("Population")] <- "mean" # mean of a single point = point estimate
  table(r1$population_summary, useNA="ifany")
  r1$population_dist <- NA
  r1$population_dist[grep("5", r1$population)] <- "5km"
  r1$population_dist[grep("2", r1$population)] <- "2km"
  r1$population_dist[grep("10", r1$population)] <- "10km"
  r1$population_dist[is.na(r1$population_dist)] <- "0km"
  r1$population_dist[r1$population == "not included"] <- "not included"
  table(r1$population_dist, useNA="ifany")
  
  r1$settlement_type <- NA
  r1$settlement_type[grep("not included", r1$settlement)] <- "not included"
  r1$settlement_type[grep("settle", r1$settlement)] <- "settlement"
  table(r1$settlement_type, useNA="ifany")
  r1$settlement_summary <- sapply(r1$settlement, FUN=function(x)strsplit(as.character(x), "_", fixed=TRUE)[[1]][1])
  table(r1$settlement_summary, useNA="ifany")
  r1$settlement_dist <- NA
  r1$settlement_dist[grep("5", r1$settlement)] <- "5km"
  r1$settlement_dist[grep("2", r1$settlement)] <- "2km"
  r1$settlement_dist[grep("10", r1$settlement)] <- "10km"
  r1$settlement_dist[is.na(r1$settlement_dist)] <- "0km"
  r1$settlement_dist[r1$settlement == "not included"] <- "not included"
  table(r1$settlement_dist, useNA="ifany")
  
  r1$modeltype <- factor(r1$modeltype)
  r1$cass_type <- factor(r1$cass_type)
  r1$cass_summary <- factor(r1$cass_summary)
  r1$cass_dist <- factor(r1$cass_dist)
  r1$population_type <- factor(r1$population_type)
  r1$population_summary <- factor(r1$population_summary)
  r1$population_dist <- factor(r1$population_dist)
  r1$settlement_type <- factor(r1$settlement_type)
  r1$settlement_summary <- factor(r1$settlement_summary)
  r1$settlement_dist <- factor(r1$settlement_dist)
  
  return(r1)
}

r1_CIV <- getSummaries(r1_CIV)
r2_CIV <- getSummaries(r2_CIV)
r3_CIV <- getSummaries(r3_CIV)
r4_CIV <- getSummaries(r4_CIV)
r5_CIV <- getSummaries(r5_CIV)
r6_CIV <- getSummaries(r6_CIV)
r7_CIV <- getSummaries(r7_CIV)

r1_CIV_pres <- getSummaries(r1_CIV_pres)
r2_CIV_pres <- getSummaries(r2_CIV_pres)
r3_CIV_pres <- getSummaries(r3_CIV_pres)
r4_CIV_pres <- getSummaries(r4_CIV_pres)
r5_CIV_pres <- getSummaries(r5_CIV_pres)
r6_CIV_pres <- getSummaries(r6_CIV_pres)
r7_CIV_pres <- getSummaries(r7_CIV_pres)

r1_UGA <- getSummaries(r1_UGA)
r2_UGA <- getSummaries(r2_UGA)
r3_UGA <- getSummaries(r3_UGA)
r4_UGA <- getSummaries(r4_UGA)
r5_UGA <- getSummaries(r5_UGA)
r6_UGA <- getSummaries(r6_UGA)
r7_UGA <- getSummaries(r7_UGA)

r1_UGA_pres <- getSummaries(r1_UGA_pres)
r2_UGA_pres <- getSummaries(r2_UGA_pres)
r3_UGA_pres <- getSummaries(r3_UGA_pres)
r4_UGA_pres <- getSummaries(r4_UGA_pres)
r5_UGA_pres <- getSummaries(r5_UGA_pres)
r6_UGA_pres <- getSummaries(r6_UGA_pres)
r7_UGA_pres <- getSummaries(r7_UGA_pres)


r1_CIV$cass_dist <- factor(r1_CIV$cass_dist, levels=c("not included","0km", "2km", "5km", "10km"))
r1_UGA$cass_dist <- factor(r1_UGA$cass_dist, levels=c("not included","0km", "2km", "5km", "10km"))

r1_CIV$settlement_dist <- factor(r1_CIV$settlement_dist, levels=c("not included","0km", "2km", "5km", "10km"))
r1_UGA$settlement_dist <- factor(r1_UGA$settlement_dist, levels=c("not included","0km", "2km", "5km", "10km"))
r1_CIV$population_dist <- factor(r1_CIV$population_dist, levels=c("not included","0km", "2km", "5km", "10km"))
r1_UGA$population_dist <- factor(r1_UGA$population_dist, levels=c("not included","0km", "2km", "5km", "10km"))


# key summary figures.
# using raw data rather than model means. Things would be clearer if we used predictedmeans for each group

ggplot(r1_CIV, aes(x=modeltype, y=AIC, colour=modeltype)) + geom_boxplot()
ggplot(r1_CIV, aes(x=settlement_type, y=AIC, colour=population_type)) + geom_boxplot()
r1_CIV$cass_dist <- factor(r1_CIV$cass_dist, levels=c("0km", "2km", "5km", "10km"))
ggplot(r1_CIV, aes(x=cass_dist, y=AIC, colour=cass_summary)) + geom_boxplot() + facet_wrap( ~ cass_summary)

ggplot(r1_CIV, aes(x=population_summary, y=AIC, colour=population_dist)) + geom_boxplot() 
ggplot(r1_CIV, aes(x=settlement_summary, y=AIC, colour=settlement_dist)) + geom_boxplot() 

ggplot(r1_UGA, aes(x=modeltype, y=AIC, colour=modeltype)) + geom_boxplot()
ggplot(r1_UGA, aes(x=cass_type, y=AIC, colour=modeltype)) + geom_boxplot()
ggplot(r1_UGA, aes(x=settlement_type, y=AIC, colour=population_type)) + geom_boxplot()
r1_UGA$cass_dist <- factor(r1_UGA$cass_dist, levels=c("0km", "2km", "5km", "10km"))

ggplot(r1_UGA, aes(x=cass_dist, y=AIC, colour=cass_summary)) + geom_boxplot() + facet_wrap( ~ cass_summary)
ggplot(r1_UGA, aes(x=population_summary, y=AIC, colour=population_dist)) + geom_boxplot() 
ggplot(r1_UGA, aes(x=settlement_summary, y=AIC, colour=settlement_dist)) + geom_boxplot() 



# given main effect is gam, only plot the gam models in main figure




# ******************************************************
# Figure 8 - Regression model outputs --------------

g1 <- ggplot(r1_CIV %>% filter(modeltype == "gam"), aes(x=cass_type, y=AIC, colour=cass_dist)) + geom_boxplot() + ggtitle("C\U00F4te d'Ivoire") + guides(colour=guide_legend(title="Distance")) +  scale_colour_discrete(drop = FALSE)

g2 <- ggplot(r1_CIV %>% filter(modeltype == "gam"), aes(x=settlement_type, y=AIC, colour=settlement_dist)) + geom_boxplot() + ggtitle(" ") + guides(colour=guide_legend(title="Distance")) +   scale_colour_discrete(drop = FALSE)

g3 <- ggplot(r1_CIV %>% filter(modeltype == "gam"), aes(x=population_type, y=AIC, colour=population_dist)) + geom_boxplot() + ggtitle(" ") + guides(colour=guide_legend(title="Distance")) +   scale_colour_discrete(drop = FALSE)

g4 <- ggplot(r1_UGA %>% filter(modeltype == "gam"), aes(x=cass_type, y=AIC, colour=cass_dist)) + geom_boxplot() + ggtitle("Uganda") + guides(colour=guide_legend(title="Distance")) +   scale_colour_discrete(drop = FALSE)

g5 <- ggplot(r1_UGA %>% filter(modeltype == "gam"), aes(x=settlement_type, y=AIC, colour=settlement_dist)) + geom_boxplot() + ggtitle(" ") + guides(colour=guide_legend(title="Distance")) +   scale_colour_discrete(drop = FALSE)

g6 <- ggplot(r1_UGA %>% filter(modeltype == "gam"), aes(x=population_type, y=AIC, colour=population_dist)) + geom_boxplot() + ggtitle(" ") + guides(colour=guide_legend(title="Distance")) +   scale_colour_discrete(drop = FALSE)


print(
  ggarrange(g1, g2, g3, g4, g5, g6, nrow=2, ncol=3, common.legend=TRUE, legend="right")
)





# ******************************************************
# Figure 9 - Fitted model -----------------------

# the "top" model



mod_CIV <- gam(tot_cassava_area ~ s(mean_Prod.10000,min_lspop.2000)+s(mean_settle.2000), data=CIVdata)

mod_UGA <- gam(tot_cassava_area ~ s(Cass_HA,median_lspop.5000)+s(mean_settle.2000), data=UGAdata)

par(mfrow=c(2,2), mar=c(4, 8, 4, 1))
plot(mod_CIV, select=1, scheme=2, ylim=c(0, 30), xlim=c(0, 200), xlab="CassavaMap production (mean at 10km)", ylab="Population density\n(minimum at 2km)", main="C\U00F4te d'Ivoire")
plot(mod_CIV, select=2, main="",  xlim=c(0, 0.15), xlab="Settlement density (2km)", ylab="Centred spline")

plot(mod_UGA, select=1, scheme=2, ylim=c(0, 250), xlim=c(0, 40), ylab="Population Density\n(Median at 5km)", xlab="CassavaMap Harvest area (point location)", main="Uganda")
plot(mod_UGA, select=2, main="",  xlim=c(0, 0.2), xlab="Settlement density (2km)", ylab="Centred spline")


