
# ModelVsSurvey_baseline

# A comparison of the observed cassava production vs modelled cassava production (from CassavaMap) at the point locations in Cote d'Ivoire and Uganda


library(mgcv)
library(ggplot2)
library(ggpubr)
library(ggcorrplot)

rm(list=ls())

# read in processed data. DOI 10.6084/m9.figshare.26983603
dat_civ <- read.csv("dat_civ.csv")
dat_uga <- read.csv("dat_uga.csv")


# Correlation across sampled locations between different measures of cassava production (total, monoculture, intercropped etc.) and modelled cassava production from CassavaMap

cor(dat_civ[, c("tot_cassava_area","tot_cassava_area_w","tot_monoculture_area","tot_intercrop_area","Cass_Prod","Cass_HA","SPAM2010","MapSPAM_HA")], method="spearman", use="pairwise")

cor(dat_uga[, c("tot_cassava_area","tot_cassava_area_w","tot_monoculture_area","tot_intercrop_area","Cass_Prod","Cass_HA","SPAM2010","MapSPAM_HA")], method="spearman", use="pairwise")




# Figure 4 - Cassava distribution characteristics ----
res_names <- c("tot_cassava_area", "tot_monoculture_area", "tot_intercrop_area", "tot_area_ind_plants")

g1 <- ggplot(dat_civ[dat_civ$tot_cassava_area > 0, ], aes(x=tot_monoculture_area)) + geom_histogram(bins=10) + theme_bw() + labs(x="Total Monocultured area per location", y="Count") + ggtitle("C\U00F4te d'Ivoire") 

g2 <- ggplot(dat_civ[dat_civ$tot_cassava_area > 0, ], aes(x=tot_intercrop_area)) + geom_histogram(bins=10) + theme_bw() + labs(x="Total Intercropped area per location", y="Count") + ggtitle(" ")

civ_corr <- cor(dat_civ[, c(res_names)], use="pairwise", method="spearman")
colnames(civ_corr) <- c("Total", "Monoculture", "Intercrop", "Individual plants")
row.names(civ_corr) <- c("Total", "Monoculture", "Intercrop", "Individual plants")

g3 <- ggcorrplot(civ_corr, type="upper")


g4 <- ggplot(dat_uga[dat_uga$tot_cassava_area > 0, ], aes(x=tot_monoculture_area)) + geom_histogram(bins=10) + theme_bw() + labs(x="Total Monocultured area per location", y="Count") + ggtitle("Uganda") 

g5 <- ggplot(dat_uga[dat_uga$tot_cassava_area > 0, ], aes(x=tot_intercrop_area)) + geom_histogram(bins=10) + theme_bw() + labs(x="Total Intercropped area per location", y="Count") + ggtitle(" ")

uga_corr <- cor(dat_uga[, c(res_names)], use="pairwise", method="spearman")
colnames(uga_corr) <- c("Total", "Monoculture", "Intercrop", "Individual plants")
row.names(uga_corr) <- c("Total", "Monoculture", "Intercrop", "Individual plants")

g6 <- ggcorrplot(uga_corr, type="upper")

print(
  ggarrange(g1, g2, g3, g4, g5, g6)
)



# Baseline regression models of observed cassava production vs modelled cassava production ------

# Production in CIV 
c <- min(dat_civ$Cass_Prod[dat_civ$Cass_Prod > 0], na.rm=TRUE) / 2
m1 <- lm(tot_cassava_area ~ Cass_Prod, data=dat_civ)
m2 <- lm(tot_cassava_area ~ log(Cass_Prod + c), data=dat_civ)
m4 <- gam(tot_cassava_area ~ s(Cass_Prod), data=dat_civ)
AIC(m1)
AIC(m2)
AIC(m4)
par(mfrow=c(2,2))
plot(m2)
summary(m2)$adj.r.squared

g1 <- ggplot(dat_civ, aes(x=log(Cass_Prod + c), y=tot_cassava_area)) + geom_point() + labs(x="Predicted Cassva production (log)", y="Total Cassava Area")
g2 <- ggplot(dat_civ, aes(x=log(Cass_Prod + c), y=tot_cassava_area_w)) + geom_point() + labs(x="Predicted Cassva production (log)", y="Total Cassava Area (weighted)")
g3 <- ggplot(dat_civ, aes(x=log(Cass_Prod + c), y=tot_monoculture_area)) + geom_point() + labs(x="Predicted Cassva production (log)", y="Total Monoculture Area")
g4 <- ggplot(dat_civ, aes(x=log(Cass_Prod + c), y=tot_intercrop_area)) + geom_point() + labs(x="Predicted Cassva production (log)", y="Total Intercropped Area")
ggarrange(g1,g2,g3,g4)


# Harvest Area in CIV
c <- min(dat_civ$Cass_HA[dat_civ$Cass_HA > 0], na.rm=TRUE) / 2
m1 <- lm(tot_cassava_area ~ Cass_HA, data=dat_civ)
m2 <- lm(tot_cassava_area ~ log(Cass_HA + c), data=dat_civ)
m4 <- gam(tot_cassava_area ~ s(Cass_HA), data=dat_civ)
AIC(m1)
AIC(m2)
AIC(m4)
par(mfrow=c(2,2))
plot(m2)
summary(m2)$adj.r.squared



# Cassava presence in CIV
g1 <- ggplot(dat_civ, aes(y=log(Cass_Prod + c), x=cass_area_presence)) + geom_boxplot() + labs(y="Predicted Cassva production (log)", x="Presence/absence of cassava production") + geom_point()
g2 <- ggplot(dat_civ, aes(y=log(Cass_HA + c), x=cass_area_presence)) + geom_boxplot() + labs(y="Predicted Cassva harvest area (log)", x="Presence/absence of cassava production") + geom_point()
g3 <- ggplot(dat_civ, aes(y=log(SPAM2010), x=cass_area_presence)) + geom_boxplot() + labs(y="Predicted Cassva production SPAM (log)", x="Presence/absence of cassava production") + geom_point()
ggarrange(g1, g2, g3, nrow=1)

offset <- min(dat_civ$Cass_Prod[dat_civ$Cass_Prod > 0], na.rm=TRUE)/2
t.test(log(Cass_Prod + offset) ~ cass_area_presence, data=dat_civ)

offset <- min(dat_civ$Cass_HA[dat_civ$Cass_HA > 0], na.rm=TRUE)/2
t.test(log(Cass_HA + offset) ~ cass_area_presence, data=dat_civ)




# Production in Uganda
c <- min(dat_uga$Cass_Prod[dat_uga$Cass_Prod > 0], na.rm=TRUE) / 2
m1 <- lm(tot_cassava_area ~ Cass_Prod, data=dat_uga)
m2 <- lm(tot_cassava_area ~ log(Cass_Prod + c), data=dat_uga)
m4 <- gam(tot_cassava_area ~ s(Cass_Prod), data=dat_uga)
AIC(m1)
AIC(m2)
AIC(m4)
par(mfrow=c(2,2))
plot(m2)
summary(m2)$adj.r.squared


g1 <- ggplot(dat_uga, aes(x=log(Cass_Prod + c), y=tot_cassava_area)) + geom_point() + labs(x="Predicted Cassva production (log)", y="Total Cassava Area")
g2 <- ggplot(dat_uga, aes(x=log(Cass_Prod + c), y=tot_cassava_area_w)) + geom_point() + labs(x="Predicted Cassva production (log)", y="Total Cassava Area (weighted)")
g3 <- ggplot(dat_uga, aes(x=log(Cass_Prod + c), y=tot_monoculture_area)) + geom_point() + labs(x="Predicted Cassva production (log)", y="Total Monoculture Area")
g4 <- ggplot(dat_uga, aes(x=log(Cass_Prod + c), y=tot_intercrop_area)) + geom_point() + labs(x="Predicted Cassva production (log)", y="Total Intercropped Area")
ggarrange(g1,g2,g3,g4)




# Harvest Area in Uganda
c <- min(dat_uga$Cass_HA[dat_uga$Cass_HA > 0], na.rm=TRUE) / 2
m1 <- lm(tot_cassava_area ~ Cass_HA, data=dat_uga)
m2 <- lm(tot_cassava_area ~ log(Cass_HA + c), data=dat_uga)
m4 <- gam(tot_cassava_area ~ s(Cass_HA), data=dat_uga)
AIC(m1)
AIC(m2)
AIC(m4)
par(mfrow=c(2,2))
plot(m1)
summary(m1)$adj.r.squared




# Cassava presence in Uganda
g1 <- ggplot(dat_uga, aes(y=log(Cass_Prod + c), x=cass_area_presence)) + geom_boxplot() + labs(y="Predicted Cassva production (log)", x="Presence/absence of cassava production") + geom_point()
g2 <- ggplot(dat_uga, aes(y=log(Cass_HA + c), x=cass_area_presence)) + geom_boxplot() + labs(y="Predicted Cassva harvest area (log)", x="Presence/absence of cassava production") + geom_point()
g3 <- ggplot(dat_uga, aes(y=log(SPAM2010), x=cass_area_presence)) + geom_boxplot() + labs(y="Predicted Cassva production SPAM (log)", x="Presence/absence of cassava production") + geom_point()
ggarrange(g1, g2, g3, nrow=1)

offset <- min(dat_uga$Cass_Prod[dat_uga$Cass_Prod > 0], na.rm=TRUE)/2
t.test(log(Cass_Prod + offset) ~ cass_area_presence, data=dat_uga)

offset <- min(dat_uga$Cass_HA[dat_uga$Cass_HA > 0], na.rm=TRUE)/2
t.test(log(Cass_HA + offset) ~ cass_area_presence, data=dat_uga)





# ****************************************************
# Figure 5 - Survey vs model summaries ----

dat_civ$cass_area_presence <- as.character(dat_civ$cass_area_presence)
dat_civ$cass_area_presence[dat_civ$cass_area_presence == "TRUE"] <- "Present"
dat_civ$cass_area_presence[dat_civ$cass_area_presence == "FALSE"] <- "Absent"
dat_civ$cass_area_presence <- as.factor(dat_civ$cass_area_presence)

c_civ <- min(dat_civ$Cass_Prod[dat_civ$Cass_Prod > 0], na.rm=TRUE) / 2
cha_civ <- min(dat_civ$Cass_Prod[dat_civ$Cass_HA > 0], na.rm=TRUE) / 2
c_uga <- min(dat_civ$Cass_Prod[dat_uga$Cass_Prod > 0], na.rm=TRUE) / 2
cha_uga <- min(dat_uga$Cass_HA[dat_uga$Cass_HA > 0], na.rm=TRUE) / 2

dat_uga$cass_area_presence <- as.character(dat_uga$cass_area_presence)
dat_uga$cass_area_presence[dat_uga$cass_area_presence == "TRUE"] <- "Present"
dat_uga$cass_area_presence[dat_uga$cass_area_presence == "FALSE"] <- "Absent"
dat_uga$cass_area_presence <- as.factor(dat_uga$cass_area_presence)

g1 <- ggplot(dat_civ, aes(y=log(Cass_Prod + c_civ), x=cass_area_presence)) + geom_boxplot() + labs(y="CassavaMap predicted production (log)", x="Cassava production") + geom_point() + ggtitle("C\U00F4te d'Ivoire")

g2 <- ggplot(dat_civ, aes(y=log(Cass_HA + cha_civ), x=cass_area_presence)) + geom_boxplot() + labs(y="CassavaMap predicted harvest area (log)", x="Cassava production") + geom_point() + ggtitle(" ")

g3 <- ggplot(dat_civ, aes(y=log(SPAM2010), x=cass_area_presence)) + geom_boxplot() + labs(y="MapSPAM predicted production (log)", x="Cassava production") + geom_point() + ggtitle(" ")

g4 <- ggplot(dat_uga, aes(y=log(Cass_Prod + c_uga), x=cass_area_presence)) + geom_boxplot() + labs(y="CassavaMap predicted production (log)", x="Cassava production") + geom_point() + ggtitle("Uganda")

g5 <- ggplot(dat_uga, aes(y=log(Cass_HA + cha_uga), x=cass_area_presence)) + geom_boxplot() + labs(y="CassavaMap predicted harvest area (log)", x="Cassava production") + geom_point() + ggtitle(" ")

g6 <- ggplot(dat_uga, aes(y=log(SPAM2010), x=cass_area_presence)) + geom_boxplot() + labs(y="MapSPAM predicted production (log)", x="Cassava production") + geom_point() + ggtitle(" ")


print(
  ggarrange(g1, g2, g3, g4, g5, g6, nrow=2, ncol=3)
)







