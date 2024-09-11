

# Geographical trends in the survey data were summarised by i) linear models accounting for administrative regions and ii) generalized additive models along the different transects of the sampling. For both countries, the total cassava area was first log-transformed and separate additive terms were fitted over longitude and latitude independently. There was insufficient data to fit an interaction between the two. 

# Geographical trends in the CassavaMap predictions are summarised through generalized additive models (GAMs) using the dissolved buffer extraction of the spatial maps of the survey locations to investigate large-scale regional changes. Models were fitted to the natural logarithm of the prediction production with additive smooth terms for longitude and latitude.


library(ggplot2)
library(ggpubr)
library(lattice)


library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(rgeos)

rm(list=ls())

# read in processed data. DOI 10.6084/m9.figshare.26983603
dat_civ <- read.csv("dat_civ.csv")
dat_uga <- read.csv("dat_uga.csv")

# get country shapes
civ <- ne_countries(scale = "medium", returnclass = "sf", country="Ivory Coast")
uga <- ne_countries(scale = "medium", returnclass = "sf", country="Uganda")

# ******************************************************
# Figure 6 - Spatial trends in Ivory Coast --------------



g1 <- ggplot(civ) + geom_sf() + geom_point(data=dat_civ, aes(x=Centre_Lon, y=Centre_Lat)) + theme_bw() + labs(x="Longitude", y="Latitude") + geom_point(data = dat_civ, aes(x=Centre_Lon, y=Centre_Lat, size=tot_cassava_area_w, colour=tot_cassava_area_w)) +  labs(x="Longitude", y="Latitude") + theme_bw() + guides(size="none", colour=guide_legend(title="Weighted Cassava \nproduction")) + xlim(-6, -3) + ylim(5, 8)

offset <- min(dat_civ$tot_cassava_area_w[dat_civ$tot_cassava_area_w > 0]) / 2
gm <- gam(log(tot_cassava_area_w + offset) ~s(Centre_Lon) + s(Centre_Lat), data=dat_civ)

newdata1 <- data.frame(Centre_Lon = seq(from=min(dat_civ$Centre_Lon), to=max(dat_civ$Centre_Lon), length=100), Centre_Lat = rep(mean(dat_civ$Centre_Lat), 100))

newdata2 <- data.frame(Centre_Lat = seq(from=min(dat_civ$Centre_Lat), to=max(dat_civ$Centre_Lat), length=100), Centre_Lon = rep(mean(dat_civ$Centre_Lon), 100))

pred1 <- predict(gm, newdata1, se=TRUE)
newdata1$log_cassava <- pred1$fit
newdata1$lower <- pred1$fit - 1.96*pred1$se.fit
newdata1$upper <- pred1$fit + 1.96*pred1$se.fit

pred2 <- predict(gm, newdata2, se=TRUE)
newdata2$log_cassava <- pred2$fit
newdata2$lower <- pred2$fit - 1.96*pred2$se.fit
newdata2$upper <- pred2$fit + 1.96*pred2$se.fit

g2 <- ggplot(newdata1, aes(x=Centre_Lon, y=log_cassava)) +
  geom_ribbon(data = newdata1, alpha = 0.3,
              aes(ymin = lower, ymax = upper, fill = "confidence interval")) +
  geom_line(data = newdata1, aes(color = "GAM")) +
  scale_fill_manual(values = "lightblue", name = NULL) +
  scale_color_manual(values = "darkblue", name = NULL) +
  theme_bw() + labs(x= "Longitude", y="Cassava Production (log)") + guides(fill="none", colour="none")

g3 <- ggplot(newdata1, aes(x=Centre_Lat, y=log_cassava)) +
  geom_ribbon(data = newdata2, alpha = 0.3,
              aes(ymin = lower, ymax = upper, fill = "confidence interval")) +
  geom_line(data = newdata2, aes(color = "GAM")) +
  scale_fill_manual(values = "lightblue", name = NULL) +
  scale_color_manual(values = "darkblue", name = NULL) +
  theme_bw() + labs(x= "Latitude", y="Cassava Production (log)") + guides(fill="none", colour="none")

# read in modelled cassava production . Converted from original raster into points. Raster located at Szyniszewska, Anna (2020). CassavaMap. figshare. Dataset. https://doi.org/10.6084/m9.figshare.9745118.v1
civ_prod_10000 <- read.csv("civ_prod_10000.txt")

offset <- min(civ_prod_10000$GRID_CODE[civ_prod_10000$GRID_CODE > 0], na.rm=TRUE) / 2
gm_prod <- gam(log(GRID_CODE + offset) ~ s(Longitude) + s(Latitude), data=civ_prod_10000)

g4 <-  ggplot(civ_prod_10000, aes(x=Longitude, y=Latitude, colour=GRID_CODE)) + geom_point() + theme_bw() + guides(colour=guide_legend(title="CassavaMap predicted \nproduction")) + coord_fixed()

newdata1 <- data.frame(Longitude = seq(from=min(civ_prod_10000$Longitude), to=max(civ_prod_10000$Longitude), length=100), Latitude = rep(mean(civ_prod_10000$Latitude), 100))

newdata2 <- data.frame(Latitude = seq(from=min(civ_prod_10000$Latitude), to=max(civ_prod_10000$Latitude), length=100), Longitude = rep(mean(civ_prod_10000$Longitude), 100))

pred1 <- predict(gm_prod, newdata1, se=TRUE)
newdata1$log_cassava <- pred1$fit
newdata1$lower <- pred1$fit - 1.96*pred1$se.fit
newdata1$upper <- pred1$fit + 1.96*pred1$se.fit

pred2 <- predict(gm_prod, newdata2, se=TRUE)
newdata2$log_cassava <- pred2$fit
newdata2$lower <- pred2$fit - 1.96*pred2$se.fit
newdata2$upper <- pred2$fit + 1.96*pred2$se.fit

g5 <- ggplot(newdata1, aes(x=Longitude, y=log_cassava)) +
  geom_ribbon(data = newdata1, alpha = 0.3,
              aes(ymin = lower, ymax = upper, fill = "confidence interval")) +
  geom_line(data = newdata1, aes(color = "GAM")) +
  scale_fill_manual(values = "lightblue", name = NULL) +
  scale_color_manual(values = "darkblue", name = NULL) +
  theme_bw() + labs(x= "Longitude", y="CassavaMap Production (log)") + guides(fill="none", colour="none")

g6 <- ggplot(newdata1, aes(x=Latitude, y=log_cassava)) +
  geom_ribbon(data = newdata2, alpha = 0.3,
              aes(ymin = lower, ymax = upper, fill = "confidence interval")) +
  geom_line(data = newdata2, aes(color = "GAM")) +
  scale_fill_manual(values = "lightblue", name = NULL) +
  scale_color_manual(values = "darkblue", name = NULL) +
  theme_bw() + labs(x= "Latitude", y="CassavaMap Production (log)") + guides(fill="none", colour="none")


print(
  ggarrange(g1, g2, g3, g4, g5, g6, nrow=2, ncol=3, widths=c(1.4, 1, 1))
)


# ******************************************************
# Figure 7 - Spatial trends in Uganda --------------



gs2 <- ggplot(uga) + geom_sf() + geom_point(data=dat_uga, aes(x=Centre_Lon, y=Centre_Lat)) + theme_bw() + labs(x="Longitude", y="Latitude") + geom_point(data = dat_uga, aes(x=Centre_Lon, y=Centre_Lat, size=tot_monoculture_area, colour=tot_monoculture_area)) +  labs(x="Longitude", y="Latitude") + theme_bw() + guides(size="none", colour=guide_legend(title="Area in monoculture")) + xlim(30, 35) + ylim(0,4)

gs3 <- ggplot(uga) + geom_sf() + geom_point(data=dat_uga, aes(x=Centre_Lon, y=Centre_Lat)) + theme_bw() + labs(x="Longitude", y="Latitude") + geom_point(data = dat_uga, aes(x=Centre_Lon, y=Centre_Lat, size=tot_intercrop_area, colour=tot_intercrop_area)) +  labs(x="Longitude", y="Latitude") + theme_bw() + guides(size="none", colour=guide_legend(title="Area in intercropped")) + xlim(30, 35) + ylim(0,4)

ggarrange(gs2, gs3, nrow=1)


g1 <- ggplot(uga) + geom_sf() + geom_point(data=dat_uga, aes(x=Centre_Lon, y=Centre_Lat)) + theme_bw() + labs(x="Longitude", y="Latitude") + geom_point(data = dat_uga, aes(x=Centre_Lon, y=Centre_Lat, size=tot_cassava_area_w, colour=tot_cassava_area_w)) +  labs(x="Longitude", y="Latitude") + theme_bw() + guides(size="none", colour=guide_legend(title="Weighted Cassava \nproduction")) + xlim(30, 35) + ylim(0,4)

offset <- min(dat_uga$tot_cassava_area_w[dat_uga$tot_cassava_area_w > 0]) / 2
gm <- gam(log(tot_cassava_area_w + offset) ~s(Centre_Lon) + s(Centre_Lat), data=dat_uga)

newdata1 <- data.frame(Centre_Lon = seq(from=min(dat_uga$Centre_Lon), to=max(dat_uga$Centre_Lon), length=100), Centre_Lat = rep(mean(dat_uga$Centre_Lat), 100))

newdata2 <- data.frame(Centre_Lat = seq(from=min(dat_uga$Centre_Lat), to=max(dat_uga$Centre_Lat), length=100), Centre_Lon = rep(mean(dat_uga$Centre_Lon), 100))

pred1 <- predict(gm, newdata1, se=TRUE)
newdata1$log_cassava <- pred1$fit
newdata1$lower <- pred1$fit - 1.96*pred1$se.fit
newdata1$upper <- pred1$fit + 1.96*pred1$se.fit

pred2 <- predict(gm, newdata2, se=TRUE)
newdata2$log_cassava <- pred2$fit
newdata2$lower <- pred2$fit - 1.96*pred2$se.fit
newdata2$upper <- pred2$fit + 1.96*pred2$se.fit

g2 <- ggplot(newdata1, aes(x=Centre_Lon, y=log_cassava)) +
  geom_ribbon(data = newdata1, alpha = 0.3,
              aes(ymin = lower, ymax = upper, fill = "confidence interval")) +
  geom_line(data = newdata1, aes(color = "GAM")) +
  scale_fill_manual(values = "lightblue", name = NULL) +
  scale_color_manual(values = "darkblue", name = NULL) +
  theme_bw() + labs(x= "Longitude", y="Cassava Production (log)") + guides(fill="none", colour="none")

g3 <- ggplot(newdata1, aes(x=Centre_Lat, y=log_cassava)) +
  geom_ribbon(data = newdata2, alpha = 0.3,
              aes(ymin = lower, ymax = upper, fill = "confidence interval")) +
  geom_line(data = newdata2, aes(color = "GAM")) +
  scale_fill_manual(values = "lightblue", name = NULL) +
  scale_color_manual(values = "darkblue", name = NULL) +
  theme_bw() + labs(x= "Latitude", y="Cassava Production (log)") + guides(fill="none", colour="none")

# read in modelled cassava production . Converted from original raster into points. Raster located at Szyniszewska, Anna (2020). CassavaMap. figshare. Dataset. https://doi.org/10.6084/m9.figshare.9745118.v1
uga_prod_10000 <- read.csv("uga_prod_10000.txt")

offset <- min(uga_prod_10000$GRID_CODE[uga_prod_10000$GRID_CODE > 0], na.rm=TRUE) / 2
gm_prod <- gam(log(GRID_CODE + offset) ~ s(Longitude) + s(Latitude), data=uga_prod_10000)

g4 <-  ggplot(uga_prod_10000, aes(x=Longitude, y=Latitude, colour=GRID_CODE)) + geom_point() + theme_bw() + guides(colour=guide_legend(title="CassavaMap predicted \nproduction")) + coord_fixed()

newdata1 <- data.frame(Longitude = seq(from=min(uga_prod_10000$Longitude), to=max(uga_prod_10000$Longitude), length=100), Latitude = rep(mean(uga_prod_10000$Latitude), 100))

newdata2 <- data.frame(Latitude = seq(from=min(uga_prod_10000$Latitude), to=max(uga_prod_10000$Latitude), length=100), Longitude = rep(mean(uga_prod_10000$Longitude), 100))

pred1 <- predict(gm_prod, newdata1, se=TRUE)
newdata1$log_cassava <- pred1$fit
newdata1$lower <- pred1$fit - 1.96*pred1$se.fit
newdata1$upper <- pred1$fit + 1.96*pred1$se.fit

pred2 <- predict(gm_prod, newdata2, se=TRUE)
newdata2$log_cassava <- pred2$fit
newdata2$lower <- pred2$fit - 1.96*pred2$se.fit
newdata2$upper <- pred2$fit + 1.96*pred2$se.fit

g5 <- ggplot(newdata1, aes(x=Longitude, y=log_cassava)) +
  geom_ribbon(data = newdata1, alpha = 0.3,
              aes(ymin = lower, ymax = upper, fill = "confidence interval")) +
  geom_line(data = newdata1, aes(color = "GAM")) +
  scale_fill_manual(values = "lightblue", name = NULL) +
  scale_color_manual(values = "darkblue", name = NULL) +
  theme_bw() + labs(x= "Longitude", y="CassavaMap Production (log)") + guides(fill="none", colour="none")

g6 <- ggplot(newdata1, aes(x=Latitude, y=log_cassava)) +
  geom_ribbon(data = newdata2, alpha = 0.3,
              aes(ymin = lower, ymax = upper, fill = "confidence interval")) +
  geom_line(data = newdata2, aes(color = "GAM")) +
  scale_fill_manual(values = "lightblue", name = NULL) +
  scale_color_manual(values = "darkblue", name = NULL) +
  theme_bw() + labs(x= "Latitude", y="CassavaMap Production (log)") + guides(fill="none", colour="none")


print(
  ggarrange(g1, g2, g3, g4, g5, g6, nrow=2, ncol=3, widths=c(1.4,1,1))
)

