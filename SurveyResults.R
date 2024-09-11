
# SurveyResults


# Script to investigate survey data
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

# get field data
fields_civ <- read.csv("KLH_Civ_cassava_fields_poly.csv")
fields_uga <- read.csv("KLH_UGA_cassava_fields_poly.csv")


# CIV -----

ggplot(civ) + geom_sf() + geom_point(data=dat_civ, aes(x=Centre_Lon, y=Centre_Lat)) + theme_bw() + labs(x="Longitude", y="Latitude")
dim(dat_civ)

# number of locations with cassava
table(dat_civ$tot_cassava_area > 0, useNA="ifany")

# number of cassava fields
hist(table(fields_civ$field_id), breaks=-.5:35.5, col="grey", xlab="Number of separate fields in Cassava producing sites", main="")
box()

# size of cassava fields
mean(fields_civ$area_m2)
quantile(fields_civ$area_m2)
hist(fields_civ$area_m2)
aggregate(area_m2 ~ Name, data=fields_civ, FUN=mean)
aggregate(area_m2 ~ Name, data=fields_civ, FUN=median)
aggregate(area_m2 ~ Name, data=fields_civ, FUN=quantile, probs=0.25)

# number of locations with different cassava production systems
table(dat_civ$tot_area_ind_plants > 0, useNA="ifany")
table(dat_civ$tot_monoculture_area > 0, useNA="ifany")
table(dat_civ$tot_intercrop_area > 0, useNA="ifany")

# look at these in more detail
table(dat_civ$tot_ind_plants)

g1 <- ggplot(dat_civ[dat_civ$tot_cassava_area > 0, ], aes(x=tot_monoculture_area)) + geom_histogram(bins=10) + theme_bw() + labs(x="Total area per location", y="Count") + ggtitle("Monoculture") 
g2 <- ggplot(dat_civ[dat_civ$tot_cassava_area > 0, ], aes(x=tot_intercrop_area)) + geom_histogram(bins=10) + theme_bw() + labs(x="Total area per location", y="Count") + ggtitle("Intercrop") 
ggarrange(g1, g2)

mean(dat_civ[dat_civ$tot_cassava_area > 0, "tot_monoculture_area"])
median(dat_civ[dat_civ$tot_cassava_area > 0, "tot_monoculture_area"])
quantile(dat_civ[dat_civ$tot_cassava_area > 0, "tot_monoculture_area"])

mean(dat_civ[dat_civ$tot_cassava_area > 0, "tot_intercrop_area"])
median(dat_civ[dat_civ$tot_cassava_area > 0, "tot_intercrop_area"])
quantile(dat_civ[dat_civ$tot_cassava_area > 0, "tot_intercrop_area"])

# look at number of fields in each cropping system
table(fields_civ$Name)
table(fields_civ$Name, fields_civ$field_id)
propTable <- as.matrix(table(fields_civ$Name, fields_civ$field_id)) / matrix(colSums(as.matrix(table(fields_civ$Name, fields_civ$field_id))), nrow=2, ncol=51, byrow=TRUE)
levelplot(as.matrix(table(fields_civ$Name, fields_civ$field_id)), asp=1, xlab="", ylab="Field site", scales=list(y=list(lab=NULL)), main="Number of Fields")
levelplot(propTable, asp=1, xlab="", ylab="Field site", scales=list(y=list(lab=NULL)), main="Proportion of Fields", col.regions=heat.colors(50))
hist(propTable[1,], col="grey", main="", xlab="Proportion of Intercropped Fields", breaks=10)
box()


# figure of the cassava production (and weighted)
ggplot(dat_civ, aes(x=tot_cassava_area, y=tot_cassava_area_w)) + geom_point()

hist(dat_civ$tot_cassava_area_w, col="grey", breaks=20, xlab="Weighted Cassava Production (per m2)", main="")
box()

ggplot(dat_civ, aes(x=Centre_Lon, y=Centre_Lat, colour=cass_area_presence)) + geom_point(size=2) + labs(x="Longitude", y="Latitude") + theme_bw() + guides(size="none") + coord_fixed()
ggplot(dat_civ, aes(x=Centre_Lon, y=Centre_Lat, size=tot_cassava_area_w, colour=tot_cassava_area_w)) + geom_point() + labs(x="Longitude", y="Latitude") + theme_bw() + guides(size="none", colour=guide_legend(title="Weighted Cassava Production (per m2)")) + coord_fixed()






# Uganda -----

ggplot(uga) + geom_sf() + geom_point(data=dat_uga, aes(x=Centre_Lon, y=Centre_Lat)) + theme_bw() + labs(x="Longitude", y="Latitude")
dim(dat_uga)

# number of locations with cassava
table(dat_uga$tot_cassava_area > 0, useNA="ifany")

# number of cassava fields
hist(table(fields_uga$field_id), breaks=-.5:35.5, col="grey", xlab="Number of separate fields in Cassava producing sites", main="")
box()


# number of locations with different cassava production systems
table(dat_uga$tot_area_ind_plants > 0, useNA="ifany")
table(dat_uga$tot_monoculture_area > 0, useNA="ifany")
table(dat_uga$tot_intercrop_area > 0, useNA="ifany")

# look at these in more detail
table(dat_uga$tot_ind_plants)

g1 <- ggplot(dat_uga[dat_uga$tot_cassava_area > 0, ], aes(x=tot_monoculture_area)) + geom_histogram(bins=10) + theme_bw() + labs(x="Total area per location", y="Count") + ggtitle("Monoculture") 
g2 <- ggplot(dat_uga[dat_uga$tot_cassava_area > 0, ], aes(x=tot_intercrop_area)) + geom_histogram(bins=10) + theme_bw() + labs(x="Total area per location", y="Count") + ggtitle("Intercrop") 
ggarrange(g1, g2)

g1 <- ggplot(dat_uga[dat_uga$tot_cassava_area > 0, ], aes(y=tot_monoculture_area)) + geom_boxplot() + theme_bw() + labs(y="Total area per location", y="Count") + ggtitle("Monoculture") 
g2 <- ggplot(dat_uga[dat_uga$tot_cassava_area > 0, ], aes(y=tot_intercrop_area)) + geom_boxplot() + theme_bw() + labs(y="Total area per location", y="Count") + ggtitle("Intercrop") 
ggarrange(g1, g2)

# look at number of fields in each cropping system
table(fields_uga$Name)
table(fields_uga$Name, fields_uga$field_id)
propTable <- as.matrix(table(fields_uga$Name, fields_uga$field_id)) / matrix(colSums(as.matrix(table(fields_uga$Name, fields_uga$field_id))), nrow=2, ncol=76, byrow=TRUE)
levelplot(as.matrix(table(fields_uga$Name, fields_uga$field_id)), asp=1, xlab="", ylab="Field site", scales=list(y=list(lab=NULL)), main="Number of Fields")
levelplot(propTable, asp=1, xlab="", ylab="Field site", scales=list(y=list(lab=NULL)), main="Proportion of Fields", col.regions=heat.colors(50))
hist(propTable[1,], col="grey", main="", xlab="Proportion of Intercropped Fields", breaks=10)
box()


# figure of the cassava production (and weighted)
ggplot(dat_uga, aes(x=tot_cassava_area, y=tot_cassava_area_w)) + geom_point()

hist(dat_uga$tot_cassava_area_w, col="grey", breaks=20, xlab="Weighted Cassava Production (per m2)", main="")
box()

ggplot(dat_uga, aes(x=Centre_Lon, y=Centre_Lat, colour=cass_area_presence)) + geom_point(size=2) + labs(x="Longitude", y="Latitude") + theme_bw() + guides(size="none") + coord_fixed()
ggplot(dat_uga, aes(x=Centre_Lon, y=Centre_Lat, size=tot_cassava_area_w, colour=tot_cassava_area_w)) + geom_point() + labs(x="Longitude", y="Latitude") + theme_bw() + guides(size="none", colour=guide_legend(title="Weighted Cassava Production (per m2)")) + coord_fixed()

# size of cassava fields
mean(fields_uga$area_m2)
quantile(fields_uga$area_m2)
hist(fields_uga$area_m2)
aggregate(area_m2 ~ Name, data=fields_uga, FUN=mean)
aggregate(area_m2 ~ Name, data=fields_uga, FUN=median)
aggregate(area_m2 ~ Name, data=fields_uga, FUN=quantile, probs=0.75)

mean(dat_uga[dat_uga$tot_cassava_area > 0, "tot_monoculture_area"])
median(dat_uga[dat_uga$tot_cassava_area > 0, "tot_monoculture_area"])
quantile(dat_uga[dat_uga$tot_cassava_area > 0, "tot_monoculture_area"])

mean(dat_uga[dat_uga$tot_cassava_area > 0, "tot_intercrop_area"])
median(dat_uga[dat_uga$tot_cassava_area > 0, "tot_intercrop_area"])
quantile(dat_uga[dat_uga$tot_cassava_area > 0, "tot_intercrop_area"])
