####################################################################################################
# Langston Ford and Alan Tapper
#
# Who Gets to Live Near Rail Transit?
# Effects of Rail Transit Access and Walkability on the Housing Market in Large Southern Cities
#
# Models
#
# This file contains code for runnning several different models on our data. It takes in a shapefile
# for each city that was written by dataprepdriver.R. It applies several models and calculates
# several statistics, and then creates plots. The models and statistics are:
#
# Calculation of neighbors using queen contiguity
#
# OLS on the following pairs of variables (controlling for Walk Score®):
#   Walk distance, Income
#   Walk distance, Value
#
# Moran scatterplot and Moran's I statistics for the following variables:
#   Value
#   Walk Score®
# 
# Residuals of spatially lagged model for the following variables:
#   Value
####################################################################################################

library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)
library(spdep)
library(stargazer)
library(ggeffects)
library(spatialreg)
library(tmap)
library(car)

setwd("/Users/langstonford/Library/CloudStorage/OneDrive-RiceUniversity/Junior 2023-2024/2024 Spring Semester/SOCI 460/Research Project/Models")

# Values
atlanta_values <- st_read(dsn="atlanta.block_group_full_data", layer="atlanta.block_group_full_data",
                         stringsAsFactors=FALSE, options="ENCODING=latin1")
dallas_values <- st_read(dsn="dallas.block_group_full_data", layer="dallas.block_group_full_data",
                         stringsAsFactors=FALSE, options="ENCODING=latin1")
dc_values <- st_read(dsn="dc.block_group_full_data", layer="dc.block_group_full_data",
                         stringsAsFactors=FALSE, options="ENCODING=latin1")
houston_values <- st_read(dsn="houston.block_group_full_data", layer="houston.block_group_full_data",
                         stringsAsFactors=FALSE, options="ENCODING=latin1")
miami_values <- st_read(dsn="miami.block_group_full_data", layer="miami.block_group_full_data",
                         stringsAsFactors=FALSE, options="ENCODING=latin1")

# Stops
atlanta_stops <- st_read(dsn="atl.stops.no_repeats", layer="atl.stops.no_repeats",
                        stringsAsFactors=FALSE, options="ENCODING=latin1")
dallas_stops <- st_read(dsn="dal.stops.no_repeats", layer="dal.stops.no_repeats",
                        stringsAsFactors=FALSE, options="ENCODING=latin1")
dc_stops <- st_read(dsn="dc.stops.no_repeats", layer="dc.stops.no_repeats",
                        stringsAsFactors=FALSE, options="ENCODING=latin1")
houston_stops <- st_read(dsn="hou.stops.no_repeats", layer="hou.stops.no_repeats",
                        stringsAsFactors=FALSE, options="ENCODING=latin1")
miami_stops <- st_read(dsn="mia.stops.no_repeats", layer="mia.stops.no_repeats",
                        stringsAsFactors=FALSE, options="ENCODING=latin1")

ggplot() + 
  geom_sf(data = dallas_values, aes(fill = log(WALKDIS)), color="lightgrey") +
  geom_sf(data = dallas_stops, aes (color = "Transit Stop"))+
  scale_fill_distiller( palette = "RdYlBu", na.value = "white", direction = 1)+
  scale_color_manual( values = "black", name="")+
  labs( title = "Distance from Transit Stop by Dallas\nBlock Group Overlayed with Transit Stops",
        fill = "Log Distance\nfrom Transit Stop" ) + 
  coord_sf()+
  theme_void()+
  theme(plot.title = element_text(family = "Helvetica Bold", size =  20, hjust = .5),
        text = element_text(family = "Helvetica Bold"),
        legend.direction = "vertical",
        panel.grid = element_blank())


# Troglodytic Models for use on presentation
# Early version with less robust results and graphic focused 



# Create columns with ln values for each variable 

dallas_values <- dallas_values %>% 
  mutate(log.VALUE = log(VALUE),
         log.WALKTIM = log(WALKTIM),
         log.WALKDIS = log(WALKDIS),
         log.INCOME = log(INCOME))


# Filter model based off of 2 hour walk time
mod2 <- lm(log.VALUE ~ log.WALKDIS + WALKSCO, filter(dallas_values, WALKTIM <= 3600))
summary(mod2)
stargazer(mod2, type = "text")

# Create substantive effects model with Walking Distance 
p1 <- ggpredict(mod2, "log.WALKDIS")

plot(p1)+
  labs (title = "Controlled Substantive Effects of Walk Distance on Home Value in Dallas",
        y = "Log Walk Distance from Nearest Transit Stop",
        x = "Log Median Owner-Occupied Home Value by Block Group")+
  theme_minimal()+
  theme(text = element_text(family = "Helvetica Bold"),
        panel.grid = element_blank())
# Median Income

mod3 <- lm(log.INCOME ~ log.WALKDIS + WALKSCO, filter(dallas_values, WALKTIM <= 3600))
summary(mod3)
stargazer(mod3, type = "text")
p2<- ggpredict(mod3, "log.WALKDIS")

plot(p2)+
  labs (title = "Controlled Substantive Effects of Walk Distance on Income in Dallas",
        y = "Log Walk Distance from Nearest Transit Stop",
        x = "Log Median Household Income in Past 12 months by Block Group")+
  theme_minimal()+
  theme(text = element_text(family = "Helvetica Bold"),
        panel.grid = element_blank())



qqPlot(mod2)

dc_values$WALKTIM[dc_values$WALKTIM == 0] <- 1

# DC
mod3 <- lm(log(VALUE) ~  log(WALKTIM)+ WALKSCO, filter(dc_values, WALKTIM <= 1800))
summary(mod3)

mod4 <- lm(WALKTIM ~ WALKDIS,dc_values)
summary(mod4)

p1 <- ggpredict(mod3, "WALKTIM")
plot(p1)

qqPlot(mod2)


dv <- dallas_values %>%
  filter(!is.na(VALUE), !is.na(INCOME),!is.na(WALKDIS))
#  #  #  #  #  #  #  #  #  #  #  #  #  #


# STEP 2:  Construct Spatial Weights

# Queen Contiguity-based neighbors

# Filter out block groups with no neighbors
dv <- dv %>%
  filter(!sapply(poly2nb(dv, queen=TRUE), sum) == 0)

dv_nb.queen <- poly2nb(dv, queen=TRUE)



# visualize

plot(st_geometry(dv), border="gray", lwd=2)
plot(dv_nb.queen, coords=st_geometry(dv),
     lwd=2, col="firebrick", pch=16, cex=0.75, add=TRUE)


# Spatial Weights

# matrix format
bmat <- nb2mat(dv_nb.queen, style="B")

str(wmat)
wmat[1:10, 1:10]

# list format
blist <- nb2listw(dv_nb.queen, style="B")
str(wlist)



dv <- dv %>%
  filter( WALKTIM <= 3600)

dv <- dv %>%
  filter(!sapply(poly2nb(dv, queen=TRUE), sum) == 0)
dv_nb.queen <- poly2nb(dv, queen=TRUE)
clist <- nb2listw(dv_nb.queen, style="B")


# STEP 3:  Build Moran scatter plot



# Moran scatter plot using standardized Median House value

# Standardize the variable by subtracting the mean and dividing by the sd
stdvar.income <- scale(x=dv$VALUE, center=TRUE, scale=TRUE)
dv$income_std <- as.vector(stdvar.income)

# value and spatial lag
y <- dv$income_std
y_lag <- lag.listw(blist, dv$income_std)

# range of values
maxy <- max(abs(c(y, y_lag)))

# Moran's I value
morlm <- lm(y_lag ~ y)


plot(y, y_lag, xlim=c(-maxy, maxy), ylim=c(-maxy, maxy),
     xlab="Median Value of Owner-Occupied House by Block Group in Dallas", 
     ylab="Spatial Lag of Median Value")
title(paste0("Moran Scatterplot  I=", round(morlm$coefficients[2], 4)))

# slope (Moran’s I)
abline(morlm$coefficients[1], morlm$coefficients[2], col="red")

# mean values
abline(h=0, lty=2, col="blue")
abline(v=0, lty=2, col="blue")

# box for 1 standard deviation
lines(c(-1, -1, 1, 1, -1), c(-1, 1, 1, -1, -1), col="purple")

# labels
text(-maxy, maxy, adj=c(0, 0.75), xpd=NA, cex=0.8, paste0("(low, high)\n",
                                                          "location with low value\nneighbors with high values"))
text(maxy, maxy, adj=c(1, 0.75), xpd=NA, cex=0.8, paste0("(high, high)\n",
                                                         "location with high value\nneighbors with high values"))
text(maxy, -maxy, adj=c(1, 0.25), xpd=NA, cex=0.8, paste0("(high, low)\n",
                                                          "location with high value\nneighbors with low values"))
text(-maxy, -maxy, adj=c(0, 0.25), xpd=NA, cex=0.8, paste0("(low, low)\n",
                                                           "location with low value\nneighbors with low values"))


# Moran scatter plot using Walk Score

# Standardize the variable by subtracting the mean and dividing by the sd
stdvar.income <- scale(x=dv$WALKSCO, center=TRUE, scale=TRUE)
dv$income_std <- as.vector(stdvar.income)

# value and spatial lag
y <- dv$income_std
y_lag <- lag.listw(blist, dv$income_std)

# range of values
maxy <- max(abs(c(y, y_lag)))

# Moran's I value
morlm <- lm(y_lag ~ y)


plot(y, y_lag, xlim=c(-maxy, maxy), ylim=c(-maxy, maxy),
     xlab="Median Household Income by Block Group in Dallas", 
     ylab="Spatial Lag of Median Income")
title(paste0("Moran Scatterplot  I=", round(morlm$coefficients[2], 4)))

# slope (Moran’s I)
abline(morlm$coefficients[1], morlm$coefficients[2], col="red")

# mean values
abline(h=0, lty=2, col="blue")
abline(v=0, lty=2, col="blue")

# box for 1 standard deviation
lines(c(-1, -1, 1, 1, -1), c(-1, 1, 1, -1, -1), col="purple")

# labels
text(-maxy, maxy, adj=c(0, 0.75), xpd=NA, cex=0.8, paste0("(low, high)\n",
                                                          "location with low value\nneighbors with high values"))
text(maxy, maxy, adj=c(1, 0.75), xpd=NA, cex=0.8, paste0("(high, high)\n",
                                                         "location with high value\nneighbors with high values"))
text(maxy, -maxy, adj=c(1, 0.25), xpd=NA, cex=0.8, paste0("(high, low)\n",
                                                          "location with high value\nneighbors with low values"))
text(-maxy, -maxy, adj=c(0, 0.25), xpd=NA, cex=0.8, paste0("(low, low)\n",
                                                           "location with low value\nneighbors with low values"))



# Moran scatter plot using walk distance

# Standardize the variable by subtracting the mean and dividing by the sd
stdvar.income <- scale(x=dv$WALKDIS, center=TRUE, scale=TRUE)
dv$income_std <- as.vector(stdvar.income)

table(is.na(dv$WALKTIM))

# value and spatial lag
y <- dv$income_std
y_lag <- lag.listw(blist, dv$income_std)

# range of values
maxy <- max(abs(c(y, y_lag)))

# Moran's I value
morlm <- lm(y_lag ~ y)

op <- par(family = "Helvetica Bold")

plot(y, y_lag, xlim=c(-maxy, maxy), ylim=c(-maxy, maxy),
     xlab="Walking Distance from Nearest Transit Station by Block Group in Dallas", 
     ylab="Spatial Lag of Walking Distance")
title(paste0("Moran Scatterplot  I=", round(morlm$coefficients[2], 4)))
      par( op)

# slope (Moran’s I)
abline(morlm$coefficients[1], morlm$coefficients[2], col="red")

# mean values
abline(h=0, lty=2, col="blue")
abline(v=0, lty=2, col="blue")

# box for 1 standard deviation
lines(c(-1, -1, 1, 1, -1), c(-1, 1, 1, -1, -1), col="purple")

# labels
text(-maxy, maxy, adj=c(0, 0.75), xpd=NA, cex=0.8, paste0("(low, high)\n",
                                                          "location with low value\nneighbors with high values"))
text(maxy, maxy, adj=c(1, 0.75), xpd=NA, cex=0.8, paste0("(high, high)\n",
                                                         "location with high value\nneighbors with high values"))
text(maxy, -maxy, adj=c(1, 0.25), xpd=NA, cex=0.8, paste0("(high, low)\n",
                                                          "location with high value\nneighbors with low values"))
text(-maxy, -maxy, adj=c(0, 0.25), xpd=NA, cex=0.8, paste0("(low, low)\n",
                                                           "location with low value\nneighbors with low values"))

# Moran’s I using Housing Unit Value

moran.test(dv$VALUE, listw=blist)


# Moran’s I using Walk distance

moran.test(dv$WALKDIS, listw=blist)



# Extract the residuals from ols.fit using the resid() function  
# and add it to the tr object  (or use ols.fit$residuals)

dv.f <- filter(dallas_values, 
               WALKTIM <= 3600,
                !is.na(VALUE),
               !is.na(WALKDIS))


dv.f$resid <- resid(mod2)


# Map the residuals
tm_shape(dv.f) +
  tm_polygons("resid", textNA="NA", title="Residuals",
              pal=brewer.pal(5, "Reds")) +
  tm_style("white")

dv.f %>% 
  ggplot()+
  geom_sf(aes(fill = resid))+
  scale_fill_distiller( palette = "RdYlBu", na.value = "white", direction = 1)+
  labs( title = "Regression Residuals by Dallas Block Group",
        fill = "Residauls" ) + 
  coord_sf()+
  theme_void()+
  theme(plot.title = element_text(family = "Helvetica Bold", size =  20, hjust = .5),
        text = element_text(family = "Helvetica Bold"),
        legend.direction = "vertical",
        panel.grid = element_blank())


# Do you see any evidence of clustering (i.e., spatial autocorrelation)?


# Run the Moran's I test on the residuals

mod2.1 <- lm(log.VALUE ~ log.WALKDIS + WALKSCO, dv)

dv <- dv %>% 
  mutate(log.VALUE = log(VALUE),
         log.WALKTIM = log(WALKTIM),
         log.WALKDIS = log(WALKDIS),
         log.INCOME = log(INCOME))

summary(mod2.1)

lm.morantest(model=mod2.1, listw=clist)

# Evidence of spatial autocorrelation in the residuals
#  that is not accounted for by the covariates (i.e., GDP)


# Which type of spatial model is likely to be best?
modtest1 <- lm.RStests(model=mod2.1, listw=clist,
                       test=c("RSerr", "RSlag", "adjRSerr", "adjRSlag"))
summary(modtest1)


# STEP 3c:  Estimate the Spatially Lagged Model


# Spatially lagged model
sldv.fit <- lagsarlm(log.VALUE ~ log.WALKDIS, data=dv, listw=clist)
summary(sldv.fit)

# Results show that the effect of GDP on turnout is not as strong
#  as the previous (misspecified) OLS results.
# The spatial lag variable also has a sizable effect.

# The output includes the results from an LM test on the residuals of the model.
# The LM statistic tells us there is no longer significant
#  spatial autocorrelation in the residuals of this model.



dv$sldv_resid <- residuals(sldv.fit)
summary(dv$sldv_resid)


# Map the residuals
dv %>% 
  ggplot()+
  geom_sf(aes(fill = sldv_resid))+
  scale_fill_distiller( palette = "RdYlBu", na.value = "white", direction = 1)+
  labs( title = "Spatially Lagged Regression\nResiduals by Dallas Block Group",
        fill = "Residuals" ) + 
  coord_sf()+
  theme_void()+
  theme(plot.title = element_text(family = "Helvetica Bold", size =  20, hjust = .5),
        text = element_text(family = "Helvetica Bold"),
        legend.direction = "vertical",
        panel.grid = element_blank())

# Test for the presence of spatial autocorrelation in the residuals
moran.test(sldv.fit$residuals, listw=clist, alternative="two.sided")



# STEP 4a:  Impacts

# Examine the source of the spatial effects in the spatial lag model

sldv.effects <- impacts(sldv.fit, listw=clist)
sldv.effects

# These results show the Total, Direct and Indirect effect
# of each of the independent variables on the dependent variable

sldv.effects2 <- impacts(sldv.fit, listw=clist, R = 999)
sldv.effects2
summary(sldv.effects2, zstats=TRUE, short=TRUE)



# Neo-postmodernist stage Models implemented for use on paper
# Agghhhhh For loop

mv <- miami_values %>%
  filter(!is.na(VALUE), !is.na(INCOME),!is.na(WALKDIS))
hv <- houston_values %>%
  filter(!is.na(VALUE), !is.na(INCOME),!is.na(WALKDIS))
av <- atlanta_values %>%
  filter(!is.na(VALUE), !is.na(INCOME),!is.na(WALKDIS))
dcv <- dc_values %>%
  filter(!is.na(VALUE), !is.na(INCOME),!is.na(WALKDIS))
dv <- dallas_values %>%
  filter(!is.na(VALUE), !is.na(INCOME),!is.na(WALKDIS))


#  #  #  #  #  #  #  #  #  #  #  #  #  #


# STEP 2:  Construct Spatial Weights

# Queen Contiguity-based neighbors




cityname <- c("mv", "hv", "av", "dcv", "dv")

for (city in cityname) {
  
  # Filter out block groups with no neighbors
  assign(paste0(city),
    filter(get(city), !sapply(poly2nb(get(city), queen=TRUE), sum) == 0)
        )
  # Create neighbors
  assign(paste0(city, "_nb.queen"), poly2nb(get(city), queen=TRUE)
        )
  # Spatial Weights: list format
  assign( paste0("blist.", city), nb2listw(get(paste0(city, "_nb.queen")))
  )
} # Close for loop



# visualize

plot(st_geometry(av), border="gray", lwd=2)
plot(av_nb.queen, coords=st_geometry(av),
     lwd=2, col="firebrick", pch=16, cex=0.75, add=TRUE)





# STEP 3:  Build Moran scatter plot


# Change variable used to get different Moran's I plots
for (city in cityname) {
  
  # Standardize the variable by subtracting the mean and dividing by the sd
  stdvar.income <- scale(x=get(city)$VALUE, center=TRUE, scale=TRUE)
  assign( paste0(city), mutate(get(city),
                    income_std = as.vector(stdvar.income)))
}


  for (city in cityname) {
  # value and spatial lag
  assign(paste0("y.", city), 
         get(city)$income_std)
  
  assign(paste0("y_lag.", city),
          lag.listw(get(paste0("blist.", city)), get(city)$income_std))
  
  # range of values
  assign(paste0("maxy.", city), 
         max(abs(c(get(paste0("y.", city) ), get(paste0("y_lag.", city)))))
         )
  
  # Moran's I value
  assign(paste0("morlm.", city), 
         lm( get(paste0("y_lag.", city)) ~ get(paste0("y.", city)))
        )
  
}



#Plotting

#Rather than running a for loop, this "for loop" runs a singular value stored in an object
#replace value stored in cities for easily switch the city plotted

namecity = function(city) {
  if (city == "av") 
    {print("Atlanta")}
  else if (city == "hv") 
    {print("Houston")}
  else if (city == "mv") 
    {print("Miami")}
  else if (city == "dcv") 
  {print("D.C>")}
  else{print("Dallas")}
}


# The lord laughs at the wicked, for he knows their day is coming Psalms 37:12
cities <- "mv"

for (city in cities) {
    plot(get(paste0("y.", city)),
     get(paste0("y_lag.", city)),
     xlim=c(-get(paste0("maxy.", city)), 
            get(paste0("maxy.", city))), 
     ylim=c(-get(paste0("maxy.", city)),
            get(paste0("maxy.", city))),
     xlab=paste0("Median Home Value ", "by Block Group in " , namecity(city)), 
     ylab=paste0("Spatial Lag of ", "Median Home Value"))
title(paste0("Moran Scatterplot  I=", round(get(paste0("morlm.", city))$coefficients[2], 4)))

# slope (Moran’s I)
abline(get(paste0("morlm.", city))$coefficients[1], get(paste0("morlm.", city))$coefficients[2], col="red")

# mean values
abline(h=0, lty=2, col="blue")
abline(v=0, lty=2, col="blue")

# box for 1 standard deviation
lines(c(-1, -1, 1, 1, -1), c(-1, 1, 1, -1, -1), col="purple")

# labels
text(-get(paste0("maxy.", city)), get(paste0("maxy.", city)), adj=c(0, 0.75), xpd=NA, cex=0.8, paste0("(low, high)\n",
                                                          "location with low value\nneighbors with high values"))
text(get(paste0("maxy.", city)), get(paste0("maxy.", city)), adj=c(1, 0.75), xpd=NA, cex=0.8, paste0("(high, high)\n",
                                                         "location with high value\nneighbors with high values"))
text(get(paste0("maxy.", city)), -get(paste0("maxy.", city)), adj=c(1, 0.25), xpd=NA, cex=0.8, paste0("(high, low)\n",
                                                          "location with high value\nneighbors with low values"))
text(-get(paste0("maxy.", city)), -get(paste0("maxy.", city)), adj=c(0, 0.25), xpd=NA, cex=0.8, paste0("(low, low)\n",
                                                           "location with low value\nneighbors with low values"))

}



# I am not for looping this 

# Moran’s I using Housing Unit Value

for (city in cityname) {
  assign(paste0("moran.", city),
         moran.test(get(city)$VALUE, listw=get(paste0("blist.", city) )
                   )
         )
}


moran.av
moran.dcv 
moran.hv
moran.mv
moran.dv


# Moran’s I using Walk distance
for (city in cityname) {
  assign(paste0("moran.", city),
         moran.test(get(city)$WALKDIS, listw=get(paste0("blist.", city) )
         )
  )
}

moran.av
moran.dcv 
moran.hv
moran.mv
moran.dv




# Back to Modeling 
# Create Distance Filter
for (city in cityname){
  assign(paste0(city),
         filter(get(city),
                WALKDIS <= 4000))
  
}



# Create Log values and run regression models
for (city in cityname){
  assign(paste0(city),
    mutate(get(city),
           log.VALUE = log(VALUE),
           log.WALKTIM = log(WALKTIM),
           log.WALKDIS = log(WALKDIS),
           log.INCOME = log(INCOME))
        )
  assign(paste0("mod1.", city),
         lm(log.VALUE ~ log.WALKDIS + WALKSCO, data = get(city)))
  assign(paste0(city),
         mutate(get(city),
           resid = resid(get(paste0("mod1.", city)))
                )
          )
  
}


for (city in cityname){
assign(paste0("modws.", city),
       lm(log.WALKDIS ~ WALKSCO, data = get(city)))
}


stargazer(modws.av, modws.dcv, modws.hv, modws.mv, modws.dv, type = "text")

library (ggeffects)
p1 <- ggpredict(modws.av, "WALKSCO")
 plot(p1)
qqPlot(mod1.av)

plot(resid(mod1.dcv))



# Check with stargazer then export to html file
stargazer(mod1.av, mod1.dcv, mod1.hv, mod1.mv, mod1.dv, type = "text")


stargazer(mod1.av, mod1.dcv, mod1.hv, mod1.mv, mod1.dv, type="html",
          dep.var.labels=c("Atlanta","D.C.", "Houston", "Miami", "Dallas"),
          covariate.labels=c("Walking Distance (m)","WalkScore"), out="models4000.htm")


# Map the residuals
tm_shape(hv) +
  tm_polygons("resid", textNA="NA", title="Residuals",
              pal=brewer.pal(5, "Reds")) +
  tm_style("white")

hv %>% 
  ggplot()+
  geom_sf(aes(fill = resid))+
  scale_fill_distiller( palette = "RdYlBu", na.value = "white", direction = 1)+
  labs( title = "Regression Residuals by Dallas Block Group",
        fill = "Residauls" ) + 
  coord_sf()+
  theme_void()+
  theme(plot.title = element_text(family = "Helvetica Bold", size =  20, hjust = .5),
        text = element_text(family = "Helvetica Bold"),
        legend.direction = "vertical",
        panel.grid = element_blank())



#Do you see any evidence of clustering (i.e., spatial autocorrelation)?
  
  
# Run the Moran's I test on the residuals

summary(mod1.hv)


for (city in cityname) {
  assign(paste0("moran_mod.", city),
         lm.morantest(model = get(paste0("mod1.", city)), listw=get(paste0("blist.", city) )
         )
  )
}
moran_mod.av
moran_mod.dcv
moran_mod.hv
moran_mod.mv
moran_mod.dv

# Evidence of spatial autocorrelation in the residuals
#  that is not accounted for by the covariates (i.e., GDP)


# Which type of spatial model is likely to be best?
modtest1 <- lm.RStests(model=mod1.dv, listw=blist.dv,
                       test=c("RSerr", "RSlag", "adjRSerr", "adjRSlag"))
summary(modtest1)


# STEP 3c:  Estimate the Spatially Lagged Model


# Create spatially lagged models in for loop 
for (city in cityname) {
  assign(paste0("sldv.fit.", city),
         lagsarlm(log.VALUE ~ log.WALKDIS, data=get(city), listw=get(paste0("blist.", city) )
                  )
         )
  
}


# Reference code: sldv.fit <- lagsarlm(log.VALUE ~ log.WALKDIS, data=dv, listw=clist)
summary(sldv.fit.av)
summary(sldv.fit.dcv)
summary(sldv.fit.hv)
summary(sldv.fit.mv)
summary(sldv.fit.dv)

# Results show that the effect of Walking Distance on Home Value is not as great as we thought
#  as the previous (misspecified) OLS results.
# The spatial lag variable also has a sizable effect.

# The output includes the results from an LM test on the residuals of the model.
# The LM statistic tells us there is no longer significant
#  spatial autocorrelation in the residuals of this model.



for (city in cityname) {
  assign(city,
         mutate(get(city), 
                sldv_resid = residuals(get(paste0("sldv.fit.", city)))
         )
  )
  
}

summary(dv$sldv_resid)


# Map the residuals for Atlanta
av %>% 
  ggplot()+
  geom_sf(aes(fill = sldv_resid))+
  scale_fill_distiller( palette = "RdYlBu", na.value = "white", direction = 1)+
  labs( title = "Spatially Lagged Regression\nResiduals by Atlanta Block Group",
        fill = "Residuals" ) + 
  coord_sf()+
  theme_void()+
  theme(plot.title = element_text(family = "Helvetica Bold", size =  20, hjust = .5),
        text = element_text(family = "Helvetica Bold"),
        legend.direction = "vertical",
        panel.grid = element_blank())

# Map the residuals for D.C.
dcv %>% 
  ggplot()+
  geom_sf(aes(fill = sldv_resid))+
  scale_fill_distiller( palette = "RdYlBu", na.value = "white", direction = 1)+
  labs( title = "Spatially Lagged Regression\nResiduals by D.C. Block Group",
        fill = "Residuals" ) + 
  coord_sf()+
  theme_void()+
  theme(plot.title = element_text(family = "Helvetica Bold", size =  20, hjust = .5),
        text = element_text(family = "Helvetica Bold"),
        legend.direction = "vertical",
        panel.grid = element_blank())

# Map the residuals for Houston
hv %>% 
  ggplot()+
  geom_sf(aes(fill = sldv_resid))+
  scale_fill_distiller( palette = "RdYlBu", na.value = "white", direction = 1)+
  labs( title = "Spatially Lagged Regression\nResiduals by Houston Block Group",
        fill = "Residuals" ) + 
  coord_sf()+
  theme_void()+
  theme(plot.title = element_text(family = "Helvetica Bold", size =  20, hjust = .5),
        text = element_text(family = "Helvetica Bold"),
        legend.direction = "vertical",
        panel.grid = element_blank())

# Map the residuals for Miami
mv %>% 
  ggplot()+
  geom_sf(aes(fill = sldv_resid))+
  scale_fill_distiller( palette = "RdYlBu", na.value = "white", direction = 1)+
  labs( title = "Spatially Lagged Regression\nResiduals by Miami Block Group",
        fill = "Residuals" ) + 
  coord_sf()+
  theme_void()+
  theme(plot.title = element_text(family = "Helvetica Bold", size =  20, hjust = .5),
        text = element_text(family = "Helvetica Bold"),
        legend.direction = "vertical",
        panel.grid = element_blank())

# Map the residuals for Dallas
dv %>% 
  ggplot()+
  geom_sf(aes(fill = sldv_resid))+
  scale_fill_distiller( palette = "RdYlBu", na.value = "white", direction = 1)+
  labs( title = "Spatially Lagged Regression\nResiduals by Dallas Block Group",
        fill = "Residuals" ) + 
  coord_sf()+
  theme_void()+
  theme(plot.title = element_text(family = "Helvetica Bold", size =  20, hjust = .5),
        text = element_text(family = "Helvetica Bold"),
        legend.direction = "vertical",
        panel.grid = element_blank())



# Test for the presence of spatial autocorrelation in the residuals


moran.test(sldv.fit.dv$residuals, listw =blist.dv, alternative="two.sided")



# STEP 4a:  Impacts

# Examine the source of the spatial effects in the spatial lag model

sldv.effects <- impacts(sldv.fit, listw=clist)
sldv.effects

# These results show the Total, Direct and Indirect effect
# of each of the independent variables on the dependent variable

sldv.effects2 <- impacts(sldv.fit, listw=clist, R = 999)
sldv.effects2
summary(sldv.effects2, zstats=TRUE, short=TRUE)

