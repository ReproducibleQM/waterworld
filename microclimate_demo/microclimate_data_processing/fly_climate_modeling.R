library(car)
library(plyr)
library(nlme)
library(lme4)
library(ggplot2)
library(doBy)
library(reshape2)
library(AICcmodavg)
library(vegan)


#Packages for mapping
#load packages
maps.SDM.pack<-c("lmtest" ,"raster" ,"rgdal" ,"dismo" ,"rJava" ,"maptools" ,"ROCR" ,"randomForest" ,"mgcv" ,
                 "maps" ,"sp" ,"SDMTools" ,"rgeos" ,"gbm" ,"boot", "tigris", "devtools", "broom", "rworldmap",
                 "plyr", "ggplot2", "ggmap", "tigris", "leaflet", "acs", "spatial.tools", "data.table")
lapply(maps.SDM.pack, require, character.only=T) 
data(wrld_simpl)
require(stringr)

#Emily's to-do notes
#Think about how we could incorporate phylogeny into the model for diversity. 
#Add a predictor based on temperature variability?

#Read in data
fly.climate <-read.csv(file="../microclimate_data_processing/fly_data_with_weather.csv", header=T)
head(fly.climate)
names(fly.climate)

landcover <-read.csv(file="../microclimate_data_processing/site_ndvi_imperv.csv", header=T)
head(landcover)
names(landcover)

dist_natarea <- read.csv(file="../microclimate_data_processing/dis_CPAD.csv", header=T)
head(dist_natarea)
names(dist_natarea)

#Merge into one master dataframe
fly.climate <- merge(fly.climate, landcover, by="site")
fly.climate <- merge(fly.climate, dist_natarea, by= "site")

#Create a variable summarizing total phorid abundance.
fly.climate[,9:108]
fly.climate$total.abundance <- (rowSums(fly.climate[,9:108]))

#Create variables for species richness, diversity, and evenness.
#These metrics are per day to standardize for sampling intensity.
fly.climate$Richness <- round(specnumber(fly.climate[,9:108])/fly.climate$SamplingDurationDays,1)
fly.climate$Shannon_div <- round((apply(fly.climate[,9:108], 1, function(x) diversity(x))/fly.climate$SamplingDurationDays),2)
fly.climate$Evenness <- round(fly.climate$Shannon_div/log(round(fly.climate$Richness,0)),1)

#Check these calculations.
fly.climate.metrics <- fly.climate[,c(9,10,175:178)]
#fly.climate$total.abundance.perday <- fly.climate$total.abundance/fly.climate$SamplingDurationDays
#fly.climate$total.abundance.perday <- round(fly.climate$total.abundance.perday,1)

#Total number of flies in database
#colSums(fly.climate[,9:108])
#fly.climate$total.abundance <- sum(fly.climate[,9:108])

#Look at correlations between predictor vars
#air temp and soil temp are super correlated
#air temp and rh and kind of correlated
#soil temp and rh are not so correlated
scatterplotMatrix(~temp_air_mean+ rh_mean+ tempsoil1_mean, data= fly.climate,
                  main="Correlations: mean temp, relative humidity, soil moisture")

scatterplotMatrix(~temp_air_mean+ rh_mean+ tempsoil1_mean + evi_mean_50, data= fly.climate,
                  main="Correlations: mean temp, relative humidity, soil moisture")

scatterplotMatrix(~temp_air_mean+ rh_mean+ tempsoil1_mean + imp_mean_50, data= fly.climate,
                  main="Correlations: mean temp, relative humidity, soil moisture")

scatterplotMatrix(~temp_air_mean+ rh_mean+ tempsoil1_mean + evi_mean_100, data= fly.climate,
                  main="Correlations: mean temp, relative humidity, soil moisture")

scatterplotMatrix(~temp_air_mean+ rh_mean+ tempsoil1_mean + imp_mean_100, data= fly.climate,
                  main="Correlations: mean temp, relative humidity, soil moisture")


#Get summaries of fly data
summary(fly.climate)

#Melt the data so that all flies, abundance, evenness, diversity, and richness get their own rows. 
#"count" = variable of interest for richness, diversity evenness, which are already standardized for sampling duration.
#"count per day" = variable of interest for all species and total phorid abundance.
melted_data <- melt(fly.climate, id=c("X", "site","year", "week", "SiteName",
                           "SampleDateStart", "SamplingDurationDays", "SampleDateEnd",
                           "textdate", "temp_air_max","temp_air_min", 
                           "temp_air_mean", "rh_max", "rh_min", "rh_mean","tempsoil1_max",                 
                           "tempsoil1_min", "tempsoil1_mean", "tempsoil5_max", "tempsoil5_min",
                           "tempsoil5_mean", "dd_air", "dd_soil1", "dd_soil5", "dd_air_accum",                  
                           "dd_soil1_accum", "dd_soil5_accum", "dd_soil5_accum",
                           "evi_mean_50",                   
                           "evi_mean_100",                   "evi_mean_200",                  
                           "evi_mean_250",                   "evi_mean_300",                  
                           "evi_mean_400",                   "evi_mean_500",                  
                           "evi_mean_1000",                  "evi_mean_1500",                 
                           "evi_mean_2000",                  "evi_mean_3000",                 
                           "evi_sd_50",                      "evi_sd_100",                    
                           "evi_sd_200",                     "evi_sd_250",                    
                           "evi_sd_300",                     "evi_sd_400",                    
                          "evi_sd_500",                     "evi_sd_1000",                   
                           "evi_sd_1500",                    "evi_sd_2000",                   
                           "evi_sd_3000",                    "imp_mean_50",                   
                           "imp_mean_100",                   "imp_mean_200",                  
                           "imp_mean_250",                   "imp_mean_300",                  
                           "imp_mean_400",                   "imp_mean_500",                  
                           "imp_mean_1000",                  "imp_mean_1500",                 
                           "imp_mean_2000",                  "imp_mean_3000",                 
                           "imp_sd_50",                      "imp_sd_100",                    
                           "imp_sd_200",                     "imp_sd_250",                    
                           "imp_sd_300",                     "imp_sd_400",                    
                           "imp_sd_500",                     "imp_sd_1000",                   
                           "imp_sd_1500",                    "imp_sd_2000",                   
                           "imp_sd_3000", "Longitude", "Latitude", "dis_CPAD_meter"))

colnames(melted_data)[colnames(melted_data)=="value"] <- "count"
colnames(melted_data)[colnames(melted_data)=="variable"] <- "species"

cdata <- ddply(melted_data, c("species"), summarise,
               N    = length(count),
               mean = mean(count),
               sd   = sd(count),
               se   = sd / sqrt(N)
)


#Sort to determine what the most abundant species is, and start modeling on that species. 
ordered <- cdata[order(cdata$mean),]

#Create a variable representing adult count per day.
melted_data$avg_count_per_day <- melted_data$count/melted_data$SamplingDurationDays

#M agarici is the most abundant species by far, so let's focus 
#first on its climatic preferences.
names(melted_data)

sub_magarici <- subset(melted_data, species == "M.agarici")

#Preliminary plots showing M. agarici responses to climatic vars 
plot(sub_magarici$temp_air_mean, sub_magarici$avg_count_per_day)
plot(sub_magarici$temp_air_min, sub_magarici$avg_count_per_day)
plot(sub_magarici$temp_air_max, sub_magarici$avg_count_per_day)
plot(sub_magarici$rh_mean, sub_magarici$avg_count_per_day)
plot(sub_magarici$rh_min, sub_magarici$avg_count_per_day)
plot(sub_magarici$rh_max, sub_magarici$avg_count_per_day)
plot(sub_magarici$tempsoil1_mean, sub_magarici$avg_count_per_day)
plot(sub_magarici$tempsoil1_min, sub_magarici$avg_count_per_day)
plot(sub_magarici$tempsoil1_max, sub_magarici$avg_count_per_day)
plot(sub_magarici$tempsoil5_min, sub_magarici$avg_count_per_day)
plot(sub_magarici$tempsoil5_max, sub_magarici$avg_count_per_day)
plot(sub_magarici$dd_air_accum, sub_magarici$avg_count_per_day)

hist(sub_magarici$avg_count_per_day)
hist(log(sub_magarici$avg_count_per_day)+1)


#Preliminary plots to show M. agarici responses to land cover vars
plot(sub_magarici$evi_mean_50, sub_magarici$avg_count_per_day)
plot(sub_magarici$evi_mean_100, sub_magarici$avg_count_per_day)
plot(sub_magarici$evi_mean_200, sub_magarici$avg_count_per_day)
plot(sub_magarici$evi_mean_300, sub_magarici$avg_count_per_day)
plot(sub_magarici$evi_mean_500, sub_magarici$avg_count_per_day)
plot(sub_magarici$evi_mean_1000, sub_magarici$avg_count_per_day)
plot(sub_magarici$evi_mean_3000, sub_magarici$avg_count_per_day)
plot(sub_magarici$imp_mean_50, sub_magarici$avg_count_per_day)
plot(sub_magarici$imp_mean_100, sub_magarici$avg_count_per_day)
plot(sub_magarici$imp_mean_200, sub_magarici$avg_count_per_day)
plot(sub_magarici$imp_mean_300, sub_magarici$avg_count_per_day)
plot(sub_magarici$imp_mean_500, sub_magarici$avg_count_per_day)
plot(sub_magarici$imp_mean_1000, sub_magarici$avg_count_per_day)
plot(sub_magarici$imp_mean_3000, sub_magarici$avg_count_per_day)

hist(sub_magarici$avg_count_per_day)
hist(log(sub_magarici$avg_count_per_day)+1)



#Model selection for M. agarici using AICc
#Best air temperature predictor is mean temp
Cand.models <- list()
Cand.models[[1]] <- lme(log(avg_count_per_day+1)~I(temp_air_mean^2)+temp_air_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=sub_magarici, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(log(avg_count_per_day+1)~I(temp_air_min^2)+temp_air_min+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=sub_magarici, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(log(avg_count_per_day+1)~I(temp_air_max^2)+temp_air_max+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=sub_magarici, method="ML", na.action="na.omit")
Cand.models[[4]] <- lme(log(avg_count_per_day+1)~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=sub_magarici, method="ML", na.action="na.omit")
Modnames <- c("mean air temp", "min air temp", "max air temp", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))

summary(Cand.models[[1]])
plot(Cand.models[[1]])
#vif(Cand.models[[1]])



#best soil temp predictor
#best soil temperature predictor is min temp
sub_magarici_nonasoil <- subset(sub_magarici, tempsoil1_mean!= "NA" & tempsoil1_min != "NA" & tempsoil1_max != "NA")
Cand.models <- list()
Cand.models[[1]] <- lme(log(avg_count_per_day+1)~I(tempsoil1_mean^2)+ tempsoil1_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=sub_magarici_nonasoil, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(log(avg_count_per_day+1)~I(tempsoil1_min^2)+ tempsoil1_min+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=sub_magarici_nonasoil, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(log(avg_count_per_day+1)~I(tempsoil1_max^2)+ tempsoil1_max+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=sub_magarici_nonasoil, method="ML", na.action="na.omit")
Cand.models[[4]] <- lme(log(avg_count_per_day+1)~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=sub_magarici_nonasoil, method="ML", na.action="na.omit")
Modnames <- c("mean tempsoil1", "min tempsoil1", "max tempsoil1", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))

summary(Cand.models[[2]])

plot(Cand.models[[2]])


#best rh predictor
#best predictor is minimum rh
Cand.models <- list()
Cand.models[[1]] <- lme(log(avg_count_per_day+1)~I(rh_mean^2) + rh_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=sub_magarici, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(log(avg_count_per_day+1)~I(rh_min^2) + rh_min+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=sub_magarici, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(log(avg_count_per_day+1)~I(rh_max^2) + rh_max+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=sub_magarici, method="ML", na.action="na.omit")
Cand.models[[4]] <- lme(log(avg_count_per_day+1)~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=sub_magarici, method="ML", na.action="na.omit")
Modnames <- c("mean rh", "min rh", "max rh", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))

summary(Cand.models[[2]])
plot((Cand.models[[2]]))
#vif(Cand.models[[1]])


#choose between impervious and NDVI
#Null
Cand.models <- list()
Cand.models[[1]] <- lme(log(avg_count_per_day+1)~I(imp_mean_50^2) + imp_mean_50 + Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=sub_magarici, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(log(avg_count_per_day+1)~I(evi_mean_50^2) + evi_mean_50 + Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=sub_magarici, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(log(avg_count_per_day+1)~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=sub_magarici, method="ML", na.action="na.omit")
Modnames <- c("impervious", "ndvi", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))



#Plot most important predictors for M. agarici.
ggplot(sub_magarici, aes(x=temp_air_mean, y=avg_count_per_day, colour=SiteName)) + 
  geom_point() 

ggplot(sub_magarici, aes(x=tempsoil1_min, y=avg_count_per_day, colour=SiteName)) + 
  geom_point() + theme_bw()

ggplot(sub_magarici, aes(x=rh_min, y=avg_count_per_day, colour=SiteName)) + 
  geom_point() + theme_bw()

ggplot(sub_magarici, aes(x=week, y=avg_count_per_day, colour=SiteName)) + 
  geom_point() + theme_bw()



#Model with most impt predictors of M. agarici
magarici_nona <- subset(sub_magarici, temp_air_mean != "NA" & tempsoil1_min != "NA" & rh_min != "NA")
mod1_magarici <- lme(log(avg_count_per_day+1)~I(rh_min^2) + rh_min + I(tempsoil1_min^2) + tempsoil1_min +
                          I(temp_air_mean^2)+temp_air_mean + imp_mean_50 + Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, 
                        data=magarici_nona, method="ML", na.action="na.omit")
summary(mod1_magarici)
vif(mod1_magarici)

#Use Christie's code to look at fit for this model. 
plot(mod1_magarici)

x<-(1:length(magarici_nona$count))
magarici_nona$predicted<-(exp(predict(mod1_magarici, magarici_nona)))

plot(x, magarici_nona$predicted, ylim=c(0, 50))
plot(x, magarici_nona$avg_count_per_day, ylim=c(0, 50))

#let's reshape these data and make a nice plot to show how well the model fits peaks
model.performance<-as.data.frame(cbind(x,magarici_nona$predicted,magarici_nona$avg_count_per_day))
names(model.performance)[1]<-"number"
names(model.performance)[2]<-"Predicted"
names(model.performance)[3]<-"Observed"

model.performance.1<-melt(model.performance, id="number")

#now we can do a two faceted plot to show this
pal<-c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695', '#050561')
model.plot<-ggplot(model.performance.1, aes(number, value, fill=as.factor(variable)))+
  scale_fill_manual(values=pal)+
  geom_point(colour="black", pch=21, size=2)+
  theme_bw(base_size = 20)+
  ylim(0,50)+
  facet_wrap(~variable, ncol=1)+
  guides(fill=FALSE)+
  xlab("\nObservation number")+
  ylab("# Adults captured\n")
model.plot


#Look at these data with summary per site 
#to remove temporal effects and focus
#on differences across sites
magarici.summary<-ddply(magarici_nona, c("SiteName"), summarise, 
                        avg_mean_temp= mean(temp_air_mean),
                        max_magarici_per_day= max(avg_count_per_day),
                        avg_magarici_per_day= mean(avg_count_per_day), 
                        avgpred= mean(predicted),
                        n=length(avg_count_per_day),
                        se_mean_magarici= sd(avg_count_per_day)/sqrt(length(avg_count_per_day)))
head(magarici.summary)

#Plot mean abundance per day per site
magarici_mean <-ggplot(magarici.summary, aes(x=avg_mean_temp, y=avgpred))+
   # scale_fill_manual(values=pal)+
  
  geom_smooth(aes(x=avg_mean_temp, y=avgpred), color="black", method="lm", formula = y ~ poly(x,2), se=FALSE)+
#  geom_point(colour="black", pch=21, size=4)+
  geom_point(data=magarici.summary,aes(x=avg_mean_temp, y=avg_magarici_per_day,
                 fill=factor(SiteName)), color="black", pch=21, size=4) + 
  geom_errorbar(data=magarici.summary,aes(ymin=avg_magarici_per_day-se_mean_magarici, 
                    ymax=avg_magarici_per_day+se_mean_magarici), colour="black", width=.1) +
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Site"))+
  theme(legend.key=element_blank(), legend.text=element_text(size=10),
        axis.title=element_text(size=14), axis.text= element_text(size=12),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("Mean average temperature (°C)")+
  ylab(expression(paste("Mean ", italic("M. agarici"), " captured per day")))
magarici_mean
ggsave(file="/Users/emilymeineke/Dropbox/BioSCAN Weather Station Data (1)/microclimate_data_processing/phorid_figures_ekm/m.agarici_abundance.pdf", height=10, width=22, unit='cm')

#Plot peak abundance per site per day
magarici_max <-ggplot(magarici.summary, aes(x=avg_mean_temp, y=avgpred))+
  
  # scale_fill_manual(values=pal)+
  geom_smooth(aes(avg_mean_temp, avgpred), color="black", method="lm", formula = y ~ poly(x,2), se=FALSE)+
    geom_point(data=magarici.summary,aes(x=avg_mean_temp, y=max_magarici_per_day,
                                         fill=factor(SiteName)), color="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Site"))+
  theme(legend.key=element_blank(), legend.text=element_text(size=10),
        axis.title=element_text(size=14), axis.text= element_text(size=12),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("Mean average temperature")+
  ylab(expression(paste("Max ", italic("M. agarici"), " captured per day")))
magarici_max




#Model of total abundance
##plots of all species to get a feel for their responses

#Subset data to only include total abundance
T_abundance <- subset(melted_data, species == "total.abundance")

#Preliminary plots showing total community (abundance) responses to climatic vars 
plot(T_abundance$temp_air_mean, T_abundance$avg_count_per_day)
plot(T_abundance$temp_air_min, T_abundance$avg_count_per_day)
plot(T_abundance$temp_air_max, T_abundance$avg_count_per_day)
plot(T_abundance$rh_mean, T_abundance$avg_count_per_day)
plot(T_abundance$rh_min, T_abundance$avg_count_per_day)
plot(T_abundance$rh_max, T_abundance$avg_count_per_day)
plot(T_abundance$tempsoil1_mean, T_abundance$avg_count_per_day)
plot(T_abundance$tempsoil1_min, T_abundance$avg_count_per_day)
plot(T_abundance$tempsoil1_max, T_abundance$avg_count_per_day)
plot(T_abundance$tempsoil5_min, T_abundance$avg_count_per_day)
plot(T_abundance$tempsoil5_max, T_abundance$avg_count_per_day)
plot(T_abundance$dd_air_accum, T_abundance$avg_count_per_day)

hist(T_abundance$avg_count_per_day)
hist(log(T_abundance$avg_count_per_day)+1)


#Preliminary plots to show abundance responses to land cover vars
plot(T_abundance$evi_mean_50, T_abundance$avg_count_per_day)
plot(T_abundance$evi_mean_100, T_abundance$avg_count_per_day)
plot(T_abundance$evi_mean_200, T_abundance$avg_count_per_day)
plot(T_abundance$evi_mean_300, T_abundance$avg_count_per_day)
plot(T_abundance$evi_mean_500, T_abundance$avg_count_per_day)
plot(T_abundance$evi_mean_1000, T_abundance$avg_count_per_day)
plot(T_abundance$evi_mean_3000, T_abundance$avg_count_per_day)
plot(T_abundance$imp_mean_50, T_abundance$avg_count_per_day)
plot(T_abundance$imp_mean_100, T_abundance$avg_count_per_day)
plot(T_abundance$imp_mean_200, T_abundance$avg_count_per_day)
plot(T_abundance$imp_mean_300, T_abundance$avg_count_per_day)
plot(T_abundance$imp_mean_500, T_abundance$avg_count_per_day)
plot(T_abundance$imp_mean_1000, T_abundance$avg_count_per_day)
plot(T_abundance$imp_mean_3000, T_abundance$avg_count_per_day)

hist(T_abundance$avg_count_per_day)
hist(log(T_abundance$avg_count_per_day)+1)



#Model selection for abundance using AICc
#Best air temperature predictor is mean temp
Cand.models <- list()
Cand.models[[1]] <- lme(log(avg_count_per_day+1)~I(temp_air_mean^2)+temp_air_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=T_abundance, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(log(avg_count_per_day+1)~I(temp_air_min^2)+temp_air_min+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=T_abundance, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(log(avg_count_per_day+1)~I(temp_air_max^2)+temp_air_max+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=T_abundance, method="ML", na.action="na.omit")
Cand.models[[4]] <- lme(log(avg_count_per_day+1)~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=T_abundance, method="ML", na.action="na.omit")
Modnames <- c("mean air temp", "min air temp", "max air temp", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))

summary(Cand.models[[1]])
plot(Cand.models[[1]])
#vif(Cand.models[[1]])



#best soil temp predictor
#best soil temperature predictor is min temp
T_abundance_nonasoil <- subset(T_abundance, tempsoil1_mean!= "NA" & tempsoil1_min != "NA" & tempsoil1_max != "NA")
Cand.models <- list()
Cand.models[[1]] <- lme(log(avg_count_per_day+1)~I(tempsoil1_mean^2)+ tempsoil1_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=T_abundance_nonasoil, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(log(avg_count_per_day+1)~I(tempsoil1_min^2)+ tempsoil1_min+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=T_abundance_nonasoil, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(log(avg_count_per_day+1)~I(tempsoil1_max^2)+ tempsoil1_max+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=T_abundance_nonasoil, method="ML", na.action="na.omit")
Cand.models[[4]] <- lme(log(avg_count_per_day+1)~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=T_abundance_nonasoil, method="ML", na.action="na.omit")
Modnames <- c("mean tempsoil1", "min tempsoil1", "max tempsoil1", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))

summary(Cand.models[[2]])

plot(Cand.models[[2]])


#best rh predictor
#best predictor is minimum rh
Cand.models <- list()
Cand.models[[1]] <- lme(log(avg_count_per_day+1)~I(rh_mean^2) + rh_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=T_abundance, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(log(avg_count_per_day+1)~I(rh_min^2) + rh_min+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=T_abundance, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(log(avg_count_per_day+1)~I(rh_max^2) + rh_max+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=T_abundance, method="ML", na.action="na.omit")
Cand.models[[4]] <- lme(log(avg_count_per_day+1)~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=T_abundance, method="ML", na.action="na.omit")
Modnames <- c("mean rh", "min rh", "max rh", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))

summary(Cand.models[[2]])
plot((Cand.models[[2]]))
#vif(Cand.models[[1]])


#choose between impervious and NDVI
#Null
Cand.models <- list()
Cand.models[[1]] <- lme(log(avg_count_per_day+1)~I(imp_mean_50^2) + imp_mean_50 + Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=T_abundance, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(log(avg_count_per_day+1)~I(evi_mean_50^2) + evi_mean_50 + Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=T_abundance, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(log(avg_count_per_day+1)~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=T_abundance, method="ML", na.action="na.omit")
Modnames <- c("impervious", "ndvi", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))

summary(Cand.models[[2]])
summary(Cand.models[[1]])

#Plot most important predictors for total abundance
ggplot(T_abundance, aes(x=temp_air_mean, y=avg_count_per_day, colour=SiteName)) + 
  geom_point() 

ggplot(T_abundance, aes(x=tempsoil1_min, y=avg_count_per_day, colour=SiteName)) + 
  geom_point() + theme_bw()

ggplot(T_abundance, aes(x=rh_min, y=avg_count_per_day, colour=SiteName)) + 
  geom_point() + theme_bw()

ggplot(T_abundance, aes(x=week, y=avg_count_per_day, colour=SiteName)) + 
  geom_point() + theme_bw()



#Model with most impt predictors of total abundance
T_abundance_nona <- subset(T_abundance, temp_air_mean != "NA" & tempsoil1_min != "NA" & rh_min != "NA")
mod1_T_abundance <- lme(log(avg_count_per_day+1)~I(rh_min^2) + rh_min + I(tempsoil1_min^2) + tempsoil1_min +
                       I(temp_air_mean^2)+temp_air_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, 
                     data=T_abundance_nona, method="ML", na.action="na.omit")
summary(mod1_T_abundance)
vif(mod1_T_abundance)

#Use Christie's code to look at fit for this model. 
plot(mod1_T_abundance)

x<-(1:length(T_abundance_nona$count))
T_abundance_nona$predicted<-(exp(predict(mod1_T_abundance, T_abundance_nona)))

plot(x, T_abundance_nona$predicted, ylim=c(0, 100))
plot(x, T_abundance_nona$avg_count_per_day, ylim=c(0, 100))

#let's reshape these data and make a nice plot to show how well the model fits peaks
model.performance<-as.data.frame(cbind(x,T_abundance_nona$predicted,T_abundance_nona$avg_count_per_day))
names(model.performance)[1]<-"number"
names(model.performance)[2]<-"Predicted"
names(model.performance)[3]<-"Observed"

model.performance.1<-melt(model.performance, id="number")

#now we can do a two faceted plot to show this
pal<-c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695', '#050561')
model.plot<-ggplot(model.performance.1, aes(number, value, fill=as.factor(variable)))+
  scale_fill_manual(values=pal)+
  geom_point(colour="black", pch=21, size=2)+
  theme_bw(base_size = 20)+
  ylim(0,50)+
  facet_wrap(~variable, ncol=1)+
  guides(fill=FALSE)+
  xlab("\nObservation number")+
  ylab("# Adults captured\n")
model.plot


#Look at these data with summary per site 
#to remove temporal effects and focus
#on differences across sites
T_abundance.summary<-ddply(T_abundance_nona, c("SiteName"), summarise, 
                        avg_mean_temp= mean(temp_air_mean),
                        max_T_abundance_per_day= max(avg_count_per_day),
                        avg_T_abundance_per_day= mean(avg_count_per_day), 
                        avgpred= mean(predicted),
                        n=length(avg_count_per_day),
                        se_mean_T_abundance= sd(avg_count_per_day)/sqrt(length(avg_count_per_day)))
head(T_abundance.summary)


##Why won't the smoothed line show here or in the peak plot below? 
T_abundance_mean <-ggplot(T_abundance.summary, aes(x=avg_mean_temp, y=avgpred))+
  # scale_fill_manual(values=pal)+
  
  geom_smooth(aes(x=avg_mean_temp, y=avgpred), color="black", method="lm", formula = y ~ poly(x,2), se=FALSE)+
  #  geom_point(colour="black", pch=21, size=4)+
  geom_point(data=T_abundance.summary,aes(x=avg_mean_temp, y=avg_T_abundance_per_day,
                                       fill=factor(SiteName)), color="black", pch=21, size=4) + 
  geom_errorbar(data=T_abundance.summary,aes(ymin=avg_T_abundance_per_day-se_mean_T_abundance, 
                                          ymax=avg_T_abundance_per_day+se_mean_T_abundance), colour="black", width=.1) +
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Site"))+
  theme(legend.key=element_blank(), legend.text=element_text(size=10),
        axis.title=element_text(size=14), axis.text= element_text(size=12),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("Mean average temperature")+
  ylab("Mean phorids captured per day")
T_abundance_mean
ggsave(file="/Users/emilymeineke/Dropbox/BioSCAN Weather Station Data (1)/microclimate_data_processing/phorid_figures_ekm/total_abundance.pdf", height=10, width=22, unit='cm')

#Plotting peak abundance per site
T_abundance_max <-ggplot(T_abundance.summary, aes(x=avg_mean_temp, y=avgpred))+
  
  # scale_fill_manual(values=pal)+
  geom_smooth(aes(avg_mean_temp, avgpred), color="black", method="lm", formula = y ~ poly(x,2), se=FALSE)+
  geom_point(data=T_abundance.summary,aes(x=avg_mean_temp, y=max_T_abundance_per_day,
                                       fill=factor(SiteName)), color="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Site"))+
  theme(legend.key=element_blank(), legend.text=element_text(size=10),
        axis.title=element_text(size=14), axis.text= element_text(size=12),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("Mean average temperature")+
  ylab("Max phorids captured per day")
T_abundance_max



#Evenness

#Subset data to only include evenness
Even <- subset(melted_data, species == "Evenness")
Even <- subset(Even, count != "Inf")

#Preliminary plots showing total community (evenness) responses to climatic vars 
plot(Even$temp_air_mean, Even$count)
plot(Even$temp_air_min, Even$count)
plot(Even$temp_air_max, Even$count)
plot(Even$rh_mean, Even$count)
plot(Even$rh_min, Even$count)
plot(Even$rh_max, Even$count)
plot(Even$tempsoil1_mean, Even$count)
plot(Even$tempsoil1_min, Even$count)
plot(Even$tempsoil1_max, Even$count)
plot(Even$tempsoil5_min, Even$count)
plot(Even$tempsoil5_max, Even$count)
plot(Even$dd_air_accum, Even$count)

hist(Even$count)
hist(log(Even$count)+1)


#Preliminary plots to show how evenness responds to land cover vars
plot(Even$evi_mean_50, Even$count)
plot(Even$evi_mean_100, Even$count)
plot(Even$evi_mean_200, Even$count)
plot(Even$evi_mean_300, Even$count)
plot(Even$evi_mean_500, Even$count)
plot(Even$evi_mean_1000, Even$count)
plot(Even$evi_mean_3000, Even$count)
plot(Even$imp_mean_50, Even$count)
plot(Even$imp_mean_100, Even$count)
plot(Even$imp_mean_200, Even$count)
plot(Even$imp_mean_300, Even$count)
plot(Even$imp_mean_500, Even$count)
plot(Even$imp_mean_1000, Even$count)
plot(Even$imp_mean_3000, Even$count)

hist(Even$count)
hist(log(Even$count)+1)



#Model selection for evenness using AICc
#Best air temperature predictor is min temp
Cand.models <- list()
Cand.models[[1]] <- lme(count~I(temp_air_mean^2)+temp_air_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Even, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(count~I(temp_air_min^2)+temp_air_min+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Even, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(count~I(temp_air_max^2)+temp_air_max+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Even, method="ML", na.action="na.omit")
Cand.models[[4]] <- lme(count~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Even, method="ML", na.action="na.omit")
Modnames <- c("mean air temp", "min air temp", "max air temp", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))

summary(Cand.models[[2]])
plot(Cand.models[[2]])
#vif(Cand.models[[1]])



#best soil temp predictor
#best soil temperature predictor any, doesn't seem important
Even_nonasoil <- subset(Even, tempsoil1_mean!= "NA" & tempsoil1_min != "NA" & tempsoil1_max != "NA")
Cand.models <- list()
Cand.models[[1]] <- lme(count~I(tempsoil1_mean^2)+ tempsoil1_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Even_nonasoil, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(count~I(tempsoil1_min^2)+ tempsoil1_min+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Even_nonasoil, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(count~I(tempsoil1_max^2)+ tempsoil1_max+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Even_nonasoil, method="ML", na.action="na.omit")
Cand.models[[4]] <- lme(count~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Even_nonasoil, method="ML", na.action="na.omit")
Modnames <- c("mean tempsoil1", "min tempsoil1", "max tempsoil1", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))

summary(Cand.models[[2]])

plot(Cand.models[[2]])


#best rh predictor
#best predictor is mean rh, but it's the same as null
Cand.models <- list()
Cand.models[[1]] <- lme(count~I(rh_mean^2) + rh_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Even, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(count~I(rh_min^2) + rh_min+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Even, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(count~I(rh_max^2) + rh_max+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Even, method="ML", na.action="na.omit")
Cand.models[[4]] <- lme(count~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Even, method="ML", na.action="na.omit")
Modnames <- c("mean rh", "min rh", "max rh", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))

summary(Cand.models[[2]])
plot((Cand.models[[2]]))
#vif(Cand.models[[1]])

#Land cover vars
#Null
Cand.models <- list()
Cand.models[[1]] <- lme(log(avg_count_per_day+1)~I(imp_mean_50^2) + imp_mean_50 + Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Even, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(log(avg_count_per_day+1)~I(evi_mean_50^2) + evi_mean_50 + Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Even, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(log(avg_count_per_day+1)~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Even, method="ML", na.action="na.omit")
Modnames <- c("impervious", "ndvi", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))



#Model with most impt predictors of evenness
#Evenness declines with higher minimum temperatures. 
#If this is correct, evenness declines with distance from natural areas. That makes sense. 
Even_nona <- subset(Even, temp_air_min != "NA" & tempsoil1_min != "NA" & rh_max != "NA")
mod1_Even <- lme(count~ I(temp_air_min^2)+temp_air_min+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, 
                        data=Even_nona, method="ML", na.action="na.omit")
summary(mod1_Even)
vif(mod1_Even)

#Use Christie's code to look at fit for this model. 
plot(mod1_Even)

x<-(1:length(Even_nona$count))
Even_nona$predicted<-(predict(mod1_Even, Even_nona))

plot(x, Even_nona$predicted, ylim=c(0, 5))
plot(x, Even_nona$count, ylim=c(0, 5))

#let's reshape these data and make a nice plot to show how well the model fits peaks
model.performance<-as.data.frame(cbind(x,Even_nona$predicted,Even_nona$count))
names(model.performance)[1]<-"number"
names(model.performance)[2]<-"Predicted"
names(model.performance)[3]<-"Observed"

model.performance.1<-melt(model.performance, id="number")

#now we can do a two faceted plot to show this
pal<-c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695', '#050561')
model.plot<-ggplot(model.performance.1, aes(number, value, fill=as.factor(variable)))+
  scale_fill_manual(values=pal)+
  geom_point(colour="black", pch=21, size=2)+
  theme_bw(base_size = 20)+
  ylim(0,1)+
  facet_wrap(~variable, ncol=1)+
  guides(fill=FALSE)+
  xlab("\nObservation number")+
  ylab("Evenness")
model.plot


#Look at these data with summary per site 
#to remove temporal effects and focus
#on differences across sites
Even.summary<-ddply(Even_nona, c("SiteName"), summarise, 
                           avg_min_temp= mean(temp_air_min),
                           max_Even_per_day= max(count),
                           avg_Even_per_day= mean(count), 
                           avgpred= mean(predicted),
                    avg_dis_natarea = mean(dis_CPAD_meter),
                           n=length(count),
                           se_mean_Even= sd(count)/sqrt(length(count)))
head(Even.summary)


##Plot mean evenness per site
#Temperature
Even_mean <- ggplot(Even.summary, aes(x=avg_min_temp, y=avgpred))+
  # scale_fill_manual(values=pal)+
  
  geom_smooth(aes(x=avg_min_temp, y=avgpred), color="black", method="lm", se=FALSE)+
  #  geom_point(colour="black", pch=21, size=4)+
  geom_point(data=Even.summary,aes(x=avg_min_temp, y=avg_Even_per_day,
                                          fill=factor(SiteName)), color="black", pch=21, size=4) + 
  geom_errorbar(data=Even.summary,aes(ymin=avg_Even_per_day-se_mean_Even, 
                                             ymax=avg_Even_per_day+se_mean_Even), colour="black", width=.1) +
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Site"))+
  theme(legend.key=element_blank(), legend.text=element_text(size=10),
        axis.title=element_text(size=14), axis.text= element_text(size=12),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("Mean minimum temperature")+
  ylab("Mean Pielou evenness")
Even_mean
ggsave(file="/Users/emilymeineke/Dropbox/BioSCAN Weather Station Data (1)/microclimate_data_processing/phorid_figures_ekm/evenness_mintemp.pdf", height=10, width=22, unit='cm')


#Plot peak evenness per site
Even_max <-ggplot(Even.summary, aes(x=avg_min_temp, y=avgpred))+
  
  # scale_fill_manual(values=pal)+
  geom_smooth(aes(avg_min_temp, avgpred), color="black", method="lm", formula = y ~ poly(x,2), se=FALSE)+
  geom_point(data=Even.summary,aes(x=avg_min_temp, y=max_Even_per_day,
                                          fill=factor(SiteName)), color="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Site"))+
  theme(legend.key=element_blank(), legend.text=element_text(size=10))+
  xlab("Mean minimum temperature")+
  ylab("Max Pielou evenness")
Even_max


#Distance from natural areas
#Plot average
#I don't know why these errorbars don't have horizontal lines like the others. 
Even_dist <-ggplot(Even.summary, aes(x=avg_dis_natarea, y=avgpred))+
  geom_smooth(aes(x=avg_dis_natarea, y=avgpred), color="black", method="lm", se=FALSE)+
  geom_point(data=Even.summary,aes(x=avg_dis_natarea, y=avg_Even_per_day,
                                   fill=factor(SiteName)), color="black", pch=21, size=4) + 
  geom_errorbar(data=Even.summary, aes(ymin=avg_Even_per_day-se_mean_Even, 
                                      ymax=avg_Even_per_day+se_mean_Even), colour="black", width=.1) +
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Site"))+
  theme(legend.key=element_blank(), legend.text=element_text(size=10),
        axis.title=element_text(size=14), axis.text= element_text(size=12),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("Distance to natural area")+
  ylab("Mean Pielou evenness")
Even_dist
ggsave(file="/Users/emilymeineke/Dropbox/BioSCAN Weather Station Data (1)/microclimate_data_processing/phorid_figures_ekm/evenness_distnaturalareas.pdf", height=10, width=22, unit='cm')



#Richness
#Subset data to only include richness
Rich <- subset(melted_data, species == "Richness")

#Preliminary plots showing total community (abundance) responses to climatic vars 
plot(Rich$temp_air_mean, Rich$count)
plot(Rich$temp_air_min, Rich$count)
plot(Rich$temp_air_max, Rich$count)
plot(Rich$rh_mean, Rich$count)
plot(Rich$rh_min, Rich$count)
plot(Rich$rh_max, Rich$count)
plot(Rich$tempsoil1_mean, Rich$count)
plot(Rich$tempsoil1_min, Rich$count)
plot(Rich$tempsoil1_max, Rich$count)
plot(Rich$tempsoil5_min, Rich$count)
plot(Rich$tempsoil5_max, Rich$count)
plot(Rich$dd_air_accum, Rich$count)

hist(Rich$count)
hist(log(Rich$count)+1)


#Preliminary plots to show how richness responds to land cover vars
plot(Rich$evi_mean_50, Rich$count)
plot(Rich$evi_mean_100, Rich$count)
plot(Rich$evi_mean_200, Rich$count)
plot(Rich$evi_mean_300, Rich$count)
plot(Rich$evi_mean_500, Rich$count)
plot(Rich$evi_mean_1000, Rich$count)
plot(Rich$evi_mean_3000, Rich$count)
plot(Rich$imp_mean_50, Rich$count)
plot(Rich$imp_mean_100, Rich$count)
plot(Rich$imp_mean_200, Rich$count)
plot(Rich$imp_mean_300, Rich$count)
plot(Rich$imp_mean_500, Rich$count)
plot(Rich$imp_mean_1000, Rich$count)
plot(Rich$imp_mean_3000, Rich$count)

hist(Rich$count)
hist(log(Rich$count)+1)



#Model selection for richness using AICc
#Best air temperature predictor is mean temp
Cand.models <- list()
Cand.models[[1]] <- lme(log(count+1)~I(temp_air_mean^2)+temp_air_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Rich, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(log(count+1)~I(temp_air_min^2)+temp_air_min+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Rich, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(log(count+1)~I(temp_air_max^2)+temp_air_max+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Rich, method="ML", na.action="na.omit")
Cand.models[[4]] <- lme(log(count+1)~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Rich, method="ML", na.action="na.omit")
Modnames <- c("mean air temp", "min air temp", "max air temp", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))

summary(Cand.models[[1]])
plot(Cand.models[[1]])
#vif(Cand.models[[1]])



#best soil temp predictor
#best soil temperature predictor is min temp
Rich_nonasoil <- subset(Rich, tempsoil1_mean!= "NA" & tempsoil1_min != "NA" & tempsoil1_max != "NA")
Cand.models <- list()
Cand.models[[1]] <- lme(log(count+1)~I(tempsoil1_mean^2)+ tempsoil1_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Rich_nonasoil, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(log(count+1)~I(tempsoil1_min^2)+ tempsoil1_min+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Rich_nonasoil, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(log(count+1)~I(tempsoil1_max^2)+ tempsoil1_max+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Rich_nonasoil, method="ML", na.action="na.omit")
Cand.models[[4]] <- lme(log(count+1)~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Rich_nonasoil, method="ML", na.action="na.omit")
Modnames <- c("mean tempsoil1", "min tempsoil1", "max tempsoil1", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))

summary(Cand.models[[2]])

plot(Cand.models[[2]])


#best rh predictor
#best predictor is mean rh
Cand.models <- list()
Cand.models[[1]] <- lme(log(count+1)~I(rh_mean^2) + rh_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Rich, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(log(count+1)~I(rh_min^2) + rh_min+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Rich, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(log(count+1)~I(rh_max^2) + rh_max+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Rich, method="ML", na.action="na.omit")
Cand.models[[4]] <- lme(log(count+1)~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Rich, method="ML", na.action="na.omit")
Modnames <- c("mean rh", "min rh", "max rh", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))

summary(Cand.models[[1]])
plot((Cand.models[[1]]))
#vif(Cand.models[[1]])


#Land cover vars
#Null
Cand.models <- list()
Cand.models[[1]] <- lme(log(avg_count_per_day+1)~I(imp_mean_50^2) + imp_mean_50 + Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Rich, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(log(avg_count_per_day+1)~I(evi_mean_50^2) + evi_mean_50 + Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Rich, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(log(avg_count_per_day+1)~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Rich, method="ML", na.action="na.omit")
Modnames <- c("impervious", "ndvi", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))



#Model with most impt predictors of richness
Rich_nona <- subset(Rich, temp_air_mean != "NA" & tempsoil1_min != "NA" & rh_mean != "NA")
mod1_Rich <- lme(count~I(rh_mean^2) + rh_mean + I(tempsoil1_min^2) + tempsoil1_min +
                          I(temp_air_mean^2)+temp_air_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, 
                        data=Rich_nona, method="ML", na.action="na.omit")
summary(mod1_Rich)
vif(mod1_Rich)

#Use Christie's code to look at fit for this model. 
plot(mod1_Rich)

x<-(1:length(Rich_nona$count))
Rich_nona$predicted<-(predict(mod1_Rich, Rich_nona))

plot(x, Rich_nona$predicted, ylim=c(0, 10))
plot(x, Rich_nona$count, ylim=c(0, 10))

#let's reshape these data and make a nice plot to show how well the model fits peaks
model.performance<-as.data.frame(cbind(x,Rich_nona$predicted,Rich_nona$count))
names(model.performance)[1]<-"number"
names(model.performance)[2]<-"Predicted"
names(model.performance)[3]<-"Observed"

model.performance.1<-melt(model.performance, id="number")

#now we can do a two faceted plot to show this
pal<-c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695', '#050561')
model.plot<-ggplot(model.performance.1, aes(number, value, fill=as.factor(variable)))+
  scale_fill_manual(values=pal)+
  geom_point(colour="black", pch=21, size=2)+
  theme_bw(base_size = 20)+
  ylim(0,10)+
  facet_wrap(~variable, ncol=1)+
  guides(fill=FALSE)+
  xlab("\nObservation number")+
  ylab("# Adults captured\n")
model.plot


#Look at these data with summary per site 
#to remove temporal effects and focus
#on differences across sites
Rich.summary<-ddply(Rich_nona, c("SiteName"), summarise, 
                           avg_mean_temp= mean(temp_air_mean),
                           max_Rich_per_day= max(count),
                           avg_Rich_per_day= mean(count), 
                           avgpred= mean(predicted),
                           avg_mean_rh = mean(rh_mean),
                           n=length(count),
                           se_mean_Rich= sd(count)/sqrt(length(count)))
head(Rich.summary)


#Mean temperature
#Average richness per trap per day
Rich_mean <-ggplot(Rich.summary, aes(x=avg_mean_temp, y=avgpred))+
  # scale_fill_manual(values=pal)+
  
  geom_smooth(aes(x=avg_mean_temp, y=avgpred), color="black", method="lm", formula = y ~ poly(x,2), se=FALSE)+
  #  geom_point(colour="black", pch=21, size=4)+
  geom_point(data=Rich.summary,aes(x=avg_mean_temp, y=avg_Rich_per_day,
                                          fill=factor(SiteName)), color="black", pch=21, size=4) + 
  geom_errorbar(data=Rich.summary,aes(ymin=avg_Rich_per_day-se_mean_Rich, 
                                             ymax=avg_Rich_per_day+se_mean_Rich), colour="black", width=.1) +
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Site"))+
  theme(legend.key=element_blank(), legend.text=element_text(size=10),
        axis.title=element_text(size=14), axis.text= element_text(size=12),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("Mean average temperature")+
  ylab("Mean species richness")
Rich_mean
ggsave(file="/Users/emilymeineke/Dropbox/BioSCAN Weather Station Data (1)/microclimate_data_processing/phorid_figures_ekm/richness_meantemp.pdf", height=10, width=22, unit='cm')

#Plotting peak abundance per site
Rich_max <-ggplot(Rich.summary, aes(x=avg_mean_temp, y=avgpred))+
  
  # scale_fill_manual(values=pal)+
  geom_smooth(aes(avg_mean_temp, avgpred), color="black", method="lm", formula = y ~ poly(x,2), se=FALSE)+
  geom_point(data=Rich.summary,aes(x=avg_mean_temp, y=max_Rich_per_day,
                                          fill=factor(SiteName)), color="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Site"))+
  theme(legend.key=element_blank(), legend.text=element_text(size=10),
        axis.title=element_text(size=14), axis.text= element_text(size=12),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("Mean average temperature")+
  ylab("Max species richness")
Rich_max



#This is interesting: Mean relative humidity is also important for richness
Rich_mean <-ggplot(Rich.summary, aes(x=avg_mean_rh, y=avgpred))+
  geom_smooth(aes(x=avg_mean_rh, y=avgpred), color="black", method="lm", formula = y ~ poly(x,2), se=FALSE)+
  geom_point(data=Rich.summary,aes(x=avg_mean_rh, y=avg_Rich_per_day,
                                   fill=factor(SiteName)), color="black", pch=21, size=4) + 
  geom_errorbar(data=Rich.summary,aes(ymin=avg_Rich_per_day-se_mean_Rich, 
                                      ymax=avg_Rich_per_day+se_mean_Rich), colour="black", width=.1) +
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Site"))+
  theme(legend.key=element_blank(), legend.text=element_text(size=10),
        axis.title=element_text(size=14), axis.text= element_text(size=12),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  theme(legend.key=element_blank(), legend.text=element_text(size=10))+
  xlab("Mean average relative humidity")+
  ylab("Mean species richness")
Rich_mean
ggsave(file="/Users/emilymeineke/Dropbox/BioSCAN Weather Station Data (1)/microclimate_data_processing/phorid_figures_ekm/richness_meanrelativehumidity.pdf", height=10, width=22, unit='cm')


#Plotting peak richness per site
Rich_max <-ggplot(Rich.summary, aes(x=avg_mean_rh, y=avgpred))+
  
  # scale_fill_manual(values=pal)+
  geom_smooth(aes(avg_mean_rh, avgpred), color="black", method="lm", formula = y ~ poly(x,2), se=FALSE)+
  geom_point(data=Rich.summary,aes(x=avg_mean_rh, y=max_Rich_per_day,
                                   fill=factor(SiteName)), color="black", pch=21, size=4)+
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Site"))+
  theme(legend.key=element_blank(), legend.text=element_text(size=10))+
  xlab("Mean average relative humidity")+
  ylab("Max species richness")
Rich_max




#Alpha Diversity 
#Subset data to only include total abundance
Shannon.div <- subset(melted_data, species == "Shannon_div")

#Preliminary plots showing total community (abundance) responses to climatic vars 
plot(Shannon.div$temp_air_mean, Shannon.div$count)
plot(Shannon.div$temp_air_min, Shannon.div$count)
plot(Shannon.div$temp_air_max, Shannon.div$count)
plot(Shannon.div$rh_mean, Shannon.div$count)
plot(Shannon.div$rh_min, Shannon.div$count)
plot(Shannon.div$rh_max, Shannon.div$count)
plot(Shannon.div$tempsoil1_mean, Shannon.div$count)
plot(Shannon.div$tempsoil1_min, Shannon.div$count)
plot(Shannon.div$tempsoil1_max, Shannon.div$count)
plot(Shannon.div$tempsoil5_min, Shannon.div$count)
plot(Shannon.div$tempsoil5_max, Shannon.div$count)
plot(Shannon.div$dd_air_accum, Shannon.div$count)

hist(Shannon.div$count)
hist(log(Shannon.div$count)+1)


#Preliminary plots to show how diversity responds to land cover vars
plot(Shannon.div$evi_mean_50, Shannon.div$count)
plot(Shannon.div$evi_mean_100, Shannon.div$count)
plot(Shannon.div$evi_mean_200, Shannon.div$count)
plot(Shannon.div$evi_mean_300, Shannon.div$count)
plot(Shannon.div$evi_mean_500, Shannon.div$count)
plot(Shannon.div$evi_mean_1000, Shannon.div$count)
plot(Shannon.div$evi_mean_3000, Shannon.div$count)
plot(Shannon.div$imp_mean_50, Shannon.div$count)
plot(Shannon.div$imp_mean_100, Shannon.div$count)
plot(Shannon.div$imp_mean_200, Shannon.div$count)
plot(Shannon.div$imp_mean_300, Shannon.div$count)
plot(Shannon.div$imp_mean_500, Shannon.div$count)
plot(Shannon.div$imp_mean_1000, Shannon.div$count)
plot(Shannon.div$imp_mean_3000, Shannon.div$count)

hist(Shannon.div$count)
hist(log(Shannon.div$count)+1)



#Model selection for diversity using AICc
#Best air temperature predictor is max temp
Cand.models <- list()
Cand.models[[1]] <- lme(count~I(temp_air_mean^2)+temp_air_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Shannon.div, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(count~I(temp_air_min^2)+temp_air_min+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Shannon.div, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(count~I(temp_air_max^2)+temp_air_max+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Shannon.div, method="ML", na.action="na.omit")
Cand.models[[4]] <- lme(count~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Shannon.div, method="ML", na.action="na.omit")
Modnames <- c("mean air temp", "min air temp", "max air temp", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))

summary(Cand.models[[1]])
plot(Cand.models[[1]])
#vif(Cand.models[[1]])



#best soil temp predictor
#Null
Shannon.div_nonasoil <- subset(Shannon.div, tempsoil1_mean!= "NA" & tempsoil1_min != "NA" & tempsoil1_max != "NA")
Cand.models <- list()
Cand.models[[1]] <- lme(count~I(tempsoil1_mean^2)+ tempsoil1_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Shannon.div_nonasoil, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(count~I(tempsoil1_min^2)+ tempsoil1_min+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Shannon.div_nonasoil, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(count~I(tempsoil1_max^2)+ tempsoil1_max+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Shannon.div_nonasoil, method="ML", na.action="na.omit")
Cand.models[[4]] <- lme(count~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Shannon.div_nonasoil, method="ML", na.action="na.omit")
Modnames <- c("mean tempsoil1", "min tempsoil1", "max tempsoil1", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))


#best rh predictor
#best predictor is mean rh
Cand.models <- list()
Cand.models[[1]] <- lme(count~I(rh_mean^2) + rh_mean+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Shannon.div, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(count~I(rh_min^2) + rh_min+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Shannon.div, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(count~I(rh_max^2) + rh_max+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Shannon.div, method="ML", na.action="na.omit")
Cand.models[[4]] <- lme(count~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Shannon.div, method="ML", na.action="na.omit")
Modnames <- c("mean rh", "min rh", "max rh", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))

summary(Cand.models[[1]])
plot((Cand.models[[1]]))
#vif(Cand.models[[1]])


#Land cover vars
#Null
Cand.models <- list()
Cand.models[[1]] <- lme(log(avg_count_per_day+1)~I(imp_mean_50^2) + imp_mean_50 + Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Shannon.div, method="ML", na.action="na.omit")
Cand.models[[2]] <- lme(log(avg_count_per_day+1)~I(evi_mean_50^2) + evi_mean_50 + Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Shannon.div, method="ML", na.action="na.omit")
Cand.models[[3]] <- lme(log(avg_count_per_day+1)~1+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, data=Shannon.div, method="ML", na.action="na.omit")
Modnames <- c("impervious", "ndvi", "Null")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))





#Plot most important predictors for total abundance
ggplot(Shannon.div, aes(x=temp_air_mean, y=count, colour=SiteName)) + 
  geom_point() 

ggplot(Shannon.div, aes(x=tempsoil1_min, y=count, colour=SiteName)) + 
  geom_point() + theme_bw()

ggplot(Shannon.div, aes(x=rh_mean, y=count, colour=SiteName)) + 
  geom_point() + theme_bw()

ggplot(Shannon.div, aes(x=week, y=count, colour=SiteName)) + 
  geom_point() + theme_bw()



#Model with most impt predictors of alpha diversity
Shannon.div_nona <- subset(Shannon.div, temp_air_max != "NA" & rh_mean != "NA")
mod2_Shannon.div <- lme(count~I(rh_mean^2) + rh_mean + I(temp_air_max^2)+temp_air_max+ Latitude + Longitude + dis_CPAD_meter, random = ~ 1 |site, 
                        data=Shannon.div_nona, method="ML", na.action="na.omit")
summary(mod2_Shannon.div)
vif(mod2_Shannon.div)

#Use Christie's code to look at fit for this model. 
plot(mod1_Shannon.div)

x<-(1:length(Shannon.div_nona$count))
Shannon.div_nona$predicted<-(predict(mod2_Shannon.div, Shannon.div_nona))

#Predictions are really low. Why? Something went wonky with a calculation. 
plot(x, Shannon.div_nona$predicted, ylim=c(0, 2))
plot(x, Shannon.div_nona$count, ylim=c(0, 2))

#let's reshape these data and make a nice plot to show how well the model fits peaks
model.performance<-as.data.frame(cbind(x,Shannon.div_nona$predicted,Shannon.div_nona$count))
names(model.performance)[1]<-"number"
names(model.performance)[2]<-"Predicted"
names(model.performance)[3]<-"Observed"

model.performance.1<-melt(model.performance, id="number")

#now we can do a two faceted plot to show this
pal<-c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695', '#050561')
model.plot<-ggplot(model.performance.1, aes(number, value, fill=as.factor(variable)))+
  scale_fill_manual(values=pal)+
  geom_point(colour="black", pch=21, size=2)+
  theme_bw(base_size = 20)+
  ylim(0,2)+
  facet_wrap(~variable, ncol=1)+
  guides(fill=FALSE)+
  xlab("\nObservation number")+
  ylab("# Adults captured\n")
model.plot


#Look at these data with summary per site 
#to remove temporal effects and focus
#on differences across sites
Shannon.div.summary<-ddply(Shannon.div_nona, c("SiteName"), summarise, 
                           avg_max_temp= mean(temp_air_max),
                           max_Shannon.div_per_day= max(count),
                           avg_Shannon.div_per_day= mean(count), 
                           avg_rh = mean(rh_mean),
                           avgpred= mean(predicted),
                           n=length(count),
                           se_mean_Shannon.div= sd(count)/sqrt(length(count)))
head(Shannon.div.summary)


Shannon.div_maxtemp <-ggplot(Shannon.div.summary, aes(x=avg_max_temp, y=avgpred))+
  # scale_fill_manual(values=pal)+
  
  geom_smooth(aes(x=avg_max_temp, y=avgpred), color="black", method="lm", formula = y ~ poly(x,2), se=FALSE)+
  #  geom_point(colour="black", pch=21, size=4)+
  geom_point(data=Shannon.div.summary,aes(x=avg_max_temp, y=avg_Shannon.div_per_day,
                                          fill=factor(SiteName)), color="black", pch=21, size=4) + 
  geom_errorbar(data=Shannon.div.summary,aes(ymin=avg_Shannon.div_per_day-se_mean_Shannon.div, 
                                             ymax=avg_Shannon.div_per_day+se_mean_Shannon.div), colour="black", width=.1) +
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Site"))+
  theme(legend.key=element_blank(), legend.text=element_text(size=10),
        axis.title=element_text(size=14), axis.text= element_text(size=12),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("Average daily maximum temperature")+ #Make sure this is correct
  ylab("Mean Shannon-Weiner diversity")
Shannon.div_maxtemp
ggsave(file="/Users/emilymeineke/Dropbox/BioSCAN Weather Station Data (1)/microclimate_data_processing/phorid_figures_ekm/alphadiv_maxtemp.pdf", height=10, width=22, unit='cm')

#Plot mean relative humidity ^2
Shannon.div_meanrh <-ggplot(Shannon.div.summary, aes(x=avg_rh, y=avgpred))+
  # scale_fill_manual(values=pal)+
  
  geom_smooth(aes(x=avg_rh, y=avgpred), color="black", method="lm", formula = y ~ poly(x,2), se=FALSE)+
  #  geom_point(colour="black", pch=21, size=4)+
  geom_point(data=Shannon.div.summary,aes(x=avg_rh, y=avg_Shannon.div_per_day,
                                          fill=factor(SiteName)), color="black", pch=21, size=4) + 
  geom_errorbar(data=Shannon.div.summary,aes(ymin=avg_Shannon.div_per_day-se_mean_Shannon.div, 
                                             ymax=avg_Shannon.div_per_day+se_mean_Shannon.div), colour="black", width=.1) +
  theme_bw(base_size = 20)+
  guides(fill=guide_legend(title="Site"))+
  theme(legend.key=element_blank(), legend.text=element_text(size=10),
        axis.title=element_text(size=14), axis.text= element_text(size=12),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  xlab("Average daily relative humidity")+ #Make sure this is correct
  ylab("Mean Shannon-Weiner diversity")
Shannon.div_meanrh
ggsave(file="/Users/emilymeineke/Dropbox/BioSCAN Weather Station Data (1)/microclimate_data_processing/phorid_figures_ekm/alphadiv_meanrelativehumidity.pdf", height=10, width=22, unit='cm')



#Determine correlations between land cover and environmental data
plot(fly.climate$imp_mean_50, fly.climate$temp_air_mean)
plot(fly.climate$imp_mean_100, fly.climate$temp_air_mean)
plot(fly.climate$imp_mean_1000, fly.climate$temp_air_mean)
plot(fly.climate$imp_mean_2000, fly.climate$temp_air_mean)

plot(fly.climate$evi_mean_50, fly.climate$temp_air_mean)
plot(fly.climate$evi_mean_100, fly.climate$temp_air_mean)
plot(fly.climate$evi_mean_1000, fly.climate$temp_air_mean)
plot(fly.climate$evi_mean_2000, fly.climate$temp_air_mean)

#. . .  and land cover/ land cover data
plot(fly.climate$evi_mean_50, fly.climate$imp_mean_50)
plot(fly.climate$evi_mean_100, fly.climate$imp_mean_100)
plot(fly.climate$evi_mean_1000, fly.climate$imp_mean_1000)
plot(fly.climate$evi_mean_2000, fly.climate$imp_mean_2000)

#. . . and land cover/ fly data
plot(fly.climate$imp_mean_50, fly.climate$total.abundance)
plot(fly.climate$imp_mean_100, fly.climate$total.abundance)
plot(fly.climate$imp_mean_1000, fly.climate$total.abundance)
plot(fly.climate$imp_mean_2000, fly.climate$total.abundance)

plot(fly.climate$evi_mean_50, fly.climate$Richness)
plot(fly.climate$evi_mean_100, fly.climate$Richness)
plot(fly.climate$evi_mean_1000, fly.climate$Richness)
plot(fly.climate$evi_mean_2000, fly.climate$Richness)


###Full data summaries by site to work with
site_summaries <- ddply(fly.climate, .(site, Latitude, Longitude), summarize, 
                        mean_max_airtemp = mean(temp_air_max), 
                        mean_min_airtemp = mean(temp_air_min), 
                        mean_airtemp = mean(temp_air_mean), 
                        max_rh = mean(rh_max), 
                        min_rh = mean(rh_min), 
                        mean_rh = mean(rh_mean), 
                        max_soil1 = mean(tempsoil1_max), 
                        mean_soil1 = mean(tempsoil1_mean), 
                        min_soil1 = mean(tempsoil1_min), 
                        max_soil5 = mean(tempsoil5_max), 
                        min_soil5 = mean(tempsoil5_min), 
                        mean_soil5 = mean(tempsoil5_mean),
                        mean_evi50 = mean(evi_mean_50), 
                        mean_evi100 = mean(evi_mean_100), 
                        mean_imp50 = mean(imp_mean_50), 
                        mean_imp100 = mean(imp_mean_100),
                        mean_distna = mean(dis_CPAD_meter), 
                        mean_totabund = mean(total.abundance), 
                        mean_richness = mean(Richness), 
                        mean_div = mean(Shannon_div))
site_summaries

fly.climate.even <- subset(fly.climate, Evenness != "Inf")
even_summary <- ddply(fly.climate.even, .(site), summarize, mean_even = mean(Evenness))

merged <- merge(site_summaries, fly.climate.even, by="site")

coordinates(site_summaries) <- ~ Longitude + Latitude
plot(site_summaries)

ma_counties <- counties(state = 25, cb = TRUE, resolution = '20m')

states <- map_data("state")
dim(states)
california <- subset(states, region %in% c("california"))

#Plot California
ggplot(data = california) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)



#Get county-level data
counties <- map_data("county")
ca_county <- subset(counties, region == "california")
la_counties <- subset(ca_county, subregion == "los angeles")
                      
                      #|subregion == "los orange"|
                        #  subregion == "ventura"|subregion == "san bernardino"|
                        #  subregion == "riverside")

#Plot California
ggplot(data = la_counties) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)

library(ggmap)

#data.shape<-readOGR(".",layer="CityBoundary")

sbbox <- make_bbox(lon = la_counties$long, lat = la_counties$lat, f = .1)
sbbox

#stations <- spTransform(data.shape, CRS(proj4string(la_counties)))
#crs(data.shape) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#sq_map <- get_map(location = sbbox, maptype = "satellite", source = "google")
sq_map <- get_map('los angeles county', maptype = "satellite")


#Plot fly variables on a map.
#Alpha diversity map
La_map_withpoints <- ggmap(sq_map) + geom_point(site_summaries, mapping= aes(x=Longitude, y=Latitude, size= mean_div), colour="red") + 
  scale_x_continuous(limits = c(-118.4, -118.15), expand = c(0, 0)) +
  scale_y_continuous(limits = c(33.85, 34.2), expand = c(0, 0))
La_map_withpoints
#ggsave(file="/Users/emilymeineke/Dropbox/BioSCAN Weather Station Data (1)/microclimate_data_processing/phorid_figures_ekm/alphadiv_map.pdf", height=8, width=30, unit='cm')

#Evenness map
La_map_withpoints <- ggmap(sq_map) + geom_point(merged, mapping= aes(x=Longitude, y=Latitude, size= Evenness), colour="red") + 
  scale_x_continuous(limits = c(-118.4, -118.15), expand = c(0, 0)) +
  scale_y_continuous(limits = c(33.85, 34.2), expand = c(0, 0))
La_map_withpoints
#ggsave(file="/Users/emilymeineke/Dropbox/BioSCAN Weather Station Data (1)/microclimate_data_processing/phorid_figures_ekm/alphadiv_map.pdf", height=8, width=30, unit='cm')

#Richness map
La_map_withpoints <- ggmap(sq_map) + geom_point(merged, mapping= aes(x=Longitude, y=Latitude, size= Richness), colour="red") + 
  scale_x_continuous(limits = c(-118.4, -118.15), expand = c(0, 0)) +
  scale_y_continuous(limits = c(33.85, 34.2), expand = c(0, 0))
La_map_withpoints
#ggsave(file="/Users/emilymeineke/Dropbox/BioSCAN Weather Station Data (1)/microclimate_data_processing/phorid_figures_ekm/alphadiv_map.pdf", height=8, width=30, unit='cm')

#Total abundance
La_map_withpoints <- ggmap(sq_map) + geom_point(merged, mapping= aes(x=Longitude, y=Latitude, size= total.abundance), colour="red") + 
  scale_x_continuous(limits = c(-118.4, -118.15), expand = c(0, 0)) +
  scale_y_continuous(limits = c(33.85, 34.2), expand = c(0, 0))
La_map_withpoints
#ggsave(file="/Users/emilymeineke/Dropbox/BioSCAN Weather Station Data (1)/microclimate_data_processing/phorid_figures_ekm/alphadiv_map.pdf", height=8, width=30, unit='cm')



#Plot predictors on a map.
#Distance to natural area map
La_map_withpoints <- ggmap(sq_map) + geom_point(merged, mapping= aes(x=Longitude, y=Latitude, size= dis_CPAD_meter), colour="red") + 
  scale_x_continuous(limits = c(-118.4, -118.15), expand = c(0, 0)) +
  scale_y_continuous(limits = c(33.85, 34.2), expand = c(0, 0))
La_map_withpoints

#Mean temperature map
La_map_withpoints <- ggmap(sq_map) + geom_point(merged, mapping= aes(x=Longitude, y=Latitude, size= temp_air_mean), colour="red") + 
  scale_x_continuous(limits = c(-118.4, -118.15), expand = c(0, 0)) +
  scale_y_continuous(limits = c(33.85, 34.2), expand = c(0, 0))
La_map_withpoints

#Max temperature map
La_map_withpoints <- ggmap(sq_map) + geom_point(merged, mapping= aes(x=Longitude, y=Latitude, size= temp_air_max), colour="red") + 
  scale_x_continuous(limits = c(-118.4, -118.15), expand = c(0, 0)) +
  scale_y_continuous(limits = c(33.85, 34.2), expand = c(0, 0))
La_map_withpoints

#Min temperature map
La_map_withpoints <- ggmap(sq_map) + geom_point(merged, mapping= aes(x=Longitude, y=Latitude, size= temp_air_min), colour="red") + 
  scale_x_continuous(limits = c(-118.4, -118.15), expand = c(0, 0)) +
  scale_y_continuous(limits = c(33.85, 34.2), expand = c(0, 0))
La_map_withpoints

#NDVI map
La_map_withpoints <- ggmap(sq_map) + geom_point(merged, mapping= aes(x=Longitude, y=Latitude, size= evi_mean_50), colour="red") + 
  scale_x_continuous(limits = c(-118.4, -118.15), expand = c(0, 0)) +
  scale_y_continuous(limits = c(33.85, 34.2), expand = c(0, 0))
La_map_withpoints

#Impervious map
La_map_withpoints <- ggmap(sq_map) + geom_point(merged, mapping= aes(x=Longitude, y=Latitude, size= imp_mean_50), colour="red") + 
  scale_x_continuous(limits = c(-118.4, -118.15), expand = c(0, 0)) +
  scale_y_continuous(limits = c(33.85, 34.2), expand = c(0, 0))
La_map_withpoints








#Beta diversity (NMDS)

#starting out using individual observations as samples so we can include weather. Will repeat 
#with site level observations integrated over time

#observation level NMDS

fly.climate.1<-fly.climate[complete.cases(fly.climate),]

species.matrix<-fly.climate.1[,9:108]
env.matrix<-fly.climate.1[,-(9:108)]

#because NMDS isn't very good at handling very rare taxa or samples that produced few captures, 
#we need to set some rules for removing these observations/taxa

#first let's cut super-rare taxa
species.matrix<-species.matrix[, colSums(species.matrix)>5]

#now let's cut the observations with few specimens captured
species.matrix.1<-species.matrix[rowSums(species.matrix)>1,]
env.matrix.1<-env.matrix[rowSums(species.matrix)>1,]

library(vegan)

ord<-metaMDS(species.matrix.1, autotransform = F)
ord
plot(ord)

#it becomes pretty bare bones when you plot it all and see how autocorrelated it gets!
fit.env<-envfit(ord~week+temp_air_mean+rh_min+imp_mean_50+
                  dis_CPAD_meter, 
                env.matrix.1, perm=999, na.rm=T)
fit.env

#

#need to emulate the ggplot colour palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#we have 30 sites
ggpal<-gg_color_hue(30)

most_abund<-colSums(species.matrix.1)>500

plot(ord, disp='sites', type='n')
#so many obs/site so it's just a cloud of points
with(env.matrix.1, points(ord, display = "sites", col = "black", pch = 21, bg = ggpal[site], cex=0.75))
plot(fit.env, col="blue", cex=0.75)
orditorp(ord.1, display="species", select=which(most_abund==T))


#now let's repeat with the whole community found at a site- see if this works

#okay, tally the number of each fly at each site

flycounts<-species.matrix
flycounts$site<-env.matrix$site

flycounts.melted<-melt(flycounts, variable.name="species", value.names="count", 
                       id.vars="site")
flycounts.bysite<-dcast(flycounts.melted, site~species, sum)
flycounts.bysite$site<-NULL

site_summaries <- ddply(fly.climate, .(site, Latitude, Longitude), summarize, 
                        mean_max_airtemp = mean(temp_air_max), 
                        mean_min_airtemp = mean(temp_air_min), 
                        mean_airtemp = mean(temp_air_mean), 
                        max_rh = mean(rh_max), 
                        min_rh = mean(rh_min), 
                        mean_rh = mean(rh_mean), 
                        max_soil1 = mean(tempsoil1_max), 
                        mean_soil1 = mean(tempsoil1_mean), 
                        min_soil1 = mean(tempsoil1_min), 
                        max_soil5 = mean(tempsoil5_max), 
                        min_soil5 = mean(tempsoil5_min), 
                        mean_soil5 = mean(tempsoil5_mean),
                        mean_evi50 = mean(evi_mean_50), 
                        mean_evi100 = mean(evi_mean_100), 
                        mean_imp50 = mean(imp_mean_50), 
                        mean_imp100 = mean(imp_mean_100),
                        mean_distna = mean(dis_CPAD_meter), 
                        mean_totabund = mean(total.abundance), 
                        mean_richness = mean(Richness), 
                        mean_div = mean(Shannon_div))
site_summaries.1<-site_summaries[complete.cases(site_summaries),]

#ordination by site totals

ord.1<-metaMDS(flycounts.bysite, autotransform = F)
ord.1
plot(ord.1)

#it becomes pretty bare bones when you plot it all and see how autocorrelated it gets!
fit.env.1<-envfit(ord.1~mean_min_airtemp+
                    mean_rh+mean_evi50+mean_distna, site_summaries.1, perm=999, na.rm=T)
fit.env.1




plot(ord.1, disp='sites', type='n')
with(site_summaries.1, points(ord.1, display = "sites", col = "black", pch = 21, bg = ggpal[site], cex=0.75))
plot(fit.env.1, col="blue", cex=0.75)
orditorp(ord.1, display="species", select=which(most_abund==T))
