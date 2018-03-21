
#give columns in data frame R friendly names

namelist<-c('obs', 'date_time', 'sol_rad', 'voltage', 'current',
  'wetness', 'temp_air', 'rh', 'dewpt', 'tempsoil1', 'tempsoil5',
  'water_cont', 'battery')



#get a list of file names:

setwd("../alldata/")
file_list<-list.files()


library(tidyverse)
#create an empty data frame to put stuff in
dataset<-data.frame(matrix(vector(), 0, 14,
                             dimnames=list(c(), c(namelist, 'site'))),
                      stringsAsFactors=F)

#loop through the files, merge 'em together
for (i in 1:length(file_list)){
  #read data in. There's tab spearatef files marked with + in there, so let's see if we can 
  #use this to tell the tabs from the csvs
  if (grepl("tab", file_list[i])){
    data<-read.delim(file=file_list[i], header=T, sep="\t")
    } else {
    data<-read.csv(file=file_list[i], header=T) #read data in as a csv
    }
  
  #rename columns- going to be more complicated than I'd hoped
  # data files include different numbers of variables, with varying variable names
  #so we need to loop through columns in the frame
  #and rename them based on partial string matches
  varnames <-colnames(data)
  
  for(j in 1:length(varnames)){
    if (grepl("Date", varnames[j])){
      varnames[j]<-"date_time"
    }
    if (grepl("Solar", varnames[j])){
      varnames[j]<-"sol_rad"
    }
    if (grepl("Volt", varnames[j])){
      varnames[j]<-"voltage"
    }
    if (grepl("Current", varnames[j])){
      varnames[j]<-"current"
    }
    if (grepl("Wetness", varnames[j])){
      varnames[j]<-"wetness"
    }
    if (grepl("RH", varnames[j])){
      varnames[j]<-"rh"
    }
    if (grepl("Dew", varnames[j])){
      varnames[j]<-"dewpt"
    }
    if (grepl("Water", varnames[j])){
      varnames[j]<-"water_cont"
    }
    if (grepl("Batt", varnames[j])){
      varnames[j]<-"battery"
    }
    if (grepl("Temp", varnames[j])){
      teststring<-gsub("[[:space:]]", "", varnames[j], fixed = TRUE)#remove whitespace which is tripping grep up
      teststring
      if (grepl("F", varnames[j])){ #thanks, America
        data[j]<-(data[j]-32)*5/9
      }
      if (grepl("1.cm", as.character(varnames[j]))) {
        varnames[j]<-"tempsoil1"
      } else if (grepl("5.cm", as.character(varnames[j]))) {
        varnames[j]<-"tempsoil5"
      } else {
        varnames[j]<-"temp_air"
      }
  
    }
    
  }
  #now use varnames to rename columns
  names(data)<-varnames
  #because sometimes there is a comma thousands separator in sol_rad
  data$sol_rad<-as.numeric(gsub(",", "", data$sol_rad))
  
  site<-substr(file_list[i], 1, 6) #pull site name out of file  
  data$site<-rep(site, length(data[1])) #assign it to all rows in the data frame
  dataset<-bind_rows(dataset, data)
  
}

setwd("../microclimate_data_processing")

#now let's take a look
summary(dataset)

#right, okay, HOBO data loggers use , erm, unconventional nulls (encoded as -888.88)
#but it looks like there are some rounding issues if the summary is to be believed. 
#let's re-encode them in an r-friendly way

dataset[dataset< -888]<-NA
summary(dataset)

#let's also get rid of impossible and improbable values that are likely a result of equipment malfunction
#this is LA so let's say air temp is never getting below -5C, and the ground never freezes

dataset$temp_air[dataset$temp_air< -10]<-NA
dataset$tempsoil1[dataset$tempsoil1< -0.5]<-NA
dataset$tempsoil5[dataset$tempsoil5< -0.5]<-NA
dataset$dewpt[dataset$dewpt< 0]<-NA

#even though we're probably not going to use it, voltage must always be negative, water content positive

dataset$voltage[dataset$voltage> 0]<-NA
dataset$water_cont[dataset$water_cont< 0]<-NA

summary(dataset)

#okay, let's see what we can do with the date variable to make it usable
#looks like there's a few different date formats so we'll have to fix that
#up before we can do anything.

library(lubridate)

#first pull out all the data with good dates
dataset.isodate<-dataset[which(grepl("^201", dataset$date_time)),]

#now pull out all the ones with bad dates
dataset.bad.date<-dataset[which(!grepl("^201", dataset$date_time)),]

#otherwise it's probably in American-style
dateformat<-as.character(parse_date_time(dataset.bad.date$date_time, orders="mdy hms"))
#looks like that did the job
dataset.bad.date$date_time<-dateformat
 
#rebind the data
dataset<-bind_rows(dataset.isodate, dataset.bad.date)


dataset$year<-year(dataset$date_time)
dataset$doy<-yday(dataset$date_time)
dataset$hour<-hour(dataset$date_time)

summary(dataset)
#OMG it's working. Okay, now we can use these data!

#let's start by working with air temperature!!

#we need to reduce the resolution of the data because of its size, so we can
#look for meaningful gaps
library(plyr)

hourly.temp<-ddply(dataset, c("site", "year", "doy", "hour"), summarise,
                  temp_air=mean(temp_air), rh=mean(rh),
                  tempsoil1=mean(tempsoil1), tempsoil5=mean(tempsoil5))

library(reshape2)
#cast data as crosstab by air temperature
hourly.temp.wide<-dcast(hourly.temp[1:5], year+doy+hour~site, mean)
#also cast length by crosstab- then we can use this for culling out the tails of the data
hourly.temp.wide.counts<-dcast(hourly.temp[1:5], year+doy+hour~site, length)
hourly.temp.wide.counts$n_reporting<-rowSums(hourly.temp.wide.counts[,4:34])

#now use n_reporting to cull out times when the network wasn't fully 'up'
#let's define it as more than 18 stations reporting
hourly.temp.wide.up<-hourly.temp.wide[which(hourly.temp.wide.counts$n_reporting>18),]

summary(hourly.temp.wide.up)
#so site 2 is a mess. woo boy. It's not reporting at any time that the rest
#of the network is up. I think we just need to cut it out
hourly.temp.wide.up$Site02<-NULL

#now create the matrix for the PCA- we want to identify sites that are most
#similar to each other so they can be used to estimate missing data

#we want to reshape the data again to make it long, and then sub in 
#the other environmental values so we can figure out which sites are most
#similar by PCA


hourly.temp.up<-melt(hourly.temp.wide.up, id.vars=c("year", "doy", "hour"))
# fix up those column names
names(hourly.temp.up)<-c("year", "doy", "hour", "site", "temp_air")
#merge back in the other observations based on when network is up
#this will be the operational dataset, with holes
network.up.with.holes<-merge(hourly.temp.up, hourly.temp, all.x = T)

#so for the PCA, we need data without NAs. 
network.no.NA<-na.omit(network.up.with.holes)

#all right! Let's PCA this up!
network.matrix<-network.no.NA[5:8]

library(vegan)
site.pca <- rda(network.matrix)
site.pca
biplot(site.pca)

#now, the majority of the variation is in PC1, with a bit in PC2, so we're
#good to go ahead just using those two axes to determine site similarity

#site.fit<-envfit(site.pca, site, data=network.no.NA)

#does not work- we're at the limitations of what my tablet can do. BUT! 
#perhaps we can do the same with data of lower resolution- if we take it down
#to daily max min temps and average RH we can get at the same thing
#but with less computational muscle needed.

daily.temp<-ddply(network.up.with.holes, c("site", "year", "doy"), summarise,
                  temp_air_max=max(temp_air), temp_air_min=min(temp_air),
                  rh=mean(rh),
                  tempsoil1_max=max(tempsoil1), tempsoil1_min=min(tempsoil1),
                  tempsoil5_max=max(tempsoil5), tempsoil5_min=min(tempsoil5))
daily.temp<-na.omit(daily.temp)

daily.matrix<-daily.temp[4:10]
site.pca <- rda(daily.matrix)
site.pca
biplot(site.pca)

site.fit<-envfit(site.pca~site, data=daily.temp)
site.fit

#whoohoo! we have centroids! now we just need to find the pairwise distance
#between each site combination
site.scores<-as.data.frame(site.fit$factors$centroids)

#compute best pairs
site.scores$site<-sub('site', '', rownames(site.scores))


for(i in 1:length(site.scores$PC1)){
  thissite<-site.scores$site[i]
  x<-site.scores$PC1[i]
  y<-site.scores$PC2[i]
  xdif<-x-site.scores$PC1
  ydif<-y-site.scores$PC2
  newvec<-sqrt(xdif^2+ydif^2)
  site.scores<-cbind(site.scores, newvec)
  names(site.scores)[names(site.scores) == "newvec"] <- thissite
}

# so site 24 has a soil sensor problem so we're just going to have
#to match it using air temp and rh
#let's repeat the PCA on data without soil temps

daily.temp.24<-ddply(network.up.with.holes, c("site", "year", "doy"), summarise,
                  temp_air_max=max(temp_air), temp_air_min=min(temp_air),
                  rh=mean(rh))
daily.temp.24<-na.omit(daily.temp.24)

daily.matrix.24<-daily.temp.24[4:6]
site.pca.24 <- rda(daily.matrix.24)
site.pca.24

site.fit.24<-envfit(site.pca.24~site, data=daily.temp.24)
site.fit.24
site.scores.24<-as.data.frame(site.fit.24$factors$centroids)

#compute best pairs
site.scores.24$site<-sub('site', '', rownames(site.scores.24))


for(i in 1:length(site.scores.24$PC1)){
  thissite<-site.scores.24$site[i]
  x<-site.scores.24$PC1[i]
  y<-site.scores.24$PC2[i]
  xdif<-x-site.scores.24$PC1
  ydif<-y-site.scores.24$PC2
  newvec<-sqrt(xdif^2+ydif^2)
  site.scores.24<-cbind(site.scores.24, newvec)
  names(site.scores.24)[names(site.scores.24) == "newvec"] <- thissite
}

#okay, now make a data frame out of best predictors

sitelist<-c("Site01","Site03","Site04","Site05","Site06","Site07","Site08",
            "Site09","Site10","Site11","Site12","Site13","Site14","Site15",
            "Site16","Site17","Site18","Site19","Site20","Site21","Site22",
            "Site23","Site25","Site26","Site27","Site28","Site29","Site30",
            "Site31")
predlist<-c("Site18","Site25","Site09","Site16","Site12","Site28","Site10",
            "Site23","Site08","Site27","Site06","Site20","Site28","Site26",
            "Site05","Site31","Site01","Site01","Site22","Site25","Site20",
            "Site11","Site03","Site15","Site11","Site07","Site30","Site29",
            "Site17")

#we'll treat site 24 separately, but it's matched with site 7 and we can only
# do air temp and RH there

#let's give the data we're working with a shorter name
raw<-network.up.with.holes

#deal with the elusive site 24 first, that way we have a one-off that we can
#attach our data to down the line

site.var<-raw[which(raw$site == "Site24"),]
pred.var<-raw[which(raw$site == "Site07"),]
all.together<-merge(site.var, pred.var, by=c("year", "doy", "hour"), all.x=T)
# air temp corrections
air.model<-lm(temp_air.x~temp_air.y, data=all.together)
int<-air.model$coefficients[1]
slope<-air.model$coefficients[2]
all.together$temp_air_corrected<-ifelse(!is.na(all.together$temp_air.x),
                                        all.together$temp_air.x,
                                        int+slope*all.together$temp_air.y)
# RH corrections
rh.model<-lm(rh.x~rh.y, data=all.together)
int<-rh.model$coefficients[1]
slope<-rh.model$coefficients[2]
all.together$rh_corrected<-ifelse(!is.na(all.together$rh.x),
                                  all.together$rh.x,
                                  int+slope*all.together$rh.y)
#create dummy columns for soil temp
all.together$tempsoil1_corrected<-all.together$tempsoil1.x
all.together$tempsoil5_corrected<-all.together$tempsoil5.x

#cull out the crud we don't need anymore
data.cleaned<-all.together[-c(5:13)]
names(data.cleaned)<-names(site.var)
summary(data.cleaned)
#that did the job!

#create a frame for data that one pass doesn't work on
needs.another.pass<-data.cleaned[0,]

#now we need to do that for the rest of the data
#create a loop (oh noes loops!) to deal with it

for (i in 1:length(sitelist)){#first pull out site, and best predictor site data
  site.var<-raw[which(raw$site == sitelist[i]),]
  pred.var<-raw[which(raw$site == predlist[i]),]
  all.together<-merge(site.var, pred.var, by=c("year", "doy", "hour"), all.x=T)
  # air temp corrections
  air.model<-lm(temp_air.x~temp_air.y, data=all.together)
  int<-air.model$coefficients[1]
  slope<-air.model$coefficients[2]
  all.together$temp_air_corrected<-ifelse(!is.na(all.together$temp_air.x),
                                          all.together$temp_air.x,
                                          int+slope*all.together$temp_air.y)
  # RH corrections
  rh.model<-lm(rh.x~rh.y, data=all.together)
  int<-rh.model$coefficients[1]
  slope<-rh.model$coefficients[2]
  all.together$rh_corrected<-ifelse(!is.na(all.together$rh.x),
                                          all.together$rh.x,
                                          int+slope*all.together$rh.y)
  # tempsoil1 corrections
  ts1.model<-lm(tempsoil1.x~tempsoil1.y, data=all.together)
  int<-ts1.model$coefficients[1]
  slope<-ts1.model$coefficients[2]
  all.together$tempsoil1_corrected<-ifelse(!is.na(all.together$tempsoil1.x),
                                    all.together$tempsoil1.x,
                                    int+slope*all.together$tempsoil1.y)
  # tempsoil5 corrections
  ts5.model<-lm(tempsoil5.x~tempsoil5.y, data=all.together)
  int<-ts5.model$coefficients[1]
  slope<-ts5.model$coefficients[2]
  all.together$tempsoil5_corrected<-ifelse(!is.na(all.together$tempsoil5.x),
                                           all.together$tempsoil5.x,
                                           int+slope*all.together$tempsoil5.y)
  
  #cull out the crud we don't need anymore
  cleaned.up<-all.together[-c(5:13)]
  names(cleaned.up)<-names(site.var)
  #create a conditional to filter the data- if it's fixed, add it to the main
  #data frame. If it's not, put it in another frame that we repeat this exercise
  #with, with the second best sites
  if(any(is.na(cleaned.up))){
    needs.another.pass<-bind_rows(needs.another.pass, cleaned.up)
  }else{
    data.cleaned<-bind_rows(data.cleaned, cleaned.up)
  }
}
summary(data.cleaned)
summary(needs.another.pass)
summary(as.factor(data.cleaned$site))
summary(as.factor(needs.another.pass$site))
#ok, looks like we got about half of them. not too bad. Need to prepare a
#list of second best sites and then do the same
#okay, now make a data frame out of second best predictors

sitelist2<-c("Site03","Site05","Site07","Site16","Site17","Site21",
             "Site23","Site25","Site28","Site29","Site30","Site31")
predlist2<-c("Site23","Site12","Site31","Site12","Site29", "Site03",
             "Site09","Site21","Site31","Site17","Site17", "Site29")

#create a frame for data that second pass doesn't work on
needs.another.pass.2<-data.cleaned[0,]

for (i in 1:length(sitelist2)){#first pull out site, and best predictor site data
  site.var<-needs.another.pass[which(needs.another.pass$site == sitelist2[i]),]
  pred.var<-raw[which(raw$site == predlist2[i]),]
  all.together<-merge(site.var, pred.var, by=c("year", "doy", "hour"), all.x=T)
  # air temp corrections
  air.model<-lm(temp_air.x~temp_air.y, data=all.together)
  int<-air.model$coefficients[1]
  slope<-air.model$coefficients[2]
  all.together$temp_air_corrected<-ifelse(!is.na(all.together$temp_air.x),
                                          all.together$temp_air.x,
                                          int+slope*all.together$temp_air.y)
  # RH corrections
  rh.model<-lm(rh.x~rh.y, data=all.together)
  int<-rh.model$coefficients[1]
  slope<-rh.model$coefficients[2]
  all.together$rh_corrected<-ifelse(!is.na(all.together$rh.x),
                                    all.together$rh.x,
                                    int+slope*all.together$rh.y)
  # tempsoil1 corrections
  ts1.model<-lm(tempsoil1.x~tempsoil1.y, data=all.together)
  int<-ts1.model$coefficients[1]
  slope<-ts1.model$coefficients[2]
  all.together$tempsoil1_corrected<-ifelse(!is.na(all.together$tempsoil1.x),
                                           all.together$tempsoil1.x,
                                           int+slope*all.together$tempsoil1.y)
  # tempsoil5 corrections
  ts5.model<-lm(tempsoil5.x~tempsoil5.y, data=all.together)
  int<-ts5.model$coefficients[1]
  slope<-ts5.model$coefficients[2]
  all.together$tempsoil5_corrected<-ifelse(!is.na(all.together$tempsoil5.x),
                                           all.together$tempsoil5.x,
                                           int+slope*all.together$tempsoil5.y)
  
  #cull out the crud we don't need anymore
  cleaned.up<-all.together[-c(5:13)]
  names(cleaned.up)<-names(site.var)
  #create a conditional to filter the data- if it's fixed, add it to the main
  #data frame. If it's not, put it in another frame that we repeat this exercise
  #with, with the second best sites
  if(any(is.na(cleaned.up))){
    needs.another.pass.2<-bind_rows(needs.another.pass.2, cleaned.up)
  }else{
    data.cleaned<-bind_rows(data.cleaned, cleaned.up)
  }
}

summary(data.cleaned)
summary(needs.another.pass.2)
summary(as.factor(data.cleaned$site))
summary(as.factor(needs.another.pass.2$site))
#so close, only 6 sites still with holes, and a small number at that
# let's do a 3rd pass

#okay, now make a data frame out of third best predictors

sitelist3<-c("Site07","Site21","Site25","Site28")
predlist3<-c("Site17","Site06","Site06","Site17")

#create a frame for data that second pass doesn't work on
needs.another.pass.3<-data.cleaned[0,]

for (i in 1:length(sitelist3)){#first pull out site, and best predictor site data
  site.var<-needs.another.pass.2[which(needs.another.pass.2$site == sitelist3[i]),]
  pred.var<-raw[which(raw$site == predlist3[i]),]
  all.together<-merge(site.var, pred.var, by=c("year", "doy", "hour"), all.x=T)
  # air temp corrections
  air.model<-lm(temp_air.x~temp_air.y, data=all.together)
  int<-air.model$coefficients[1]
  slope<-air.model$coefficients[2]
  all.together$temp_air_corrected<-ifelse(!is.na(all.together$temp_air.x),
                                          all.together$temp_air.x,
                                          int+slope*all.together$temp_air.y)
  # RH corrections
  rh.model<-lm(rh.x~rh.y, data=all.together)
  int<-rh.model$coefficients[1]
  slope<-rh.model$coefficients[2]
  all.together$rh_corrected<-ifelse(!is.na(all.together$rh.x),
                                    all.together$rh.x,
                                    int+slope*all.together$rh.y)
  # tempsoil1 corrections
  ts1.model<-lm(tempsoil1.x~tempsoil1.y, data=all.together)
  int<-ts1.model$coefficients[1]
  slope<-ts1.model$coefficients[2]
  all.together$tempsoil1_corrected<-ifelse(!is.na(all.together$tempsoil1.x),
                                           all.together$tempsoil1.x,
                                           int+slope*all.together$tempsoil1.y)
  # tempsoil5 corrections
  ts5.model<-lm(tempsoil5.x~tempsoil5.y, data=all.together)
  int<-ts5.model$coefficients[1]
  slope<-ts5.model$coefficients[2]
  all.together$tempsoil5_corrected<-ifelse(!is.na(all.together$tempsoil5.x),
                                           all.together$tempsoil5.x,
                                           int+slope*all.together$tempsoil5.y)
  
  #cull out the crud we don't need anymore
  cleaned.up<-all.together[-c(5:13)]
  names(cleaned.up)<-names(site.var)
  #create a conditional to filter the data- if it's fixed, add it to the main
  #data frame. If it's not, put it in another frame that we repeat this exercise
  #with, with the second best sites
  if(any(is.na(cleaned.up))){
    needs.another.pass.3<-bind_rows(needs.another.pass.3, cleaned.up)
  }else{
    data.cleaned<-bind_rows(data.cleaned, cleaned.up)
  }
}
summary(data.cleaned)
summary(needs.another.pass.3)
summary(as.factor(data.cleaned$site))
summary(as.factor(needs.another.pass.3$site))

#victory! WE HAVE A complete dataset!


#so right now, we have data at an hourly resolution, and we want weekly
#measures. We'll want degree day accumulation, max, min, and means for 
#all the temperature measures, and mean, max and min for rh
#first let's compute dd accumulation! This is easy because we're going 
#computing this on hourly averages already, so we can treat it like a 
#rectangle rather than having to model any curves. SCORE!

#first choose a threshold. I'm going to go with 10C because no one
#suggested different (but I'll make it a variable so it's easy to change)

thresh<-10
#compute hourly degree day computation. 
data.cleaned$dd_air<-ifelse(data.cleaned$temp_air<=thresh, 0,
                            (data.cleaned$temp_air-thresh)/24)
data.cleaned$dd_soil1<-ifelse(data.cleaned$tempsoil1<=thresh, 0,
                            (data.cleaned$tempsoil1-thresh)/24)
data.cleaned$dd_soil5<-ifelse(data.cleaned$tempsoil5<=thresh, 0,
                            (data.cleaned$tempsoil5-thresh)/24)

#we need a "week" measure now that we can compute summaries based on
#needs a bit of finesssing to get the data into a format where we 
#can work that

#Do an arithmetic operation on January 1 of the given year- add DOY
#and subtract 1 for the first day of the year
#normally I'd use ISOWEEK to count weeks from monday but that may introduce
#some difficulties with discontinuities at the year changeover-
#will have to see the insect data to ensure the data lines up in a reasonable
#way!

data.cleaned$week<-as.numeric(week(ymd(paste(data.cleaned$year, "01 01", sep=" "))+
                             days(data.cleaned$doy)-1))


weekly.weather<-ddply(data.cleaned, c("site", "year", "week"), summarise,
                  temp_air_max=max(temp_air), temp_air_min=min(temp_air),
                  temp_air_mean=mean(temp_air),
                  rh_max=max(rh), rh_min=min(rh), rh_mean=mean(rh),
                  tempsoil1_max=max(tempsoil1), tempsoil1_min=min(tempsoil1),
                  tempsoil1_mean=mean(tempsoil1),
                  tempsoil5_max=max(tempsoil5), tempsoil5_min=min(tempsoil5),
                  tempsoil5_mean=mean(tempsoil5),
                  dd_air=sum(dd_air), dd_soil1=sum(dd_soil1), 
                  dd_soil5=sum(dd_soil5))

#for degree day accumulation, this will be a bit trickier, because we need to 
#think about what represents a 'season' in this climate- ie, when do we start
#accumulations. When we plot dd accumulations by week of year, the lowest does,
#conveniently, occur at the discontinuity at year switchover, but we might 
#have to adjust this assumption down the line
#(but for the meantime, this works, so let's do it ;) )

weekly.weather<-weekly.weather[order(weekly.weather$site, 
                                      weekly.weather$year,
                                      weekly.weather$week),]

# let's steal and modify the accumulation function from Hermann et al 2016
accum<-function(dd, weekvec, sitevec, start){
  #if startday is not given, assume it's day 1
  if(missing(start)) {
    start<-1
  } else {
    start<-start
  }
  dd.accum<-c()
  for (i in 1:length(dd)){
    #hmm, need a way to sum up over the year, starting anew for each year.
    #this should do it
    if (weekvec[i]==1){
      dd.accum.week=0
    }
    #we also want to restart accumulation if this is a point in the data
    #where there is a transition between sites!
    if (i>1){
      if (sitevec[i-1]!=sitevec[i]){
        dd.accum.week=0
      }
    }
    #finally, we want to start counting at the start of the data
    if (i==1){
      dd.accum.week=0
    }
    #the accumulation on day i is the degree day accumulation before
    #plus the dd accumulated on that day
    dd.accum.week<-dd.accum.week+dd[i]
    #but if the degdays are accumulating before the startday, we want to forget them
    if (weekvec[i]<start){
      dd.accum.week=0
    }
    #add that day's accumulation to the vector
    dd.accum<-c(dd.accum, dd.accum.week)
  }
  return (dd.accum)
}

#compute those DD accumulations!

weekly.weather$dd_air_accum<-accum(weekly.weather$dd_air, 
                                   weekly.weather$week,
                                   weekly.weather$site, start=1)
weekly.weather$dd_soil1_accum<-accum(weekly.weather$dd_soil1, 
                                   weekly.weather$week,
                                   weekly.weather$site, start=1)
weekly.weather$dd_soil5_accum<-accum(weekly.weather$dd_soil5, 
                                     weekly.weather$week,
                                     weekly.weather$site, start=1)

#compute photoperiod 

#bring in a data table with site location info
coords<-read.csv(file="../flydata/site_info.csv", header=T)

#next, we want the site number to be in the same format as the weather data

fixsite<-function(thing){ # a function to add the word site to the numeric site data
  if (thing<10){
    thing1<-paste("Site0", thing, sep="")
  }else{
    thing1<-paste("Site", thing, sep="")
  }
  return (thing1)
}

coords$site<-mapply(fixsite, coords$site)

#create a function that computes the photoperiod for a given site on a given sample day
phtp<-function(year, week, siteweather){
  #compute the date at the end of the week
  library(lubridate)
  date<-as.Date(paste(year, week, 1, sep="-"), "%Y-%U-%u")+6 #add six days to end of the week
  if (week>52){#if the data is overlapping the year transition, just compute it for the first week of next year
    week<-1
    year<-year+1
    date<-as.Date(paste(year, week, 1, sep="-"), "%Y-%U-%u")#and then don't add the six days
  }
  doy<-yday(date)
  #look up the latitude at the site
  lat<-coords$Latitude[which(coords$site==siteweather)]
  library(geosphere)
  photoperiod<-daylength(lat, doy)
  return (photoperiod)
}

weekly.weather$photoperiod<-mapply(phtp, weekly.weather$year, weekly.weather$week, 
                                   weekly.weather$site)

#finally, add the lat and long into the weather file

weekly.weather<-merge(weekly.weather, coords, by="site", all.x = T)

#yess!! all right, let's export this bad boy to CSV

write.csv(weekly.weather, "weather_data_compiled_weekly_res.csv")
