#read in the data
fly<-read.csv(file="../flydata/FlyDataFixedByTerry.csv", header=T)


#there's a few problems with formatting coming in

#let's start by getting rid of the index number column, which is not meanignful in our case
fly[1]<-NULL

#also na.omit it- there's one observation without site info
fly<-na.omit(fly)

#rename site name and numbers columns
 
names(fly)[1:2]<-c("SiteName","site")

#get rid of total column, which will be a problem for diversity stuff- can compute it later when needed
fly$Total<-NULL

#now, for fiddly stuff. Get rid of double periods that R made from the spaces after periods when we 
#imported the data

colnames(fly) <- gsub("\\.\\.+", ".", colnames(fly))

#next, we want the site number to be in the same format as the weather data

fixsite<-function(thing){ # a function to add the word site to the numeric site data
  if (thing<10){
    thing1<-paste("Site0", thing, sep="")
  }else{
    thing1<-paste("Site", thing, sep="")
  }
  return (thing1)
}

fly$site<-mapply(fixsite, fly$site)

#now we need to get the dates into the same format that we have them in the weather database.
library(lubridate)

#because the date in this dataset is when the trap was deployed, and we want to see what's
#going on in the trap after the exposure, we need to compute the end date

fly$SampleDateEnd<-mdy(fly$SampleDateStart)+fly$SamplingDurationDays

#pull year, week
fly$year<-year(fly$SampleDateEnd)
fly$week<-week(fly$SampleDateEnd)


#bring in the weather data we processed earlier
weather<-read.csv(file="weather_data_compiled_weekly_res.csv", header=T)
weather$X<-NULL

#and smoosh it all together! We only need to pull the data relevant to the insect samples

fly.weather<-merge(fly, weather, by=c("site", "year", "week"), all.x = TRUE)

#yess!! all right, let's export this bad boy to CSV

write.csv(fly.weather, "fly_data_with_weather.csv")