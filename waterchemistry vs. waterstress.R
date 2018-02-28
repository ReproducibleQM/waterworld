#octothorpe
#adding in csv's of useful data
waterchem<-read.csv(file="wsamarch2_2009/waterchemistry.csv")
summary(waterchem)
str(waterchem)
riparian<-read.csv(file="wsamarch2_2009/riparian.csv")
summary(riparian)
watershedstress<-read.csv(file="wsamarch2_2009/watershedstressor.csv")
plot("PAGT~xwatershedstress")
?merge
stressandchem<- merge(watershedstress,waterchem)
plot(stressandchem$PAGT,stressandchem$NO3)
plot(stressandchem$PAGT,stressandchem$NH4)
#Akaike information criterion plots- use this inductive method to tease apart 
#variables and determine which explains the variation the best
#plotting x,y from watershed stress and chem merged dataset
#road miles vs. conductivity
plot(stressandchem$RDDENS,stressandchem$COND)

?plot
plot(stressandchem$PAGT,stressandchem$NH4)
plot(stressandchem$PAGT,stressandchem$NO3)
plot(stressandchem$PAGT,stressandchem$NO3)

