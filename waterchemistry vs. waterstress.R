#octothorpe
#adding in csv's of useful data
waterchem<-read.csv(file="wsamarch2_2009/waterchemistry.csv")
watershedstress<-read.csv(file="wsamarch2_2009/watershedstressor.csv")
fishcover<-read.csv(file="wsamarch2_2009/fishcover.csv")
streamvelocity<-read.csv(file="wsamarch2_2009/streamvelocity.csv")
wsa.bencnt.genus<-read.csv(file="wsamarch2_2009/wsa_benmet300_ts_final.csv")
stressandchem<- merge(watershedstress,waterchem)
#Troubled merges...
vel.stress.chem<-merge(stressandchem,streamvelocity)
#get rid of the date col variable, which is variable bwtween the two data frames
stressandchem$DATE_COL<-NULL
wsa.bencnt.genus$DATE_COL<-NULL
#visit number is not relevant for certain parameters, so it was only measured at visit 1
stressandchem$VISIT_NO<-NULL

benthicstressandchem<-merge(stressandchem,wsa.bencnt.genus, by=c("SITE_ID", "YEAR"))
streamvelocity$VISIT_NO<-NULL
vbsc<-merge(benthicstressandchem,streamvelocity, by=c("SITE_ID", "YEAR"), all.x=T)

#write to csv
write.csv(vbsc,"VBSC.csv")
#
#pull in simpleNARS file
simpleNARS<-read.csv(file="simpleNARS.csv")

simpleNARS$pH<-(-1)*log10((simpleNARS$pH)/1000000)
summary(simpleNARS)


simpleNARS$NH4norm<-simpleNARS$NH4/simpleNARS$LANDAREA
simpleNARS$pHnorm<-simpleNARS$pH/simpleNARS$LANDAREA
simpleNARS$CONDnorm<-simpleNARS$COND/simpleNARS$LANDAREA
simpleNARS$FLOW_m3s<-simpleNARS$FLOW_CFS/(3.28^3)
simpleNARS$FLOWnorm<-simpleNARS$FLOW_m3s/simpleNARS$LANDAREA
simpleNARS$LANDOTHER<-100-(simpleNARS$PWETL+simpleNARS$PURB+simpleNARS$PAGT+simpleNARS$PFOR)
#Akaike information criterion plots- use this inductive method to tease apart 
#variables and determine which explains the variation the best
#plotting x,y from watershed stress and chem merged dataset
#road miles vs. conductivity
plot(stressandchem$RDDENS,stressandchem$COND)

plot(stressandchem$PAGT,stressandchem$NH4)
plot(stressandchem$PAGT,stressandchem$NO3)
plot(stressandchem$PAGT,stressandchem$NO3)

#Testing Assumptions
PAGTNH4 <- lm(NH4~PAGT,data=stressandchem)
shapiro.test(resid(PAGTNH4))
#shapiro- higher p-values are significant

PAGTNO3<- lm(NO3~PAGT,data=stressandchem)
shapiro.test(resid(PAGTNO3))


#Testing Assumptions
PAGTNH4 <- lm(NH4~PAGT,data=stressandchem)
shapiro.test(resid(PAGTNH4))

RoadCond<- lm(RDDENS~COND, data=stressandchem)
shapiro.test(resid(RoadCond))


#calls ggplot
library(ggplot2)
#Example graph
pal<-c("#ffffb2")
shape1<-c(21)
Nitrogengraph<-ggplot(stressandchem, aes(NH4,NO3))+
  geom_point(colour="black",size=4,fill=pal,pch=shape1)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nNH4")+
  ylab("NO3")
# #Call the graph
# Nitrogengraph
# #save to PDF
# pdf("Nitrogengraph.pdf",height = 6,width = 8)
# Nitrogengraph
# dev.off()

#Watershed size vs. % ag?
pal<-c("#ffffb2")
shape1<-c(21)
AgbyWSsize<-ggplot(stressandchem, aes(LANDAREA,PAGT))+
  geom_point(colour="black",size=4,fill=pal,pch=shape1)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nLand area")+
  ylab("% Ag")

#Call graph
AgbyWSsize

#Watershed size vs. Ca?

pal<-c("#ffffb2")
shape1<-c(21)
CabyWSsize<-ggplot(stressandchem, aes(LANDAREA,CA))+
  geom_point(colour="black",size=4,fill=pal,pch=shape1)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nLand area")+
  ylab("Ca")

#Call graph
CabyWSsize

#Function to pull regression text out
lm_eqn<-function(df){
  m<-lm(y~x,df)
  eq<-substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 2),
                      b = format(coef(m)[2], digits = 2),
                      r2 = format(summary(m)$r.squared,digits=3)))
  as.character(as.expression(eq))
}

#Ca vs. Mg?
library(ggplot2)
df<-stressandchem[,c("CA","MG")]
names(df)<-c("x","y")
pal<-c("#ffffb2")
shape1<-c(21)
CaMg<-ggplot(stressandchem, aes(CA,MG))+
  geom_point(colour="black",size=4,fill=pal,pch=shape1)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  geom_text(x=10000,y=17000,label=lm_eqn(df),parse=TRUE)+
  geom_smooth(method=lm,col="firebrick",se=FALSE)+
  xlab("\nCa")+
  ylab("Mg")

# #Call graph
# CaMg
# 
# #save to PDF
# pdf("CaMg.pdf",height=6,width=8)
# CaMg
# dev.off()


#Now that we have imported a merged file with stream velocity, watershed stress, watershed chem, macroinvertebrate diversity and fish cover, we can look at instantaneous discharge and various ion concentrations. We can then look at those values compared to macroinvertebrate diversity data. 

















































































#Road Density vs. Turbidity 
library(ggplot2)
lm_eqn<-function(df){
  m<-lm(y~x,df)
  eq<-substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 2),
                      b = format(coef(m)[2], digits = 2),
                      r2 = format(summary(m)$r.squared,digits=3)))
  as.character(as.expression(eq))
}
df<-stressandchem[,c("RDDENS","TURB")]
names(df)<-c("x","y")
pal<-c("#ffffb2")
shape1<-c(21)
ROADSTURB<-ggplot(stressandchem, aes(RDDENS,TURB))+
  geom_point(colour="black",size=4,fill=pal,pch=shape1)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  geom_text(x=5,y=4000,label=lm_eqn(df),parse=TRUE)+
  geom_smooth(method=lm,col="firebrick",se=FALSE)+
  xlab("Road Density")+
  ylab("Turbidity")

#Call graph
ROADSTURB
# 
# #save to PDF
# pdf("ROADSTURB.pdf",height=6,width=8)
# ROADSTURB
# dev.off()

#Since the linear regression doesn't fit, try a Poisson- this code still needs work
modelP<-glm(RDDENS ~ TURB, data=stressandchem, poisson)
summary(modelP)


df<-stressandchem[,c("RDDENS","TURB")]
names(df)<-c("x","y")
pal<-c("#ffffb2")
shape1<-c(21)
ROADSTURB<-ggplot(stressandchem, aes(RDDENS,TURB))+
  geom_point(colour="black",size=4,fill=pal,pch=shape1)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  geom_text(x=5,y=4000,label=lm_eqn(df),parse=TRUE)+
  geom_smooth(method=lm,col="firebrick",se=FALSE)+
  xlab("Road Density")+
  ylab("Turbidity")


#to assess percent urban area in watershed to conductivity
df<-stressandchem[,c("PURB","COND")]
names(df)<-c("x","y")
pal<-c("#ffffb2")
shape1<-c(21)
URBANCOND<-ggplot(stressandchem, aes(PURB,COND))+
  geom_point(colour="black",size=4,fill=pal,pch=shape1)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  geom_text(x=5,y=4000,label=lm_eqn(df),parse=TRUE)+
  geom_smooth(method=lm,col="firebrick",se=FALSE)+
  xlab("% Urban")+
  ylab("Conductivity")

URBANCOND


#to assess percent urban area in watershed to nitrate
df<-stressandchem[,c("PURB","NO3")]
names(df)<-c("x","y")
pal<-c("#ffffb2")
shape1<-c(21)
URBANNO3<-ggplot(stressandchem, aes(PURB,COND))+
  geom_point(colour="black",size=4,fill=pal,pch=shape1)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  geom_text(x=5,y=4000,label=lm_eqn(df),parse=TRUE)+
  geom_smooth(method=lm,col="firebrick",se=FALSE)+
  xlab("% Urban")+
  ylab("Nitrate")

URBANNO3

#Analysis to run: DO vs % urban, ag, temp, ions that influence DO?, macro diversity, EPT abundance

library(AICcmodavg)
library(car)
library(nlme)
library(doBy)
library(reshape2)
library(vegan)
library(lmtest)
library(raster)
library(rgdal)
library(dismo)
library(rjava)
library(maptools)
library(ROCR)
library(randomForest)
library(mgcv)
library(maps)
library(sp)
library(SDMTools)
library(rgeos)
library(gbm)
library(boot)
library(tigris)
library(devtools)
library(broom)
library(rworldmap)
library(plyr)
library(ggplot2)
library(ggmap)
library(leaflet)
library(acs)
library(spatial.tools)
library(data.table)


#AICc gives you the residual variation, and you choose the candidate model that gives the lowest residual variation. 

#use dataset vbsc to determine the best predictor for 
        
#Create linear models to assess independent variables effect on dependent variables.... land use on concentrations

landuseNH4<-lm(NH4~PAGT, data=vbsc, na.action=na.omit)
plot(landuseNH4)



#Model selection for M. agarici using AICc
#Best air temperature predictor is mean temp
#Land use predictor for NH4
#since we only have 1 or 2 data point per site, it doesn't make sense to use 'random'... 
#x^2 + x for multiple regression
Cand.models <- list()
Cand.models[[1]] <- lm(log(NH4+1)~I(PAGT^2)+PAGT, data=vbsc, na.action="na.omit")
Cand.models[[2]] <- lm(log(NH4+1)~I(PFOR^2)+PFOR, data=vbsc, na.action="na.omit")
Cand.models[[3]] <- lm(log(NH4+1)~I(PURB^2)+PURB, data=vbsc, na.action="na.omit")
Cand.models[[4]] <- lm(log(NH4+1)~PAGT, data=vbsc, na.action="na.omit")
Modnames <- c("%Agrculture", "%Forest", "%Urban", "%Ag-linear")

(aict <- aictab(cand.set = Cand.models, modnames=Modnames, sort=TRUE))

summary(Cand.models[[1]])


plot(simpleNARS$SITE_ID,simpleNARS$pH)

