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
benthicstressandchem<-merge(stressandchem,wsa.bencnt.genus)
velocity.benthic.stress.chem<-merge(benthicstressandchem,streamvelocity)

#Akaike information criterion plots- use this inductive method to tease apart 
#variables and determine which explains the variation the best
#plotting x,y from watershed stress and chem merged dataset
#road miles vs. conductivity
plot(stressandchem$RDDENS,stressandchem$COND)

?plot
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
#Call the graph
Nitrogengraph
#save to PDF
pdf("Nitrogengraph.pdf",height = 6,width = 8)
Nitrogengraph
dev.off()

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

#Call graph
CaMg

#save to PDF
pdf("CaMg.pdf",height=6,width=8)
CaMg
dev.off()


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

#save to PDF
pdf("ROADSTURB.pdf",height=6,width=8)
ROADSTURB
dev.off()

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




