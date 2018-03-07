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

<<<<<<< HEAD
=======
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
>>>>>>> 3519ef77dec07bc32564889a382f4a5c2d4a765f
