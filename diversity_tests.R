# using merged from merging_datasets

library(vegan)
library(ggplot2)
library(plyr)
library(reshape)

#rarefying data
merged_rar<-cbind(merged[,c(1:18)],rrarefy(merged[,-c(1:18)],6678))

# calculating diversity measures

Richness<-specnumber(merged_rar[,-c(1:18)])
Shannons.diversity<-diversity(merged_rar[,-c(1:18)])
Evenness<-diversity(merged_rar[,-c(1:18)])/log(specnumber(merged_rar[,-c(1:18)]))

div<-data.frame(merged_rar[,1:18],Richness,Shannons.diversity,Evenness)

div$LP<-as.factor(div$LP)
div$BLK<-as.factor(div$BLK)
div$Year<-as.factor(div$Year)
# Looking at experimental effects on diversity across 2011
# note that powers represent transformation to be normal based on a Shapiro-Wilk test

power_range<-seq(9,10,.1)
results<-data.frame()
for(i in 1:length(power_range)){
	p<-shapiro.test((subset(div, Year=="2011")$Evenness)^power_range[i])$p.value
	new<-data.frame(power_range[i],p)
	results<-rbind(results,new)
	
}
plot(results[,1],results[,2])

# We firt ran model with Block as an error term and then without.  If P-values were close (within 0.005 or so), we ran it again without an error term to get Tukey's corrections
summary(aov(Richness^4.5~Month+LP+CS+Error(BLK), data=subset(div, Year=="2011")))
TukeyHSD(aov(Richness^4.5~Month+LP+CS, data=subset(div, Year=="2011")))

summary(aov(Shannons.diversity^7.2~Month+LP+CS+Error(BLK), data=subset(div, Year=="2011")))
TukeyHSD(aov(Shannons.diversity^7.2~Month+LP+CS, data=subset(div, Year=="2011")))

summary(aov(Evenness^9.4~Month+LP+CS+Error(BLK), data=subset(div, Year=="2011")))
TukeyHSD(aov(Evenness^9.4~Month+LP+CS, data=subset(div, Year=="2011")))


# Looking at experimental effects on peak biomass between 2011 and 2012
# note that powers represent transformation to be normal based on a Shapiro-Wilk test

power_range<-seq(8,9,.1)
results<-data.frame()
for(i in 1:length(power_range)){
	p<-shapiro.test((subset(div,Experiment=="LB" & ((Year=="2011" & Month=="July") | Year=="2012"))$Evenness)^power_range[i])$p.value
	new<-data.frame(power_range[i],p)
	results<-rbind(results,new)
	
}
plot(results[,1],results[,2])

summary(aov(Richness^3.9~Year+LP+CS+Error(BLK), data=subset(div,Experiment=="LB" & ((Year=="2011" & Month=="July") | Year=="2012")) ) )
TukeyHSD(aov(Richness^3.9~Year+LP+CS, data=subset(div,Experiment=="LB" & ((Year=="2011" & Month=="July") | Year=="2012")) ))

summary(aov(Shannons.diversity^6.2~Year+LP+CS+Error(BLK), data=subset(div,Experiment=="LB" & ((Year=="2011" & Month=="July") | Year=="2012")) ) )
TukeyHSD(aov(Shannons.diversity^6.2~Year+LP+CS, data=subset(div,Experiment=="LB" & ((Year=="2011" & Month=="July") | Year=="2012")) ))

summary(aov(Evenness^8.6~Year+LP+CS+Error(BLK), data=subset(div,Experiment=="LB" & ((Year=="2011" & Month=="July") | Year=="2012")) ) )
TukeyHSD(aov(Evenness^8.6~Year+LP+CS, data=subset(div,Experiment=="LB" & ((Year=="2011" & Month=="July") | Year=="2012")) ))

# Looking at experimental effects on peak biomass in 2012 between rhizosphere and bulk soil
# note that powers represent transformation to be normal based on a Shapiro-Wilk test

power_range<-seq(20,21,.1)
results<-data.frame()
for(i in 1:length(power_range)){
	p<-shapiro.test((subset(div,Year=="2012" & LP != "3")$Evenness^power_range[i]))$p.value
	new<-data.frame(power_range[i],p)
	results<-rbind(results,new)
	
}
plot(results[,1],results[,2])

summary(aov(Richness^2.5~Experiment+LP+CS+Error(BLK), data=subset(div, Year=="2012" & LP != "3")))
TukeyHSD(aov(Richness^2.5~Experiment+LP+CS, data=subset(div, Year=="2012" & LP != "3")))

summary(aov(Shannons.diversity^10.3~Experiment+LP+CS+Error(BLK), data=subset(div, Year=="2012" & LP != "3")))
TukeyHSD(aov(Shannons.diversity^10.3~Experiment+LP+CS, data=subset(div, Year=="2012" & LP != "3")))

summary(aov(Evenness^20.9~Experiment+LP+CS+Error(BLK), data=subset(div, Year=="2012" & LP != "3" )))
TukeyHSD(aov(Evenness^20.9~Experiment+LP+CS, data=subset(div, Year=="2012" & LP != "3" )))