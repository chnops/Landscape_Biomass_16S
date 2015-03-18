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

