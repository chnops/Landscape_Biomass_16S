# using merged from merging_datasets

library(vegan)
library(ggplot2)
library(plyr)
library(reshape)


# calculating diversity measures
Richness<-specnumber(merged[,-c(1:18)])
Shannons.diversity<-diversity(merged[,-c(1:18)])
Evenness<-diversity(merged[,-c(1:18)])/log(specnumber(merged[,-c(1:18)]))

div<-data.frame(merged[,1:18],Richness,Shannons.diversity,Evenness)

div$LP<-as.factor(div$LP)
div$BLK<-as.factor(div$BLK)

# Looking at experimental effects on diversity across 2011
# note that powers represent transformation to be normal based on a Shapiro-Wilk test
list(unique(div$CS))
power_range<-seq(1,5,.1)
results<-data.frame()
for(i in 1:length(power_range)){
	p<-shapiro.test((subset(div, Year=="2011")$Richness)^power_range[i])$p.value
	new<-data.frame(power_range[i],p)
	results<-rbind(results,new)
	
}
plot(results[,1],results[,2])


summary(test<-aov(Richness^4.1~Month+LP+CS, data=subset(div, Year=="2011")))
TukeyHSD(test)

summary(test<-aov(diversity^7.1~Month*LP*CS+Error(BLK), data=subset(div, Year=="2011")))
TukeyHSD(test)

summary(test<-aov(evenness^9.8~Month*LP*CS+Error(BLK), data=subset(div, Year=="2011")))
TukeyHSD(test)