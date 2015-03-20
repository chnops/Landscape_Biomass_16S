# doing rank abundance starting with merged_rar from previous analysis of rhizo

data.rhizo<-subset(merged_rar, Year=="2012" & LP != "3")

names(data.rhizo[,1:20])

# making a variable to do rank abundance within
library(reshape2)
exp.units<-paste(data.rhizo$Experiment,data.rhizo$LP,data.rhizo$BLK,data.rhizo$CS)
data.rhizo<-cbind(exp.units,data.rhizo)
dim(data.rhizo)
ranked<-data.frame()
for(i in 1:length(exp.units)){
	
	temp<-subset(data.rhizo, exp.units==exp.units[i])
	temp_melt<-melt(temp[,c(11:13,15,20:10289)],id=c("Experiment","LP","BLK","CS"))
	temp_melt<-merge(temp_melt, otu_w_taxonomy,by.x="variable",by.y="OTU.ID")

	temp_cast<-dcast(temp_melt,Experiment+LP+BLK+CS~order, value.var="value",fun.aggregate=sum)
	temp_melt<-melt(temp_cast, id=c("Experiment","LP","BLK","CS"))
	temp_melt<-arrange(temp_melt, -value)
	temp_melt$rank<-seq(1,dim(temp_melt)[1],1)
	ranked<-rbind(ranked,temp_melt)
	print(i/length(exp.units))
}

#Break into rhizo and bulk and merge
head(rhizo)

rhizo<-subset(ranked, Experiment=="Rhizo")
bulk<-subset(ranked, Experiment=="LB")

merge_rank<-merge(rhizo,bulk, by=c("LP","BLK","CS","variable"))

head(merge_rank)

merge_rank$delta_rank<-merge_rank$rank.x-merge_rank$rank.y

# calculating delta rank by each order

library(plyr)

stats<-ddply(merge_rank, .(variable),summarise,.progress="text",
delta=mean(delta_rank),
hi=quantile(delta_rank,0.975),
lo=quantile(delta_rank,0.025)
)
head(stats)

# looking at orders that increase moving away from rhizo ( greater than 0)
bulk_enriched<-subset(stats, lo > 0)

# looking at orders that increase moving away from rhizo ( greater than 0)
bulk_enriched<-subset(stats, hi < 0)