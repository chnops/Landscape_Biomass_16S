#First reading in data

dataset<-read.csv("~/Desktop/hargreaves collab/Archive/LBrhizo_nosingletons.csv",header=T,check.names=F)
dim(dataset)
# rarefy to 6678 later
meta<-read.csv("~/Desktop/hargreaves collab/Archive/LB&Rhizo_16S_Metadata.csv",header=T)
names(dataset)[2]<-"Sample.ID"

head(dataset)

# merging metadata and dataset

merged<-merge(meta,dataset,by="Sample.ID")
dim(merged)

summary(merged$Sample.ID)

# removed samples that we are unsure about
merged<-merged[-c(133:136),]

head(merged[,1:50])

# removing due to low sequencing count
merged<-subset(merged, Removed.due.to.low.sequence.count.!="Yes" & Year != "2009" & CS != "4")
head(merged[,1:10])


