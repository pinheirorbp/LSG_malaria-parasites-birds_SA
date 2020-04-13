### Script 1: Exploring the dataset
library(ggplot2)
library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
library(ape)
library(picante)
library(reshape2)
Dataset <- read.csv("Dataset.csv", h = T, sep = ",", colClasses = c(rep("character",6),NA, NA,rep("character",8)),na.strings = c("","NA"))[-1]
### com_data: table where each line is a local community ####
com_data=unique(Dataset[c("Community","Latitude","Longitude","Biome")])
com_data=com_data[order(com_data$Community),]
com_data$Longitude=as.numeric(com_data$Longitude)
# ploting a map for communities
world=map_data("worldHires")
world2=world[world$long>-85&world$long<(-30)&world$lat>-57&world$lat<15,]
ggplot() +
  geom_polygon(data = world2, aes(x=long, y = lat, group = group), color="black", fill="white") +
  coord_fixed(1) +
  guides (fill=F) +
  geom_point(data = com_data, aes(x = Longitude, y = Latitude, color=Biome), size = 2) +
  theme_bw()
rm(world, world2)
### Basic information for each community
d1=group_by(Dataset,Community)
#nhost: individual birds sampled
com_data$nhost=summarize(d1,ind=length(unique(Host.Voucher)))$ind
#shost: bird species richness
com_data$shost=summarize(d1,s=length(unique(Host.Latin.Name)))$s
#phydiversity: phylogenetic diversity
Bird.tree <- read.tree("bird_phylogeny.tre")
pd.matrix <- cophenetic.phylo(Bird.tree)
Table=Dataset
Table$Host.Latin.Name <- gsub(Table$Host.Latin.Name, pattern = " ", replacement = "_")
Occ.matrix <- dcast(data = Table, formula = Table$Community ~ Table$Host.Latin.Name, fun.aggregate = length)
rownames(Occ.matrix) <- Occ.matrix[,1]
Occ.matrix <- Occ.matrix[,-1]
com_data$phydiversity = picante::mpd(samp = Occ.matrix,  dis = pd.matrix, abundance.weighted = T)
rm(Occ.matrix,Table,pd.matrix,Bird.tree)
# dinf: frequency of double infections
d2=filter(d1,!is.na(Lineage1.ID))
com_data$doubinf=summarize(d2,dinf=sum(duplicated(Host.Voucher)))$dinf
#ninf: individual birds infected
com_data$ninf=summarize(d1,inf=sum(!is.na(Lineage1.ID)))$inf-com_data$doubinf
#prevgeral: general prevalence of avian malaria (Haemoproteus + Parahaemoproteus + Plasmodium)
com_data$prevgeral=com_data$ninf*100/com_data$nhost
# HAprev: Haemoproteus prevalence
d3=filter(d2,Lineage1.Genus=="HA")
nHAinf=rep(0,nrow(com_data))
HAdoubinf=summarize(d3,dinf=sum(duplicated(Host.Voucher)))
HAinf=summarize(d3,inf=sum(!is.na(Lineage1.ID)))
nHAinf[is.element(com_data$Community,HAinf$Community)]=HAinf$inf-HAdoubinf$dinf
com_data$HAprev=nHAinf*100/com_data$nhost
rm(nHAinf,HAinf,HAdoubinf)
# HAs: Haemoproteus lineage richness
com_data$HAs=0
HAs=summarize(d3,s=length(unique(Lineage1.ID)))
com_data$HAs[is.element(com_data$Community,HAs$Community)]=HAs$s
rm(HAs)
# PAprev: Parahemoproteus prevalence
d4=filter(d2,Lineage1.Genus=="PA")
nPAinf=rep(0,nrow(com_data))
PAdoubinf=summarize(d4,dinf=sum(duplicated(Host.Voucher)))
PAinf=summarize(d4,inf=sum(!is.na(Lineage1.ID)))
nPAinf[is.element(com_data$Community,PAinf$Community)]=PAinf$inf-PAdoubinf$dinf
com_data$PAprev=nPAinf*100/com_data$nhost
rm(nPAinf,PAinf,PAdoubinf)
# PAs: Parahaemoproteus lineage richness
com_data$PAs=0
PAs=summarize(d4,s=length(unique(Lineage1.ID)))
com_data$PAs[is.element(com_data$Community,PAs$Community)]=PAs$s
rm(PAs)
# PLprev: Plasmodium prevalence
d5=filter(d2,Lineage1.Genus=="PL")
nPLinf=rep(0,nrow(com_data))
PLdoubinf=summarize(d5,dinf=sum(duplicated(Host.Voucher)))
PLinf=summarize(d5,inf=sum(!is.na(Lineage1.ID)))
nPLinf[is.element(com_data$Community,PLinf$Community)]=PLinf$inf-PLdoubinf$dinf
com_data$PLprev=nPLinf*100/com_data$nhost
rm(nPLinf,PLinf,PLdoubinf)
# PLs: Plasmodium lineage richness
com_data$PLs=0
PLs=summarize(d5,s=length(unique(Lineage1.ID)))
com_data$PLs[is.element(com_data$Community,PLs$Community)]=PLs$s
rm(PLs)
# removing
rm(d1,d2,d3,d4,d5)
save(com_data,file="com_data.RData")
### biome_data: table where each line is a biome ####
biome_data=data.frame(biome=sort(unique(com_data$Biome)))
biome_data$communities=as.character(table(com_data$Biome))
d1=group_by(Dataset,Biome)
#nhost: individual birds sampled
biome_data$nhost=summarize(d1,ind=length(unique(Host.Voucher)))$ind
#shost: bird species richness
biome_data$shost=summarize(d1,s=length(unique(Host.Latin.Name)))$s
# dinf: double infections
d2=filter(d1,!is.na(Lineage1.ID))
biome_data$doubinf=summarize(d2,dinf=sum(duplicated(Host.Voucher)))$dinf
#ninf: number of infected birds
biome_data$ninf=summarize(d1,inf=sum(!is.na(Lineage1.ID)))$inf-biome_data$doubinf
#prevgeral: general prevalence of Avian Malaria
com_data$prevgeral=com_data$ninf*100/com_data$nhost
# HAprev: Haemoproteus prevalence
d3=filter(d2,Lineage1.Genus=="HA")
nHAinf=rep(0,nrow(biome_data))
HAdoubinf=summarize(d3,dinf=sum(duplicated(Host.Voucher)))
HAinf=summarize(d3,inf=sum(!is.na(Lineage1.ID)))
nHAinf[is.element(biome_data$biome,HAinf$Biome)]=HAinf$inf-HAdoubinf$dinf
biome_data$HAprev=nHAinf*100/biome_data$nhost
rm(nHAinf,HAinf,HAdoubinf)
# HAs: Haemoproteus lineage richness
biome_data$HAs=0
HAs=summarize(d3,s=length(unique(Lineage1.ID)))
biome_data$HAs[is.element(biome_data$biome,HAs$Biome)]=HAs$s
rm(HAs)
# PAprev: Parahaemoproteus prevalence
d4=filter(d2,Lineage1.Genus=="PA")
nPAinf=rep(0,nrow(biome_data))
PAdoubinf=summarize(d4,dinf=sum(duplicated(Host.Voucher)))
PAinf=summarize(d4,inf=sum(!is.na(Lineage1.ID)))
nPAinf[is.element(biome_data$biome,PAinf$Biome)]=PAinf$inf-PAdoubinf$dinf
biome_data$PAprev=nPAinf*100/biome_data$nhost
rm(nPAinf,PAinf,PAdoubinf)
# PAs: Parahaemoproteus lineage richness
biome_data$PAs=0
PAs=summarize(d4,s=length(unique(Lineage1.ID)))
biome_data$PAs[is.element(biome_data$biome,PAs$Biome)]=PAs$s
rm(PAs)
# PLprev: Plasmodium prevalence
d5=filter(d2,Lineage1.Genus=="PL")
nPLinf=rep(0,nrow(biome_data))
PLdoubinf=summarize(d5,dinf=sum(duplicated(Host.Voucher)))
PLinf=summarize(d5,inf=sum(!is.na(Lineage1.ID)))
nPLinf[is.element(biome_data$biome,PLinf$Biome)]=PLinf$inf-PLdoubinf$dinf
biome_data$PLprev=nPLinf*100/biome_data$nhost
rm(nPLinf,PLinf,PLdoubinf)
# PLs: Plasmodium lineage richness
biome_data$PLs=0
PLs=summarize(d5,s=length(unique(Lineage1.ID)))
biome_data$PLs[is.element(biome_data$biome,PLs$Biome)]=PLs$s
rm(PLs)
# removing
rm(d1,d2,d3,d4,d5)
save(biome_data,file = "biome_data.RData")