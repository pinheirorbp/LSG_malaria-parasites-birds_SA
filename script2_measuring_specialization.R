### Script 2: calculating the network specialization metrics
library(reshape2)
library(bipartite)
DS <- read.csv("Dataset.csv", h = T, sep = ",", colClasses = c(rep("character",6),rep(NA,2),rep("character",8)),na.strings = c("","NA"))
# load("com_data.RData")
#DS.inf: dataset only with infected samples
DS.inf <- subset(DS, !is.na(DS$Lineage1.Name))
### Calculating connectance, H2 and other information for each local network ####
Datasets.com <- split.data.frame(DS,DS$Community)
for (cc in 1:length(Datasets.com)){
  #datase of the community "cc"
  dataset.c <- Datasets.com[[cc]]
  #only infected samples in community "cc"
  dataset.inf.c <- subset(dataset.c, !is.na(dataset.c$Lineage1.Name))
  #interaction matrix of community "cc"
  im.c <- as.matrix(dcast(formula = dataset.inf.c$Host.Latin.Name ~ dataset.inf.c$Lineage1.Name, data = dataset.inf.c,
                          fun.aggregate = length))
  rownames(im.c) <- im.c[,1]
  im.c <- im.c[,-1]
  class(im.c) <- "numeric"
  if(is.matrix(im.c)){
    #shos.mat: richness of infected birds
    com_data$shos.mat[cc] <- nrow(im.c)
    #spar: parasite lineage richness
    com_data$spar[cc] <- ncol(im.c)
    #H2'
    com_data$H2[cc] <- networklevel(im.c, index = "H2")
    # connectance
    com_data$connectance[cc]<-networklevel(im.c, index = "connectance")
  }
  else{ com_data$shos.mat[cc] <- com_data$spar[cc] <- com_data$H2[cc] <- com_data$connectance[cc]<- NA
  }
}
# save(com_data,file="com_data.RData")
rm(dataset.c,dataset.inf.c,Datasets.com,DS.inf,im.c,cc,DS)
### Calculating DSI for each local network ####
## To install package Dizzy: ##
# install.packages("remotes")
# remotes::install_github("leorjorge/dizzy")
library(dizzy)
library(ape)
library(reshape2)
Table <- read.csv("Dataset.csv", header = T, na.strings = c("NA",""))
Table$Host.Latin.Name <- gsub(Table$Host.Latin.Name, pattern = " ", replacement = "_")
#Array with interaction matrices 
Array <- dizzy::RecordToArray(data = Table,cons = "Lineage1.Name",res = "Host.Latin.Name", loc = "Community")
# Bird abundances per local
Occ.Mat <- dcast(data = Table, formula = Host.Latin.Name ~ Community, fun.aggregate = length)
rownames(Occ.Mat) <- Occ.Mat[,1]
Occ.Mat <- Occ.Mat[,-1]
# Loading the bird phylogeny
Tree <- read.tree(file = "bird_phylogeny.tre")
# Measuring DSI
Dizzy.comp <- dizzy::dsicom(Int = Array, Dist = Tree, Abund = Occ.Mat, Rep = 999, Part = F)
com_data$DSI=Dizzy.comp$dsicom
rm(Occ.Mat,Table,Array,Dizzy.comp,Tree)
save(com_data,file="com_data.RData")