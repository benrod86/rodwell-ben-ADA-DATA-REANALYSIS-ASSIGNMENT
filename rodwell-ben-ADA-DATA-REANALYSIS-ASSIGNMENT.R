setwd("~/repos/rodwell-ben-ADA-DATA-REANALYSIS-ASSIGNMENT")
library(tidyverse)
library(curl)
library(ggplot2)


## LOAD DATA

d <- read.csv("Pitheciine_Tooth_data.csv")
head(d)
str(d)
d <- d[,1:7]
d <- 
head(d)
str(d)
aotus <- d[1:18,]
callicebus <- d[19:39,]
pithecia <- d[40:53,]
chiropotes <- d[54:64,]
cacajao <- d[65:75,]
d$Genus
names(d)

## Aotus > Pithecia; DNE, RFI, SR, OPCR
ttest01 <- t.test(x=aotus$Energy,y=pithecia$Energy, alternative="greater", conf.level=0.95)
ttest02 <- t.test(x=aotus$RFI,y=pithecia$RFI, alternative="greater", conf.level=0.95)
ttest03 <- t.test(x=aotus$SR,y=pithecia$SR, alternative="greater", conf.level=0.95)
ttest04 <- t.test(x=aotus$OPC,y=pithecia$OPC, alternative="greater", conf.level=0.95, na.rm = TRUE)

## Aotus > Chiropotes; DNE, RFI, SR, OPCR
ttest05 <- t.test(x=aotus$Energy, y=chiropotes$Energy, alternative="greater", conf.level=0.95)
ttest06 <- t.test(x=aotus$RFI, y=chiropotes$RFI, alternative="greater", conf.level=0.95)
ttest07 <- t.test(x=aotus$SR, y=chiropotes$SR, alternative="greater", conf.level=0.95)
ttest08 <- t.test(x=aotus$OPC, y=chiropotes$OPC, alternative="greater", conf.level=0.95)

## Aotus > Cacajao; DNE, RFI, OPCR, SRA
ttest09 <- t.test(x=aotus$Energy, y=cacajao$Energy, alternative="greater", conf.level=0.95)
ttest10 <- t.test(x=aotus$RFI, y=cacajao$RFI, alternative="greater", conf.level=0.95)
ttest11 <- t.test(x=aotus$OPC, y=cacajao$OPC, alternative="greater", conf.level=0.95)
ttest12 <- t.test(x=aotus$SR, y=cacajao$SR, alternative="greater", conf.level=0.95)

## Callicebus > Pithecia; RFI, OPCR, DNE, SRA
ttest13 <- t.test(x=callicebus$RFI,y=pithecia$RFI, alternative="greater", conf.level=0.95)
ttest14 <- t.test(x=callicebus$OPC,y=pithecia$OPC, alternative="greater", conf.level=0.95)
ttest15 <- t.test(x=callicebus$Energy,y=pithecia$Energy, alternative="greater", conf.level=0.95)
ttest16 <- t.test(x=callicebus$SR,y=pithecia$SR, alternative="greater", conf.level=0.95)

## Callicebus > Chiropotes; RFI, DNE, OPCR, SRA
ttest17 <- t.test(x=callicebus$RFI, y=chiropotes$RFI, alternative="greater", conf.level=0.95)
ttest18 <- t.test(x=callicebus$Energy, y=chiropotes$Energy, alternative="greater", conf.level=0.95)
ttest19 <- t.test(x=callicebus$OPC, y=chiropotes$OPC, alternative="greater", conf.level=0.95)
ttest20 <- t.test(x=callicebus$SR, y=chiropotes$SR, alternative="greater", conf.level=0.95)

## Callicebus > Cacajao; OPCR, RFI, DNE, SRA
ttest21 <- t.test(x=callicebus$OPC, y=cacajao$OPC, alternative="greater", conf.level=0.95)
ttest22 <- t.test(x=callicebus$RFI, y=cacajao$RFI, alternative="greater", conf.level=0.95)
ttest23 <- t.test(x=callicebus$Energy, y=cacajao$Energy, alternative="greater", conf.level=0.95)
ttest24 <- t.test(x=callicebus$SR, y=cacajao$SR, alternative="greater", conf.level=0.95)

## Pithecia > Chiropotes; OPCR, SRA, RFI, DNE
ttest25 <- t.test(x=pithecia$OPC, y=chiropotes$OPC, alternative="greater", conf.level=0.95)
ttest26 <- t.test(x=pithecia$SR, y=chiropotes$SR, alternative="greater", conf.level=0.95)
ttest27 <- t.test(x=pithecia$RFI, y=chiropotes$RFI, alternative="greater", conf.level=0.95)
ttest28 <- t.test(x=pithecia$Energy, y=chiropotes$Energy, alternative="greater", conf.level=0.95)

## Pithecia > Cacajao; DNE, RFI, SRA, OPCR
ttest29 <- t.test(x=pithecia$Energy, y=cacajao$Energy, alternative="greater", conf.level=0.95)
ttest30 <- t.test(x=pithecia$RFI, y=cacajao$RFI, alternative="greater", conf.level=0.95)
ttest31 <- t.test(x=pithecia$SR, y=cacajao$SR, alternative="greater", conf.level=0.95)
ttest32 <- t.test(x=pithecia$OPC, y=cacajao$OPC, alternative="greater", conf.level=0.95)

## Chiropotes ?? Cacajao; RFI, DNE, SRA, OPCR
ttest33<- t.test(x=chiropotes$RFI, y=cacajao$RFI, alternative="two.sided", conf.level=0.95)
ttest34 <- t.test(x=chiropotes$Energy, y=cacajao$Energy, alternative="two.sided", conf.level=0.95)
ttest35 <- t.test(x=chiropotes$SR, y=cacajao$SR, alternative="two.sided", conf.level=0.95)
ttest36 <- t.test(x=chiropotes$OPC, y=cacajao$OPC, alternative="two.sided", conf.level=0.95)

ttest01
ttest02
ttest03
ttest04
ttest05
ttest06
ttest07
ttest08
ttest09
ttest10
ttest11
ttest12
ttest13
ttest14
ttest15
ttest16
ttest17
ttest18
ttest19
ttest20
ttest21
ttest22
ttest23
ttest24
ttest25
ttest26
ttest27
ttest28
ttest29
ttest30
ttest31
ttest32
ttest33
ttest34
ttest35
ttest36
names(ttest01)

pvals <- c(ttest01$p.value, ttest02$p.value, ttest03$p.value, ttest04$p.value,
           ttest05$p.value, ttest06$p.value, ttest07$p.value, ttest08$p.value,
           ttest09$p.value, ttest10$p.value, ttest11$p.value, ttest12$p.value,
           ttest13$p.value, ttest14$p.value, ttest15$p.value, ttest16$p.value,
           ttest17$p.value, ttest18$p.value, ttest19$p.value, ttest20$p.value,
           ttest21$p.value, ttest22$p.value, ttest23$p.value, ttest24$p.value, 
           ttest25$p.value, ttest26$p.value, ttest27$p.value, ttest28$p.value,
           ttest29$p.value, ttest30$p.value, ttest31$p.value, ttest32$p.value,
           ttest33$p.value, ttest34$p.value, ttest35$p.value, ttest36$p.value)




pvaluesDNE <- pairwise.t.test(d$Energy, d$Genus, p.adj = "bonferroni")
pvaluesRFI <- pairwise.t.test(d$RFI, d$Genus, p.adj = "bonferroni")
pvaluesOPC <- pairwise.t.test(d$OPC, d$Genus, p.adj = "bonferroni")
pvaluesSR <- pairwise.t.test(d$SR, d$Genus, p.adj = "bonferroni")

pvaluesDNE
pvaluesRFI
pvaluesOPC
pvaluesSR




#### PCA
library(FactoMineR)
library(factoextra)
d <- read.csv("Pitheciine_Tooth_data.csv")

head(d)
d2 <- d[,3:7]
d2


d3 <- log(d2[,2:5])
d4 <- d2[,1]


d3
d4 <- as.data.frame(d4)
d4
d5 <- acbind(d3,d4)
d5
d6 <- d5[-8,]
d6

d7 <- d4[2:75,]
d7 <- as.data.frame(d7)


pca <- prcomp(d6[,-5], scale. = TRUE)
plot(pca)
pca
names(pca)
plot(pca$x, )
pca$x

d8 <- cbind(d7, pca$x)
d8

pcaplot <- ggplot(data = d8, aes(PC1, PC2,) + geom_point(color = d7)
pcaplot


 
 
 
 
 
##### MANOVA #####
 
library(geiger)
tree <- read.nexus("tree.nex") 
data <- read.csv("Pitheciine_means.csv")
dat <- data[,c(1,3:6)]
dat
row.names(dat) <- data$Species
dat <- dat[,2:5]
dat
dat <- as.matrix(dat)
dat

grp<-as.factor(c("Fruit", "Fruit", "Fruit", "Fruit", "Fruit", "Seeds", "Seeds", "Seeds", "Seeds", "Seeds", "Seeds"))
names(grp)=rownames(dat)

## MANOVA

x=aov.phylo(dat~grp, tree, nsim=5000, test="Wilks")
print(attributes(x)$summary) # summary table
summary(x)
str(x)
xs <- summary(x)
pp <- xs$stats
pp[1,6]           

plist <- list()
xx <- for (i in 1:10) {
  x=aov.phylo(dat~grp, tree, nsim=5000, test="Wilks")
  xs <- summary(x)
  pp <- xs$stats
  plist <- pp[1,6]
  return(plist)
}
pplist


plist <- list()
xx <- for (i in 1:10) {
  x=aov.phylo(dat~grp, tree, nsim=5000, test="Wilks")
  xs <- summary(x)
  pp <- xs$stats
  ppp <- pp[1,6]
  plist[i] <- print(ppp)
}
plist 
