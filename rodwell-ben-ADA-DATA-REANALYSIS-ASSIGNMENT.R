setwd("~/repos/rodwell-ben-ADA-DATA-REANALYSIS-ASSIGNMENT")
library(tidyverse)
library(curl)
library(ggplot2)


# Load adata and examine the structure of it
d <- read.csv("Pitheciine_Tooth_data.csv")
head(d)
str(d)
levels(d$Taxon)

# Exploratory data analysis
# Summary stats by species - Mean and SD for each variable and containing each of these.
# The vector will be used to construct a table showing these data for each species

# Aotus azare
aazSRmean <- mean(d$SR[d$Taxon == "Aotus_azare"])
aazSRsd <- sd(d$SR[d$Taxon == "Aotus_azare"])

aazRFImean <- mean(d$RFI[d$Taxon == "Aotus_azare"])
aazRFIsd <- sd(d$RFI[d$Taxon == "Aotus_azare"])

aazDNEmean <- mean(d$Energy[d$Taxon == "Aotus_azare"])
aazDNEsd <- sd(d$Energy[d$Taxon == "Aotus_azare"])

aazOPCmean <- mean(d$OPC[d$Taxon == "Aotus_azare"])
aazOPCsd <- sd(d$OPC[d$Taxon == "Aotus_azare"])

aaz <- c(aazSRmean, aazSRsd, aazRFImean, aazRFIsd, aazDNEmean, aazDNEsd, aazOPCmean, aazOPCsd)

# Aotus nigriceps
aniSRmean <- mean(d$SR[d$Taxon == "Aotus_nigriceps"])
aniSRsd <- sd(d$SR[d$Taxon == "Aotus_nigriceps"])

aniRFImean <- mean(d$RFI[d$Taxon == "Aotus_nigriceps"])
aniRFIsd <- sd(d$RFI[d$Taxon == "Aotus_nigriceps"])

aaaa <- d$Energy[d$Taxon == "Aotus_nigriceps"]
aotusnigriceps <- aaaa[-4]
aniDNEmean <- mean(aotusnigriceps)
aniDNEsd <- sd(aotusnigriceps)

aniOPCmean <- mean(d$OPC[d$Taxon == "Aotus_nigriceps"])
aniOPCsd <- sd(d$OPC[d$Taxon == "Aotus_nigriceps"])

ani <- c(aniSRmean, aniSRsd, aniRFImean, aniRFIsd, aniDNEmean, aniDNEsd, aniOPCmean, aniOPCsd)


# Callicebus donacophilus
cadonSRmean <- mean(d$SR[d$Taxon == "Callicebus_donacophilus"])
cadonSRsd <- sd(d$SR[d$Taxon == "Callicebus_donacophilus"])

cadonRFImean <- mean(d$RFI[d$Taxon == "Callicebus_donacophilus"])
cadonRFIsd <- sd(d$RFI[d$Taxon == "Callicebus_donacophilus"])

cadonDNEmean <- mean(d$Energy[d$Taxon == "Callicebus_donacophilus"])
cadonDNEsd <- sd(d$Energy[d$Taxon == "Callicebus_donacophilus"])

cadonOPCmean <- mean(d$OPC[d$Taxon == "Callicebus_donacophilus"])
cadonOPCsd <- sd(d$OPC[d$Taxon == "Callicebus_donacophilus"])

cadon <- c(cadonSRmean, cadonSRsd, cadonRFImean, cadonRFIsd, cadonDNEmean, cadonDNEsd, cadonOPCmean, cadonOPCsd)


# Callicebus moloch
camolSRmean <- mean(d$SR[d$Taxon == "Callicebus_moloch"])
camolSRsd <- sd(d$SR[d$Taxon == "Callicebus_moloch"])

camolRFImean <- mean(d$RFI[d$Taxon == "Callicebus_moloch"])
camolRFIsd <- sd(d$RFI[d$Taxon == "Callicebus_moloch"])

camolDNEmean <- mean(d$Energy[d$Taxon == "Callicebus_moloch"])
camolDNEsd <- sd(d$Energy[d$Taxon == "Callicebus_moloch"])

camolOPCmean <- mean(d$OPC[d$Taxon == "Callicebus_moloch"])
camolOPCsd <- sd(d$OPC[d$Taxon == "Callicebus_moloch"])

camol <- c(camolSRmean, camolSRsd, camolRFImean, camolRFIsd, camolDNEmean, camolDNEsd, camolOPCmean, camolOPCsd)


# Callicebus torquatus
catorSRmean <- mean(d$SR[d$Taxon == "Callicebus_torquatus"])
catorSRsd <- sd(d$SR[d$Taxon == "Callicebus_torquatus"])

catorRFImean <- mean(d$RFI[d$Taxon == "Callicebus_torquatus"])
catorRFIsd <- sd(d$RFI[d$Taxon == "Callicebus_torquatus"])

catorDNEmean <- mean(d$Energy[d$Taxon == "Callicebus_torquatus"])
catorDNEsd <- sd(d$Energy[d$Taxon == "Callicebus_torquatus"])

catorOPCmean <- mean(d$OPC[d$Taxon == "Callicebus_torquatus"])
catorOPCsd <- sd(d$OPC[d$Taxon == "Callicebus_torquatus"])

cator <- c(catorSRmean, catorSRsd, catorRFImean, catorRFIsd, catorDNEmean, catorDNEsd, catorOPCmean, catorOPCsd)


# Pithecia monachus
pimonSRmean <- mean(d$SR[d$Taxon == "Pithecia_monachus"])
pimonSRsd <- sd(d$SR[d$Taxon == "Pithecia_monachus"])

pimonRFImean <- mean(d$RFI[d$Taxon == "Pithecia_monachus"])
pimonRFIsd <- sd(d$RFI[d$Taxon == "Pithecia_monachus"])

pimonDNEmean <- mean(d$Energy[d$Taxon == "Pithecia_monachus"])
pimonDNEsd <- sd(d$Energy[d$Taxon == "Pithecia_monachus"])

pimonOPCmean <- mean(d$OPC[d$Taxon == "Pithecia_monachus"])
pimonOPCsd <- sd(d$OPC[d$Taxon == "Pithecia_monachus"])

pimon <- c(pimonSRmean, pimonSRsd, pimonRFImean, pimonRFIsd, pimonDNEmean, pimonDNEsd, pimonOPCmean, pimonOPCsd)



# Pithecia pithecia
pipiSRmean <- mean(d$SR[d$Taxon == "Pithecia_pithecia"])
pipiSRsd <- sd(d$SR[d$Taxon == "Pithecia_pithecia"])

pipiRFImean <- mean(d$RFI[d$Taxon == "Pithecia_pithecia"])
pipiRFIsd <- sd(d$RFI[d$Taxon == "Pithecia_pithecia"])

pipiDNEmean <- mean(d$Energy[d$Taxon == "Pithecia_pithecia"])
pipiDNEsd <- sd(d$Energy[d$Taxon == "Pithecia_pithecia"])

pipiOPCmean <- mean(d$OPC[d$Taxon == "Pithecia_pithecia"])
pipiOPCsd <- sd(d$OPC[d$Taxon == "Pithecia_pithecia"])

pipi <- c(pipiSRmean, pipiSRsd, pipiRFImean, pipiRFIsd, pipiDNEmean, pipiDNEsd, pipiOPCmean, pipiOPCsd)



# Chiropotes albinasus
chialSRmean <- mean(d$SR[d$Taxon == "Chiropotes_albinasus"])
chialSRsd <- sd(d$SR[d$Taxon == "Chiropotes_albinasus"])

chialRFImean <- mean(d$RFI[d$Taxon == "Chiropotes_albinasus"])
chialRFIsd <- sd(d$RFI[d$Taxon == "Chiropotes_albinasus"])

chialDNEmean <- mean(d$Energy[d$Taxon == "Chiropotes_albinasus"])
chialDNEsd <- sd(d$Energy[d$Taxon == "Chiropotes_albinasus"])

chialOPCmean <- mean(d$OPC[d$Taxon == "Chiropotes_albinasus"])
chialOPCsd <- sd(d$OPC[d$Taxon == "Chiropotes_albinasus"])

chial <- c(chialSRmean, chialSRsd, chialRFImean, chialRFIsd, chialDNEmean, chialDNEsd, chialOPCmean, chialOPCsd)



# Chiropotes satanas
chisaSRmean <- mean(d$SR[d$Taxon == "Chiropotes_satanas"])
chisaSRsd <- sd(d$SR[d$Taxon == "Chiropotes_satanas"])

chisaRFImean <- mean(d$RFI[d$Taxon == "Chiropotes_satanas"])
chisaRFIsd <- sd(d$RFI[d$Taxon == "Chiropotes_satanas"])

chisaDNEmean <- mean(d$Energy[d$Taxon == "Chiropotes_satanas"])
chisaDNEsd <- sd(d$Energy[d$Taxon == "Chiropotes_satanas"])

chisaOPCmean <- mean(d$OPC[d$Taxon == "Chiropotes_satanas"])
chisaOPCsd <- sd(d$OPC[d$Taxon == "Chiropotes_satanas"])

chisa <- c(chisaSRmean, chisaSRsd, chisaRFImean, chisaRFIsd, chisaDNEmean, chisaDNEsd, chisaOPCmean, chisaOPCsd)


# Cacajao calvus
cacalSRmean <- mean(d$SR[d$Taxon == "Cacajao_calvus"])
cacalSRsd <- sd(d$SR[d$Taxon == "Cacajao_calvus"])

cacalRFImean <- mean(d$RFI[d$Taxon == "Cacajao_calvus"])
cacalRFIsd <- sd(d$RFI[d$Taxon == "Cacajao_calvus"])

cacalDNEmean <- mean(d$Energy[d$Taxon == "Cacajao_calvus"])
cacalDNEsd <- sd(d$Energy[d$Taxon == "Cacajao_calvus"])

cacalOPCmean <- mean(d$OPC[d$Taxon == "Cacajao_calvus"])
cacalOPCsd <- sd(d$OPC[d$Taxon == "Cacajao_calvus"])

cacal <- c(cacalSRmean, cacalSRsd, cacalRFImean, cacalRFIsd, cacalDNEmean, cacalDNEsd, cacalOPCmean, cacalOPCsd)




# Cacajao melanocephalus
camelSRmean <- mean(d$SR[d$Taxon == "Cacajao_melanocephalus"])
camelSRsd <- sd(d$SR[d$Taxon == "Cacajao_melanocephalus"])

camelRFImean <- mean(d$RFI[d$Taxon == "Cacajao_melanocephalus"])
camelRFIsd <- sd(d$RFI[d$Taxon == "Cacajao_melanocephalus"])

camelDNEmean <- mean(d$Energy[d$Taxon == "Cacajao_melanocephalus"])
camelDNEsd <- sd(d$Energy[d$Taxon == "Cacajao_melanocephalus"])

camelOPCmean <- mean(d$OPC[d$Taxon == "Cacajao_melanocephalus"])
camelOPCsd <- sd(d$OPC[d$Taxon == "Cacajao_melanocephalus"])

camel <- c(camelSRmean, camelSRsd, camelRFImean, camelRFIsd, camelDNEmean, camelDNEsd, camelOPCmean, camelOPCsd)


# Combining the variables into a dataframe
rows2 <- c("Aotus azarae", "Aotus nigriceps", "Callicebus donacophilus", "Callicebus moloch", "Callicebus torquatus",
           "Pithecia monachus", "Pithecia pithecia", "Chiropotes albinasus", "Chiropotes satanas", "Cacajao calvus",
           "Cacajao melanocephalus")
tablespecies <- rbind(aaz, ani, cadon, camol, cator, pimon, pipi, chial, chisa, cacal, camel)
tablespecies
sumstatsspecies <- as.data.frame(tablespecies, row.names = T)
sumstatsspecies
rownames(sumstatsspecies) <- rows2
colnames(sumstatsspecies) <- c("SRA Mean", "SRA SD", "RFI Mean", "RFI SD", "DNE Mean", "DNE SD", "OPCR Mean", "OPCR SD")
str(sumstatsspecies)
view(sumstatsspecies)


# Summary statistics by genus - Mean, Minimum, Maximum, and SD for each variable and containing each of these.
# The vector will be used to construct a table showing these data for each genus

# AOTUS
amSRA <- mean(d$SR[d$Genus == "Aotus"])
aminSRA <- min(d$SR[d$Genus == "Aotus"])
amaxSRA <- max(d$SR[d$Genus == "Aotus"])
asdSRA <- sd(d$SR[d$Genus == "Aotus"])

amRFI <- mean(d$RFI[d$Genus == "Aotus"])
aminRFI <- min(d$RFI[d$Genus == "Aotus"])
amaxRFI <- max(d$RFI[d$Genus == "Aotus"])
asdRFI <- sd(d$RFI[d$Genus == "Aotus"])

# Aotus has an "NA" value the "Energy" variable for one of the specimen in the original datset that needs to first be removed
aotusDNE <- d$Energy[d$Genus == "Aotus"]
aotusDNE <- aotusDNE[-8 ]
amDNE <- mean(aotusDNE)
aminDNE <- min(aotusDNE)
amaxDNE <- max(aotusDNE)
asdDNE <- sd(aotusDNE)

amOPC <- mean(d$OPC[d$Genus == "Aotus"])
aminOPC <- min(d$OPC[d$Genus == "Aotus"])
amaxOPC <- max(d$OPC[d$Genus == "Aotus"])
asdOPC <- sd(d$OPC[d$Genus == "Aotus"])

# Callicebus
calmSRA <- mean(d$SR[d$Genus == "Callicebus"])
calminSRA <- min(d$SR[d$Genus == "Callicebus"])
calmaxSRA <- max(d$SR[d$Genus == "Callicebus"])
calsdSRA <- sd(d$SR[d$Genus == "Callicebus"])

calmRFI <- mean(d$RFI[d$Genus == "Callicebus"])
calminRFI <- min(d$RFI[d$Genus == "Callicebus"])
calmaxRFI <- max(d$RFI[d$Genus == "Callicebus"])
calsdRFI <- sd(d$RFI[d$Genus == "Callicebus"])

calmDNE <- mean(d$Energy[d$Genus == "Callicebus"])
calminDNE <- min(d$Energy[d$Genus == "Callicebus"])
calmaxDNE <- max(d$Energy[d$Genus == "Callicebus"])
calsdDNE <- sd(d$Energy[d$Genus == "Callicebus"])

calmOPC <- mean(d$OPC[d$Genus == "Callicebus"])
calminOPC <- min(d$OPC[d$Genus == "Callicebus"])
calmaxOPC <- max(d$OPC[d$Genus == "Callicebus"])
calsdOPC <- sd(d$OPC[d$Genus == "Callicebus"])

# Pithecia
pmSRA <- mean(d$SR[d$Genus == "Pithecia"])
pminSRA <- min(d$SR[d$Genus == "Pithecia"])
pmaxSRA <- max(d$SR[d$Genus == "Pithecia"])
psdSRA <- sd(d$SR[d$Genus == "Pithecia"])

pmRFI <- mean(d$RFI[d$Genus == "Pithecia"])
pminRFI <- min(d$RFI[d$Genus == "Pithecia"])
pmaxRFI <- max(d$RFI[d$Genus == "Pithecia"])
psdRFI <- sd(d$RFI[d$Genus == "Pithecia"])

pmDNE <- mean(d$Energy[d$Genus == "Pithecia"])
pminDNE <- min(d$Energy[d$Genus == "Pithecia"])
pmaxDNE <- max(d$Energy[d$Genus == "Pithecia"])
psdDNE <- sd(d$Energy[d$Genus == "Pithecia"])

pmOPC <- mean(d$OPC[d$Genus == "Pithecia"])
pminOPC <- min(d$OPC[d$Genus == "Pithecia"])
pmaxOPC <- max(d$OPC[d$Genus == "Pithecia"])
psdOPC <- sd(d$OPC[d$Genus == "Pithecia"])

# Chiropotes
chmSRA <- mean(d$SR[d$Genus == "Chiropotes"])
chminSRA <- min(d$SR[d$Genus == "Chiropotes"])
chmaxSRA <- max(d$SR[d$Genus == "Chiropotes"])
chsdSRA <- sd(d$SR[d$Genus == "Chiropotes"])

chmRFI <- mean(d$RFI[d$Genus == "Chiropotes"])
chminRFI <- min(d$RFI[d$Genus == "Chiropotes"])
chmaxRFI <- max(d$RFI[d$Genus == "Chiropotes"])
chsdRFI <- sd(d$RFI[d$Genus == "Chiropotes"])

chmDNE <- mean(d$Energy[d$Genus == "Chiropotes"])
chminDNE <- min(d$Energy[d$Genus == "Chiropotes"])
chmaxDNE <- max(d$Energy[d$Genus == "Chiropotes"])
chsdDNE <- sd(d$Energy[d$Genus == "Chiropotes"])

chmOPC <- mean(d$OPC[d$Genus == "Chiropotes"])
chminOPC <- min(d$OPC[d$Genus == "Chiropotes"])
chmaxOPC <- max(d$OPC[d$Genus == "Chiropotes"])
chsdOPC <- sd(d$OPC[d$Genus == "Chiropotes"])

# Cacajao
cacmSRA <- mean(d$SR[d$Genus == "Cacajao"])
cacminSRA <- min(d$SR[d$Genus == "Cacajao"])
cacmaxSRA <- max(d$SR[d$Genus == "Cacajao"])
cacsdSRA <- sd(d$SR[d$Genus == "Cacajao"])

cacmRFI <- mean(d$RFI[d$Genus == "Cacajao"])
cacminRFI <- min(d$RFI[d$Genus == "Cacajao"])
cacmaxRFI <- max(d$RFI[d$Genus == "Cacajao"])
cacsdRFI <- sd(d$RFI[d$Genus == "Cacajao"])

cacmDNE <- mean(d$Energy[d$Genus == "Cacajao"])
cacminDNE <- min(d$Energy[d$Genus == "Cacajao"])
cacmaxDNE <- max(d$Energy[d$Genus == "Cacajao"])
cacsdDNE <- sd(d$Energy[d$Genus == "Cacajao"])

cacmOPC <- mean(d$OPC[d$Genus == "Cacajao"])
cacminOPC <- min(d$OPC[d$Genus == "Cacajao"])
cacmaxOPC <- max(d$OPC[d$Genus == "Cacajao"])
cacsdOPC <- sd(d$OPC[d$Genus == "Cacajao"])

# Combining the variables to put into a table
# Create vectors containing each of the variables just calculated for each genus
aotussum <- c(amSRA, aminSRA, amaxSRA, asdSRA, amRFI, aminRFI, amaxRFI, asdRFI,
    amDNE, aminDNE, amaxDNE, asdDNE, amOPC, aminOPC, amaxOPC, asdOPC)
calsum <- c(calmSRA, calminSRA, calmaxSRA, calsdSRA, calmRFI, calminRFI, calmaxRFI,
    calsdRFI, calmDNE, calminDNE, calmaxDNE, calsdDNE, calmOPC, calminOPC, calmaxOPC, calsdOPC)
psum <- c(pmSRA, pminSRA, pmaxSRA, psdSRA, pmRFI, pminRFI, pmaxRFI, psdRFI,
              pmDNE, pminDNE, pmaxDNE, psdDNE, pmOPC, pminOPC, pmaxOPC, psdOPC)
chsum <- c(chmSRA, chminSRA, chmaxSRA, chsdSRA, chmRFI, chminRFI, chmaxRFI, chsdRFI,
              chmDNE, chminDNE, chmaxDNE, chsdDNE, chmOPC, chminOPC, chmaxOPC, chsdOPC)
cacsum <- c(cacmSRA, cacminSRA, cacmaxSRA, cacsdSRA, cacmRFI, cacminRFI, cacmaxRFI, cacsdRFI,
              cacmDNE, cacminDNE, cacmaxDNE, cacsdDNE, cacmOPC, cacminOPC, cacmaxOPC, cacsdOPC)
# Create a vector for the row names
rows <- c("SRA Mean", "SRA Min", "SRA Max", "SRA SD", "RFI Mean", "RFI Min", "RFI Max", "RFI SD",
          "DNE Mean", "DNE Min", "DNE Max", "DNE SD","OPCR Mean", "OPCR Min", "OPCR Max", "OPCR SD")
# combine the row names with the variables as columns and turn it into a dataframe
table <- cbind(rows, aotussum, calsum, psum, chsum, cacsum)
table
sumstatsgenus <- as.data.frame(table, row.names = T)
sumstatsgenus
# Set the names of the columns to the different genera
colnames(sumstatsgenus) <- c("", "Aotus", "Callicebus", "Pithecia", "Chiropotes", "Cacajao")
str(sumstatsgenus)
# since all the variables are returned as factors they should be converted back to numeric variables
sumstatsgenus$Aotus <- as.numeric(as.character(sumstatsgenus$Aotus))
sumstatsgenus$Callicebus <- as.numeric(as.character(sumstatsgenus$Callicebus))
sumstatsgenus$Pithecia <- as.numeric(as.character(sumstatsgenus$Pithecia))
sumstatsgenus$Chiropotes <- as.numeric(as.character(sumstatsgenus$Chiropotes))
sumstatsgenus$Cacajao <- as.numeric(as.character(sumstatsgenus$Cacajao))
# convert the first colunm to be the set rownames of the dataframe
row.names(sumstatsgenus) <- rows
sumstatsgenus <- sumstatsgenus[,-1]
View(sumstatsgenus)





# ANOVA and pairwise comparisons
# DNE
library(tidyverse)


# LOAD DATA
d <- read.csv("Pitheciine_Tooth_data.csv")
head(d)
str(d)

DNEaov <- aov(data = d, Energy ~ Genus)
summary(DNEaov)
DNEttest2way <- pairwise.t.test(d$Energy, d$Genus, alternative = "two.sided", p.adjust.method = "holm")
DNEttest1way <- pairwise.t.test(d$Energy, d$Genus, alternative = "less", p.adjust.method = "holm")
DNEttest2way
DNEttest1way


# OPC
OPCaov <- aov(data = d, OPC ~ Genus)
summary(OPCaov)
OPCttest2way <- pairwise.t.test(d$OPC, d$Genus, alternative = "two.sided", p.adjust.method = "holm")
OPCttest1way <- pairwise.t.test(d$OPC, d$Genus, alternative = "", p.adjust.method = "holm")
OPCttest2way
OPCttest1way


# RFI
RFIaov <- aov(data = d, RFI ~ Genus)
summary(RFIaov)
RFIttest2way <- pairwise.t.test(d$RFI, d$Genus, alternative = "two.sided", p.adjust.method = "holm")
RFIttest1way <- pairwise.t.test(d$RFI, d$Genus, alternative = "greater", p.adjust.method = "holm")
RFIttest2way
RFIttest1way


# SR
SRaov <- aov(data = d, SR ~ Genus)
summary(SRaov)
SRttest2way <- pairwise.t.test(d$SR, d$Genus, alternative = "two.sided", p.adjust.method = "holm")
SRttest1way <- pairwise.t.test(d$SR, d$Genus, alternative = "greater", p.adjust.method = "holm")
SRttest2way
SRttest1way



aotus <- d[1:18,]
callicebus <- d[19:39,]
pithecia <- d[40:53,]
chiropotes <- d[54:64,]
cacajao <- d[65:75,]
aotus
names(d)






# Aotus > Pithecia; DNE, RFI, SR, OPCR
ttest01 <- t.test(x=aotus$Energy,y=pithecia$Energy, alternative="greater", conf.level=0.95)
ttest02 <- t.test(x=aotus$RFI,y=pithecia$RFI, alternative="greater", conf.level=0.95)
ttest03 <- t.test(x=aotus$SR,y=pithecia$SR, alternative="greater", conf.level=0.95)
ttest04 <- t.test(x=aotus$OPC,y=pithecia$OPC, alternative="greater", conf.level=0.95, na.rm = TRUE)

# Aotus > Chiropotes; DNE, RFI, SR, OPCR
ttest05 <- t.test(x=aotus$Energy, y=chiropotes$Energy, alternative="greater", conf.level=0.95)
ttest06 <- t.test(x=aotus$RFI, y=chiropotes$RFI, alternative="greater", conf.level=0.95)
ttest07 <- t.test(x=aotus$SR, y=chiropotes$SR, alternative="greater", conf.level=0.95)
ttest08 <- t.test(x=aotus$OPC, y=chiropotes$OPC, alternative="greater", conf.level=0.95)

# Aotus > Cacajao; DNE, RFI, OPCR, SRA
ttest09 <- t.test(x=aotus$Energy, y=cacajao$Energy, alternative="greater", conf.level=0.95)
ttest10 <- t.test(x=aotus$RFI, y=cacajao$RFI, alternative="greater", conf.level=0.95)
ttest11 <- t.test(x=aotus$OPC, y=cacajao$OPC, alternative="greater", conf.level=0.95)
ttest12 <- t.test(x=aotus$SR, y=cacajao$SR, alternative="greater", conf.level=0.95)

# Callicebus > Pithecia; RFI, OPCR, DNE, SRA
ttest13 <- t.test(x=callicebus$RFI,y=pithecia$RFI, alternative="greater", conf.level=0.95)
ttest14 <- t.test(x=callicebus$OPC,y=pithecia$OPC, alternative="greater", conf.level=0.95)
ttest15 <- t.test(x=callicebus$Energy,y=pithecia$Energy, alternative="greater", conf.level=0.95)
ttest16 <- t.test(x=callicebus$SR,y=pithecia$SR, alternative="greater", conf.level=0.95)

# Callicebus > Chiropotes; RFI, DNE, OPCR, SRA
ttest17 <- t.test(x=callicebus$RFI, y=chiropotes$RFI, alternative="greater", conf.level=0.95)
ttest18 <- t.test(x=callicebus$Energy, y=chiropotes$Energy, alternative="greater", conf.level=0.95)
ttest19 <- t.test(x=callicebus$OPC, y=chiropotes$OPC, alternative="greater", conf.level=0.95)
ttest20 <- t.test(x=callicebus$SR, y=chiropotes$SR, alternative="greater", conf.level=0.95)

# Callicebus > Cacajao; OPCR, RFI, DNE, SRA
ttest21 <- t.test(x=callicebus$OPC, y=cacajao$OPC, alternative="greater", conf.level=0.95)
ttest22 <- t.test(x=callicebus$RFI, y=cacajao$RFI, alternative="greater", conf.level=0.95)
ttest23 <- t.test(x=callicebus$Energy, y=cacajao$Energy, alternative="greater", conf.level=0.95)
ttest24 <- t.test(x=callicebus$SR, y=cacajao$SR, alternative="greater", conf.level=0.95)

# Pithecia > Chiropotes; OPCR, SRA, RFI, DNE
ttest25 <- t.test(x=pithecia$OPC, y=chiropotes$OPC, alternative="greater", conf.level=0.95)
ttest26 <- t.test(x=pithecia$SR, y=chiropotes$SR, alternative="greater", conf.level=0.95)
ttest27 <- t.test(x=pithecia$RFI, y=chiropotes$RFI, alternative="greater", conf.level=0.95)
ttest28 <- t.test(x=pithecia$Energy, y=chiropotes$Energy, alternative="greater", conf.level=0.95)

# Pithecia > Cacajao; DNE, RFI, SRA, OPCR
ttest29 <- t.test(x=pithecia$Energy, y=cacajao$Energy, alternative="greater", conf.level=0.95)
ttest30 <- t.test(x=pithecia$RFI, y=cacajao$RFI, alternative="greater", conf.level=0.95)
ttest31 <- t.test(x=pithecia$SR, y=cacajao$SR, alternative="greater", conf.level=0.95)
ttest32 <- t.test(x=pithecia$OPC, y=cacajao$OPC, alternative="greater", conf.level=0.95)

# Chiropotes ?? Cacajao; RFI, DNE, SRA, OPCR
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


pvals <- as.data.frame(pvals)
pvals





# PCA
library(ggplot2)
library(plyr)
# Load the data and set it up to run the PCA
d <- read.csv("Pitheciine_Tooth_data.csv")
head(d)
# create data frame with only the measured variables
d1 <- d[,3:7]
head(d1)
# log transform the data
d1 <- log(d1[,2:5])
head(d1)
d1 <- cbind(d$Genus, d1)
# remove the row with an NA value
d1 <- d1[-8,]
head(d1)
str(d1)
# Run the PCA and inspect the output
pca <- prcomp(d1[,-1], scale = T)
pca
names(pca)
summary(pca)



# Create a table showing the Eigenvalues, % variance accounted for, and coefficients for each variable
names(pca)
# Eigenvalues for each component
pca$var <- pca$sdev * pca$sdev
pca$var
# % varianvce accounted for by each component
pcavar <- c((pca$var[1]/4)*100, (pca$var[2]/4)*100, (pca$var[3]/4)*100, (pca$var[4]/4)*100)
# Making sure that the combined % variance accounted for by each component sums up to 100%
sum(pcavar)
# Combine the Eigenvalues, % variance, and coefficients for each variable into a single table
pcatable <- cbind(pca$var, pcavar, pca$rotation[1,], pca$rotation[2,], pca$rotation[3,], pca$rotation[4,])
pcatable <- as.data.frame(pcatable)
colnames(pcatable) <- c("Eigenvalue", "% Variance", "SRA", "RFI", "DNE", "OPCR")
pcatable
View(pcatable)



# Ploting the PCA
# setup a dataframe with the first 2 principal components and the original dataset used to calculate it
str(pca)
head(pca$x)
d2 <- cbind(d1, pca$x[,1:4])
head(d2)
colnames(d2) <- c("Genus", "SR", "Energy", "RFI", "OPC", "PC1", "PC2", "PC3", "PC4")
head(d2)


# Determine the convex hulls for the scatterplot
find_hull <- function(d2) d2[chull(d2$PC1, d2$PC2), ]
hulls <- ddply(d2, "Genus", find_hull)




# Plot the data first 2 PC's along with the convex hulls mapped onto the different genera 
pcaplot <- ggplot(d2, aes(x = PC1, y = PC2, col = Genus, fill = Genus)) +
  geom_point(shape = 16) + geom_polygon(data = hulls, alpha = .25) +
  xlim(-4, 4)  + ylim(-3, 3)
pcaplot


# BOXPLOTS OF VARIABLES
library(ggplot2)
library(ggpubr)
d <- read.csv("Pitheciine_Tooth_data.csv")
head(d)

# Relevel the geneus names 
d$Genus <- factor(d$Genus, levels = c("Aotus", "Callicebus", "Pithecia", "Chiropotes", "Cacajao"))
levels(d$Genus)

# setup a vector to italicize the x value text 

italic.text <- element_text(face = c("italic"), color = "black", size = 12)
# create the boxplot objects
SRbox <- ggplot(d, aes(x = Genus, y = SR)) +
  geom_boxplot() +xlab("") + ggtitle("SRA") + ylab("SRA") +
  theme(axis.text.x = italic.text, plot.title = element_text(hjust = 0.5) )
RFIbox <- ggplot(d, aes(x = Genus, y = RFI)) +
  geom_boxplot() + xlab("") + ggtitle("RFI") + ylab("RFI") +
  theme(axis.text.x = italic.text, plot.title = element_text(hjust = 0.5) )
DNEbox <- ggplot(d, aes(x = Genus, y = Energy)) +
  geom_boxplot() + xlab("") + ggtitle("DNE") + ylab("DNE") +
  theme(axis.text.x = italic.text, plot.title = element_text(hjust = 0.5) )
OPCRbox <- ggplot(d, aes(x = Genus, y = OPC)) +
  geom_boxplot() + xlab("") + ggtitle("OPCR") + ylab("OPCR") +
  theme(axis.text.x = italic.text, plot.title = element_text(hjust = 0.5) )

# Combine the different plots into a single object
figure3 <- ggarrange(SRbox, RFIbox, DNEbox, OPCRbox, ncol = 2, nrow = 2)
figure3




# MANOVA
 
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


# Code to run the MANOVA
x=aov.phylo(dat~grp, tree, nsim=5000, test="Wilks")
print(attributes(x)$summary)
str(print(attributes(x)$summary))
summary(x)
str(x)



# Needs to be repeated 10 times, and phylogenetic p-value from each
# simulation averaged

# identify thephylogenetically controled p-value
print(attributes(x)$summary[7])
levels(print(attributes(x)$summary[7]))

# create a dummy variable, and
# write a loop for storing the phylogenetic p-values into the dummy variable
plist <- vector()
xx <- for (i in 1:10) {
  x=aov.phylo(dat~grp, tree, nsim=5000, test="Wilks")
  xs <- summary(x)
  pp <- xs$stats
  ppp <- pp[1,6]
  plist[i] <- print(attributes(x)$summary[7])
}
head(plist)
plist

# turn the list of p-values into a dataframe
plist2 <- as.data.frame(plist)
phyloPvals <- plist2
phyloPvals <- as.numeric(plist2[1,])
phyloPvals

# Average the p-values
mean(phyloPvals)



# Construct the table of values from the MANOVA
levels(print(attributes(x)$summary))
wilks <- (attributes(x)$summary[2])
Approx_f <- (attributes(x)$summary[3])
num_df <- (attributes(x)$summary[4])
den_df <- (attributes(x)$summary[5])
standard_pvalue <- (attributes(x)$summary[6])

manovatable <- cbind(wilks, Approx_f, num_df, den_df, standard_pvalue, mean(phyloPvals))
manovatable <- manovatable[1,]
manovatable
View(manovatable)

