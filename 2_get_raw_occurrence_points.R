### Author: Emily Beckman  ###  Date: 02/05/2020                                |

### DESCRIPTION:
  # This script provides instructions and code chunks for downloading and
  #   compiling wild occurrence points from:
    # GLOBAL DATABASES
      # Global Biodiversity Information Facility (GBIF)
      # Integrated Digitized Biocollections (iDigBio)
    # NATIONAL DATABASES
      # U.S. Herbaria Consortia (SERNEC, SEINet, etc.)
      # Forest Inventory and Analysis (FIA) Program of the USDA Forest Service

### INPUT:
  # target_taxa_with_syn.csv (list of target taxa)
    # columns:
      # 1. "taxon_name" (genus, species, infra rank, and infra name, all
      #    separated by one space each; hybrid symbol should be " x ", rather
      #    than "_" or "✕", and go between genus and species)
      # 2. (optional) "taxon_name_acc" (accepted taxon name you have chosen)
      # 3+ (optional) other data you want to keep with taxa info

### OUTPUTS:
    # gbif_raw.csv
    # idigbio_raw.csv
    # herbaria_raw.csv
    # fia_raw.csv

#################
### LIBRARIES ###
#################

library(spocc)
library(rgbif)

##############
### SCRIPT ###
##############

setwd("./Desktop")

################################################################################
# A) Global Biodiversity Information Facility (GBIF)
################################################################################

# GBIF account user information
  # !!! FILL THIS IN WITH YOUR INFO:
user <- "user"
pwd <- "pwd"
email <- "email@company.org"

# read in taxa list
taxon_list <- read.csv("Desiderata_withSyn_Feb2020.csv", header = T,
  na.strings=c("","NA"), colClasses="character")
#taxon_list <- taxon_list %>% filter(is.na(taxon_type) |
#                                     taxon_type != "cultivar");nrow(taxon_list)
# list of target taxon names
taxon_names <- taxon_list[,1]

# get GBIF taxon keys for all taxa in target list
keys <- sapply(taxon_names,function(x) name_backbone(name=x)$speciesKey,
  simplify = "array")
# remove duplicate and NULL keys
keys_nodup <- keys[!duplicated(keys) & keys != "NULL"]
# create vector of keys as input into gbif download
gbif_taxon_keys <- vector(mode="numeric")
for(i in 2:length(keys_nodup)){
  gbif_taxon_keys <- c(gbif_taxon_keys,keys_nodup[[i]][1])
}; sort(gbif_taxon_keys)

# download GBIF data (Darwin Core Archive format)
gbif_download <- occ_download(
                     pred_in("taxonKey", gbif_taxon_keys),
                     pred_in("basisOfRecord", c("PRESERVED_SPECIMEN",
                        "HUMAN_OBSERVATION","FOSSIL_SPECIMEN","OBSERVATION",
                        "UNKNOWN","MACHINE_OBSERVATION","MATERIAL_SAMPLE",
                        "LITERATURE")),
                     #pred("hasCoordinate", TRUE),
                     #pred("hasGeospatialIssue", FALSE),
                     format = "DWCA", #"SIMPLE_CSV"
                     user=user,pwd=pwd,
                     email=email)
# must wait for download to complete before running next line;
# it may take a while (up to 3 hours) if you have a large taxa list;
# you can check download status here: https://www.gbif.org/user/download

# load gbif data just downloaded
gbif_download # !!! PASTE "Download key" as first argument in next two lines !!!
   # download to current working directory and unzip before reading in
occ_download_get(key="0013834-200221144449610", overwrite=TRUE)
unzip("0013834-200221144449610.zip")
  # read in data
gbif_raw <- fread("occurrence.txt",quote=""); unique(gbif_raw$taxonKey)
# write file
write.csv(gbif_raw, "gbif_raw.csv")

################################################################################
# B) Integrated Digitized Biocollections (iDigBio)
################################################################################

# First, download raw data
  # Go to https://www.idigbio.org/portal/search
  # Type your target genus name into the "Scientific Name" box on the left hand
  #    side and check the "Must have map point" checkbox
  # Click the "Download" tab, type in your email, and click the download button
  #   (down arrow within circle)

# If you have more than one target genus, repeat the above steps for the
#   other genera

# Your downloads will pop up in the "Downloads" section;
# click "Click To Download" for each

# Move all the zipped files you downloaded into a "idigbio_read_in" folder
#   within your working directory
# Unzip each file and pull the "occurrence.csv" file out into the
#   "idigbio_read_in" folder -- obviously "keep both" when prompted;
#   *this unzip step could eventually be coded in here....

# read in raw occurrence points
file_list <- list.files(path = "idigbio_read_in", pattern = ".csv",
  full.names = T)
file_dfs <- lapply(file_list, read.csv, colClasses = "character",
  na.strings=c("","NA"),strip.white=T,fileEncoding="UTF-8")
length(file_dfs) #10

# stack datasets to create one dataframe
idigbio_raw <- data.frame()
for(file in seq_along(file_dfs)){
  idigbio_raw <- rbind(idigbio_raw, file_dfs[[file]])
}; nrow(idigbio_raw) #55062
# write file
write.csv(idigbio_raw, "idigbio_raw.csv")

################################################################################
# C) U.S. Herbaria Consortia (SERNEC, SEINet, etc.)
################################################################################

# First, download raw data
  # Go to http://sernecportal.org/portal/collections/harvestparams.php
  # Type your target genus name into the "scientific name" box and click
  #   "List Display"
  # Click the Download Specimen Data button (arrow pointing down into a box),
  #   in the top right corner
  # In the pop-up window, select the "Darwin Core" radio button,
  #   uncheck everything in the "Data Extensions:" section, and
  #   select the "UTF-8 (unicode)" radio button
  #   leave other fields as-is
  # Click "Download Data"

# If you have more than one target genus, repeat the above steps for the
#   other genera

# Move all the zipped files you downloaded into a "sernec_read_in" folder
#   within your working directory
# Unzip each file and pull the "occurrences.csv" file out into the
#   "sernec_read_in" folder -- obviously "keep both" when prompted;
#   *this unzip step could eventually be coded in here....

# read in raw occurrence points
file_list <- list.files(path = "sernec_read_in", pattern = ".csv",
  full.names = T)
file_dfs <- lapply(file_list, read.csv, colClasses = "character",
  na.strings=c("","NA"),strip.white=T,fileEncoding="latin1")
length(file_dfs) #10

# stack datasets to create one dataframe
sernec_raw <- data.frame()
for(file in seq_along(file_dfs)){
  sernec_raw <- rbind(sernec_raw, file_dfs[[file]])
}; nrow(sernec_raw) #135884
# write file
write.csv(sernec_raw, "sernec_raw.csv")

################################################################################
# D) Forest Inventory and Analysis (FIA); Program of the USDA Forest Service
################################################################################

# First, download raw data
  # Go to https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html
  # Either download the "TREE" file (e.g., "AL_TREE.csv") for each state
  #   (works well if you only need a few) or scroll to the bottom of the
  #   page and download "TREE.csv", which gives data for all states combined
  #   (9.73 GB)

# read in taxa list
taxon_list <- read.csv("Desiderata_withSyn_Feb2020.csv", header = T,
  na.strings=c("","NA"), colClasses="character")
#taxon_list <- taxon_list %>% filter(is.na(taxon_type) |
#                                     taxon_type != "cultivar");nrow(taxon_list)
# list of target taxon names
taxon_names <- taxon_list[,1]

# join taxa list to FIA species codes


# make a list of unique FIA species codes to select from the database
species_codes <- sort(unique(taxon_list$fia_code))

setwd("./.."); setwd("./FIA/FIA_CSV_DATA")

# read in tree data, which lists all species and the plots in which they were found
# this one will take time to read in
# treeAL <- read.csv("AL_TREE.csv")
treeAL <- read.csv("AL_TREE.csv")
# first we want to ensure that all the trees in this sample are live
treeAL <- treeAL[treeAL$STATUSCD == 1, ]
# and make a new target_sp data frame
output_data <- data.frame()
# Now we can cycle through our vector of rare target_sp species codes and extract those
# rows from Alabama

for (sp in 1:length(species_codes)){
  target_sp <- treeAL[which(treeAL$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
  }

summary(output_data$SPCD)
unique(output_data$SPCD)
# Looks good!!
rm(treeAL)

############### New state

# treeAZ <- read.csv("AZ_TREE.csv")
treeAZ <- read.csv("AZ_TREE.csv")
treeAZ <- treeAZ[treeAZ$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeAZ[which(treeAZ$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeAZ)

# treeAR <- read.csv("AR_TREE.csv")
treeAR <- read.csv("AR_TREE.csv")
treeAR <- treeAR[treeAR$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeAR[which(treeAR$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeAR)

# treeCA <- read.csv("CA_TREE.csv")
treeCA <- read.csv("CA_TREE.csv")
treeCA <- treeCA[treeCA$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeCA[which(treeCA$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeCA)

# treeCO <- read.csv("CO_TREE.csv")
treeCO <- read.csv("CO_TREE.csv")
treeCO <- treeCO[treeCO$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeCO[which(treeCO$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeCO)

# treeCT <- read.csv("CT_TREE.csv")
treeCT <- read.csv("CT_TREE.csv")
treeCT <- treeCT[treeCT$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeCT[which(treeCT$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeCT)

# treeDE <- read.csv("DE_TREE.csv")
treeDE <- read.csv("DE_TREE.csv")
treeDE <- treeDE[treeDE$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeDE[which(treeDE$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeDE)

# treeFL <- read.csv("FL_TREE.csv")
treeFL <- read.csv("FL_TREE.csv")
treeFL <- treeFL[treeFL$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeFL[which(treeFL$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeFL)

# treeGA <- read.csv("GA_TREE.csv")
treeGA <- read.csv("GA_TREE.csv")
treeGA <- treeGA[treeGA$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeGA[which(treeGA$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeGA)

# treeID <- read.csv("ID_TREE.csv")
treeID <- read.csv("ID_TREE.csv")
treeID <- treeID[treeID$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeID[which(treeID$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeID)

# treeIL <- read.csv("IL_TREE.csv")
treeIL <- read.csv("IL_TREE.csv")
treeIL <- treeIL[treeIL$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeIL[which(treeIL$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeIL)

# treeIN <- read.csv("IN_TREE.csv")
treeIN <- read.csv("IN_TREE.csv")
treeIN <- treeIN[treeIN$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeIN[which(treeIN$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeIN)

# treeIA <- read.csv("IA_TREE.csv")
treeIA <- read.csv("IA_TREE.csv")
treeIA <- treeIA[treeIA$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeIA[which(treeIA$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeIA)

# treeKS <- read.csv("KS_TREE.csv")
treeKS <- read.csv("KS_TREE.csv")
treeKS <- treeKS[treeKS$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeKS[which(treeKS$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeKS)

# treeKY <- read.csv("KY_TREE.csv")
treeKY <- read.csv("KY_TREE.csv")
treeKY <- treeKY[treeKY$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeKY[which(treeKY$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeKY)

# treeLA <- read.csv("LA_TREE.csv")
treeLA <- read.csv("LA_TREE.csv")
treeLA <- treeLA[treeLA$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeLA[which(treeLA$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeLA)

# treeME <- read.csv("ME_TREE.csv")
treeME <- read.csv("ME_TREE.csv")
treeME <- treeME[treeME$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeME[which(treeME$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeME)

# treeMD <- read.csv("MD_TREE.csv")
treeMD <- read.csv("MD_TREE.csv")
treeMD <- treeMD[treeMD$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeMD[which(treeMD$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeMD)

# treeMA <- read.csv("MA_TREE.csv")
treeMA <- read.csv("MA_TREE.csv")
treeMA <- treeMA[treeMA$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeMA[which(treeMA$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeMA)

# treeMI <- read.csv("MI_TREE.csv")
treeMI <- read.csv("MI_TREE.csv")
treeMI <- treeMI[treeMI$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeMI[which(treeMI$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeMI)

# treeMN <- read.csv("MN_TREE.csv")
treeMN <- read.csv("MN_TREE.csv")
treeMN <- treeMN[treeMN$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeMN[which(treeMN$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeMN)

# treeMS <- read.csv("MS_TREE.csv")
treeMS <- read.csv("MS_TREE.csv")
treeMS <- treeMS[treeMS$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeMS[which(treeMS$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeMS)

# treeMO <- read.csv("MO_TREE.csv")
treeMO <- read.csv("MO_TREE.csv")
treeMO <- treeMO[treeMO$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeMO[which(treeMO$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeMO)

# treeMT <- read.csv("MT_TREE.csv")
treeMT <- read.csv("MT_TREE.csv")
treeMT <- treeMT[treeMT$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeMT[which(treeMT$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeMT)

# treeNE <- read.csv("NE_TREE.csv")
treeNE <- read.csv("NE_TREE.csv")
treeNE <- treeNE[treeNE$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeNE[which(treeNE$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeNE)

# treeNV <- read.csv("NV_TREE.csv")
treeNV <- read.csv("NV_TREE.csv")
treeNV <- treeNV[treeNV$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeNV[which(treeNV$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeNV)

# treeNH <- read.csv("NH_TREE.csv")
treeNH <- read.csv("NH_TREE.csv")
treeNH <- treeNH[treeNH$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeNH[which(treeNH$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeNH)

# treeNJ <- read.csv("NJ_TREE.csv")
treeNJ <- read.csv("NJ_TREE.csv")
treeNJ <- treeNJ[treeNJ$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeNJ[which(treeNJ$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeNJ)

# treeNM <- read.csv("NM_TREE.csv")
treeNM <- read.csv("NM_TREE.csv")
treeNM <- treeNM[treeNM$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeNM[which(treeNM$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeNM)

# treeNY <- read.csv("NY_TREE.csv")
treeNY <- read.csv("NY_TREE.csv")
treeNY <- treeNY[treeNY$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeNY[which(treeNY$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeNY)

# treeNC <- read.csv("NC_TREE.csv")
treeNC <- read.csv("NC_TREE.csv")
treeNC <- treeNC[treeNC$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeNC[which(treeNC$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeNC)

# treeND <- read.csv("ND_TREE.csv")
treeND <- read.csv("ND_TREE.csv")
treeND <- treeND[treeND$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeND[which(treeND$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeND)

# treeOH <- read.csv("OH_TREE.csv")
treeOH <- read.csv("OH_TREE.csv")
treeOH <- treeOH[treeOH$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeOH[which(treeOH$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeOH)

# treeOK <- read.csv("OK_TREE.csv")
treeOK <- read.csv("OK_TREE.csv")
treeOK <- treeOK[treeOK$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeOK[which(treeOK$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeOK)

# treeOR <- read.csv("OR_TREE.csv")
treeOR <- read.csv("OR_TREE.csv")
treeOR <- treeOR[treeOR$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeOR[which(treeOR$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeOR)

# treePA <- read.csv("PA_TREE.csv")
treePA <- read.csv("PA_TREE.csv")
treePA <- treePA[treePA$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treePA[which(treePA$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treePA)

# treeRI <- read.csv("RI_TREE.csv")
treeRI <- read.csv("RI_TREE.csv")
treeRI <- treeRI[treeRI$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeRI[which(treeRI$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeRI)

# treeSC <- read.csv("SC_TREE.csv")
treeSC <- read.csv("SC_TREE.csv")
treeSC <- treeSC[treeSC$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeSC[which(treeSC$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeSC)

# treeSD <- read.csv("SD_TREE.csv")
treeSD <- read.csv("SD_TREE.csv")
treeSD <- treeSD[treeSD$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeSD[which(treeSD$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeSD)

# treeTN <- read.csv("TN_TREE.csv")
treeTN <- read.csv("TN_TREE.csv")
treeTN <- treeTN[treeTN$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeTN[which(treeTN$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeTN)

# treeTX <- read.csv("TX_TREE.csv")
treeTX <- read.csv("TX_TREE.csv")
treeTX <- treeTX[treeTX$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeTX[which(treeTX$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeTX)

# treeUT <- read.csv("UT_TREE.csv")
treeUT <- read.csv("UT_TREE.csv")
treeUT <- treeUT[treeUT$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeUT[which(treeUT$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeUT)

# treeVT <- read.csv("VT_TREE.csv")
treeVT <- read.csv("VT_TREE.csv")
treeVT <- treeVT[treeVT$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeVT[which(treeVT$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeVT)

# treeVA <- read.csv("VA_TREE.csv")
treeVA <- read.csv("VA_TREE.csv")
treeVA <- treeVA[treeVA$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeVA[which(treeVA$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeVA)

# treeWA <- read.csv("WA_TREE.csv")
treeWA <- read.csv("WA_TREE.csv")
treeWA <- treeWA[treeWA$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeWA[which(treeWA$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeWA)

# treeWV <- read.csv("WV_TREE.csv")
treeWV <- read.csv("WV_TREE.csv")
treeWV <- treeWV[treeWV$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeWV[which(treeWV$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeWV)

# treeWI <- read.csv("WI_TREE.csv")
treeWI <- read.csv("WI_TREE.csv")
treeWI <- treeWI[treeWI$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeWI[which(treeWI$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeWI)

# treeWY <- read.csv("WY_TREE.csv")
treeWY <- read.csv("WY_TREE.csv")
treeWY <- treeWY[treeWY$STATUSCD == 1, ]

for (sp in 1:length(species_codes)){
  target_sp <- treeWY[which(treeWY$SPCD==species_codes[sp]),]
  output_data <- rbind(output_data, target_sp)
}
rm(treeWY)

write.csv(x = output_data, file = "fia_tree_raw_GA2_8_23.csv")
