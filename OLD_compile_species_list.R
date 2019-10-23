### Author: Emily Beckman  ###  Date: 06/28/19

### DESCRIPTION:
  # This script takes a list of species and joins it to all related names (synonyms) in GBIF, and matches to species codes used by FIA.
  # Also, GBIF data for all target species is downloaded and can then be used in "compile_occurrence_data.R" script.
  # It is an important first step to gather synonyms for each target species before you download data from other occurrence data
  # platforms, to make sure you don't miss important points; GBIF provides an easy way to get most of the synonyms other platforms will have.

### INPUTS:
    # 1. list of target species (IMLS_species_desiderata.csv)
      # two columns: sp_full_name (genus, species, infra rank, and infra name) & orig_list ("Y")
    # 2. fia tree list (FIA_speciesList_2017.csv)
      # three columns of use to us: FIA.Code,	PLANTS.Code, & Common.Name

### OUTPUT:
    # 1. target species list matched to GBIF keys and FIA codes (IMLS_sp_list_joined.csv)
      # GBIF-sourced columns include:
        # speciesKey,taxonKey,scientificName,acceptedScientificName,acceptedTaxonKey,taxonomicStatus,order,family,orderKey,familyKey,genusKey
      # Some other helpful columns are added in this script:
        # genus,specificEpithet,infraspecificEpithet1,infraspecificEpithet2,genus_species,our_accepted_name


###############
## LIBRARIES ##
###############

library(googledrive)
library(httpuv)
library(rgbif)
library(data.table)
library(bit64)
library(tidyr)
library(plyr)
library(dplyr)


########################################
# 1. Load species list and find GBIF keys
########################################

setwd("./Desktop/GBIF_read_in")

# read in species list
drive_download("IMLS_species_desiderata",type="csv",overwrite=T) # download from Google Drive
sp_list_acc <- read.csv("IMLS_species_desiderata.csv", header = T, na.strings=c("","NA"), colClasses="character") # read from local
sp_names <- sp_list_acc[,1] # create list of target species names

# create list of GBIF species keys for all species in target list
keys1 <- sapply(sp_names, function(x) name_backbone(name=x)$speciesKey, simplify = "array")
  keys <- paste("taxonKey = ",keys1[1],",2880444",sep="") # !!! Q. sadleriana is 2880444 ; issue with GBIF backbone so not found automatically
  for(i in 2:length(keys1)){
    keys <- paste(keys,keys1[i],sep=",")
  }; keys

########################
# 2. Download GBIF data
########################

# download data (Darwin Core Archive format) from GBIF by species keys
gbif_download <- occ_download(keys,
                     "basisOfRecord = PRESERVED_SPECIMEN,HUMAN_OBSERVATION,FOSSIL_SPECIMEN,OBSERVATION,UNKNOWN,MACHINE_OBSERVATION,MATERIAL_SAMPLE,LITERATURE",
                     user="ebeckman",pwd="Quercus51",email="ebeckman@mortonarb.org")
gbif_download # Download key: 0003455-190621201848488

# WAIT FOR THIS DOWNLOAD TO COMPLETE BEFORE RUNNING NEXT LINE
# you can check download status here: https://www.gbif.org/user/download
# it may take a while (up to 3 hours) if you have a large species list

# !!! PASTE "Download key" FROM ABOVE as first argument
occ_download_get(key="0003455-190621201848488", overwrite=TRUE) # download GBIF data to current working directory; Download file size: 348.27 MB
unzip("0003455-190621201848488.zip") # unzip file

# read in data just downloaded
gbif_raw <- fread("occurrence.txt",quote=""); unique(gbif_raw$taxonKey)

# this used to work to import GBIF data directly to R but updated packages & R version and something is off now:
  # gbif_raw <- occ_download_get("0003346-190621201848488", overwrite = TRUE) %>% occ_download_import()

#########################################
# 3. Add GBIF taxon codes to species list
#########################################

# create sp_full_name column of accepted gbif names by pasting together genus, species, and infra rank columns
subsp <- gbif_raw %>% filter(gbif_raw$taxonRank %in% "SUBSPECIES")
  subsp$sp_full_name <- paste(subsp$genus,subsp$specificEpithet,"subsp.",subsp$infraspecificEpithet,sep=" ")
var <- gbif_raw %>% filter(gbif_raw$taxonRank %in% "VARIETY")
  var$sp_full_name <- paste(var$genus,var$specificEpithet,"var.",var$infraspecificEpithet,sep=" ")
form <- gbif_raw %>% filter(gbif_raw$taxonRank %in% "FORM")
  form$sp_full_name <- paste(form$genus,form$specificEpithet,"f.",form$infraspecificEpithet,sep=" ")
the_rest <- gbif_raw %>% filter(gbif_raw$taxonRank %in% "SPECIES")
  the_rest$sp_full_name <- paste(the_rest$genus,the_rest$specificEpithet,sep=" ")
gbif_raw <- rbind(subsp,var,form,the_rest); sort(unique(gbif_raw$sp_full_name))

# keep only the pertinent columns
gbif_join <- gbif_raw %>% distinct(sp_full_name,speciesKey,taxonKey,scientificName,acceptedScientificName,
                                   acceptedTaxonKey,taxonomicStatus,order,family,orderKey,familyKey,genusKey)

# join gbif codes/species names to species list
sp_list_join <- join(sp_list_acc,gbif_join,by="sp_full_name",type="full"); nrow(sp_list_join) #211
  sp_list_join[which(is.na(sp_list_join$speciesKey)),] # these are species names which had to GBIF name match; if zero, everything is good!

##########################
# 4. Add FIA species codes
##########################

# join to fia tree list
drive_download("FIA_speciesList_2017.csv",overwrite=T) # download from Google Drive
fia_list <- read.csv("FIA_speciesList_2017.csv", header = T, na.strings=c("","NA"), colClasses="character") # read from local
  fia_codes <- fia_list[,c(1:3,8)] # keep only pertinent columns
sp_list_join2 <- join(sp_list_join,fia_codes,by="sp_full_name",type="left",match="first")

##################################
# 5. Final touches to species list
##################################

# create columns for each part of species names
sp_list_join3 <- sp_list_join2 %>% separate(sp_full_name, c("genus","specificEpithet","infraspecificEpithet1",
                                                            "infraspecificEpithet2")," ",remove=F)
sp_list_join3$genus_species <- paste(sp_list_join3$genus,sp_list_join3$specificEpithet,sep=" ")

# fill our_accepted_name column based on speciesKey
sp_list_join3$our_accepted_name <- NA
  orig <- sp_list_join3[which(sp_list_join3$orig_list == "Y"),]; nrow(orig) #23
    orig$our_accepted_name <- orig$sp_full_name
  new <- sp_list_join3[which(is.na(sp_list_join3$orig_list)),]; nrow(new) #188
    for(i in 1:nrow(new)){
      for(j in 1:nrow(orig)){
        match <- grepl(pattern = new$speciesKey[i], x = orig$speciesKey[j], fixed = T)
          if(match == TRUE){
            new$our_accepted_name[i] <- orig$sp_full_name[j]
            new$sp_of_concern[i] <- orig$sp_of_concern[j]
            new$US_STATES_AND_S_RANKS[i] <- orig$US_STATES_AND_S_RANKS[j]
          }
        }
      }
  sp_list_join4 <- rbind(orig,new)

# write final file
write.csv(sp_list_join4, "IMLS_sp_list_joined.csv")







# an alternate way to do first part of step 3; shows the "×" symbol in front of hybrids, while the first method does not
# create sp_full_name column based on scientificName column
#infra <-  dplyr::filter(gbif_raw, grepl((" subsp. "),scientificName,fixed=T) |
#                                  grepl((" var. "),scientificName,fixed=T) |
#                                  grepl((" f. "),scientificName,fixed=T)); nrow(infra) # locate records for infra taxa
# separate scientificName into first four elements to cut off the author at end
#  name <- infra %>% separate("scientificName",c("one","two","three","four"),sep=" ",fill="right",extra="warn")
#  infra$sp_full_name <- paste(name$one,name$two,name$three,name$four,sep=" ") # join names back together
#the_rest <- anti_join(gbif_raw,infra,by="taxonKey"); nrow(the_rest) # separate and join first two parts of scientificName for non-infra taxa
#  name <- the_rest %>% separate("scientificName", c("one","two"),sep=" ", fill="right", extra="warn")
#  the_rest$sp_full_name <- paste(name$one,name$two,sep=" ")
#gbif_raw <- rbind(infra,the_rest) # bind infra and non-infra taxa back together
#  sort(unique(gbif_raw$sp_full_name))
