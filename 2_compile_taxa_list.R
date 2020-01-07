## WORKING SCRIPT; just pushing to github for tracking purposes
  # end has code bits that likely don't belong here;
  # pasted there for now

### Author: Emily Beckman  ###  Date: 10/22/19

### DESCRIPTION:
  # Matches target taxa to GBIF taxonomic backbone (adds speciesKey) and FIA
  # species codes; GBIF in situ occurrence data is also downloaded in the
  # process

### INPUTS:
  # taxa_list_inclu_syn.csv
    # two columns:
      # 1. taxon_name_acc (accepted name)
      # 2. taxon_name (synonym)

### OUTPUTS:
  # taxa_list_joine.csv
  # gbif_raw.csv


#################
### LIBRARIES ###
#################

library(rgbif)
library(plyr)
library(dplyr)
library(tidyr)


#################
### FUNCTIONS ###
#################


##############
### SCRIPT ###
##############

setwd("./Desktop")

##################################
# 1. Load taxa list with synonyms
##################################

# read in taxa list
taxa_list <- read.csv("target_taxa_inclu_syn.csv", header = T,
  na.strings=c("","NA"), colClasses="character"); nrow(taxa_list)
taxa_names <- taxa_list[,2]

########################
# 2. Download GBIF data
########################

# download data from gbif
keys_raw <- unique(sapply(taxa_names, function(x)
  name_backbone(name=x)$speciesKey, USE.NAMES=F))
keys <- paste("taxonKey = ",keys_raw[1],sep="")
  for(i in 2:length(keys_raw)){
    if(!is.null(keys_raw[[i]])){
      keys <- paste(keys,keys_raw[[i]],sep=",")
    }
  }; keys
  # for some reason the second "basisOfRecord" line below does not work if
  # broken up by tabs
gbif_download <- occ_download(keys,
                              "basisOfRecord = PRESERVED_SPECIMEN,HUMAN_OBSERVATION,OBSERVATION,UNKNOWN,MACHINE_OBSERVATION,MATERIAL_SAMPLE,LITERATURE",
                              "hasCoordinate = TRUE",
                              user="ebeckman",pwd="Quercus51",
                              email="ebeckman@mortonarb.org")
# !! WAIT FOR THIS DOWNLOAD TO COMPLETE BEFORE RUNNING NEXT LINE
  # can check download status here: https://www.gbif.org/user/download
# read in data just downloaded
download_get <- occ_download_get(gbif_download[1], overwrite = TRUE)
gbif_raw <- occ_download_import(as.download(download_get[1]))
  nrow(gbif_raw)

########################################
# 3. Join GBIF taxon codes to taxa list
########################################

# create GBIF taxon codes table to match with taxa list
  # create taxon_name_acc column of accepted GBIF names
gbif <- gbif_raw %>%
  mutate(taxon_full_name=ifelse(taxonRank=="VARIETY",
                    paste(genus,specificEpithet,"var.",infraspecificEpithet),
        (taxon_full_name=ifelse(taxonRank=="FORM",
                    paste(genus,specificEpithet,"form.",infraspecificEpithet),
        (taxon_full_name=ifelse(taxonRank=="SUBSPECIES",
                    paste(genus,specificEpithet,"subsp.",infraspecificEpithet),
                    paste(genus,specificEpithet)))))))
sort(unique(gbif$taxon_full_name))
  # keep only the pertinent columns
gbif_short <- gbif %>% distinct(taxon_full_name,scientificName,
                                acceptedScientificName,taxonKey,
                                acceptedTaxonKey,taxonomicStatus,order,
                                family,orderKey,familyKey,genusKey,
                                speciesKey)
                                #,genus,specificEpithet,infraspecificEpithet
# join gbif data to taxa list
  # replace forma symbol so it is consistent
gbif_short$taxon_full_name <- gsub("form.","f.",gbif_short$taxon_full_name,
  fixed=T)
gbif_short <- gbif_short %>% separate("scientificName", c("genus2","species2",
  "infra_rank2","infra_name2"),sep=" ", fill="right", extra="warn", remove=F)
# search for matches to infraspecific key words
matching <- c(grep(" var. ",gbif_short$scientificName,fixed=T),
              grep(" f. ",gbif_short$scientificName,fixed=T),
              grep(" subsp. ",gbif_short$scientificName,fixed=T))
gbif_short$taxon_full_name2 <- NA
gbif_short$taxon_full_name2[matching] <- paste(gbif_short$genus2[matching],
  gbif_short$species2[matching],gbif_short$infra_rank2[matching],
  gbif_short$infra_name2[matching])
gbif_short$taxon_full_name2[-matching] <- paste(gbif_short$genus2[-matching],
  gbif_short$species2[-matching])

  unique(gbif_short$taxon_full_name2) # check results

for(i in 1:nrow(gbif_short)){
  if(!identical(gbif_short$taxon_full_name2[i],gbif_short$taxon_full_name_orig[i])){

  }
}

taxa_list_join <- join(taxa_list,gbif_short,by="taxon_full_name",type="full")
gbif_short$taxon_full_name_orig <- gbif_short$taxon_full_name
gbif_short$taxon_full_name <- gbif_short$taxon_full_name2
taxa_list_join <- join(taxa_list,gbif_short,by="taxon_full_name",type="full")

#########################################
# 4. Join FIA species codes to taxa list
#########################################

# join taxa list to fia tree list
fia_list <- read.csv("FIA_speciesList_2017.csv", header = T,
  na.strings=c("","NA"), colClasses="character")
fia_keep <- fia_list[,c(1,2,8)]
taxa_list_join <- join(taxa_list_join,fia_keep,type="left",match="first")

####################################
# 5. Create other necessary columns
####################################

# create columns for each part of species name
taxa_list_join <- taxa_list_join %>% separate(taxon_full_name,
  c("genus","species","infra_rank","infra_name")," ",remove=F)
taxa_list_join$genus_species <- paste(taxa_list_join$genus,
  taxa_list_join$species,sep=" ")

# fill important columns based on speciesKey
#taxa_list_join$matched_with <- NA
orig <- taxa_list_join[which(!is.na(taxa_list_join$taxon_name_acc)),]
  nrow(orig) #25
new <- taxa_list_join[which(is.na(taxa_list_join$taxon_name_acc)),]
  nrow(new) #17
for(i in 1:nrow(new)){
  for(j in 1:nrow(orig)){
    if(grepl(pattern=new$speciesKey[i], x=orig$speciesKey[j], fixed=T)){
      new$taxon_name_acc[i] <- orig$taxon_full_name[j]
    }
  }
}
taxa_list_join2 <- rbind(orig,new)

write.csv(taxa_list_join2, "taxa_list_joined2.csv")





# remove infrataxa rows where the same synonym is applied to a the infrataxa and the species
  # create genus_species columns from accepted name and synonym name
unique_names_join <- unique_names_join %>% separate("taxon_name_acc", c("acc_genus","acc_species"),sep=" ", fill="right", remove="false")
unique_names_join$acc_genus_species <- paste(unique_names_join$acc_genus,unique_names_join$acc_species)
unique_names_join <- unique_names_join %>% separate("syn_name", c("syn_genus","syn_species"),sep=" ", fill="right", remove="false")
unique_names_join$syn_genus_species <- paste(unique_names_join$syn_genus,unique_names_join$syn_species)
  # add columns stating if syn_genus_species is a duplicate
#unique_names_join <- setorder(unique_names_join,"syn_genus_species") # order rows by genus_species
unique_names_join$syn_dup1 <- duplicated(unique_names_join$syn_genus_species) # check beginning to end
unique_names_join$syn_dup2 <- duplicated(unique_names_join$syn_genus_species,fromLast=T) # check end to beginning
  # add columns stating if acc_genus_species is a duplicate
#unique_names_join <- setorder(unique_names_join,"syn_genus_species") # order rows by genus_species
unique_names_join$acc_dup1 <- duplicated(unique_names_join$acc_genus_species) # check beginning to end
unique_names_join$acc_dup2 <- duplicated(unique_names_join$acc_genus_species,fromLast=T) # check end to beginning
  # add columns stating if accepted name is infraspecific
unique_names_join$acc_infra <- grepl(" f. | subsp. | var. ",unique_names_join$taxon_name_acc)
  # remove infrataxa rows where the same synonym is applied to a the infrataxa and the species
unique_names_join2 <- unique_names_join[(unique_names_join$syn_dup==T | unique_names_join$syn_dup2==T) &
                      (unique_names_join$acc_dup==T | unique_names_join$acc_dup2==T) &
                      unique_names_join$acc_infra==T,]


# if name is infra_rank and genus_species is not a duplicate, replace genus_species with syn_name
for(i in 1:nrow(unique_names_join)){
  if(unique_names_join$dup[i] == "FALSE" & unique_names_join$dup2[i] == "FALSE" & !is.na(unique_names_join$infra_rank[i])){
    unique_names_join$genus_species[i] <- unique_names_join$syn_name[i]
  }
}

# make sure matching genus_species names correspond with the same taxon_name_acc
unique_names_join$equal <- NA
unique_names_join2 <- unique_names_join %>% group_by(genus_species) %>% mutate(equal = replace(equal, n_distinct(taxon_name_acc)>1 & any(taxon_name_acc != genus_species), syn_name))

remove <- vector()
for(i in 1:nrow(unique_names_join2)){
  if(unique_names_join2$taxon_name_acc[i] == unique_names_join2$genus_species[i]){
    unique_names_join2$equal[i] <- NA
  }
  if(unique_names_join2$genus_species[i] %in% taxa_names & is.na(unique_names_join2$infra_rank[i]) & is.na(unique_names_join2$orig_list[i])){
    remove <- c(remove,i)
  }
  if(!is.na(unique_names_join2$infra_rank[i]) & !is.na(unique_names_join2$equal[i])){
    unique_names_join2$genus_species[i] <- unique_names_join2$equal[i]
    unique_names_join2$equal[i] <- NA
  }
}
unique_names_join2 <- unique_names_join2[-remove,]

# add column stating if genus_species duplicate
unique_names_join3 <- setorder(unique_names_join2,"genus_species") # order rows by genus_species
dup3 <- duplicated(unique_names_join3$equal); unique_names_join3$dup <- dup3
  tail(unique_names_join3)
dup4 <- duplicated(unique_names_join3$equal,fromLast=T); unique_names_join3$dup2 <- dup4
  tail(unique_names_join3)
# if genus_species is not a duplicate, replace genus_species with syn_name
for(i in 1:nrow(unique_names_join3)){
  if(unique_names_join3$dup[i] == "FALSE" & unique_names_join3$dup2[i] == "FALSE"){
    unique_names_join3$equal[i] <- NA
  }
}

# write CSV file of unique names
write.csv(unique_names_join3,"taxa_list_joined_8_22.csv")

##### DO MANUALLY: #####
# Sort by "equal" column
# Look at groups of names with "equal" != "NA" and matching "genus_species"
# If "ref" columns are equal, remove all rows in group
# If "ref" columns are not equal, keep the one row with highest number of unique "ref"
# Remove "dup", "dup2", and "equal" columns
# Highlight duplicate values in genus_species column
# If any highlighted values are infraspecific, check them out and likely remove

########################
# 2. Download GBIF data
########################

# read in taxa list
taxa_list_acc <- read.csv("taxa_list_new.csv", header = T, na.strings=c("","NA"), colClasses="character") # read from local
taxa_names <- taxa_list_acc[,1] # create list of target taxa names

# create list of GBIF species keys for all taxa in target list
keys1 <- sapply(taxa_names, function(x) name_backbone(name=x)$speciesKey, simplify = "array")
  keys1 # stop here and check to see if any species came back "NULL"; if they do, check GBIF and copy speciesKey (at end of URL) to command below
    keys <- paste("taxonKey = ",keys1[1],sep="") # ,",7777998"
        # Q. sadleriana is 2880444 ; issue with GBIF backbone so not found automatically
        # Malus ×heterophylla Spach	is 3001444
        # Malus heterophylla Sumn. is 7777998
  for(i in 2:length(keys1)){
    keys <- paste(keys,keys1[i],sep=",")
  }; keys

setwd("./gbif_read_in")

# download data (Darwin Core Archive format) from GBIF by species keys
gbif_download <- occ_download(keys,
                     "basisOfRecord = PRESERVED_SPECIMEN,HUMAN_OBSERVATION,FOSSIL_SPECIMEN,OBSERVATION,UNKNOWN,MACHINE_OBSERVATION,MATERIAL_SAMPLE,LITERATURE",
                     user="ebeckman",pwd="Quercus51",email="ebeckman@mortonarb.org")
gbif_download # IMLS Download key: 0013119-190621201848488
              # GA2 Download key: 0013195-190621201848488

# WAIT FOR THIS DOWNLOAD TO COMPLETE BEFORE RUNNING NEXT LINE
# you can check download status here: https://www.gbif.org/user/download
# it may take a while (up to 3 hours) if you have a large species list

# !!! PASTE "Download key" FROM ABOVE as first argument
occ_download_get(key="0013195-190621201848488", overwrite=TRUE) # download GBIF data to current working directory; Download file size: 351.37 MB ; 69.41 MB
unzip("0013195-190621201848488.zip") # unzip file

# read in data just downloaded
gbif_raw <- fread("occurrence.txt",quote=""); sort(unique(gbif_raw$taxonKey))

# write data to be used in later script
setwd("./..")
write.csv(gbif_raw, "gbif_raw.csv")

# this used to work to import GBIF data directly to R but I updated packages & R version, and something is off now:
  # gbif_raw <- occ_download_get("0003346-190621201848488", overwrite = TRUE) %>% occ_download_import()

#########################################
# 3. Add GBIF taxon codes to species list
#########################################

# create taxa_full_name column based on scientificName column
# identify records with more than just genus and species (i.e., subsp., var., or f.)
infra <-  dplyr::filter(gbif_raw, grepl((" subsp. "),scientificName,fixed=T) |
                                  grepl((" var. "),scientificName,fixed=T) |
                                  grepl((" f. "),scientificName,fixed=T)); nrow(infra) # locate records for infra taxa
# separate scientificName into first four elements to cut off the author at end
  name <- infra %>% separate("scientificName",c("one","two","three","four"),sep=" ",fill="right",extra="warn")
  infra$syn_name <- paste(name$one,name$two,name$three,name$four,sep=" ") # join names back together
the_rest <- anti_join(gbif_raw,infra,by="taxonKey"); nrow(the_rest) # separate and join first two parts of scientificName for non-infra taxa
  name <- the_rest %>% separate("scientificName", c("one","two"),sep=" ", fill="right", extra="warn")
  the_rest$syn_name <- paste(name$one,name$two,sep=" ")
gbif_raw <- rbind(infra,the_rest) # bind infra and non-infra taxa back together
  #sort(unique(gbif_raw$taxa_full_name))

# keep only the pertinent columns
gbif_join <- gbif_raw %>% distinct(syn_name,speciesKey,taxonomicStatus,acceptedScientificName)
# order to place accepted species names first
gbif_join <- gbif_join[order(factor(gbif_join$taxonomicStatus,levels=c("ACCEPTED","SYNONYM","DOUBTFUL"))),]

# join gbif codes/species names to species list
taxa_list_acc <- read.csv("species_list_joined_8_22_edited.csv", header = T, na.strings=c("","NA"), colClasses="character")
taxa_list_join <- join(taxa_list_acc,gbif_join,by="syn_name",type="left",match="first"); nrow(taxa_list_join)
  taxa_list_join[which(is.na(taxa_list_join$speciesKey)),] # these are species which have no spatial points in GBIF or are synonyms in GBIF

##########################
# 4. Add FIA species codes
##########################

# join to fia tree list
#drive_download("FIA_speciesList_2017.csv",overwrite=T) # download from Google Drive
fia_list <- read.csv("FIA_speciesList_2017.csv", header = T, na.strings=c("","NA"), colClasses="character") # read from local
  fia_codes <- fia_list[,c(1,8)] # keep only pertinent columns
taxa_list_join2 <- join(taxa_list_join,fia_codes,by="syn_name",type="left",match="first")

# write final file
write.csv(taxa_list_join2, "species_list_joined_8_23.csv")


child_itis <- itis_hierarchy(unique(names_itis_df$acc_tsn), what = "down")
test <- children(x=sp_names,db="itis")
,"col","ncbi,"))
child_col <- col_children(name = sp_names)

# create list of GBIF species keys for all species in target list
keys1 <- sapply(sp_names, function(x) name_backbone(name=x)$speciesKey, simplify = "array")
  keys1 # stop here and check to see if any species came back "NULL"; if they do, check GBIF and copy speciesKey (at end of URL) to command below
    keys <- paste("taxonKey = ",keys1[1],",2880444",",3001444",",7777998",sep="")
        # Q. sadleriana is 2880444 ; issue with GBIF backbone so not found automatically
        # Malus ×heterophylla Spach	is 3001444
        # Malus heterophylla Sumn. is 7777998
  for(i in 2:length(keys1)){
    keys <- paste(keys,keys1[i],sep=",")
  }; keys

########################
# 2. Download GBIF data
########################

setwd("./gbif_read_in")

# download data (Darwin Core Archive format) from GBIF by species keys
gbif_download <- occ_download(keys,
                     "basisOfRecord = PRESERVED_SPECIMEN,HUMAN_OBSERVATION,FOSSIL_SPECIMEN,OBSERVATION,UNKNOWN,MACHINE_OBSERVATION,MATERIAL_SAMPLE,LITERATURE",
                     user="ebeckman",pwd="Quercus51",email="ebeckman@mortonarb.org")
gbif_download # IMLS Download key: 0013119-190621201848488
              # GA2 Download key: 0013195-190621201848488

# WAIT FOR THIS DOWNLOAD TO COMPLETE BEFORE RUNNING NEXT LINE
# you can check download status here: https://www.gbif.org/user/download
# it may take a while (up to 3 hours) if you have a large species list

# !!! PASTE "Download key" FROM ABOVE as first argument
occ_download_get(key="0013195-190621201848488", overwrite=TRUE) # download GBIF data to current working directory; Download file size: 351.37 MB ; 69.41 MB
unzip("0013195-190621201848488.zip") # unzip file

# read in data just downloaded
gbif_raw <- fread("occurrence.txt",quote=""); sort(unique(gbif_raw$taxonKey))

# write data to be used in later script
setwd("./..")
write.csv(gbif_raw, "gbif_raw.csv")

# this used to work to import GBIF data directly to R but I updated packages & R version, and something is off now:
  # gbif_raw <- occ_download_get("0003346-190621201848488", overwrite = TRUE) %>% occ_download_import()

#########################################
# 3. Add GBIF taxon codes to species list
#########################################

# create sp_full_name column based on scientificName column
# identify records with more than just genus and species (i.e., subsp., var., or f.)
infra <-  dplyr::filter(gbif_raw, grepl((" subsp. "),scientificName,fixed=T) |
                                  grepl((" var. "),scientificName,fixed=T) |
                                  grepl((" f. "),scientificName,fixed=T)); nrow(infra) # locate records for infra taxa
# separate scientificName into first four elements to cut off the author at end
  name <- infra %>% separate("scientificName",c("one","two","three","four"),sep=" ",fill="right",extra="warn")
  infra$sp_full_name <- paste(name$one,name$two,name$three,name$four,sep=" ") # join names back together
the_rest <- anti_join(gbif_raw,infra,by="taxonKey"); nrow(the_rest) # separate and join first two parts of scientificName for non-infra taxa
  name <- the_rest %>% separate("scientificName", c("one","two"),sep=" ", fill="right", extra="warn")
  the_rest$sp_full_name <- paste(name$one,name$two,sep=" ")
gbif_raw <- rbind(infra,the_rest) # bind infra and non-infra taxa back together
  #sort(unique(gbif_raw$sp_full_name))

# keep only the pertinent columns
gbif_join <- gbif_raw %>% distinct(sp_full_name,speciesKey,taxonKey,scientificName,acceptedScientificName,
                                   acceptedTaxonKey,taxonomicStatus,order,family)

# join gbif codes/species names to species list
sp_list_join <- join(sp_list_acc,gbif_join,by=c("sp_full_name","taxonomicStatus"),type="full"); nrow(sp_list_join)
  sp_list_join[which(is.na(sp_list_join$speciesKey)),] # these are species which have no spatial points in GBIF or are synonyms in GBIF

##########################
# 4. Add FIA species codes
##########################

# join to fia tree list
drive_download("FIA_speciesList_2017.csv",overwrite=T) # download from Google Drive
fia_list <- read.csv("FIA_speciesList_2017.csv", header = T, na.strings=c("","NA"), colClasses="character") # read from local
  fia_codes <- fia_list[,c(1,8)] # keep only pertinent columns
sp_list_join2 <- join(sp_list_join,fia_codes,by="sp_full_name",type="left",match="first")

##################################
# 5. Final touches to species list
##################################

# create columns for each part of species names
sp_list_join3 <- sp_list_join2 %>% separate(sp_full_name, c("genus","specificEpithet","infraspecificEpithet1",
                                                            "infraspecificEpithet2")," ",remove=F)

# fill our_accepted_name column based on speciesKey
sp_list_join3$our_accepted_name <- NA
  orig <- sp_list_join3[which(sp_list_join3$orig_list == "Y"),]; nrow(orig)
    orig$our_accepted_name <- orig$sp_full_name
  new <- sp_list_join3[which(is.na(sp_list_join3$orig_list)),]; nrow(new)
    for(i in 1:nrow(new)){
      for(j in 1:nrow(orig)){
        match <- grepl(pattern = new$speciesKey[i], x = orig$speciesKey[j], fixed = T)
          if(match == TRUE){
            new$our_accepted_name[i] <- orig$sp_full_name[j]
          }
        }
      }
  sp_list_join4 <- rbind(orig,new)
  sp_list_join4 <- sp_list_join4[-which(is.na(sp_list_join4$our_accepted_name)),] # remove rows with no our_accepted_name
    nrow(sp_list_join4)

# order to place accepted species names first
sp_list_join4 <- sp_list_join4[order(factor(sp_list_join4$taxonomicStatus,levels=c("ACCEPTED","SYNONYM","DOUBTFUL"))),]
# remove dublicates by scientificName
sp_list_join5 <- sp_list_join4 %>% distinct(sp_full_name, .keep_all=T); nrow(sp_list_join5)

# see species with no GBIF occurrence points
sp_list_join5[which(is.na(sp_list_join5$speciesKey)),]

# write final file
write.csv(sp_list_join5, "species_list_joined.csv")


# an alternate way to do first part of step 3; shows the "×" symbol in front of hybrids, while the first method does not
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
