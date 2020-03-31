### Author: Emily Beckman  ###  Date: 3/6/2020                                 |

### DESCRIPTION:
  # This script

### SCRIPT OUTLINE:
  # 1.

### INPUTS:
  #
    # columns:
      # 1.

### OUTPUTS:
  ##


#################
### LIBRARIES ###
#################

library(plyr)
library(tidyverse) #ggplot2,dplyr,tidyr,readr,purrr,tibble,stringr,forcats
library(taxize)
library(batchtools)
library(textclean)
library(data.table)

#################
### FUNCTIONS ###
#################



##############
### SCRIPT ###
##############

setwd("./Desktop")

###############################################################################
# 1. Load data
###############################################################################

# read in lists
h_wallace <- read.csv("2015_helm_wallace.csv", header = T,
  na.strings=c("","NA"),colClasses="character")
    names(h_wallace); nrow(h_wallace) #618
notes1 <- read.csv("2017_notes.csv", header = T, na.strings=c(""," ","NA"),
  colClasses="character")
    names(notes1); nrow(notes1) #3507
notes2 <- read.csv("2019_notes.csv", header = T, na.strings=c(""," ","NA"),
  colClasses="character")
    names(notes2); nrow(notes2) #1060
sid <- read.csv("2019_SID.csv", header = T, na.strings=c(""," ","NA"),
  colClasses="character")
    names(sid); nrow(sid) #26711

# join two that should have been the same to start with
notes2 <- full_join(notes2,dormant)

###############################################################################
# 2. Combine duplicates
###############################################################################

# condense duplicates in SID list
unique_sid <- sid %>% group_by(taxon_name,SID_storbehav,SID_storcond,
    SID_taxon_name_notes,SID_family) %>%
  summarize(SID_refdesc_all = paste(SID_refdesc, collapse = '; ')) %>%
  ungroup()
nrow(sid); nrow(unique_sid) #24784
unique_sid <- as.data.frame(unique_sid)
# mark rows that are still duplicates and remove
unique_sid$dup <- c(duplicated(unique_sid$taxon_name, fromLast = TRUE)
  | duplicated(unique_sid$taxon_name))
duplicates <- unique_sid[which(unique_sid$dup == T),]; nrow(duplicates)
unique_sid2 <- setdiff(unique_sid,duplicates); nrow(unique_sid2)
# concatenate differing descriptions for same species and remove duplicates
duplicates <- duplicates %>% group_by(taxon_name,SID_family) %>%
  mutate(SID_storcond = paste(SID_storcond, collapse = ' | ')) %>%
  mutate(SID_refdesc_all = paste(SID_refdesc_all, collapse = ' | ')) %>%
  mutate(SID_storbehav = paste(SID_storbehav, collapse = ' | ')) %>%
  ungroup() %>% filter(!duplicated(taxon_name))
duplicates <- as.data.frame(duplicates)
# create one full dataset with all duplicates removed
unique_sid2 <- rbind(unique_sid2,duplicates); nrow(unique_sid2)
unique_sid2 <- unique_sid2 %>% select(-dup)

###############################################################################
# 3. Standardize characters
###############################################################################

# replace subspecies abbreviation
h_wallace$taxon_name <- gsub(" ssp. "," subsp. ",h_wallace$taxon_name,fixed=T)
notes1$taxon_name <- gsub(" ssp. "," subsp. ",notes1$taxon_name,fixed=T)
notes2$taxon_name <- gsub(" ssp. "," subsp. ",notes2$taxon_name,fixed=T)
unique_sid2$taxon_name <- gsub(" ssp. "," subsp. ",unique_sid2$taxon_name,fixed=T)

# replace species abbreviation
h_wallace$taxon_name <- gsub(" sp.$"," spp.",h_wallace$taxon_name)
notes1$taxon_name <- gsub(" sp.$"," spp.",notes1$taxon_name)
notes2$taxon_name <- gsub(" sp.$"," spp.",notes2$taxon_name)
unique_sid2$taxon_name <- gsub(" sp.$"," spp.",unique_sid2$taxon_name)

# replace incorrect "x" symbol
unique_sid2$taxon_name <- gsub("\\+","x",unique_sid2$taxon_name)
unique_sid2$taxon_name <- gsub(" X "," x ",unique_sid2$taxon_name)

# remove/add other characters
unique_sid2$taxon_name <- mgsub(unique_sid2$taxon_name,
    c("\'","\""),"")
notes2$taxon_name <- gsub("var.","var. ",notes2$taxon_name)

# capitalize correctly
unique_sid2$SID_family <- str_to_title(unique_sid2$SID_family)

# trim whitespace
h_wallace$taxon_name <- str_squish(h_wallace$taxon_name)
notes1$taxon_name <- str_squish(notes1$taxon_name)
notes2$taxon_name <- str_squish(notes2$taxon_name)
unique_sid2$taxon_name <- str_squish(unique_sid2$taxon_name)

###############################################################################
# 5. Join all lists
###############################################################################

# join everything together by taxon_name
all_df <- list(h_wallace,notes1,notes2,unique_sid2)
all_data <- Reduce(full_join,all_df)
  str(all_data)

# condense taxon_name_notes columns into one
all_data <- tidyr::unite(all_data,"taxon_name_notes",
  c("SHW_taxon_name_notes","R1_taxon_name_notes","R2_taxon_name_notes",
    "SID_taxon_name_notes"),sep="; ",remove=T)
all_data$taxon_name_notes <- mgsub(all_data$taxon_name_notes,
  c(" NA; ","NA; ","NA;",";NA","; NA"),"")
  unique(all_data$taxon_name_notes)

# condense family columns into one
all_data <- tidyr::unite(all_data,"family_orig",
  c("SHW_family","R1_family","R2_family","SID_family"),sep="; ",remove=T)
all_data$family_orig <- mgsub(all_data$family_orig,
  c(" NA; ","NA; ","NA;",";NA","; NA"),"")
  unique(all_data$family_orig)
  # remove duplicates in each cell
t <- setDT(all_data)[,list(family_orig =
  toString(sort(unique(strsplit(family_orig,';\\s*|\\s+')[[1]])))),
  by = taxon_name]
all_data <- all_data %>% select(-family_orig) %>% join(t)
unique(all_data$family_orig)

# reorder columns
all_data <- all_data %>% select(taxon_name,taxon_name_notes,family_orig,
  everything())
# remove any "NA" taxa
all_data <- all_data[which(!is.na(all_data$taxon_name)),]
glimpse(all_data) # Observations: 28,392
# write file
write.csv(all_data,"exceptional_species_match.csv")

###############################################################################
# 6. Create data frame of The Plant List (TPL) and International
#    Plant Names Index (IPNI) accepted names
###############################################################################

# download all accepted TPL names; takes a LONG time
#tpl_names <- tpl_get("tpl_all")
# read csv of accepted names
tpl_all <- read.csv("tpl_all.csv", header = T,
  na.strings=c("","NA"),colClasses="character")
  tpl_names <- tpl_all
# create taxon_name column
tpl_names <- unite(tpl_names, "taxon_name",
  c(Genus,Species.hybrid.marker,Species,Infraspecific.rank,
    Infraspecific.epithet), sep = " ", remove = F)
# get rid of NAs in concatenated taxon name
tpl_names$taxon_name <- mgsub(tpl_names$taxon_name,
  c("NA "," NA"," NA"," NA"," NA"), "")
# replace hybrid character
tpl_names$taxon_name <- gsub(" × "," x ",tpl_names$taxon_name,fixed=T)
tpl_names$taxon_name <- gsub("^× ","x ",tpl_names$taxon_name)
# trim whitespace
tpl_names$taxon_name <- str_squish(tpl_names$taxon_name)
# keep only relevant columns
tpl_names2 <- tpl_names %>% select(taxon_name,Family,ID,Genus,
  Species.hybrid.marker,Species,Infraspecific.rank,Infraspecific.epithet,
  Authorship,Confidence.level,Source,Taxonomic.status.in.TPL)
# standardize column names for joining later
glimpse(tpl_names2)
setnames(tpl_names2,
  old=c("Family","ID","Authorship","Confidence.level","Source",
        "Taxonomic.status.in.TPL","Genus","Species.hybrid.marker","Species",
        "Infraspecific.rank","Infraspecific.epithet"),
  new=c("family","taxonomic_source_id","taxonomic_author",
        "taxonomic_confidence","taxonomic_source","tpl_status","genus",
        "hybrid","species","infra_rank","infra_name"), skip_absent=T)
# add column stating source database
tpl_names2$taxonomic_database <- "TPL"
# remove duplicate names that are not accepted
tpl_names3 <- setdiff(tpl_names2,tpl_names2[
  which(tpl_names2$tpl_status != "Accepted" &
  (duplicated(tpl_names2$taxon_name, fromLast = TRUE) |
   duplicated(tpl_names2$taxon_name))),])
nrow(tpl_names2); nrow(tpl_names3)

# download accepted IPNI names for target families; takes a LONG time if lots
#all_data2 <- all_data2 %>% separate("taxon_name","genus",sep=" ",
#extra="warn",remove=F,fill="right")
#genera <- sort(unique(all_data2$genus))
#ipni_names <- data.frame()
#for(i in 1:length(genera)){
#  output_new <- ipni_search(genus=genera[i],output="extended")
#  ipni_names <- rbind.fill(ipni_names,output_new)
#  print(genera[i])
#}
#write.csv(ipni_names,"ipni_names_raw.csv")
# read csv of accepted names
ipni_all <- read.csv("ipni_names_raw.csv", header = T,
  na.strings=c("","NA"),colClasses="character")
  ipni_names <- ipni_all
# order by version number, for filtering later
  # replace value with NA in rows that don't have version number
ipni_names$version[which(!grepl("1",ipni_names$version))] <- NA
  # sort
ipni_names <- ipni_names %>% arrange(desc(version))
# keep only relevant columns
ipni_names2 <- ipni_names %>% select("full_name_without_authors","family","id",
  "genus","hybrid","species","infra_species","rank","authors")
# standardize column names for joining later
glimpse(ipni_names2)
setnames(ipni_names2,
  old=c("id","infra_species","rank","authors","full_name_without_authors"),
  new=c("taxonomic_source_id","infra_name","infra_rank","taxonomic_author",
        "taxon_name"), skip_absent=T)
# replace hybrid character
ipni_names2$taxon_name <- gsub(" × "," x ",ipni_names2$taxon_name,fixed=T)
ipni_names2$taxon_name <- gsub("^× ","x ",ipni_names2$taxon_name)
# add column stating source database
ipni_names2$taxonomic_database <- "IPNI"
# remove duplicates names, starting with older verions (this may be arbitrary..)
ipni_names3 <- distinct(ipni_names2,taxon_name,.keep_all=T)
nrow(ipni_names2); nrow(ipni_names3)

# stack all names from TPL and IPNI
taxonomic_names <- rbind.fill(tpl_names3,ipni_names3)
glimpse(taxonomic_names)
# keep unique values and create
#   "ref" col of all databases with duplicates,
#   "status" col of all acceptance statuses of duplicates,
#   "ref_id" col with id numbers from matching names, and
unique_names <- taxonomic_names %>% group_by(taxon_name) %>%
  summarize(
    taxonomic_database = paste(taxonomic_database,collapse = ';'),
    taxonomic_source_id = paste(taxonomic_source_id,collapse = ';'),
    family = paste(family,collapse = ';'),
    taxonomic_author = paste(taxonomic_author,collapse = ';'),
    taxonomic_status = paste(tpl_status,collapse = ';')
  ) %>%
  ungroup()
glimpse(unique_names)
# remove duplicates in ref column
unique_names$taxonomic_database <- gsub("TPL;TPL","TPL",
                                        unique_names$taxonomic_database)

###############################################################################
# 7. Query Integrated Taxonomic Information Service (ITIS) for names
#    with no match
###############################################################################

# create list of target taxa with taxonomic no match
  # select rows without taxonomic info match
test_match <- join(all_data,unique_names,type="left")
need_match <- test_match %>% filter(is.na(taxonomic_source_id))
nrow(need_match) #1960
  # unselect rows without species name
need_match <- need_match %>% filter(!grepl(" spp.",taxon_name,fixed=T))
nrow(need_match) #1727
  # unselect rows for multi-part hybrids (won't find match)
need_match <- need_match %>% filter(!(unlist(lapply(taxon_name, function(x)
                                           length(gregexpr(" ",x)[[1]]))) > 2 &
                                      grepl(" x ",taxon_name,fixed=T)))
nrow(need_match) #1699
  # list of names to match in ITIS
taxa_names <- need_match$taxon_name
taxa_names <- gsub(" x "," X ",taxa_names,fixed=T)
taxa_names <- gsub(" subsp. "," ssp. ",taxa_names,fixed=T)
# query ITIS; takes a LONG time if more than a few hundred names
itis_names <- itis_terms(taxa_names,what="scientific")
itis_names <- ldply(itis_names, data.frame) # list to data frame
itis_names2 <- itis_names[,c(2,4:6)]
# standardize column names for joining later
glimpse(itis_names2)
setnames(itis_names2,
  old=c("nameUsage","scientificName","tsn","author"),
  new=c("taxonomic_status","taxon_name","taxonomic_source_id",
        "taxonomic_author"), skip_absent=T)
# replace hybrid and subspecies characters
itis_names2$taxon_name <- gsub(" X "," x ",itis_names2$taxon_name,fixed=T)
itis_names2$taxon_name <- gsub(" ssp. "," subsp. ",itis_names2$taxon_name,fixed=T)
# add column stating source database
itis_names2$taxonomic_database <- "ITIS"
# remove duplicate names that are not accepted
itis_names3 <- setdiff(itis_names2,itis_names2[
  which(itis_names2$taxonomic_status != "accepted" &
  (duplicated(itis_names2$taxon_name, fromLast = TRUE) |
   duplicated(itis_names2$taxon_name))),])
nrow(itis_names2); nrow(itis_names3)

###############################################################################
# 8. Query Global Names Resolver (GNR) for remaining names with no match
###############################################################################

# create list of target taxa with taxonomic no match
  # select rows without taxonomic info match
need_match2 <- setdiff(need_match$taxon_name,itis_names3$taxon_name)
length(need_match2) #1375
  # list of names to match in TNRS
taxa_names <- need_match2
# query GNR; can take a while if more than a few hundred names
gnr_names <- gnr_resolve(taxa_names)
# keep only relevant columns
gnr_names2 <- gnr_names %>% select("matched_name","submitted_name",
  "data_source_title","score")
# standardize column names for joining later
glimpse(gnr_names2)
setnames(gnr_names2,
  old=c("matched_name","data_source_title","score","submitted_name"),
  new=c("taxon_name","taxonomic_source_id","taxonomic_status",
  "taxon_name_match"),skip_absent=T)
# add column stating source database
gnr_names2$taxonomic_database <- "GNR"
# remove duplicate names that are not accepted
gnr_names2 <- setorderv(gnr_names2,"taxonomic_status",order=-1)
gnr_names3 <- gnr_names2 %>% filter(!duplicated(taxon_name))
#gnr_names3 <- setdiff(gnr_names2,gnr_names2[
#  which(gnr_names2$taxonomic_confidence < 0.988 &
#  (duplicated(gnr_names2$taxon_name, fromLast = TRUE) |
#   duplicated(gnr_names2$taxon_name))),])
nrow(gnr_names2); nrow(gnr_names3)

###############################################################################
# 9. Join taxonomic data to target taxa list
###############################################################################

# stack all names from taxonomic sources (TPL,IPNI,ITIS,TNRS)
taxonomic_names <- rbind.fill(tpl_names3,ipni_names3,itis_names3,gnr_names3)
glimpse(taxonomic_names)
# keep unique values and create
#   "ref" col of all databases with duplicates,
#   "status" col of all acceptance statuses of duplicates,
#   "ref_id" col with id numbers from matching names, and
unique_names <- taxonomic_names %>% group_by(taxon_name) %>%
  summarize(
    taxonomic_database = paste(taxonomic_database,collapse = ';'),
    taxonomic_source_id = paste(taxonomic_source_id,collapse = ';'),
    family = paste(family,collapse = ';'),
    taxonomic_author = paste(taxonomic_author,collapse = ';'),
    taxonomic_status = paste(tpl_status,collapse = ';')
  ) %>%
  ungroup()
glimpse(unique_names)
# remove duplicates in ref column
table(unique_names$taxonomic_database)
unique_names$taxonomic_database <- gsub("TPL;TPL","TPL",
                                        unique_names$taxonomic_database)
# remove duplicates in family column
t <- setDT(unique_names)[,list(family =
  toString(sort(unique(strsplit(family,';\\s*|\\s+')[[1]])))),
  by = taxon_name]
unique_names <- unique_names %>% select(-family) %>% join(t)
unique_names$family <- mgsub(unique_names$family,c(" NA, ","NA, ",", NA"),"")
unique(unique_names$family)

# join to World Flora Online (WFO) family names
wfo <- read.csv("WFO_Backbone/classification.txt", header = T,
  na.strings=c(""," ","NA"),colClasses="character")
    names(sid); nrow(sid) #26711


# join to taxa list
all_data2 <- join(all_data,unique_names,type="left")
  sum(is.na(all_data2$taxonomic_source_id)) # number of names without match;758
# sort so names without match are at top before writing file
#all_data2 <- setorderv(all_data2,"taxonomic_source_id")
#write.csv(all_data2,"exceptional_species_match_taxonomy.csv")

###############################################################################
# 11. Standardize exceptional status column to compare among versions
###############################################################################

all_data2 <- all_data2 %>%
  mutate(exceptional_status1 = recode(SHW_Proposed_Exceptional_Status,
         "Exceptional / Non-exceptional" = "Exceptional?",
         "Exceptional (based on low seed production)" = "Non-exceptional?",
         "Unknown / Non-exceptional" = "Unknown",
         "Exceptional (due to extreme rarity)" = "Non-exceptional?",
         "Exceptional (based on low seed production) " = "Non-exceptional?",
         "Exceptional (based on low seed viability)" = "Non-exceptional?",
         "Exceptional (due to few seeds, at least from some populations)" =
            "Non-exceptional?",
         "Exceptional (but, it is closely related to Asimina, I don't believe there is any published or online information to the contrary.)" =
            "Exceptional?"
       )); table(all_data2$exceptional_status1)
all_data2 <- all_data2 %>%
  mutate(exceptional_status2 = recode(R1_Exceptional_Status,
         "Congener of Exceptional" = "Congener",
         "Possible Congener of Exceptional" = "Congener",
         "Possible Exceptional" = "Exceptional?",
         "Congener of Unknown" = "Congener"
       )); table(all_data2$exceptional_status2)
all_data2$exceptional_status3 <- all_data2$R2_Exceptional_Status
unique(all_data2$SID_storbehav)
all_data2 <- all_data2 %>%
  mutate(exceptional_status4 = recode(SID_storbehav,
         "Orthodox?" = "Non-exceptional?",
         "Recalcitrant" = "Exceptional",
         "Recalcitrant?" = "Exceptional",
         "Orthodox p" = "Non-exceptional?",
         "Orthodox" = "Non-exceptional",
         "Uncertain" = "Unknown",
         "Intermediate?" = "Exceptional",
         "Intermediate" = "Exceptional",
         "Intermediate`" = "Exceptional",
         "Recalcitrant?`" = "Exceptional",
         "Orthodox | Orthodox?" = "Non-exceptional?",
         "Orthodox | Orthodox | Orthodox p" = "Non-exceptional?",
         "Orthodox | Orthodox p" = "Non-exceptional?",
         "Recalcitrant | Recalcitrant?" = "Exceptional",
         .default = "Non-exceptional"
       )); table(all_data2$exceptional_status3)
# condense exceptionality status columns into one
all_data2 <- tidyr::unite(all_data2,"exceptional_status",
  c("exceptional_status1","exceptional_status2","exceptional_status3",
    "exceptional_status4"),
  sep=", ",remove=T)
# remove duplicates in exceptional_status column
t <- setDT(all_data2)[,list(exceptional_status =
  toString(sort(unique(strsplit(exceptional_status,',\\s*|\\s+')[[1]])))),
  by = taxon_name]
all_data2 <- all_data2 %>% select(-exceptional_status) %>% join(t)
all_data2$exceptional_status <- mgsub(all_data2$exceptional_status,
  c(" NA, ",", NA","NA, "," ","Congener,"),"")
table(all_data2$exceptional_status)
# remove rows without exceptional status (congeners)
all_data3 <- all_data2[which(all_data2$exceptional_status!="NA"),]
# mark rows that didn't match taxonomic data but have good reason
all_data3 <- all_data3 %>% mutate(taxonomic_database =
  ifelse(grepl(" spp.",taxon_name,fixed=T), "N/A: spp.", taxonomic_database))
all_data3 <- all_data3 %>% mutate(taxonomic_database =
  ifelse(grepl(" x ",taxon_name,fixed=T), "N/A: hybrid", taxonomic_database))

###############################################################################
# 12. Mark rows that need manual check
###############################################################################

# replace unusual characters
all_data3$SID_storcond <- stringi::stri_trans_general(
  all_data3$SID_storcond, "Latin-ASCII")
# create column listing needs
all_data3$to_do <- ""

# mark records with key SID words that may indicate exceptionality
all_data3 <- all_data3 %>% mutate(to_do =
  ifelse((grepl("short lived",SID_storcond,ignore.case=T) |
         grepl("short-lived",SID_storcond,ignore.case=T) |
         grepl("low viability",SID_storcond,ignore.case=T)) &
         (grepl("orthodox",SID_storbehav,ignore.case=T) |
         grepl("uncertain",SID_storbehav,ignore.case=T)) &
         grepl("Short-lived (SID, 2019)",R2_Additional_Justification,
          ignore.case=T),
  paste("Check SID description for exceptionality",to_do,sep=";"), to_do))
# mark records with no taxonomic database match
all_data3 <- all_data3 %>% mutate(to_do =
  ifelse(is.na(taxonomic_database), "Needs taxonomic source", to_do))
# mark records with more than one exceptional status
all_data3 <- all_data3 %>% mutate(to_do =
  ifelse(grepl(",",exceptional_status,fixed=T),
  paste("Needs final exceptionality decision",to_do,sep=";"), to_do))
# mark records with "?" in exceptional status
all_data3 <- all_data3 %>% mutate(to_do =
  ifelse(grepl("\\?",exceptional_status,fixed=T),
  paste("Choose exceptional category",to_do,sep=";"), to_do))
# mark records that may be exceptional but have no additional justification
all_data3 <- all_data3 %>% mutate(to_do =
  ifelse(grepl("Exceptional",exceptional_status,fixed=T) &
  is.na(R2_Additional_Justification),
  paste("May need additional justification",to_do,sep=";"), to_do))
# mark records that are exceptional but have no additional justification
all_data3 <- all_data3 %>% mutate(to_do =
  ifelse(exceptional_status=="Exceptional" &
  is.na(R2_Additional_Justification),
  paste("Needs additional justification",to_do,sep=";"), to_do))
# mark records with more than one family
all_data3 <- all_data3 %>% mutate(to_do =
  ifelse(grepl(",",family_orig,fixed=T),
  paste("Choose correct family",to_do,sep=";"), to_do))

# remove ending ";"
all_data3$to_do <- gsub(";$","",all_data3$to_do)
# see numbers of records marked for each category
table(all_data3$to_do)
# reorder columns
all_data3 <- all_data3 %>% select("to_do","taxon_name",
  "exceptional_status","SHW_Proposed_Exceptional_Status",
  "R1_Exceptional_Status","R2_Exceptional_Status","SID_storbehav",
  "R2_Additional_Justification",
  "taxon_name_notes","taxonomic_database","taxonomic_source_id",
  "taxonomic_author","taxonomic_status","family_orig",everything())
# sort by to-do column
all_data3 <- all_data3 %>% arrange(desc(to_do),taxon_name)
# write file
write.csv(all_data3,"exceptional_species_match_taxonomy_needs.csv")
