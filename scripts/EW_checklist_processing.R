# load packages
library(readxl)
library(tidyr)
library(stringr)
library(taxize)
library(data.table)
library(plyr)
library(dplyr)

# set working directory
main_dir <- "/Volumes/GoogleDrive/My Drive/BGCI-US EW species project 2020"
local_dir <- "./Desktop/work"

################################################################################
### Process/condense ThreatSearch export (one row for each species)
################################################################################

# read in data
threatsearch <- read_xlsx(file.path(main_dir,"raw_taxonomic_data","Extinct240820.xlsx"))
head(threatsearch)

# order scope column by priority (global first, then unknown and not global)
threatsearch$BGCI_Scope <- factor(threatsearch$BGCI_Scope,
  levels = c("Global","Global & Europe","Global & Mediterranean",
    "Global, Cape Verde","Global, Country endemic","Global, Ecuador",
    "Global, endemic","Global, Madagascar","Global, endemic?",
    "Not Global, suspected near endemic","Global?",
    "Unknown","Unknown, South Africa",
    "Not global","Not Global","Not Global, Europe","Not Global, Jordan",
    "Not Global, Luxembourg","Not Global, Malta","Not Global, Mediterranean",
    "Not Global, Regional"))

# remove double spaces, just in case there are typos
threatsearch$TaxonName <- str_squish(threatsearch$TaxonName)

# create standardized ConsAssCategory column
threatsearch$category_standardized <- str_to_upper(threatsearch$ConsAssCategory)
threatsearch <- threatsearch %>%
  mutate(category_standardized = recode(category_standardized,
    "EXTINCT" = "EX",
    "EXTINCT IN THE WILD" = "EW",
    "EXTINCT IN THE WILD (EW)" = "EW",
    "EW (EXTINCT IN THE WILD)" = "EW",
    "EXTINCT (EX)" = "EX",
    "EXTINCT/ENDANGERED" = "EX",
    "EXTINCT IN NATURAL POPULATIONS" = "EW",
    "EX" = "EX",
    "EW" = "EW",
    .missing = "EX?",
    .default = "EX?"))

# sort by assessment year then scope
# filter out non-species [var., ssp., or sp. (no epithet given)]
# group by species name then concatenate values in other columns
#   [results in one line per species]
t <- threatsearch %>%
  arrange(BGCI_Scope,desc(AssessmentYear)) %>%
  filter(!grepl("\\s.+\\s",TaxonName), #remove names with two spaces (not spp.)
         !grepl(" sp. ",TaxonName)) %>%
  group_by(TaxonName) %>%
  mutate(
    BGCI_Scope = paste(BGCI_Scope, collapse = '; '),
    category_standardized = paste(category_standardized, collapse = '; '),
    ConsAssCategory = paste(ConsAssCategory, collapse = '; '),
    AssessmentYear = paste(AssessmentYear, collapse = '; '),
    Reference = paste(Reference, collapse = '; '),
    BGCI_URL = paste(BGCI_URL, collapse = '; '),
    ID = paste(ID, collapse = '; ')) %>%
  ungroup() %>%
  distinct(TaxonName,.keep_all=T) %>%
  arrange(BGCI_Scope,category_standardized) %>%
  select(TaxonName,category_standardized,BGCI_Scope,AssessmentYear,Reference,
    BGCI_URL,ConsAssCategory,ID)

# check results
unique(t$TaxonName)
head(t)

# write file
write.csv(t, file.path(main_dir,"raw_taxonomic_data",
  "ThreatSearch_EX_08.24.20_condensed.csv"), row.names=FALSE)


################################################################################
### Get synonyms for checklist
### (WFO, Plantminer, IPNI, Tropicos?, ITIS?, POW?, TPL?)
################################################################################

# read in EW/EX checklist
checklist <- read_xlsx(file.path(main_dir,"raw_taxonomic_data",
  "EW Species List - 10.8.20.xlsx"))
glimpse(checklist)

###############
### World Flora Online (WFO)
###############

# read in WFO backbone
  # to download WFO taxonomic data, go to:
  # http://www.worldfloraonline.org/downloadData;jsessionid=94916E1F29B8ADFF5353032114B66D0E
  # scroll down to "Latest Static Version:" and click link to download
  # unzip the folder
wfo_all <- read.delim(file.path(main_dir,"raw_taxonomic_data","WFO_Backbone",
  "classification.txt"),colClasses="character")
# add accepted taxon info (original data only has "acceptedNameUsageID", not
#   actual taxon name, etc.)
lookup <- wfo_all %>%
  select(taxonID,scientificName,taxonRank,scientificNameAuthorship,family,
    genus) %>%
  dplyr::rename(acceptedNameUsageID = taxonID,
    acc.scientificName = scientificName,
    acc.taxonRank = taxonRank,
    acc.scientificNameAuthorship = scientificNameAuthorship,
    acc.family = family,
    acc.genus = genus)
wfo <- left_join(wfo_all,lookup)
# add accepted name data to "accepted" columns
wfo[is.na(wfo$acc.scientificName),c(20,22)] <-
  wfo[is.na(wfo$acc.scientificName),c(3,6)]
# keep only necessary columns, rename, and remove duplicates b/c of author diffs
wfo_standard <- wfo %>%
  select(scientificName,acc.scientificName,
    #scientificNameAuthorship,taxonRank,genus,acc.scientificNameAuthorship,
    family,taxonomicStatus) %>%
  dplyr::rename(
    taxon_name = scientificName,
    accepted_name = acc.scientificName,
    #authors.wfo = scientificNameAuthorship,
    #rank.wfo = taxonRank,
    taxonomic_status.wfo = taxonomicStatus,
    #accepted_authors.wfo = acc.scientificNameAuthorship,
    #genus.wfo = genus
    family.wfo = family) %>%
  arrange(taxonomic_status.wfo) %>%
  distinct(taxon_name,accepted_name,.keep_all=T)
wfo_standard$database.wfo <- "WFO"
str(wfo_standard)

###############
### World Checklist of Vascular Plants (WCVP)
###############

# first download backbone online at
#   http://sftp.kew.org/pub/data-repositories/WCVP/
# read in data
wcvp_all <- fread(file.path(main_dir,"raw_taxonomic_data",
  "KewNamesBackbone-oct-2019.txt"))
wcvp <- as.data.frame(wcvp_all)
# add accepted name data to "accepted" columns
wcvp[wcvp$accepted_name=="",c(11,12)] <-
  wcvp[wcvp$accepted_name=="",c(6,7)]
# keep only necessary columns, rename, and remove duplicates b/c of author diffs
wcvp_standard <- wcvp %>%
  select(taxon_name,accepted_name,family,taxonomic_status,
    #genus,authors,rank,accepted_authors,reviewed
    ) %>%
  dplyr::rename(
    #authors.wcvp = authors,
    #rank.wcvp = rank,
    taxonomic_status.wcvp = taxonomic_status,
    #accepted_authors.wcvp = accepted_authors,
    #genus.wcvp = genus
    family.wcvp = family) %>%
  arrange(taxonomic_status.wcvp) %>%
  distinct(taxon_name,accepted_name,.keep_all=T)
wcvp_standard$database.wcvp <- "WCVP"
str(wcvp_standard)

###############
### Other synonyms (2019 list and IUCN RL query)
###############

# read in synonymy data from 2019 and RL query
other_syns <- read_xlsx(file.path(main_dir,"raw_taxonomic_data",
  "2019List_and_RLQuery_synonymy.xlsx"))
glimpse(other_syns)

###############
### Bind all backbone/synonymy data together and match to checklist
###############

# join WFO and WCVP data
taxa_data <- full_join(wfo_standard,wcvp_standard)
taxa_data[] <- lapply(taxa_data, function(x) gsub("^$", NA, x))
# join to other synonyms
taxa_data <- full_join(taxa_data,other_syns)
# unite family, database, and taxonomic status columns
taxa_data <- tidyr::unite(taxa_data,"family",
  c("family.wfo","family.wcvp"),sep="; ",remove=T, na.rm=T)
taxa_data <- tidyr::unite(taxa_data,"database",
  c("database.wfo","database.wcvp","database"),sep="; ",remove=T, na.rm=T)
taxa_data <- tidyr::unite(taxa_data,"taxonomic_status",
  c("taxonomic_status.wfo","taxonomic_status.wcvp"),sep="; ",remove=T, na.rm=T)
str(taxa_data)

# get all taxon name matches to checklist
taxa_to_match <- unique(checklist$our_species_name)
taxa_in_backbone <- taxa_data[taxa_data$taxon_name %in% taxa_to_match,]
taxa_in_backbone$our_species_name <- taxa_in_backbone$taxon_name
nrow(taxa_in_backbone) #3705
# get accepted names matched to checklist and pull all taxa in backbone with
#   those accepted names
acc_to_match <- unique(taxa_in_backbone$accepted_name)
syn_in_backbone <- taxa_data[taxa_data$accepted_name %in% acc_to_match,]
  # join column listing our species name
our_names <- taxa_in_backbone[,c(2,6)]
syn_in_backbone <- left_join(syn_in_backbone,our_names)
nrow(syn_in_backbone) #31445
# join exact matches and synonyms together
all_matches <- rbind(taxa_in_backbone,syn_in_backbone)
# remove duplicates and arrange data so synonyms are together
all_matches <- all_matches %>%
  distinct(taxon_name,accepted_name,.keep_all = T) %>%
  arrange(our_species_name) %>%
  dplyr::select(our_species_name,database,taxon_name,accepted_name,
    taxonomic_status,family)
head(all_matches,n=20)
nrow(all_matches) #29949
# check to make sure all rows have our_species_name match
all_matches[is.na(all_matches$our_species_name),]

# get list of checklist species with no backbone match
checklist$taxon_name <- checklist$our_species_name
checklist_join <- left_join(checklist,taxa_data)
no_data <- checklist_join[is.na(checklist_join$accepted_name),]
nrow(no_data) #159

###############
### Check unmatched species for synonyms in
###   Integrated Taxonomic Information System (ITIS)
###############

taxa_names <- no_data$our_species_name
itis_syn <- synonyms(taxa_names, db="itis", accepted = F)
# !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS
# function to remove speices/taxa that did not have any synonyms and create data
#   frame of synonyms
synonyms.compiled <- function(syn_output){
  found <- NA
  for(i in 1:length(syn_output)){
    if(length(syn_output[[i]])>1){
      if(syn_output[[i]][1,3]!="no syns found"){
        found <- c(found,i)
        syn_output[[i]]$taxon_name <- rep(names(syn_output[i]),
                                          nrow(syn_output[[i]]))
      }
    }
  }
  found <- found[-1]
  syn_output_df <- Reduce(rbind.fill, syn_output[found])
  return(syn_output_df)
}
itis_syn_df <- synonyms.compiled(itis_syn)
str(itis_syn_df)
# keep only necessary columns, rename, and remove duplicates b/c of author diffs
itis_standard <- itis_syn_df %>%
  select(taxon_name,acc_name) %>%
  dplyr::rename(accepted_name = acc_name) %>%
  distinct(taxon_name,accepted_name,.keep_all=T)
itis_standard$database.itis <- "ITIS"
# replace characters in taxa names
itis_standard$taxon_name <- gsub(" X "," x ",itis_standard$taxon_name)
itis_standard$taxon_name <- gsub(" ssp. "," subsp. ",itis_standard$taxon_name)
# assign accepted name as taxon name if none
itis_standard[is.na(itis_standard$accepted_name),]$accepted_name <-
  itis_standard[is.na(itis_standard$accepted_name),]$taxon_name
str(itis_standard)

###############
### Bind everything together again and match to checklist
###############

# join backbone & syn data to new ITIS query
taxa_data2 <- full_join(taxa_data,itis_standard)
taxa_data2[] <- lapply(taxa_data2, function(x) gsub("^$", NA, x))
# unite database columns
taxa_data2 <- tidyr::unite(taxa_data2,"database",
  c("database","database.itis"),sep="; ",remove=T, na.rm=T)
str(taxa_data2)

# get all taxon name matches to checklist
taxa_in_backbone <- taxa_data2[taxa_data2$taxon_name %in% taxa_to_match,]
taxa_in_backbone$our_species_name <- taxa_in_backbone$taxon_name
nrow(taxa_in_backbone) #3720
# get accepted names matched to checklist and pull all taxa in backbone with
#   those accepted names
acc_to_match <- unique(taxa_in_backbone$accepted_name)
syn_in_backbone <- taxa_data[taxa_data$accepted_name %in% acc_to_match,]
  # join column listing our species name
our_names <- taxa_in_backbone[,c(2,6)]
syn_in_backbone <- left_join(syn_in_backbone,our_names)
nrow(syn_in_backbone) #31617
# join exact matches and synonyms together
all_matches <- rbind(taxa_in_backbone,syn_in_backbone)
# remove duplicates and arrange data so synonyms are together
all_matches <- all_matches %>%
  distinct(taxon_name,accepted_name,.keep_all = T) %>%
  arrange(our_species_name) %>%
  dplyr::select(our_species_name,database,taxon_name,accepted_name,
    taxonomic_status,family)
head(all_matches,n=20)
nrow(all_matches) #30010
# check to make sure all rows have our_species_name match
all_matches[is.na(all_matches$our_species_name),]
# write file
write.csv(all_matches,file.path(main_dir,"raw_taxonomic_data",
  "checklist_synonyms_2020.csv"),row.names=F)

# get list of checklist species with no backbone match
checklist$taxon_name <- checklist$our_species_name
checklist_join <- left_join(checklist,taxa_data2)
no_data <- checklist_join[is.na(checklist_join$accepted_name),]
nrow(no_data) #144
# arrange columns before writing file
no_data <- no_data %>%
  dplyr::select(our_species_name,database,taxon_name,accepted_name,
    taxonomic_status,family)
# write file
write.csv(no_data,file.path(main_dir,"raw_taxonomic_data",
  "checklist_no_taxonomic_match_2020.csv"),row.names=F)













################################################################################
### Other code bits for getting taxonomic info; not using now
################################################################################

###############
### Plantminer
###############

# query Plantminer using 'taxize' package function;
#   can take a while if more than a few hundred names
plantminer_match <- plantminer(checklist$species_name)
str(plantminer_match)
checklist_plantminer <- plantminer_match
# write file
write.csv(checklist_plantminer,file.path(main_dir,"raw_taxonomic_data",
  "Plantminer_match.csv"),row.names=F)

###############
### International Plant Name Index (IPNI)
###############

# download IPNI names for target genera using 'taxize' package function;
#   takes a LONG time if lots
#  # create vector of genera
#genera <- checklist %>%
#  separate("species_name","genus",sep=" ",extra="warn") %>%
#  select("genus") %>%
#  distinct(genus)
#genera <- as.array(genera[[1]])
#  # download ipni data
#ipni_match <- data.frame()
#for(i in 1:length(genera)){
#  output_new <- ipni_search(genus=genera[i],output="extended")
#  ipni_match <- rbind.fill(ipni_match,output_new)
#  print(genera[i])
#}
#  # write file
#write.csv(ipni_match,file.path(main_dir,"raw_taxonomic_data","ipni_names_raw.csv"),
#  row.names=F)
# read in data if already downloaded
ipni_match <- read.csv(file.path(main_dir,"raw_taxonomic_data","ipni_names_raw.csv"),
  header = T, na.strings = c("","NA"), colClasses="character")
# select rows that match checklist taxa
checklist_ipni <- ipni_match[ipni_match$full_name_without_family_and_authors %in% checklist$species_name,]
# keep only necessary columns
checklist_ipni <- checklist_ipni %>%
  select(family,genus,rank,full_name_without_family_and_authors,authors,
    basionym,basionym_author,original_basionym,original_replaced_synonym)
# write file
write.csv(checklist_ipni,file.path(main_dir,"raw_taxonomic_data",
  "IPNI_match.csv"),row.names=F)

###############
### Taxonomic Name Resolution Service (TNRS)
###############

# first download matches to your checklist online at
#   http://tnrs.iplantcollaborative.org/TNRSapp.html
# read in data
tnrs_match <- read.csv(file.path(main_dir,"raw_taxonomic_data","tnrs_match_raw.csv"),
  header = T, na.strings = c("","NA"), colClasses="character")

###############
### Kew’s Plants of the World (POW)
###############

## GET TAXONOMIC STATUS AND SYNONYMS
taxa_names <- checklist$species_name
pow_names <- data.frame()
pow_syn <- data.frame()
for(i in 1:length(taxa_names)){
  id <- get_pow(taxa_names[[i]])[1]
  if(!is.na(id)){
    output_new <- pow_lookup(id)
      acc <- data.frame(
        "taxon_name_match" = output_new$meta$name,
        "match_id" = output_new$meta$fqId,
        "acceptance" = output_new$meta$taxonomicStatus,
        "author" = output_new$meta$authors,
        "taxon_name_acc" = taxa_names[[i]]
      )
    not_acc <- data.frame(output_new$meta$accepted)
    if(length(not_acc)>0){
      acc <- data.frame(
        "taxon_name_match" = output_new$meta$accepted$name,
        "match_id" = output_new$meta$accepted$fqId,
        "acceptance" = output_new$meta$taxonomicStatus,
        "author" = output_new$meta$accepted$author,
        "taxon_name_acc" = taxa_names[[i]]
      )
    }
    syn <- data.frame(output_new$meta$synonyms)
    if(length(syn)>0){
      syn$taxon_name_acc <- taxa_names[[i]]
    }
    pow_names <- rbind.fill(pow_names,acc)
    pow_syn <- rbind.fill(pow_syn,syn)
  }
}

# !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS

# fix up acceptance df
  # add column stating which database it came from
pow_names$database <- "pow"
  # add column with authors
pow_names$match_name_with_authors <- paste(
  pow_names$taxon_name_match,pow_names$author)
  # remove duplicates except those matching legitimate names
pow_names_noDup <- pow_names
  # keep only necessary columns
pow_names_noDup <- pow_names_noDup[,c("taxon_name_acc","taxon_name_match",
  "match_id","database","acceptance","match_name_with_authors")]
pow_names_noDup$acceptance <- str_to_lower(pow_names_noDup$acceptance)
  # OPTIONAL, IF NOT LOOKING FOR CHILDREN: remove subsp., var., and f.
pow_names_noDup <- pow_names_noDup %>%
  filter(!grepl("subsp.",taxon_name_match,fixed=T) &
         !grepl("var.",taxon_name_match,fixed=T) &
         !grepl("f.",taxon_name_match,fixed=T))

# fix up synonyms df
  # add column stating which database it came from
pow_syn_df <- pow_syn
pow_syn_df$database <- "pow"
  # standardize column names for joining later
setnames(pow_syn_df,
  old = c("name","fqId","taxonomicStatus"),
  new = c("taxon_name_match","match_id","acceptance"))
  # add column with authors
pow_syn_df$match_name_with_authors <- paste(
  pow_syn_df$taxon_name_match,pow_syn_df$author)
  # remove records where taxa name and syn name are the same
pow_syn_df <- pow_syn_df[which(pow_syn_df$taxon_name_acc !=
  pow_syn_df$taxon_name_match),]
  # keep only necessary columns
pow_syn_df <- pow_syn_df[,c("taxon_name_acc","taxon_name_match",
  "match_id","database","acceptance","match_name_with_authors")]
pow_syn_df$acceptance <- str_to_lower(pow_syn_df$acceptance)

## BIND TOGETHER STATUS AND SYNONYMS

pow_all <- rbind.fill(pow_names_noDup,pow_syn_df)
head(pow_all)


###############
### Tropicos (from Missouri Botanical Garden)
###############

# if you haven't yet, set API key in your R environment and restart R
#taxize::use_tropicos() # get API
#usethis::edit_r_environ() # set API
# TROPICOS_KEY='________' # paste this in

## GET SYNONYMS

if(exists("tpkey")){
  tp_syn <- synonyms(species_names, db="tropicos", key=tpkey)
} else {
  tp_syn <- synonyms(species_names, db="tropicos")
}
rm(tpkey)

# !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS

# remove species/taxa that did not have any synonyms,
#   create data frame of synonyms,
#   and add column stating which database it came from
tp_syn_df <- synonyms.compiled(tp_syn,"tropicos")
colnames(tp_syn_df)
# standardize column names for joining later
setnames(tp_syn_df,
  old = c("taxon_name","nameid","scientificname",
          "scientificnamewithauthors"),
  new = c("taxon_name_acc","match_id","taxon_name_match",
          "match_name_with_authors"))
tp_syn_df$acceptance <- "synonym"
# replace characters in taxa names
tp_syn_df[] <- lapply(tp_syn_df, function(x) gsub(" × "," x ", x))
tp_syn_df[] <- lapply(tp_syn_df, function(x) gsub(" fo. "," f. ", x))
# keep only necessary columns
tp_syn_df <- tp_syn_df[,c("taxon_name_acc","taxon_name_match",
  "match_id","acceptance","match_name_with_authors","database")]
tp_syn_df$acceptance <- str_to_lower(tp_syn_df$acceptance)

## BIND TOGETHER STATUS AND SYNONYMS

tp_all <- rbind.fill(tp_names_noDup,tp_syn_df)
head(tp_all)

###############
### Integrated Taxonomic Information Service (ITIS)
###############

# query ITIS using 'taxize' package function;
#   takes a LONG time if more than a few hundred names
itis_match <- itis_terms(checklist$species_name,what="scientific")
itis_match <- ldply(itis_match, data.frame) # list to data frame
checklist_itis <- itis_match[,c(2,4:6)]

# keep only relevant columns
itis_names2 <- itis_names %>% dplyr::select("nameUsage",
  "scientificName")
# standardize column names for joining later
glimpse(itis_names2)
setnames(itis_names2,
  old=c("nameUsage","scientificName"),
  new=c("taxonomic_status","genus_species"), skip_absent=T)
# add column stating source database
itis_names2$taxonomic_database <- "ITIS"
# remove duplicate names that are not accepted
itis_names3 <- setorderv(itis_names2,"taxonomic_status",order=1)
itis_names3 <- itis_names2 %>% filter(!duplicated(genus_species))
nrow(itis_names2); nrow(itis_names3)

## GET SYNONYMS

itis_syn <- synonyms(taxa_names, db="itis", accepted = F)

# !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS

# remove species/taxa that did not have any synonyms,
#   create data frame of synonyms,
#   and add column stating which database it came from
itis_syn_df <- synonyms.compiled(itis_syn,"itis")
colnames(itis_syn_df)
# standardize column names for joining later
setnames(itis_syn_df,
  old = c("taxon_name","syn_name","syn_tsn","syn_author"),
  new = c("taxon_name_acc","taxon_name_match","match_id","author"))
# keep only necessary columns
itis_syn_df <- itis_syn_df[,c("taxon_name_acc","taxon_name_match","author",
  "match_id","database")]
itis_syn_df$acceptance <- "synonym"
# replace characters in taxa names
itis_syn_df[] <- lapply(itis_syn_df, function(x) gsub(" X "," x ", x))
itis_syn_df[] <- lapply(itis_syn_df, function(x) gsub(" ssp. "," subsp. ", x))
# add column with authors
itis_syn_df$match_name_with_authors <- paste(
  itis_syn_df$taxon_name_match,itis_syn_df$author)
# remove records where taxa name and syn name are the same
itis_syn_df <- itis_syn_df[which(itis_syn_df$taxon_name_acc !=
  itis_syn_df$taxon_name_match),]
# keep only necessary columns
itis_syn_df <- itis_syn_df[,c("taxon_name_acc","taxon_name_match",
  "match_id","acceptance","match_name_with_authors","database")]
itis_syn_df$acceptance <- str_to_lower(itis_syn_df$acceptance)

## BIND TOGETHER STATUS AND SYNONYMS

itis_all <- rbind.fill(itis_names_noDup,itis_syn_df)
head(itis_all)


##
### Global Names Resolver (GNR)
##

# query GNR using 'taxize' package function;
#   can take a while if more than a few hundred names
gnr_match <- gnr_resolve(checklist$species_name)
# remove duplicate names that are not accepted
gnr_names2 <- gnr_names %>%
  arrange(desc(score)) %>%
  filter(!duplicated(taxon_name))
#gnr_names3 <- setdiff(gnr_names2,gnr_names2[
#  which(gnr_names2$taxonomic_confidence < 0.988 &
#  (duplicated(gnr_names2$taxon_name, fromLast = TRUE) |
#   duplicated(gnr_names2$taxon_name))),])
nrow(gnr_names2); nrow(gnr_names3)

# standardize column names for joining later
glimpse(gnr_names2)
setnames(gnr_names2,
  old=c("matched_name","data_source_title","score","submitted_name"),
  new=c("taxon_name","taxonomic_source_id","taxonomic_status",
  "taxon_name_match"),skip_absent=T)
# add column stating source database
gnr_names2$taxonomic_database <- "GNR"

##
### The Plant List (TPL)
##

# download all accepted TPL names; takes a LONG time
  #tpl_names <- tpl_get("tpl_all")
# read csv of accepted names
tpl_all <- read.csv("taxonomic_data/tpl_all.csv", header = T,
  na.strings = c("","NA"),colClasses = "character")
  tpl_names <- tpl_all
# create taxon_name column
tpl_names <- unite(tpl_names, "taxon_name",
  c(Genus,Species.hybrid.marker,Species,Infraspecific.rank,
    Infraspecific.epithet), sep = " ", remove = F, na.rm=T)
# replace hybrid character
tpl_names$taxon_name <- gsub(" × "," x ",tpl_names$taxon_name,fixed=T)
tpl_names$taxon_name <- gsub("^× ","x ",tpl_names$taxon_name)
# trim whitespace
tpl_names$taxon_name <- str_squish(tpl_names$taxon_name)
# keep only relevant columns
tpl_names2 <- tpl_names %>% dplyr::select(taxon_name,Source,
  Taxonomic.status.in.TPL)
# standardize column names for joining later
glimpse(tpl_names2)
setnames(tpl_names2,
  old=c("Source","Taxonomic.status.in.TPL","taxon_name"),
  new=c("taxonomic_source","taxonomic_status","genus_species"),
  skip_absent=T)
# add column stating source database
tpl_names2$taxonomic_database <- "TPL"
# remove duplicate names that are not accepted
tpl_names3 <- setdiff(tpl_names2,tpl_names2[
  which(tpl_names2$taxonomic_status != "Accepted" &
  (duplicated(tpl_names2$genus_species, fromLast = TRUE) |
   duplicated(tpl_names2$genus_species))),])
nrow(tpl_names2); nrow(tpl_names3)

#   There is not an easy function for pulling synonyms from TPL :`(

tpl_names <- data.frame()
for(i in 1:length(families)){
  output_new <- tpl_get("files",family=families[i])
  output_new <- read.csv(paste("files/",families[i],".csv",sep=""), header = T,
    colClasses="character")
  tpl_names <- rbind.fill(tpl_names,output_new)
}
# standardize column names for joining later
setnames(tpl_names,
  old = c("ID","Taxonomic.status.in.TPL","Authorship"),
  new = c("match_id","acceptance","author"))
tpl_names$database <- "tpl"
# create concatenated taxon_name_acc col
tpl_names <- unite(tpl_names, "taxon_name_acc",
  c(Genus,Species.hybrid.marker,Species,Infraspecific.rank,
    Infraspecific.epithet), sep = " ", remove = F, na.rm = T)
# replace hybrid character
tpl_names$taxon_name_acc <- gsub(" × "," x ",
  tpl_names$taxon_name_acc,fixed=T)
# trim whitespace
tpl_names$taxon_name_acc <- str_squish(tpl_names$taxon_name_acc)
# fill other columns
tpl_names$taxon_name_match <- tpl_names$taxon_name_acc
tpl_names$match_name_with_authors <- paste(tpl_names$taxon_name_acc,
  tpl_names$author)
colnames(tpl_names)
# remove duplicates
tpl_names_noDup <- tpl_names
tpl_names_noDup$dup <- c(duplicated(tpl_names_noDup$taxon_name_acc,
  fromLast = TRUE) | duplicated(tpl_names_noDup$taxon_name_acc))
tpl_names_noDup <- setdiff(tpl_names_noDup,tpl_names_noDup[
  which(tpl_names_noDup$acceptance != "Accepted" & tpl_names_noDup$dup == T),])
# keep only necessary columns
tpl_names_noDup <- tpl_names_noDup[,c("taxon_name_acc","taxon_name_match",
  "match_id","database","acceptance","match_name_with_authors")]
tpl_names_noDup$acceptance <- str_to_lower(tpl_names_noDup$acceptance)
# join with taxa list and remove non-matches
tpl_all <- tpl_names_noDup %>% filter(tpl_names_noDup$taxon_name_acc %in%
  taxa_names)
head(tpl_all)
