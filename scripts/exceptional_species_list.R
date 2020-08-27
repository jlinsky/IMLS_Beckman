### Author: Emily Beckman  ###  Date: 3/6/2020

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

# rm(list=ls())
my.packages <- c('plyr', 'tidyverse', 'taxize', 'batchtools', 'textclean',
  'data.table', 'rebus')
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

#################
### FUNCTIONS ###
#################


##############
### SCRIPT ###
##############

setwd("./Desktop/exceptional_species_list")

###############################################################################
# 1. Load data
###############################################################################

# read in lists
## Sara Helm Wallace 2015
  h_wallace <- read.csv("read_in/SaraHelmWallace_vetted_5-19-20VP_6-16-20EB.csv",
    header = T, na.strings=c("","NA"), colClasses="character")
  # separate out duplicates in taxa column
  h_wallace <- separate_rows(h_wallace, taxa_names, sep = ";")
  setnames(h_wallace, old="taxa_names", new="taxon_name")
  h_wallace$dataset <- "Helm_Wallace_2015"
  # take a look
  names(h_wallace); nrow(h_wallace) #477
## Exceptional rows from 2017 version of list, that aren't in Sara Helm Wallace
  notes2017 <- read.csv("read_in/2017_notes_new.csv", header = T,
    na.strings=c(""," ","NA"), colClasses="character")
  notes2017$dataset <- "Exceptional_List_2017"
  # take a look
  names(notes2017); nrow(notes2017) #111
## 2019 version of Exceptional list, including dormancy notes from 2020
  notes2019 <- read.csv("read_in/2019_notes.csv", header = T,
    na.strings=c(""," ","NA"), colClasses="character")
  notes2019$dataset <- "Exceptional_List_2019"
  # take a look
  names(notes2019); nrow(notes2019) #1089
## Complete SID download from 2019
  sid <- read.csv("read_in/2019_SID.csv", header = T,
    na.strings=c(""," ","NA"), colClasses="character", fileEncoding="LATIN1")
  sid$dataset <- "SID_2019"
  # take a look
  names(sid); nrow(sid) #26711
## Chau et al. 2016 and 2019
  chau <- read.csv("read_in/Chau_2016_2019.csv", header = T,
    na.strings=c(""," ","NA"), colClasses="character")
  chau$dataset <- "Chau_2016_2019"
  # take a look
  names(chau); nrow(chau) #706
## IUCN Critically Endangered, Criterion D (<50 individuals), 2020 download
  iucn <- read.csv("read_in/IUCN_RedList_CR_D_downloaded_6-11-2020.csv",
    header = T, na.strings=c(""," ","NA"), colClasses="character")
  iucn$dataset <- "IUCN_CR_D_2020"
  # take a look
  names(iucn); nrow(iucn) #1126
## National Wetland Plant List (U.S. Army Corps of Engineers, 2018)
#   OBL (Obligate Wetland Plant)
  obl <- read.csv("read_in/National_OBL_2018.csv", header = T,
    na.strings=c(""," ","NA"), colClasses="character")
  obl$dataset <- "US_OBL_2018"
  # take a look
  names(obl); nrow(obl) #2233

#######
## Experimenting with SID_storcond word searching
#######

# split SID_storcond column by ";" and create new duplicate row for each
sid_unique <- separate_rows(sid, SID_storcond, sep = ";")
#sid_unique <- sid
nrow(sid) # 26711
nrow(sid_unique) # 32971

# remove double spaces
sid_unique$SID_storcond <- str_squish(sid_unique$SID_storcond)
# remove space before % sign and after dash; other small fixes
sid_unique$SID_storcond <- gsub(" %", "% ",sid_unique$SID_storcond)
sid_unique$SID_storcond <- gsub("- ", "-",sid_unique$SID_storcond)
sid_unique$SID_storcond <- gsub("afew", "a few",sid_unique$SID_storcond)
# replace unusual characters in degree celsius text
sid_unique$SID_storcond <- mgsub(sid_unique$SID_storcond,
  c("¡","¼"), "")
# make everything lowercase for easier matching
sid_unique$SID_storcond <- str_to_lower(sid_unique$SID_storcond)
# standardize MC and RH
sid_unique$SID_storcond <- mgsub(sid_unique$SID_storcond,
  c("m.c.","r.h."),c("mc","rh"))
# standardize plus/minus
sid_unique$SID_storcond <- gsub(" +/- ","±",sid_unique$SID_storcond)
# remove double spaces one more time
sid_unique$SID_storcond <- str_squish(sid_unique$SID_storcond)

# create subset for practice
sid_ex <- sid_unique#[sample(nrow(sid_unique), 300), ]

# create patterns
  ## TIME PERIOD
time <- or(
    # value and units
  (capture(
    zero_or_more(DGT) %R% optional(DOT) %R%
    or(one_or_more(DGT),"several","few")) %R%
  SPC %R% or("year","month","day","week")),
    # other key words
  "short periods",
  "rapid",
  "quickly"
)
  ## VIABILITY
    # helpers:
    # matches a percent or range of percents (to be used as helper in other
    #   match searches, to make more readable)
percent <-
  zero_or_more(DGT) %R% optional(DOT) %R% zero_or_more(DGT) %R%
  optional("%") %R% optional(or("-"," to "," and "," before storage to ")) %R%
  zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT) %R% "%"
    # match any 'connector' word
connector <- or("from ","of about ","by ","of ","to ")
    # the true matching machine:
viability <- or(
    # phrases with percent
  or("germination ","viability ") %R%
    or("change ","from ", "is reduced from ") %R% percent,
  "little loss in viability" %R% or(" \\(by "," \\(") %R% percent,
  "loss in viability of " %R% optional("about ") %R% percent,
  or("reduces ","reduction in ") %R% "viability " %R% connector %R% percent,
  "viability " %R% optional("is ") %R% "reduced " %R% connector %R% percent,
  "viability" %R% percent,
  percent %R% optional(" of maximum") %R%
    or(" germination "," viability "," germinate ") %R% optional("lost"),
  percent %R% or(" of seeds"," seeds lose viability"," survive",
    " survive desiccation"," surviving desiccation"),
    # other phrases to match (no percent)
      # all/most lost
  "complete loss",
  or("no seeds ","none ") %R%
    or("remain viable","survive","are viable"),
  or("total ","rapid ","sharp ") %R% or("loss ","reduction ") %R%
    or("in ","of ") %R% "viability",
  "total viability loss",
  "few seeds " %R% or("germinate","survive"),
  or("low ","poor ") %R% or("viability","germination"),
  "maximum advisable storage",
  "viability is " %R%
    optional(or("quickly ","rapidly ","completely ","very ")) %R%
    or("lost","reduced"),
  "viability lost",
      # half lost
  "half-life",
  "p50",
  "viability is " %R%
    optional(or("quickly ","rapidly ")) %R% "halved",
      # general flags for short-lived
  or("do ","does ") %R% "not store well",
  "intermediate-lived",
  "intermediate storage behaviour",
  "longevity is short",
  "not maintained well",
  or("seeds ","viability ") %R% "cannot be " %R%
    or("stored","maintained"),
  "short" %R% or(" ","-") %R% "lived",
  "viability declines " %R% or("rapidly","quickly"),
  "viability is " %R% optional("very ") %R% "short",
  "seeds soon lose their viability",
      # general flags for recalcitrant
  "sensitive to desiccatio",
  "should not be dried",
  "seeds damaged by desiccation",
  "seeds do not " %R% or("stand","tolerate"),
  "seeds killed",
  "further desiccation reduces viability",
  or("possibly ","likely ") %R% "recalcitrant",
  "damaged by desiccation",
      # viability maintained
  "viability " %R% optional(or("was ","can be ")) %R%
    or("maintained","retained","increases"),
  or("little ","no ") %R% or("loss in viability","viability lost"),
  "seeds " %R% optional("are ") %R% or("maintained","survive","surviving",
    "retained"),
  "long-term storage " %R% or("with","under")
)
  ## TEMPERATURE
temperature <- or(
    # degrees C
  capture(optional(" -") %R%
  zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT)) %R%
  "c",
    # simple phrases to match
      # -200c
  "liquid nitrogen",
      # 20c
  "room temperature",
  "herbarium",
  "temperate climate",
  "ambient",
  "laboratory conditions",
  "cool temperature",

      # -1c
  "sub-zero temperature",
      # -20c
  "commercial storage",
  "ipgri preferred conditions"
    # "The long-term preservation of the genetic diversity of orthodox species
    # can be ensured by storing their seeds at a low temperature (−20°C) and
    # moisture content (MC) (3%–7%) in genebanks...In 1994, The Genebank
    # Standards were published and recommended to dry seeds at 10°C–20°C and
    # 10%–15% relative humidity (RH). More recently, these drying standards
    # were modified, combining a lower temperature (5°C–20°C) and broader
    # humidity (10%–25% RH) range."
    # CITATION:
    # Whitehouse, Katherine J et al. “Further Evidence That the Genebank
    # Standards for Drying Orthodox Seeds May Not Be Optimal for Subsequent
    # Seed Longevity.” Biopreservation and biobanking vol. 16,5 (2018):
    # 327-336. doi:10.1089/bio.2018.0026
      # unsure??
  #"soil seed bank"
)
  ## DRYNESS
dessication <- or(
    # capture percent mc
  capture(zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT)) %R%
  optional(or("±","-")) %R%
  optional(capture(zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT))) %R%
  "% " %R% "mc",
    # simple phrases to match
      # 3-7% mc
  "ipgri preferred conditions",
  "ultra-dry",
  "commercial storage",
  #"liquid nitrogen",
      # 15-20% mc
  or(" ","air-") %R% "dry ",
  "air-drying",
  "herbarium",
  "ambient",
  "laboratory conditions",
  "open storage",
  "temperate climate",
      # moist
  "moist " %R% or("storage","seeds"),
    # capture percent rh
  capture(zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT)) %R%
  optional(or("±","-")) %R%
  optional(capture(zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT))) %R%
  "% " %R% "rh"
      # unsure??
  #"soil seed bank"
)

# view matches
#str_view_all(sid_ex$SID_storcond, pattern = viability)
#str_view_all(sid_ex$SID_storcond, pattern = time)
#str_view_all(sid_ex$SID_storcond, pattern = temperature)
#str_view_all(sid_ex$SID_storcond, pattern = dessication)

# extract matches
  ## TIME
  # for time we capture the number and calculate year if in different unit
time_capture <- str_match(sid_ex$SID_storcond,pattern=time)
time_capture[,2] <- mgsub(time_capture[,2],c("several","few"),"2")
time_capture[grepl("year",time_capture[,1]),2] <-
  round(as.numeric(time_capture[grepl("year",time_capture[,1]),2]),2)
time_capture[grepl("month",time_capture[,1]),2] <-
  round((as.numeric(time_capture[grepl("month",time_capture[,1]),2])/12),2)
time_capture[grepl("day",time_capture[,1]),2] <-
  round(as.numeric(time_capture[grepl("day",time_capture[,1]),2])/365,2)
time_capture[grepl("week",time_capture[,1]),2] <-
  round(as.numeric(time_capture[grepl("week",time_capture[,1]),2])/52.143,2)
sid_ex$time_verbatim <- time_capture[,1]
sid_ex$time_years <- as.numeric(time_capture[,2])
  ## VIABILITY
  # capture viability text, including percents
    #### FLAG ROWS WITH MORE THAN ONE MATCH??###
viability_capture <- str_match(sid_ex$SID_storcond,pattern=viability)
sid_ex$viability_verbatim <- viability_capture[,1]
viability_capture <- mgsub(viability_capture,
  c("half-life","p50","halved","maximum"),"50%")
viability_capture <- mgsub(viability_capture,
  c("total","complete","no seeds","none survive","none are viable",
    "none remain","viability lost","viability is lost"),"0%")
    #"rapid","few seeds","low","poor","quickly"
viability_capture <- mgsub(viability_capture,
  c("viability maintained","viability retained","no loss",
    "no viability lost","seeds maintained","seeds surviving",
    "seeds survive","seeds retained"),"100%")
capture_values <-
  capture(
    zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT)) %R%
  or("%","-"," to "," and "," before storage to ","±") %R%
  optional(capture(zero_or_more(DGT) %R% optional(DOT) %R% one_or_more(DGT)))
viability_capture_val <- str_match(viability_capture,pattern=capture_values)
    # find actual percent viable if viability lost given
viability_capture_val[grepl("loss in viability of|% viability l",
  viability_capture[,1]),2] <- round(100-
  as.numeric(viability_capture_val[grepl("loss in viability of|% viability l",
  viability_capture[,1]),2]),2)
    # if second values is given, use that value
viability_capture_val[!is.na(viability_capture_val[,3]),2] <-
  viability_capture_val[!is.na(viability_capture_val[,3]),3]
sid_ex$viability_percent <- as.numeric(viability_capture_val[,2])
  ## TEMPERATURE
  # extract temp and standardize phrases
temp_capture <- str_match(sid_ex$SID_storcond,pattern=temperature)
temp_capture[,1] <- str_squish(temp_capture[,1])
sid_ex$temperature_verbatim <- temp_capture[,1]
  # assign to bins
temp_capture[grepl("room|herbarium|temperate|laboratory|cool|ambient",
  temp_capture[,1]),1] <- "ambient (>=0 celsius)"
temp_capture[grepl("commercial|preferred|sub-zero",
  temp_capture[,1]),1] <- "preferred (<0 celsius)"
temp_capture_val <- as.numeric(temp_capture[,2])
temp_capture[temp_capture_val>=0 &
  temp_capture_val<100,1] <- "ambient (>=0 celsius)"
temp_capture[temp_capture_val<0,1] <- "preferred (<0 celsius)"
temp_capture[temp_capture_val>100,1] <- "unknown"
temp_capture[is.na(temp_capture[,1]),1] <- "unknown"
sid_ex$temperature_bin <- temp_capture[,1]
  ## DESSICATION
  # extract rh or mc and standardize phrases
dessication_capture <- str_match(sid_ex$SID_storcond,pattern=dessication)
sid_ex$dessication_verbatim <- dessication_capture[,1]
  # assign to bins
dessication_capture[grepl("air|herbarium|temperate|laboratory|open|ambient",
  dessication_capture[,1]),1] <- "ambient (8-20% mc;16-50% rh)"
dessication_capture[grepl("commercial|preferred|ultra| dry",
  dessication_capture[,1]),1] <- "preferred (<8% mc;<16% rh)"
dessication_capture[grepl("moist",
  dessication_capture[,1]),1] <- "moist (>20 mc;>50% rh)"
capture_mc <- as.numeric(dessication_capture[,2])
capture_rh <- as.numeric(dessication_capture[,4])
dessication_capture[capture_mc<=7,1] <- "preferred (<8% mc;<16% rh)"
dessication_capture[capture_mc>7 &
  capture_mc<=20,1] <- "ambient (8-20% mc;16-50% rh)"
dessication_capture[capture_mc>20,1] <- "moist (>20 mc;>50% rh)"
dessication_capture[capture_rh<16,1] <- "preferred (<8% mc;<16% rh)"
dessication_capture[capture_rh>15 &
  capture_rh<=50,1] <- "ambient (8-20% mc;16-50% rh)"
dessication_capture[capture_rh>50,1] <- "moist (>20 mc;>50% rh)"
dessication_capture[is.na(dessication_capture[,1]),1] <- "unknown"
sid_ex$dessication_bin <- dessication_capture[,1]

# write file
write.csv(sid_ex,"SID_matching_test.csv",row.names=F)

# look at data
library(ggplot2)
# basic scatter plot
## temperature bins
ggplot(sid_ex, aes(x=time_years, y=viability_percent)) +
  geom_point(alpha = 0.3,color="blue") +
  xlim(0, 25) +
  facet_wrap(~temperature_bin)
## dessication bins
ggplot(sid_ex, aes(x=time_years, y=viability_percent)) +
  geom_point(alpha = 0.3,color="blue") +
  xlim(0, 25) +
  facet_wrap(~dessication_bin)

# Change the point size, and shape
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(size=2, shape=23)





###############################################################################
# 2. Combine duplicates in SID dataset
###############################################################################

# concatenate references for duplicate taxa in SID list (this is common because
#   when a taxon has more than one reference there is a unique line for each)
sid_unique <- sid %>% group_by(taxon_name,SID_storbehav,SID_storcond,
  SID_taxon_name_notes,SID_family) %>%
  summarize(SID_refdesc_all = paste(SID_refdesc, collapse = '; ')) %>%
  ungroup()
nrow(sid_unique) #24784
sid_unique <- as.data.frame(sid_unique)

# concatenate differing descriptions for duplicate taxa and remove duplicates
sid_unique <- sid_unique %>% group_by(taxon_name,SID_family) %>%
  mutate(SID_storcond = paste(SID_storcond, collapse = ' | ')) %>%
  mutate(SID_refdesc_all = paste(SID_refdesc_all, collapse = ' | ')) %>%
  mutate(SID_storbehav = paste(SID_storbehav, collapse = ' | ')) %>%
  ungroup() %>% distinct(taxon_name, .keep_all = T)
nrow(sid_unique) #24713
sid_unique <- as.data.frame(sid_unique)

# fix capitalization
sid_unique$SID_family <- str_to_title(sid_unique$SID_family)

###############################################################################
# 3. Standardize characters
###############################################################################

# create list of datasets to cycle through for character replacements
datasets <- list(h_wallace,notes1,notes2,sid_unique)
  glimpse(datasets)

for(i in 1:length(datasets)){
  # replace subspecies abbreviation
  datasets[[i]]$taxon_name <- gsub(" ssp. ", " subsp. ",
    datasets[[i]]$taxon_name, fixed = T)
  # replace species abbreviation
  datasets[[i]]$taxon_name <- gsub(" sp.$", " spp.", datasets[[i]]$taxon_name)
  datasets[[i]]$taxon_name <- gsub(" sp. ", " spp.", datasets[[i]]$taxon_name,
    fixed = T)
  # replace incorrect "x" symbol
  datasets[[i]]$taxon_name <- gsub("\\+", "x", datasets[[i]]$taxon_name)
  datasets[[i]]$taxon_name <- gsub(" X ", " x ", datasets[[i]]$taxon_name,
    fixed = T)
  # remove/add other characters
  datasets[[i]]$taxon_name <- mgsub(datasets[[i]]$taxon_name, c("\'","\""), "")
  datasets[[i]]$taxon_name <- gsub("var.", "var. ", datasets[[i]]$taxon_name,
    fixed = T)
  # trim whitespace
  datasets[[i]]$taxon_name <- str_squish(datasets[[i]]$taxon_name)
}

###############################################################################
# 4. Join all lists
###############################################################################

# join everything together by taxon_name
all_data <- Reduce(full_join,datasets)
  str(all_data) #28414

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
  glimpse(all_data) # Observations: 28,411

###############################################################################
# 5. Standardize exceptional status column to compare among versions
###############################################################################

all_data2 <- all_data

#### FIX THIS ####
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
#         "Possible Exceptional" = "Exceptional?",
         "Congener of Unknown" = "Congener"
       )); table(all_data2$exceptional_status2)
all_data2$exceptional_status3 <- all_data2$R2_Exceptional_Status
unique(all_data2$SID_storbehav)
all_data2 <- all_data2 %>%
  mutate(exceptional_status4 = recode(SID_storbehav,
         "Orthodox?" = "Non-exceptional?",
         "Recalcitrant" = "Exceptional",
         "Recalcitrant?" = "Exceptional?",
         "Orthodox p" = "Non-exceptional",
         "Orthodox" = "Non-exceptional",
         "Uncertain" = "Unknown",
         "Intermediate?" = "Exceptional?",
         "Intermediate" = "Exceptional",
         "Intermediate`" = "Exceptional",
         "Recalcitrant?`" = "Exceptional?",
         "Orthodox | Orthodox?" = "Non-exceptional?",
         "Orthodox | Orthodox | Orthodox p" = "Non-exceptional",
         "Orthodox | Orthodox p" = "Non-exceptional",
         "Recalcitrant | Recalcitrant?" = "Exceptional?",
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

###############################################################################
# 10. Mark rows with key words in SID storage condition description
###############################################################################

# replace unusual characters
all_data3$SID_storcond <- stringi::stri_trans_general(
  all_data3$SID_storcond, "Latin-ASCII")
all_data3$SID_storcond <- gsub(" %","%",all_data3$SID_storcond)
all_data3$SID_storcond <- str_squish(all_data3$SID_storcond)

# create column for key word searches
all_data3$key_word <- ""

# function to mark records with key SID words that may indicate exceptionality
mark.sid.keywords <- function(df,keywords){
  for(i in 1:length(keywords)){
    df <- df %>% mutate(key_word =
      ifelse((grepl("Non-exceptional",exceptional_status) |
              grepl("Uncertain",exceptional_status) |
              grepl("Non-exceptional?",exceptional_status)) &
              grepl(keywords[[i]],SID_storcond,ignore.case=T),
                paste(keywords[[i]],key_word,sep=";"),key_word))
  }
  return(df)
}

SID_keywords <- c(
  "not maintained well",
  "viability is reduced",
  "reduction in viability",
  "loss in viability",
  "poor viability",
  "viability is lost",
  "viability lost",
  "moist storage",
  "short viability",
  "longevity is short",
  "viability is completely lost",
  "recalcitrant",
  "reduction of viability",
  "intermediate",
  "reduces viability",
  "reduced longevity",
  "lose viability",
  "sensitive to desiccation",
  "viability declines",
  "short periods",
  "none survive",
  "short-lived",
  "short lived",
  "few seeds survive",
  "genus",
  "dorman",
  "maintained (for|for up to) (1[0-2]|[0-9])(-([0-9][0-9]|[0-9]))? month",
  "maintained (for|for up to) 1(-[0-9])? year",
  "viability lost within 1(-[0-9])? year",
  " (1[0-9]|[0-9])\\% germination", # (after|following) (1[0-9]|[1-9]) year"
  " (1[0-9]|[0-9])\\% viability",
  " (1[0-9]|[0-9])\\% of seeds"
)
#### OTHERS TO CONSIDER
#"viability is halved"
#"several years"
#"open storage"
#"air-dry storage"

all_data3 <- mark.sid.keywords(all_data3,SID_keywords)
  unique(all_data3[which(all_data3$key_word!=""),]$SID_storcond)
  table(all_data3$key_word)

# function to remove marks for records with negated key SID words
remove.sid.keywords <- function(df,remove,replace){
  for(i in 1:length(remove)){
    df <- df %>% mutate(key_word =
      ifelse(grepl(remove[[i]],SID_storcond,ignore.case=T),
               gsub(paste0(replace[[i]],";"),"",key_word),key_word))
  }
  return(df)
}

SID_remove <- c("without loss in viability","no loss in viability",
  "little loss in viability","no reduction in viability")
SID_replace <- c("loss in viability","loss in viability",
  "loss in viability","reduction in viability")

all_data3 <- remove.sid.keywords(all_data3,SID_remove,SID_replace)
  unique(all_data3[which(all_data3$key_word!=""),]$SID_storcond)
  table(all_data3$key_word)

#all_data3$key_word <- mgsub(all_data3$key_word,
#  c("maintained (for|after|for up to) (1[0-2]|[1-9]).* month",
#    "maintained (for|after|for up to) 1.* year",
#    "viability lost within 1.* year",
#    " (1[0-9]|[1-9])\\% germination",
#    " (1[0-9]|[1-9])\\% viability",
#    " (1[0-9]|[1-9])\\% of seeds"),
#  c("maintained >1 year","maintained >1 year","maintained >1 year",
#    "0-19% germination","0-19% viability","0-19% of seeds"))
#table(all_data3$key_word)

# write file
write.csv(all_data3,"exceptional_species_match.csv")

###############################################################################
# 5. Create data frame of The Plant List (TPL), International
#    Plant Names Index (IPNI), and World Flora Online (WFO) accepted names
###############################################################################

# download all accepted TPL names; takes a LONG time
#tpl_names <- tpl_get("tpl_all")
# read csv of accepted names
tpl_all <- read.csv("tpl_all.csv", header = T, na.strings = c("","NA"),
  colClasses = "character")
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
        "taxonomic_confidence","taxonomic_source","taxonomic_status","genus",
        "hybrid","species","infra_rank","infra_name"),
  skip_absent=T)
# add column stating source database
tpl_names2$taxonomic_database <- "TPL"
# remove duplicate names that are not accepted
tpl_names3 <- setdiff(tpl_names2,tpl_names2[
  which(tpl_names2$taxonomic_status != "Accepted" &
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
ipni_all <- read.csv("ipni_names_raw.csv", header = T, na.strings = c("","NA"),
  colClasses="character")
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
        "taxon_name"),
  skip_absent=T)
# replace hybrid character
ipni_names2$taxon_name <- gsub(" × "," x ",ipni_names2$taxon_name,fixed=T)
ipni_names2$taxon_name <- gsub("^× ","x ",ipni_names2$taxon_name)
# add column stating source database
ipni_names2$taxonomic_database <- "IPNI"
# add taxonomic status column
ipni_names2$taxonomic_status <- "Accepted"
# remove duplicates names, starting with older verions (this may be arbitrary?)
ipni_names3 <- distinct(ipni_names2,taxon_name,.keep_all=T)
nrow(ipni_names2); nrow(ipni_names3)

# download WFO taxonomic data
#   go to:  http://www.worldfloraonline.org/downloadData;jsessionid=94916E1F29B8ADFF5353032114B66D0E
#   scroll down to "Latest Static Version:" and click link to download
#   unzip the folder
# read in table
wfo_all <- read.delim("WFO_Backbone/classification.txt",
  colClasses="character")
  wfo_names <- wfo_all
# keep only relevant columns
wfo_names2 <- wfo_names %>% select("scientificName","family","taxonID","genus",
  "specificEpithet","infraspecificEpithet","taxonRank",
  "scientificNameAuthorship","taxonomicStatus","references")
# standardize column names for joining later
glimpse(wfo_names2)
setnames(wfo_names2,
  old=c("scientificName","taxonID","specificEpithet","infraspecificEpithet",
    "taxonRank","scientificNameAuthorship","taxonomicStatus","references"),
  new=c("taxon_name","taxonomic_source_id","species","infra_name",
    "infra_rank","taxonomic_author","taxonomic_status","taxonomic_source"),
  skip_absent=T)
# add column stating source database
wfo_names2$taxonomic_database <- "WFO"
# remove duplicate names that are not accepted
wfo_names3 <- setdiff(wfo_names2,wfo_names2[
  which(wfo_names2$taxonomic_status != "Accepted" &
  (duplicated(wfo_names2$taxon_name, fromLast = TRUE) |
   duplicated(wfo_names2$taxon_name))),])
nrow(wfo_names2); nrow(wfo_names3)

# stack all names from TPL, IPNI, and WFO
taxonomic_names <- rbind.fill(tpl_names3,ipni_names3,wfo_names3)
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
    taxonomic_status = paste(taxonomic_status,collapse = ';')
  ) %>%
  ungroup()
glimpse(unique_names)
# remove duplicates in ref column
unique_names$taxonomic_database <- gsub("TPL;TPL","TPL",
  unique_names$taxonomic_database)
unique_names$taxonomic_database <- gsub("WFO;WFO","WFO",
  unique_names$taxonomic_database)
unique(unique_names$taxonomic_database)

###############################################################################
# 6. Query Integrated Taxonomic Information Service (ITIS) for names
#    with no match
###############################################################################

# create list of target taxa with taxonomic no match
  # select rows without taxonomic info match
test_match <- join(all_data,unique_names,type="left")
need_match <- test_match %>% filter(is.na(taxonomic_source_id))
nrow(need_match) #1243
  # unselect rows without species name
need_match <- need_match %>% filter(!grepl(" spp.",taxon_name,fixed=T))
nrow(need_match) #1010
  # unselect rows for multi-part hybrids (won't find match)
need_match <- need_match %>% filter(!(unlist(lapply(taxon_name, function(x)
  length(gregexpr(" ",x)[[1]]))) > 2 & grepl(" x ",taxon_name,fixed=T)))
nrow(need_match) #982
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
# 7. Query Global Names Resolver (GNR) for remaining names with no match
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
# 8. Join taxonomic data to target taxa list
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
    taxonomic_status = paste(taxonomic_status,collapse = ';')
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



# join to taxa list
all_data2 <- join(all_data,unique_names,type="left")
  sum(is.na(all_data2$taxonomic_source_id)) # number of names without match;758
# sort so names without match are at top before writing file
#all_data2 <- setorderv(all_data2,"taxonomic_source_id")
#write.csv(all_data2,"exceptional_species_match_taxonomy.csv")

# mark rows that didn't match taxonomic data but have good reason
all_data3 <- all_data3 %>% mutate(taxonomic_database =
  ifelse(grepl(" spp.",taxon_name,fixed=T), "N/A: spp.", taxonomic_database))
all_data3 <- all_data3 %>% mutate(taxonomic_database =
  ifelse(grepl(" x ",taxon_name,fixed=T), "N/A: hybrid", taxonomic_database))

###############################################################################
# 10. Mark rows that need manual check
###############################################################################


all_data3 <- all_data3 %>% mutate(to_do =
  ifelse((grepl("Non-exceptional",exceptional_status) |
          grepl("Uncertain",exceptional_status) |
          grepl("Non-exceptional?",exceptional_status)) &
         (grepl("short lived",SID_storcond,ignore.case=T) |
          grepl("short-lived",SID_storcond,ignore.case=T) |
          grepl("low viability",SID_storcond,ignore.case=T) |
          grepl("low viability",SID_storcond,ignore.case=T) |
          grepl("Short-lived (SID, 2019)",R2_Additional_Justification),
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
